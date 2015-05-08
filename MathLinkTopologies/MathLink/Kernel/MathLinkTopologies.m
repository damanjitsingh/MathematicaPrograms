(* ::Package:: *)

(* Wolfram Language Package *)

BeginPackage["MathLinkTopologies`"]
(* Exported symbols added here with SymbolName::usage *) 

StarTopology::usage = "StarTopology[n] consists of one master node and n slave nodes connected to the master node"
LineTopology::usage = "LineTopology[n] consists of n nodes connected in series"
TreeTopology::usage = "TreeTopology[n] contains a complete binary tree of height n"

Begin["`Private`"] (* Begin Private Context *)

StarTopology[slaveCount_]:= Module[{mylinks},
	(
		mylinks = Table[LinkLaunch[First[$CommandLine] <> " -wstp"],{slaveCount}];
		
		(*Writing to each link*)
		Map[LinkWrite[#,"sending hey from master"]&,mylinks];
		Pause[3];
		
		Map[ReceiveData[#]&,mylinks];
	    
	    Print["Finally close all the opened links"];
	  	Map[LinkClose[#]&,mylinks]
  	)
]

LineTopology[nodeCount_]:= Module[{link,count=2},
	(
		link = LinkLaunch[First[$CommandLine] <> " -wstp"];
		Print["Launching Links!!"];
		LinkWrite[link,Unevaluated@EvaluatePacket[LaunchSerialLinks[2,nodeCount]]];
		Pause[2];
		
		(**************Send data from node 1*******************************)
		Print["Sending Data"];
		LinkWrite[link,Unevaluated@EvaluatePacket[SendSerialData["Data from node 1",2,nodeCount]]];
		Pause[2];

		
		(******************Now Retrieve the data***************************)
		Print["Reading Data"];
		While[count<nodeCount,
			LinkWrite[link,Unevaluated@EvaluatePacket[SendSerialData[Evaluate[ReceiveData[link]],2,nodeCount-count]]];
			Pause[2];
			count+=1
		];
		(*Finally Receive the data for link 1*)
		ReceiveData[link];

		(******************Close the serial links***************************)
		Print["Finally Closing Links"];
		count = 2;
		While[count<nodeCount,
			LinkWrite[link,Unevaluated@EvaluatePacket[SendSerialData[Evaluate[LinkClose[link]],2,nodeCount-count]]];
			Pause[2];
			count+=1
		];
		(*Finally close the first link*)
		LinkClose[link]
	)
]

(**)
TreeTopology[height_] := Module[{mylinks,count=2},
	(
		(*Create links to intermediate nodes*)
		mylinks = Table[LinkLaunch[First[$CommandLine] <> " -wstp"],{2}];		

		Print["Launching Links!!"];
		Map[LinkWrite[#,Unevaluated@EvaluatePacket[LaunchChildLinks[2,height]]]&,mylinks];
		Pause[2];

		(**************Send data from root node to all the leaf nodes*******************************)
		Print["Sending Data"];
		Map[LinkWrite[#,Unevaluated@EvaluatePacket[SendTreeData["Data from root node",2,height]]]&,mylinks];
		Pause[6];
		
		(******************Now Retrieve the data***************************)
		Print["Reading Data"];
		While[count<height,
			Map[LinkWrite[#,Unevaluated@EvaluatePacket[SendTreeData[Evaluate[Map[ReceiveData[#]&,links]],2,height-count]]]&,mylinks];
			Pause[2];
			count+=1
		];
		(*Finally Receive the data in root node*)
		Map[ReceiveData[#]&,mylinks];

		(******************Close all the links***************************)
		Print["Finally Closing links"];
		count = 2;
		While[count<height,
			Map[LinkWrite[#,Unevaluated@EvaluatePacket[SendTreeData[Evaluate[Map[LinkClose[#]&,links]],2,height-count]]]&,mylinks];
			Pause[2];
			count+=1
		];
		(*Finally close the links from root node*)
		Map[LinkClose[#]&,mylinks]
	)
]

(*Auxiliary function*)
LaunchChildLinks[currentLevel_,height_] := 
	if[currentLevel < height,
		links = Table[LinkLaunch[First[$CommandLine]<> " -wstp"],{2}];
		Map[LinkWrite[#,Unevaluated@Evaluate[LaunchChildLinks[currentLevel+1,height]]]&,links];
		Pause[2]
		,Null
	]

(*Auxiliary function*)
LaunchSerialLinks[currentNode_,totalNodes_] := 
		if[currentNodes < totalNodes,
			link =  LinkLaunch[First[$CommandLine]<> " -wstp"];
			(*Create a forward link*)
			LinkWrite[link,Unevaluated@Evaluate[LaunchSerialLinks[currentNode+1,totalNodes]]];
			Pause[2];
			,Null
		 ]

(*Auxiliary function*)
ReceiveData[link_] := Module[{},
	(
		While[LinkReadyQ[link] == True,
		    Print[LinkRead[link]]]
	)
]

(*Auxiliary function*)
SendSerialData[data_,currentNode_,totalNodes_] :=
	if[currentNodes < totalNodes,
		LinkWrite[link,Unevaluated@Evaluate[SendSerialData[data,currentNode+1,totalNodes]]];
		Pause[2];
		,data
	]

(*Auxiliary function*)
SendTreeData[data_,currentlevel_,height_] :=
	if[currentlevel < height,
		Map[LinkWrite[#,Unevaluated@Evaluate[SendTreeData[data,currentlevel+1,height]]]&,links];
		Pause[2];
		,data
	]

End[] (* End Private Context *)
EndPackage[]
