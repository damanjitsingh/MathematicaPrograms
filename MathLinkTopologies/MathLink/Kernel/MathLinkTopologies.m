(* ::Package:: *)

(* Wolfram Language Package *)

BeginPackage["MathLinkTopologies`"]
(* Exported symbols added here with SymbolName::usage *) 

StarTopology::usage = "StarTopology[n] consists of one master node and n slave nodes connected to the master node"
LineTopology::usage = "LineTopology[n] consists of n nodes connected in series"

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

LineTopology[nodeCount_]:= Module[{link,count=1},
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
		Print["Finally Closing Data"];
		count = 1;
		While[count<nodeCount,
			LinkWrite[link,Unevaluated@EvaluatePacket[SendSerialData[Evaluate[LinkClose[link]],2,nodeCount-count]]];
			Pause[2];
			count+=1
		];
		(*Finally close the first link*)
		LinkClose[link]
	)
]

(*Auxiliary function*)
ReceiveData[link_] := Module[{},
	(
		While[LinkReadyQ[link] == True,
		    Print[LinkRead[link]]]
	)
]

(*Auxiliary function*)
LaunchSerialLinks[currentNode_,totalNodes_] := 
		if[currentNodes < totalNodes,
			link =  LinkLaunch[First[$CommandLine]<> " -wstp"];
			localTotalNodes = totalNodes;
			(*Create a forward link*)
			LinkWrite[link,Unevaluated@Evaluate[LaunchSerialLinks[currentNode+1,totalNodes]]];
			Pause[2];
			,Null
		 ]

SendSerialData[data_,currentNode_,totalNodes_] :=
	if[currentNodes < totalNodes,
		LinkWrite[link,Unevaluated@Evaluate[SendSerialData[data,currentNode+1,totalNodes]]];
		Pause[2];
		,data
	]

TreeTopology[nodeCount_] := Module[{},
	(
		

	)
]

End[] (* End Private Context *)
EndPackage[]
