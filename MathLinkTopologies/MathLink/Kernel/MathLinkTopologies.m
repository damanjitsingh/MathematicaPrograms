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

LineTopology[nodeCount_]:= Module[{link},
	(
		link = LinkLaunch[First[$CommandLine] <> " -wstp"];
		LinkWrite[link,Unevaluated@EvaluatePacket[SendSerialData[2,nodeCount]]];
		
		(*Send data from node 1*)
		LinkWrite[link,"Data from source"];
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
SendSerialData[currentNode_,totalNodes_] := Module[{link},
	(
		if[currentNodes < totalNodes,
			link =  LinkLaunch[First[$CommandLine]<> " -wstp"];
			(*Create a forward link*)
			LinkWrite[link,Evaluate[SendSerialData[currentNode+1,totalNodes]]];
			While[True,
			If[LinkReadyQ[link] == True, 
				s = LinkRead[link];
				If[s == LinkClose[]]
				 ,Null];
				
			]
			,Null
		 ]
	)
]

End[] (* End Private Context *)
EndPackage[]