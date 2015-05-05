(* Wolfram Language Package *)

BeginPackage["MathLinkTopologies`"]
(* Exported symbols added here with SymbolName::usage *) 

StarTopology::usage  = "StarTopology has one master node and four slave nodes connected to the master node"

Begin["`Private`"] (* Begin Private Context *)

links = Table[LinkLaunch["Path to WolframKernel.exe"],{4}];
(*Writing to each link*)
Map[LinkWrite[#,"sending hey from master"]&,links];

(*Now read the mesage form each link*)
While[LinkReadyQ[links[[1]]] == True,
    Print[LinkRead[links[[1]]]] ];
    
While[LinkReadyQ[links[[2]]] == True,
    Print[LinkRead[links[[2]]]] ];

While[LinkReadyQ[links[[3]]] == True,
    Print[LinkRead[links[[3]]]] ];
    
While[LinkReadyQ[links[[4]]] == True,
    Print[LinkRead[links[[4]]]] ];

End[] (* End Private Context *)

EndPackage[]