BeginPackage["TpMp`MMA`Drawing`"];

Unprotect["TpMp`MMA`Drawing`*"];
ClearAll["TpMp`MMA`Drawing`*", "TpMp`MMA`Drawing`Private`*"];

(******************************************************************************)

Draw::usage = "Draw[{...,props->{...},...}]";

F::usage  = "Fermion field.";
M::usage  = "Majorana fermion field.";
S::usage  = "Scalar field.";
V::usage  = "Vector field.";
C::usage  = "Ghost field.";
A::usage  = "Aux field.";

props::usage = "";
legs::usage = "";

Begin["`Private`"];

DressEdge[e_, l_] := 
    Module[{},
           Switch[Head[l],
                  C, {Red, Dotted, 
                      Arrow[e], {Line[e], 
                                 Inset[l[[1]], Mean[e], Automatic, Automatic, Automatic,
                                       Background -> White]}},
                  F, {Blue, Thick, 
                      Arrow[e], {Line[e], 
                                 Inset[l[[1]], Mean[e], Automatic, Automatic, Automatic,
                                       Background -> White]}},
                  M, {Gray, Thick, 
                      Arrow[e], {Line[e], 
                                 Inset[l[[1]], Mean[e], Automatic, Automatic, Automatic,
                                       Background -> White]}},
                  S, {AbsoluteThickness[2], Dashed, Red, 
                      Arrow[e], {Line[e], 
                                 Inset[l[[1]], Mean[e], Automatic, Automatic, Automatic, 
                                       Background -> White]}},
                  V, {Green, DotDashed, 
                      Arrow[e], {Line[e], 
                                 Inset[l[[1]], Mean[e], Automatic, Automatic, Automatic, 
                                       Background -> White]}},
                  A, {Black, DotDashed, 
                      Arrow[e], {Line[e], 
                                 Inset[l[[1]], Mean[e], Automatic, Automatic, Automatic,
                                       Background -> White]}}]
          ];


Draw[List[___,props->pl_,___,legs->ll_,___]] :=     
    GraphPlot[Join[Take[pl,All,2],Take[ll,All,2]], DirectedEdges -> True, MultiedgeStyle -> 0.2, 
              ImagePadding -> 0, EdgeRenderingFunction -> (DressEdge[#1, #3] &),
              VertexLabeling -> False, Method -> "SpringEmbedding"];

(* Draw[Subgraphs[ll___]] :=  *)
(*     GraphPlot[#[[1]]/.{Tad->List,Dia->List,Sbridge->List}, DirectedEdges -> True, MultiedgeStyle -> 0.2,  *)
(*               ImagePadding -> 0, EdgeRenderingFunction -> (DressEdge[#1, #3] &), *)
(*               VertexLabeling -> False, Method -> "SpringEmbedding"]& /@ List[ll]; *)

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["TpMp`MMA`Drawing`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
