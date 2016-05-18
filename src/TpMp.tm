:Evaluate:  Print["                                           "];
:Evaluate:  Print["                   _____     _____         "];
:Evaluate:  Print["                  |_   _|___|     |___     "];
:Evaluate:  Print["                    | | | . | | | | . |    "];
:Evaluate:  Print["                    |_| |  _|_|_|_|  _|    "];
:Evaluate:  Print["                        |_|       |_|      "];
:Evaluate:  Print["                                           "];

:Evaluate:  Print["TpMp - Topology Mapping"];
:Evaluate:  Print["To see available functions use Names[\"TpMp`*\"]"];
:Evaluate:  Print["Andrey Pikelner <pikelner@theor.jinr.ru>"];

:Evaluate:  BeginPackage["TpMp`"];

:Evaluate:  Unprotect["TpMp`*"];
:Evaluate:  ClearAll["TpMp`*", "TpMp`Private`*"];
    
:Evaluate:  LoadQGRAF::usage  = "Load QGRAF output in YAML format (fname.yaml) and create DB with name (fname.sqlite3)."
:Evaluate:  LoadDB::usage     = "Load SQL DB."
:Evaluate:  GetDia::usage     = "Load dia from DB."
:Evaluate:  GetDiaGraph::usage = "Get graph info for diagram."
:Evaluate:  Draw::usage  = "Plot graph."
:Evaluate:  WithField::usage  = "Find diagrams with field."
:Evaluate:  WithFieldType::usage  = "Find diagrams with specified field type (F,M,S,V,C,A)."
:Evaluate:  FermionFlow::usage    = "Get fermion flow information"
:Evaluate:  Tadpoles::usage    = "Get tadpole subdiagrams information"

:Evaluate:  Echo::usage  = "Echo command to see errors in MathLink string input interpretation command."

:Evaluate:  TestCMD::usage  = "Test dummy command."

:Evaluate:  Dia::usage  = "Diagram container."

:Evaluate:  F::usage  = "Fermion field."
:Evaluate:  M::usage  = "Majorana fermion field."
:Evaluate:  S::usage  = "Scalar field."
:Evaluate:  V::usage  = "Vector field."
:Evaluate:  C::usage  = "Ghost field."
:Evaluate:  A::usage  = "Aux field."


// Error messages
:Evaluate:  LoadQGRAF::dbexists = "Error: SQL db with name `1` already exists.";
:Evaluate:  LoadQGRAF::noinput  = "Error: Input YAML file `1` not found.";

:Evaluate:  LoadDB::noinput  = "Error: Input SQL file `1` not found.";


:Evaluate:  Begin["`Private`"];
    
// Mathematica part


:Evaluate: Draw[Dia[_,_,_,ll_,_]] :=     
    GraphPlot[ll, DirectedEdges -> True, MultiedgeStyle -> 0.2, 
              ImagePadding -> 0, EdgeRenderingFunction -> (DressEdge[#1, #3] &),
              VertexLabeling -> False, Method -> "SpringEmbedding"];

:Evaluate: DressEdge[e_, l_] := 
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

// C++ part
:Begin:
:Function: LoadQGRAF
:Pattern: LoadQGRAF[fname_]
:Arguments: {fname}
:ArgumentTypes: {ByteString}
:ReturnType: Manual
:End:
    
:Begin:
:Function: LoadDB
:Pattern: LoadDB[fname_]
:Arguments: {fname}
:ArgumentTypes: {ByteString}
:ReturnType: Manual
:End:

:Begin:
:Function: GetDia
:Pattern: GetDia[n_Integer,dbnum_Integer:1]
:Arguments: {n,dbnum}
:ArgumentTypes: {Integer,Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: GetDiaGraph
:Pattern: GetDiaGraph[n_Integer,dbnum_Integer:1]
:Arguments: {n,dbnum}
:ArgumentTypes: {Integer,Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: WithField
:Pattern: WithField[field_,dbnum_Integer:1]
:Arguments: {field,dbnum}
:ArgumentTypes: {ByteString,Integer}
:ReturnType: Manual
:End:
        
:Begin:
:Function: WithFieldType
:Pattern: WithFieldType[fieldtype_,dbnum_Integer:1]
:Arguments: {fieldtype,dbnum}
:ArgumentTypes: {ByteString,Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: FermionFlow
:Pattern: FermionFlow[n_Integer,dbnum_Integer:1]
:Arguments: {n,dbnum}
:ArgumentTypes: {Integer,Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: Tadpoles
:Pattern: Tadpoles[n_Integer,dbnum_Integer:1]
:Arguments: {n,dbnum}
:ArgumentTypes: {Integer,Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: Echo
:Pattern: Echo[str_]
:Arguments: {str}
:ArgumentTypes: {ByteString}
:ReturnType: Manual
:End:

:Begin:
:Function: TestCMD
:Pattern: TestCMD[str_]
:Arguments: {str}
:ArgumentTypes: {ByteString}
:ReturnType: Manual
:End:


:Evaluate: Scan[SetAttributes[#, {Protected, ReadProtected}]&,
                Select[Symbol /@ Names["TpMp`*"], Head[#] === Symbol &]];


:Evaluate: End[]
:Evaluate: EndPackage[]
