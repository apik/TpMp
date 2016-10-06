:Evaluate:  Print["                                           \n",
                  "                   _____     _____         \n",
                  "                  |_   _|___|     |___     \n",
                  "                    | | | . | | | | . |    \n",
                  "                    |_| |  _|_|_|_|  _|    \n",
                  "                        |_|       |_|      \n",
                  "                                           \n",
                  "TpMp - Topology Mapping\n",
                  "To see available functions use Names[\"TpMp`*\"]\n",
                  "Andrey Pikelner <pikelner@theor.jinr.ru>"];

:Evaluate:  BeginPackage["TpMp`"];

:Evaluate:  Unprotect["TpMp`*"];
:Evaluate:  ClearAll["TpMp`*", "TpMp`Private`*"];
    
:Evaluate:  LoadQGRAF::usage  = "Load QGRAF output in YAML format (fname.yaml) and create DB with name (fname.sqlite3)."
:Evaluate:  LoadDB::usage     = "Load SQL DB."
:Evaluate:  GetDia::usage     = "Load dia from DB."
:Evaluate:  GetDiaGraph::usage = "Get graph info for diagram."
:Evaluate:  WithField::usage  = "Find diagrams with field."
:Evaluate:  WithFieldType::usage  = "Find diagrams with specified field type (F,M,S,V,C,A)."
:Evaluate:  FermionFlow::usage    = "Get fermion flow information"
:Evaluate:  Tadpoles::usage    = "Get tadpole subdiagrams information"
:Evaluate:  NoSigma::usage    = "Get self-energy insertions information"

:Evaluate:  Echo::usage  = "Echo command to see errors in MathLink string input interpretation command."

:Evaluate:  TestCMD::usage  = "Test dummy command."

:Evaluate:  Dia::usage  = "Diagram container."
:Evaluate:  Subgraphs::usage  = "Diagram container."
:Evaluate:  Tad::usage  = "Tadpole head."
:Evaluate:  Dia::usage  = "Rest diagram without tadpoles head."
:Evaluate:  Sbridge::usage  = "Singulare bridge edge head."


// Error messages
:Evaluate:  LoadQGRAF::dbexists = "Error: SQL db with name `1` already exists.";
:Evaluate:  LoadQGRAF::noinput  = "Error: Input YAML file `1` not found.";

:Evaluate:  LoadDB::noinput  = "Error: Input SQL file `1` not found.";


:Evaluate:  Begin["`Private`"];
    
// Mathematica part




// C++ part
:Begin:
:Function: LoadQGRAF
:Pattern: LoadQGRAF[fname_, OptionsPattern[]]
:Arguments: {fname, If[OptionValue[OverWriteDB],1,0]}
:ArgumentTypes: {ByteString, Integer}
:ReturnType: Manual
:End:

:Evaluate: Options[LoadQGRAF]={OverWriteDB->False};
    
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
:Function: NoSigma
:Pattern: NoSigma[n_Integer,dbnum_Integer:1]
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
