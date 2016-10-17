BeginPackage["TpMp`MMA`Output`"];

Unprotect["TpMp`MMA`Output`*"];
ClearAll["TpMp`MMA`Output`*", "TpMp`MMA`Output`Private`*"];

(******************************************************************************)

EmitForm::usage = "Prepare FORM output";

(******************************************************************************)
Begin["`Private`"];

(* Find fermion chains and order legs, propagators and vertices *)
FermionChains[dia_]:=
    Module[{},
           
          ];


Options[EmitForm] = {NumDB->1, SplitFiles->False};
EmitForm[fname_String, nl_List, OptionsPattern[]]:=
    Module[{str,dia,fc},
           
           If[Not[OptionValue[SplitFiles]], str = OpenWrite[fname]];
           
           Do[
               dia = GetDia[n, OptionValue[NumDB]];
               fc  = FermionChains[dia];

               If[OptionValue[SplitFiles], str = OpenWrite[fname<>ToString[n]]];
               WriteString[str,"*--#[ n"<>ToString[n]<>" :\n"];
               ToString[fc];
               WriteString[str,"*--#] n"<>ToString[n]<>" :\n"];
               If[OptionValue[SplitFiles], Close[str]];
               
               ,{n,nl}];        (* Do *)
           
           If[Not[OptionValue[SplitFiles]], Close[str]];
           
          ];




Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["TpMp`MMA`Output`*"], Head[#] === Symbol &]];

End[];
EndPackage[];


