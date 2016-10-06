BeginPackage["TpMp`MMA`Output`"];

Unprotect["TpMp`MMA`Output`*"];
ClearAll["TpMp`MMA`Output`*", "TpMp`MMA`Output`Private`*"];

(******************************************************************************)


Print["output loaded"];

EmitForm::usage = "Prepare FORM output";

Begin["`Private`"];

EmitForm[fname_String, n_Integer]:=
    Module[{str},
           str = OpenWrite[fname];
           
           WriteString[str,"*--#[ n"<>ToString[n]<>" :"];
           
           WriteString[str,"*--#] n"<>ToString[n]<>" :"];
          ]


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["TpMp`MMA`Output`*"], Head[#] === Symbol &]];

End[];
EndPackage[];


