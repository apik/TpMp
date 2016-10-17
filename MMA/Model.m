BeginPackage["TpMp`MMA`Model`"];

Unprotect["TpMp`MMA`Model`*"];
ClearAll["TpMp`MMA`Model`*", "TpMp`MMA`Model`Private`*"];

(******************************************************************************)

NewModel::usage = "Prepare QGRAF model";
WriteModel::usage = "Prepare QGRAF model";

(******************************************************************************)
Begin["`Private`"];


NewModel[plist_,vlist_]:=
    Module[{},
           Print["sdsd"];
          ]

AntiStr[_[p1_,p2_]]:=If[p1===p2
                       ,"anti = (0)"
                       ,"anti = (-1,+1)"];

WriteModel[fname_,{plist_,vlist_}]:=
    Module[{str,fp,sp,vp,mp,gp,ap},
           str = OpenWrite[fname];

           WriteString[str,"*****************************\n"];           
           WriteString[str,"***   Dirac propagators   ***\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"\n"];

           fp = Cases[plist,TpMp`MMA`Drawing`F[___],-1];
           Print["Fermion rules ",Length[fp]];
           Do[
               WriteString[str,"[ "<>ToString[pp[[1]]]<>", "<>ToString[pp[[2]]]<>", - ; type = 'F', "<>AntiStr[pp]<>"]\n"];
               ,{pp,fp}];

           WriteString[str,"\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"***   Scalar propagators  ***\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"\n"];

           sp = Cases[plist,TpMp`MMA`Drawing`S[___],-1];
           Print["Scalar rules ",Length[sp]];
           Do[
               WriteString[str,"[ "<>ToString[pp[[1]]]<>", "<>ToString[pp[[2]]]<>", + ; type = 'S', "<>AntiStr[pp]<>"]\n"];
               ,{pp,sp}];

           WriteString[str,"\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"***   Vector propagators  ***\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"\n"];
           vp = Cases[plist,TpMp`MMA`Drawing`V[___],-1];
           Print["Vector rules ",Length[vp]];
           Do[
               WriteString[str,"[ "<>ToString[pp[[1]]]<>", "<>ToString[pp[[2]]]<>", + ; type = 'V', "<>AntiStr[pp]<>"]\n"];
               ,{pp,vp}];

           WriteString[str,"\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"*** Majorana propagators  ***\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"\n"];

           mp = Cases[plist,TpMp`MMA`Drawing`M[___],-1];
           Print["Majorana rules ",Length[mp]];
           Do[
               WriteString[str,"[ "<>ToString[pp[[1]]]<>", "<>ToString[pp[[2]]]<>", - ; type = 'M', "<>AntiStr[pp]<>"]\n"];
               ,{pp,mp}];

           WriteString[str,"\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"***   Ghost propagators   ***\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"\n"];

           gp = Cases[plist,TpMp`MMA`Drawing`M[___],-1];
           Print["Ghost rules ",Length[gp]];
           Do[
               WriteString[str,"[ "<>ToString[pp[[1]]]<>", "<>ToString[pp[[2]]]<>", - ; type = 'C', "<>AntiStr[pp]<>"]\n"];
               ,{pp,gp}];

           WriteString[str,"\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"*** Auxiliary propagators ***\n"];
           WriteString[str,"*****************************\n"];
           WriteString[str,"\n"];

           ap = Cases[plist,TpMp`MMA`Drawing`M[___],-1];
           Print["Aux rules ",Length[ap]];
           Do[
               WriteString[str,"[ "<>ToString[pp[[1]]]<>", "<>ToString[pp[[2]]]<>", - ; type = 'M', "<>AntiStr[pp]<>"]\n"];
               ,{pp,ap}];

           Close[str];
           Print["Model successfully written to the file ",fname];
          ]


(******************************************************************************)
Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["TpMp`MMA`Model`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
