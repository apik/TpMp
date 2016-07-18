(* A.Smirnov and A.Pak routines *)

ClearAll[UF];
Options[UF] := { Variables -> Table[x[i],{i,100}] };
UF[ks_,ds_,cs_,opt___Rule] :=
    Module[{degree,coeff,i,t2,t1,t0,zz,v},
           (* 29.10.10: added by AP *)
           vs = Take[Variables /. {opt} /. Options[UF], Length[ds]];
           cz = Map[Rationalize[##,0]&, cs, {0,Infinity}];
           degree = - Sum[ds[[i]] * vs[[i]], {i,1,Length[ds]}];
           coeff = 1;
           For[i = 1, i <= Length[ks], i++,
               t2 = Coefficient[degree, ks[[i]], 2];
               t1 = Coefficient[degree, ks[[i]], 1];
               t0 = Coefficient[degree, ks[[i]], 0];
               coeff = coeff * t2;
               (* 21.10.10: added by AP to avoid division by 0 *)
               If[t2 === 0, Return[{0,0,{}}]];
               degree = Together[t0 - ((t1^2)/(4*t2))]];
           degree = Together[- coeff * degree] //. cz;
           coeff = Together[coeff] //. cz;
           {coeff,Expand[degree],vs}]

(*******************************************************************************)
(* p: polynomial, return orderings of variables so that p is in canonical form *)

PolynomialOrderings[
    pn_, vs_List:{}, n_Integer:-1] :=
    Module[
        {vt, crs, gcd, cmx, cns, cas, cps, cvs, ord, max},
        (* check: variables *)
        vt = vs;
        If[vt === {}, vt = Variables[pn]];
        (* -- (1) -- *)
        (* polynomial -> coefficient rules *)
        crs = CoefficientRules[pn, vt];
        (* possible common factor *)
        gcd = PolynomialGCD @@ (Last /@ crs);
        (* rules -> matrix of exponents, coefficients *)
        cmx = Append[First[#], Simplify[Last[#]/gcd]] & /@ crs;
        (* operate on the transposed *)
        cmx = Transpose[Sort[cmx]];
        (* -- (2) -- *)
        (* initialize list of column numbers, permutations *)
        cns = Range[Length[vt]];
        cas = {{}};
        (* iterate until all variables ordered *)
        While[
            Length[First[cas]] < Length[vt],
            (* -- (3) -- *)
            (* extended permutations *)
            cps = Join @@
            (Function[ca, Append[ca, #] & /@ Complement[cns, ca]] /@ cas);
            (* -- (4) -- *)
            (* candidate vectors *)
            cvs = (cmx[[Prepend[#, -1]]]  (* coefficients, swap rows *)
                   // Transpose           (* -> columns *)
                   // Sort                (* sort rows *)
                   // Transpose           (* -> columns *)
                   // Last) & /@ cps;     (* extract vector *)
            (* -- (5) -- *)
            (* lexicographical ordering *)
            ord = Ordering[cvs];
            (* maximum vector *)
            max = cvs[[Last[ord]]];
            (* -- (6) -- *)
            (* select (maximum number of) candidate permutations *)
            cas = Part[cps, Select[ord, cvs[[#]] === max & ]];
            cas = If[n >= 0 && n < Length[cas], Take[cas, n], cas]];
        (* -- (7) -- *)
        (* result: canonical orderings *)
        cas];


(* Check scaleless *)
IsScaleless[fp_,xs_:Null] :=
    Module[{fm,cc,pr,nx},
           If[fp===0,
              True,               (* F=0 scaleless *)
              if[xs===Null,
                 xs=Union[Cases[fp,x[_],-1]]];
              (* polynomial -> list of rules *)
              fm = CoefficientRules[fp,xs];
              (* find GCD of coefficients *)
              cc = PolynomialGCD @@ (#[[2]] & /@ fm);
              (* remove common factors, normalize, sort *)
              rm = Sort[Prepend[#[[1]], Simplify[#[[2]]/cc]]& /@ fm];
              (* coefficients -> scaling powers of small parameter *)
              rs = Prepend[Drop[#,1], -Exponent[Expand[#[[1]]] /. sm->1/x, x]]& /@ rm;
              
              (* Print["points: ",rs]; *)
              
              (* Print[Length[rs]," points"]; *)
              
              (* aux function to determine rank of a set of points *)
              PRank[ps_List] := If[Length[ps] <= 1, 0, 
                                   MatrixRank @ Map[# - ps[[1]]&, Drop[ps,1]]];
              
              pr = PRank[rs];
              (* Print["dimensionality of the convex hull: ",pr]; *)
              nx = Length[xs];
              (pr < nx - 1)
             ]
          ]


(* Return three plynomials: U,F and P = F/.m->0 *)
UFP[ks_,prs_] :=
    Module[{ds,ms,up,fp,pp,xs},
           ds = First[#]^2& /@ prs;
           ms = Last  /@ prs;
           (* Print["UFP ds=",ds," ms= ",ms]; *)
           {up,pp,xs}=UF[ks,ds,{}];
           If[pp === 0,Return[{0,0,0,{}}]];
           (* Print[up,pp,xs]; *)
           fp = pp + up*(Plus @@ MapThread[(#1*#2^2)&,{xs,ms}]);
           {up,pp,fp,xs,ms}
          ]

(* For each subtopo construct transformation to minimum *)
RewireX[po_] := MapIndexed[(x[#1] -> x[#2[[1]]])&, po];

(* Generate all posible subtopologies from master topology *)
(* ks  = {k1,k2,...}*)
(* prs = {{k1,m1},{p1+k1,m2},...} *)
ToposFromAux[ks_,prs_] :=
    Module[{ds,ms,upfxm,scf},
           ds = First[#]^2& /@ prs;
           ms = Last  /@ prs;
           upfxm = {UFP[ks,#],#}& /@ Subsets[prs,{Length[ks],Length[prs]}];
           (* Print["UPFX = ",upfxm]; *)
           (* Select only scalefull topologies *)
           scf=Select[upfxm,Not[IsScaleless[#[[1,1]]*#[[1,3]],#[[1,4]]]]&];
           
           (* Map of unique topologies, key={U,F} *)
           ufmap=Association[];

           Do[
               Block[{up,pp,fp,xs,ms,props,newprops,po,xsub,upnew,ppnew,fpnew,crp,monocrp,kkU,kkCR,kkM,msbin},
                     {{up,pp,fp,xs,ms},props} = i;
                     newprops  = props;
                     (* Find polynomial ordering *)
                     po = First[PolynomialOrderings[up + fp, xs, 1]];
                     MapIndexed[(newprops[[#1]] = props[[#2[[1]]]])&, po];
                     
                     xsub = RewireX[po];
                     (* Print["po= ",po, " props: ",props, "  ===  ", newprops, " xsub = ",xsub]; *)

                     {upnew,ppnew,fpnew} = {up,pp,fp}/.xsub;
                     (* Entry in form U -> Monomials[CR[P]] -> mass[0,1,1,...] -> {CR[P],Props} *)
                     crp=CoefficientRules[ppnew,xs,DegreeReverseLexicographic];
                     (* Monomials, defined by powers in x[i] *)
                     monocrp=(First /@ crp);
                     (* Signature for mass distribution *)
                     msbin=(If[#===0,0,1]& /@ ms);
                     (* ufprop=Association[upnew -> Association[monocrp -> Association[(If[#===0,0,1]& /@ ms) -> {crp,newprops}]]]; *)

                     Print["New rule: ",Association[upnew -> Association[monocrp -> Association[msbin -> {{crp,newprops}}]]]];
                     
                     kkU=Lookup[ufmap, upnew];
                     Print["kkU=",kkU];
                     
                     If[MatchQ[kkU, Missing[_, _]],
                        (* New U value appending *)
                        AppendTo[ufmap, Association[upnew -> Association[monocrp -> Association[msbin -> {{crp,newprops}}]]]],
                        
                        (* U value already exists *)
                        kkCR=ufmap[upnew][monocrp];
                        Print["kkCR=", kkCR];
                        If[MatchQ[kkCR, Missing[_, _]],
                           AppendTo[ufmap[upnew], Association[monocrp -> Association[msbin -> {{crp,newprops}}]]],
                           
                           (* CoeffRules already exists *)
                           kkM=ufmap[upnew][monocrp][msbin];
                           Print["kkM=",kkM];
                           If[MatchQ[kkM, Missing[_, _]],
                              AppendTo[ufmap[[Key[upnew],Key[monocrp]]], Association[msbin -> {{crp,newprops}}]],
                              
                              (* Mass distrib exists *)
                              (* aptmp=ufmap[upnew][monocrp]; *)
                              Print["Going to append to: ",ufmap[upnew][monocrp][msbin]];
                              Print[msbin];
                              If[FirstPosition[ufmap[[Key[upnew],Key[monocrp],Key[msbin]]],{crp,_}] == Missing["NotFound"],
                                 ufmap[[Key[upnew],Key[monocrp],Key[msbin]]]=Append[ufmap[upnew][monocrp][msbin], {crp,newprops}]
                                ]
                              (* AppendTo[ufmap[upnew][monocrp][msbin], {crp,newprops}] *)
                             ];
                          ];
                       ];
                     

                     (* ufmap = Map[Join @@ # &,Merge[{ufmap,ufprop},Identity]]; *)
                     (* ufmap = Map[If[Length[#]==1,First[#],MergeXX[#,Identity]]&,Merge[{ufmap,ufprop},Identity]]; *)
                     (* MergeSub[l_]:= *)
                     (* FlatSub[as_]:=(If[Length[#]==1,#,Append@@ #])& /@ as; *)
                     (* ufmap = Map[FlatSub[Merge[#,Identity]]&,Merge[{ufmap,ufprop},Identity]]//.{{a_}}:>{a}; *)
                     (* ufmap = Merge[{ufmap,ufprop},Identity]; *)
                     Print["Map: ",ufmap];
                     (* AppendTo[ufmap, ({up,fp}/.xsub)->newprops]; *)
                    ]
               , {i, scf}];
           ufmap
          ]

MapOnAux[ks_,prs_,auxtop_] :=
    Module[{up,pp,fp,xs,po,upnew,ppnew,fpnew,crp,monocrp,fkey,ds,ms,
            possibleProps,nonZeroSymb,mappedWithMass,prsnew=prs},
           (* ds = First[#]^2& /@ prs; *)
           (* ms = Last[#]^2&  /@ prs; *)
           {up,pp,fp,xs,ms} = UFP[ks,prs];
           
           po = First[PolynomialOrderings[up + fp, xs, 1]];
           xsub = RewireX[po];

           prsnew = MapIndexed[(prsnew[[#1]] = prs[[#2[[1]]]])&, po];

           {upnew,ppnew,fpnew} = {up,pp,fp}/.xsub;
           crp      = CoefficientRules[ppnew,xs,DegreeReverseLexicographic];
           monocrp=(First /@ crp);
           Print["U=",upnew];
           Print["F=",fpnew];

           topos=auxtop[[Key[upnew],Key[monocrp],Key[(If[#[[2]]===0,0,1]& /@ prs)]]];

           If[MatchQ[topos,Missing[_,_]],
              
              Print["Topology not found!"];
              Return[Null],

              (* Topology found in DB *)
              sAux=(Plus @@ Table[a[j]*topos[[1,2,j,1]],{j,1,Length[prsnew]}])==0;
              arestrict=And @@ Table[(Abs[a[i]]==1||a[i]==0),{i,1,Length[prsnew]}];
              aNot0=(Plus@@Table[Abs[a[i]],{i,1,Length[prsnew]}]) != 0;

              Solve[sAux && And[arestrict] && aNot0,Table[a[j],{j,1,Length[prsnew]}]]
              (* (Plus @@ Table[a[j]*(prsnew[[j,1]] - sgn[j]*topos[[1,2,j,1]]),{j,1,Length[prsnew]}])==0 *)
             ]

              (* s1=(Plus @@ MapIndexed[a[#2[[1]]]*#1[[1]]&,prsnew])==0 *)
             (*  Solve[ *)
             (*      Append[Table[(Abs[a[i]]==1||a[i]==0),{i,1,Length[prsnew]}], *)
             (*             (Plus @@ MapIndexed[a[#2[[1]]]*#1[[1]]&,prsnew])==0], *)
             (*      Table[a[i],{i,1,Length[prsnew]}]] *)
              
             (* ] *)
           
           (* Catch[ *)

           (*     Print["ppCR = ",monocrp]; *)
           (*     GGG=Lookup[auxtop,upnew,Print["Topo with such U not found"];Throw[Null]]; *)
           (*     Print["GGG",auxtop[[Key[upnew],Key[monocrp],Key[(If[#[[2]]===0,0,1]& /@ prs)]]]]; *)
           (*     Print[Keys[GGG]]; *)
           (*     lll=Lookup[ *)
           (*         Lookup[auxtop,upnew,Print["Topo with such U not found"];Throw[Null]][[1]], *)
           (*         monocrp,Print["No ppCR"];Throw[Null]]; *)
           (*     Print["Found-----------------",lll]; *)
                     
           (*     Print["Orig CR ",First /@ ppCrules]; *)
           (*     Print["Keys:",Keys[ft]]; *)
           (*     Print[First/@(CoefficientRules[#,xs])&/@Keys[ft]]; *)

           (*     (\* Select F-poly with the same set of monomials in x[i] *\) *)
           (*     fkeys=Select[Keys[ft],(First/@(Sort[CoefficientRules[#,xs]])) == First/@ppCrules&]; *)

           (*     If[Length[fkeys] == 0, *)
           (*        Print["Topo with such F not found"]; *)
           (*        Throw[Null], *)

           (*        If[Length[fkeys] > 1, *)
           (*           Print["Ambiguity in F poly identification"]; *)
           (*           Throw[Null], *)

           (*           (\* Only one equal monomial set *\) *)
           (*           spSys = MapThread[(#1[[2]] == #2[[2]])&,{ppCrules,Sort[CoefficientRules[First[fkeys],xs]]}]; *)
                     
           (*           possibleProps=ft[First[fkeys]]; *)
                     
           (*           Print["Mass distr: ",(Last /@ #)& /@ possibleProps]; *)

           (*           (\* symbol masses can not become zero after mapping *\) *)
           (*           nonZeroSymb=And @@((# != 0)& /@ Union[Cases[ms,_Symbol]]); *)
           (*           Print[Solve[(ms==#) && nonZeroSymb]& /@ ((Last /@ #)& /@ possibleProps)]; *)

           (*           NonEmptyMass[pp_]:= Solve[(ms==(Last/@pp)) && nonZeroSymb]; *)

           (*           Print["NEM:",NonEmptyMass /@ possibleProps]; *)

           (*           (\* If mapping is possible system {m1,m2,m3,...}=={m4,m5,m6,...} has solution *\) *)
           (*           (\* Equal mass mapped on equal mass, zero on zero etc. *\) *)
           (*           mappedWithMass = Select[possibleProps, NonEmptyMass[#] =!= {}&]; *)
                     
           (*           If[Length[mappedWithMass]==0, *)
           (*              Print["No mass distribution found"]; *)
           (*              Throw[Null], *)
                        
           (*              If[Length[mappedWithMass] > 1, *)
           (*                 Print["Mass distribution ambiguity"]; *)
           (*                 Throw[Null], *)

           (*                 (\* Mass distribution found *\) *)
           (*                 Print[Solve[ (MapIndexed[sgn[#2[[1]]]*First[#1]&,prsnew]) == (First /@ First[mappedWithMass]), Table[sgn[j],{j,1,Length[prsnew]}]]]; *)
           (*                 Throw[{spSys,upnew,First[fkeys],First[mappedWithMass]}] *)
           (*                ] *)
           (*             ] *)
           (*           Print["Mass:  ",mappedWithMass]; *)

           (*          ] *)
           (*       ] *)
           (*     Print["F match: ",fkeys]; *)
           (*      ] *)

          ]