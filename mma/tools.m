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
           (* - for positive F *)
           {coeff,Expand[-degree],vs}]

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
           (* Print["Scale Fp=",fp]; *)
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
           ds = -First[#]^2& /@ prs;
           ms = Last  /@ prs;
           (* Print["UFP ds=",ds," ms= ",ms]; *)
           {up,pp,xs}=UF[ks,ds,{}];
           (* If[pp === 0,Return[{0,0,0,{}}]]; *)
           (* Print["All set:",{up,pp,xs,ms}]; *)
           If[up === 0,Return[{0,0,0,{}}]];
           fp = pp + up*(Plus @@ MapThread[(#1*#2^2)&,{xs,ms}]);
           {up,pp,fp,xs,ms}
          ]

(* For each subtopo construct transformation to minimum *)
RewireX[po_] := MapIndexed[(x[#1] -> x[#2[[1]]])&, po];

(* Generate all posible subtopologies from set of master topologies *)
(* Set of master topologies have same external and internal momentums, but different propagator expressions *)
(* and mass es on lines *)
(* ks  = {k1,k2,...}*)
(* legs= {p1,p2,p3,-p1-p2-p3} *)
(* prs = {T1[{k1,m1},{p1+k1,m2},...],T2[{k1,0},...]} *)

(* TODO:
   - Add possibility to map on exactly specified different masses and not only on signature
 *)
Options[ShrinkLine]=
    {
        MaxProps->Null                (* Maximal number of propagators in generated topology *)
    };
ShrinkLine[ks_,top_,OptionsPattern[]]:= 
    Module[{upfxm,withscale,nextlevel},
           NoNum[l_]:=(#[[2;;3]])& /@ l;

           Print["Shrinking to get lines = ",If[OptionValue[MaxProps] =!= Null,OptionValue[MaxProps],Length[top]], " in topology ", Head[top]," with ",Length[top]," propagators"];
           (* On first step we look for subsets of fixed length which may be smaller than number of irreducible scalar products *)
           upfxm = ({UFP[ks,NoNum[#]],#})& /@ Subsets[List @@ top,{If[OptionValue[MaxProps] =!= Null,OptionValue[MaxProps],Length[top]-1]}];
           Print["Generated ",Length[upfxm]," subtopos"];
           withscale=(Head[top] @@ #)& /@ Select[upfxm,Not[IsScaleless[#[[1,1]]*#[[1,3]],#[[1,4]]]]&];

           nextlevel = (Head[top]@@Last[#])& /@ withscale;
           (* Print["WS:",nextlevel]; *)
           
           If[Length[nextlevel] > 0,
              sl = Join @@ DeleteCases[ShrinkLine[ks,#]& /@ nextlevel,{}];
              (* Print["Ret sl:",sl]; *)
              If[sl =!= {{}},
                 Join[withscale,sl],
                 withscale
                ],
              withscale
             ]
          ];

Options[ToposFromAux]=
    {
        MaxProps->Null,                (* Maximal number of propagators in generated topology *)
        Verbose->False
    };
ToposFromAux[ks_,legs_,prs_List,OptionsPattern[]] :=
    Module[{ds,ms,ufmap,scf,legSubsets,vertexCons},
           (* ds = First[#]^2& /@ prs; *)

           scf={};
           Do[
               Block[{upfxm,scfTop,top},
                     top = MapIndexed[(Prepend[#1,#2[[1]]])&,topNoNum];
                     (* Print[Subsets[List @@ top,{Length[ks],Length[top]}]]; *)

                     Print["Try topo: ",top];
                     
                     scf=Join[scf,ShrinkLine[ks,top,MaxProps->If[OptionValue[MaxProps] =!= Null,OptionValue[MaxProps],Length[top]]]];
                    ]
               , {topNoNum, prs}
             ];

           Print["Scalefull momentum subsets generated, unique tipology finding started"];

           (* Print["SCFFF:",scf]; *)
           (* Print["OOOOOOOOOOOOOOOOOOOO"]; *)
           (* ms = Last  /@ prs; *)
           (* upfxm = {UFP[ks,#],#}& /@ Subsets[prs,{Length[ks],Length[prs]}]; *)
           (* Print["UPFX = ",upfxm]; *)
           (* Select only scalefull topologies *)
           (* scf=Select[upfxm,Not[IsScaleless[#[[1,1]]*#[[1,3]],#[[1,4]]]]&]; *)
           
           (* Map of unique topologies, key={U,F} *)
           ufmap=Association[];
           (* Print[ufmap]; *)
           Do[
               Block[{up,pp,fp,xs,ms,props,newprops,po,xsub,upnew,ppnew,fpnew,crp,monocrp,kkt,kkU,kkCR,kkM,msbin,topo,mvec},
                     (* Print[i]; *)
                     {{up,pp,fp,xs,ms},props} = List @@ i;
                     topo = Head[i];
                     (* Print["Topology : ",topo]; *)
                     newprops  = props;
                     (* Find polynomial ordering *)
                     po = First[PolynomialOrderings[up + fp, xs, 1]];

                     (* Print["Poly order:",po]; *)
                     (* Add numbering inside TOPO to propagators *)
                     (* STRANGE !!!!!!!! *)
                     MapIndexed[(newprops[[#2[[1]]]] = props[[#1]])&, po];
                     (* MapIndexed[(newprops[[#1]] = props[[#2[[1]]]])&, po]; *)
                     newprops = topo @@ newprops;
                     (* Print["New props:", newprops]; *)

                     xsub = RewireX[po];
                     (* Print["po= ",po, " props: ",props, "  ===  ", newprops, " xsub = ",xsub]; *)

                     {upnew,ppnew,fpnew} = {up,pp,fp}/.xsub;
                     (* Entry in form U -> Monomials[CR[P]] -> mass[0,1,1,...] -> {CR[P],Props} *)
                     crp=CoefficientRules[ppnew,xs,DegreeReverseLexicographic];
                     (* Monomials, defined by powers in x[i] *)
                     monocrp=(First /@ crp);
                     (* Signature for mass distribution *)
                     msbin=List @@ (If[#[[3]]===0,0,1]& /@ newprops);

                     (* Print["Mass sign ", msbin]; *)
                     (* ufprop=Association[upnew -> Association[monocrp -> Association[(If[#===0,0,1]& /@ ms) -> {crp,newprops}]]]; *)

                     (* Print["New rule: ",Association[upnew -> Association[monocrp -> Association[msbin -> {{crp,newprops}}]]]]; *)

                     t = Length[newprops];                     
                     kkt = Lookup[ufmap,t];
                     
                     
                     If[MatchQ[kkt, Missing[_, _]],

                        (* New t value appending *)
                        AppendTo[ufmap, Association[t -> Association[upnew -> Association[monocrp -> Association[msbin -> {{crp,newprops}}]]]]],
                        
                        
                        kkU =ufmap[t][upnew];
                        (* Print["kkU=",kkU]; *)
                        
                        If[MatchQ[kkU, Missing[_, _]],
                           (* New U value appending *)
                           AppendTo[ufmap[[Key[t]]], Association[upnew -> Association[monocrp -> Association[msbin -> {{crp,newprops}}]]]],
                           
                           (* U value already exists *)
                           kkCR=ufmap[t][upnew][monocrp];
                           (* Print["kkCR=", kkCR]; *)
                           If[MatchQ[kkCR, Missing[_, _]],
                              AppendTo[ufmap[[Key[t],Key[upnew]]], Association[monocrp -> Association[msbin -> {{crp,newprops}}]]],
                              
                              (* CoeffRules already exists *)
                              kkM=ufmap[t][upnew][monocrp][msbin];
                              (* Print["kkM=",kkM]; *)
                              If[MatchQ[kkM, Missing[_, _]],
                                 AppendTo[ufmap[[Key[t],Key[upnew],Key[monocrp]]], Association[msbin -> {{crp,newprops}}]],
                                 
                                 (* Mass distrib exists *)
                                 (* aptmp=ufmap[upnew][monocrp]; *)
                                 (* Print["Going to append to: ",ufmap[upnew][monocrp][msbin]]; *)
                                 (* Print[msbin]; *)
                                 
                                 (* Fill vector of masses to compare with *)
                                 GetMasses[pr_]:= List @@ ((#[[3]])& /@ pr);
                                 mvec = GetMasses[newprops];
                                 
                                 (* Print["M vetor ",mvec]; *)
                                 If[Length[Select[ufmap[[Key[t],Key[upnew],Key[monocrp],Key[msbin]]], mvec==GetMasses[#[[2]]]& ]] === 0,
                                    ufmap[[Key[t],Key[upnew],Key[monocrp],Key[msbin]]]=Append[ufmap[t][upnew][monocrp][msbin], {crp,newprops}]
                                ]
                              (* If[FirstPosition[ufmap[[Key[upnew],Key[monocrp],Key[msbin]]],{crp,_}] == Missing["NotFound"], *)
                              (*    ufmap[[Key[upnew],Key[monocrp],Key[msbin]]]=Append[ufmap[upnew][monocrp][msbin], {crp,newprops}] *)
                              (*   ] *)
                              (* AppendTo[ufmap[upnew][monocrp][msbin], {crp,newprops}] *)
                                ];
                             ];
                          ];
                       ];

                     (* ufmap = Map[Join @@ # &,Merge[{ufmap,ufprop},Identity]]; *)
                     (* ufmap = Map[If[Length[#]==1,First[#],MergeXX[#,Identity]]&,Merge[{ufmap,ufprop},Identity]]; *)
                     (* MergeSub[l_]:= *)
                     (* FlatSub[as_]:=(If[Length[#]==1,#,Append@@ #])& /@ as; *)
                     (* ufmap = Map[FlatSub[Merge[#,Identity]]&,Merge[{ufmap,ufprop},Identity]]//.{{a_}}:>{a}; *)
                     (* ufmap = Merge[{ufmap,ufprop},Identity]; *)
                     (* Print["Map: ",ufmap]; *)
                     (* AppendTo[ufmap, ({up,fp}/.xsub)->newprops]; *)
                    ]
               , {i, scf}];


           (*                                                          *)
           (*                                                          *)
           (*   Precalculate vertex conservation rules for each topo   *)
           (*                                                          *)
           (*                                                          *)
           
           legSubsets=
           If[Length[legs] >= 2,
              Subsets[legs[[1;;-2]],{1,Length[legs]-1}],
              {{}}
             ];
           
           vertexCons=Association[];

           Do[
               Block[{topo=Head[topprops],lc,arestrict,aNot0,
                     solInt,solExt, exlst},
                     (* Print["Conservation rules for topo ",topo]; *)
                     lc=(Plus @@ Table[a[j]*topprops[[j,1]],{j,1,Length[topprops]}]);
                     (* We find linear combination with coefficients a={-1,0,1} only *)
                     arestrict=And @@ Table[(Abs[a[i]]==1||a[i]==0),{i,1,Length[topprops]}];
                     (* We do not interested in trivial solution when all a=0 *)
                     aNot0=(Plus@@Table[Abs[a[i]],{i,1,Length[topprops]}]) != 0;

                     (* Print[lc]; *)

                     solInt=Solve[lc == 0 && And[arestrict] && aNot0,Table[a[j],{j,1,Length[topprops]}]];
                     (* Print["IntRules:",solInt]; *)
                     
                     exlst={};
                     (* Add internal rules {c1,c2,...}==0 *)
                     AppendTo[exlst,((Table[a[j],{j,1,Length[topprops]}]/.#)==0)]& /@ solInt;

                     Do[
                         (* Print["Legs:",emom]; *)
                         (* Total momentum flow through the cut is equal to external momentum *)
                         solExt=Solve[lc == (Plus@@emom) && And[arestrict] && aNot0,Table[a[j],{j,1,Length[topprops]}]];
                         (* Print["Sol:",solExt]; *)

                         AppendTo[exlst,((Table[a[j],{j,1,Length[topprops]}]/.#)==Plus @@ emom)]& /@ solExt;
                         
                         
                         ,{emom,legSubsets}]
                     
                     AppendTo[vertexCons,topo->exlst];
                    ],{topprops,prs}];
           
           
           {ufmap,vertexCons}
          ]


RemoveDotsUF[U_,F_]:=
    Module[{X,mayHaveDots},
           X = Union[Cases[U + F, x[_], -1]];
           mayHaveDots=X;
           AreHomo[poly_,x1_,x2_]:=FreeQ[Expand[poly/.x1->y-x2],x2];
           
           ReduceDotList[vars_,up_,fp_,sub_]:=
           Module[{xTo,xFrom,HomoInx1x2,zzf,zzU,zzF,subret=sub,UpSub,FpSub},
                  If[Length[vars] > 1,
                     Print["Reduce:",vars];
                     Print["U=",up];
                     Print["F=",fp];
                     Print["Sub=",subret];
                     xTo=vars[[1]];
                     xFrom=vars[[2;;-1]];
                     {UpSub,FpSub,subret}=Catch[
                         Do[
                             HomoInx1x2=(AreHomo[up,xTo,xFrom[[i]]] && AreHomo[fp,xTo,xFrom[[i]]]);
                             If[HomoInx1x2, 
                                zzf=(z[xTo,xFrom[[i]]]/.x[a_]->a);
                                zzU=up/.{xTo->zzf,xFrom[[i]]->0};
                                zzF=fp/.{xTo->zzf,xFrom[[i]]->0};
                                Print["Prepend: ",Prepend[Drop[xFrom,i],(z[xTo,xFrom[[i]]]/.x[a_]->a)]];
                                {zzU,zzF,subret}=ReduceDotList[Prepend[Drop[xFrom,i],(z[xTo,xFrom[[i]]]/.x[a_]->a)],zzU,zzF,Append[subret,zzf->(xTo+xFrom[[i]])]];
                                Throw[{zzU,zzF,subret}];
                               ];
                             ,{i,Length[xFrom]}
                           ];
                         Throw[{up,fp,subret}];
                                               ]
                     ,
                     {UpSub,FpSub,subret}]
                 ];
           ReduceDotList[mayHaveDots,U,F,{}]
          ]





(* TODO *)
(* 
   - Removing dots
   - Embeding topologies with dots instead of imaginary legs
*)

(********************************************************************************)
(**                                                                            **)
(**                 Prepare substitution rules with duplicated                 **)
(**                  propagators replaced with new symbols                     **)
(**                                                                            **)
(********************************************************************************)

RemoveDots[prs_]:=
    Module[{sqp},

           (* Combine propagators with equal squares *)
           sqp = Last /@ Normal[GroupBy[prs,Expand[#[[1]]^2]&]];

           (* Check lines with equal momentum to have equal masses *)
           If[And @@ ((SameQ @@(Last/@ #))& /@ sqp),
              
              mapTo={};
              mapFrom={};
              Do[
                  Block[{samepr},
                        (* List of propagators with the same momentum *)
                        samepr = sqp[[i]];

                        (* Rules p[i]->d[i] *)
                        (AppendTo[mapTo, # -> {d[i]*Cancel[#[[1]]/samepr[[1,1]]],#[[2]] }])& /@ samepr;
                        (* Back rules *)
                        AppendTo[mapFrom, {d[i],samepr[[1,2]]} -> samepr[[1]]];
                       ],{i,Length[sqp]}];
              Return[{mapTo,mapFrom}];
              ,

              (* Lines with different masses present *)
              Print["Different masses with same momentum, partial fractioning needed."];
              Return[Null];
             ]
          ]


(*************************** 
  
  Main function for topology mapping, input diagram must be in form:
  
  - No dots on lines
  - No additional external legs
   
  ***************************)



Options[MapOnAuxExact]={
    ExactMatch -> True,         (* Match only when all mass symbols are equal *)
    Verbose    -> False};       (* Show more output *)

MapOnAuxExact[ks_,legs_,prs_,{auxtop_,vertcons_},OptionsPattern[]] :=
    Module[{up,pp,fp,xs,po,upnew,ppnew,fpnew,crp,monocrp,fkey,ds,ms,
            possibleProps,nonZeroSymb,mappedWithMass,prsnew},
           Print["PRS: ",prs];

           prsnew = prs;
           {up,pp,fp,xs,ms} = UFP[ks,prs];

           po = First[PolynomialOrderings[up + fp, xs, 1]];
           xsub = RewireX[po];

           (* Reverse ordering ??? *)
           MapIndexed[(prsnew[[#2[[1]]]] = prs[[#1]])&, po];

           {upnew,ppnew,fpnew} = {up,pp,fp}/.xsub;
           crp      = CoefficientRules[ppnew,xs,DegreeReverseLexicographic];
           monocrp=(First /@ crp);
           (* Print["U=",upnew]; *)
           (* Print["F=",fpnew]; *)
           
           (* Print["Poly order:",po]; *)
           (* Print["prs:",prsnew]; *)

           topos=auxtop[[Key[Length[prsnew]],Key[upnew],Key[monocrp],Key[(If[#[[2]]===0,0,1]& /@ prsnew)]]];

           If[MatchQ[topos,Missing[_,_]],
              
              Print["Topology not found!", topos];
              
              Return[{Null,Null}],

              (* Topology found in DB *)
              
              Print["Mapping on topo:",topos];
              (* topos[[]]; *)
              
              If[OptionValue[ExactMatch],
                 
                 (* Check masses for exact match *)
                 Print["Exact match"];
                 GetMasses[pr_]:= List @@ ((#[[3]])& /@ pr);

                 mvec = (#[[2]])& /@ prsnew;
                 extop=Select[topos,(GetMasses[#[[2]]] == mvec)&];
                 
                 If[Length[extop] != 1,
                    Print["Exact mass distribution not found!"];
                    Return[{Null,Null}],
                    
                    (* ELSE *)
                    Block[{top,pv,auxpropnums,vcrules,redvcr},
                          extop = extop[[1]];
                          top = Head[extop[[2]]];
                          (* Extract numbers of propagators from AUX  top *)
                          (* We apply conservation rules only with such numbers *)
                          auxpropnums = List @@ ({First[#]}& /@ extop[[2]]);
                          vcrules = vertcons[top];
                          redvcr  = (Part[#[[1]],Flatten[auxpropnums]]==#[[2]])& /@ (Select[vcrules, (MatchQ[Union[Delete[#[[1]],auxpropnums]], {} | {0}])&]);

                          (* Print["RCVCR ",redvcr]; *)
                          subWrap = If[Length[legs] >= 2,(# -> wr[#])& /@ legs[[1;;-2]],{}];
                          
                          (* 
                             TODO: 
                             
                             Coefficients in front of loop mometum equal

                             *)
                          
                          (* Create propagator vector with sign variables *)
                          pv = Transpose[{MapIndexed[(sgn[ #2[[1]] ] * #1[[1]])&, prsnew]}];
                          sys   = (First[#[[1]].pv/.subWrap]-#[[2]])& /@ redvcr;
                          syscr = Flatten[CoefficientRules[#,ks]& /@ sys];
                          (* Print["Sys initial ",syscr]; *)
                          sgnsys    = And @@ ((#[[2]]==0)& /@ syscr);
                          (* Print["ks: ",ks]; *)
                          (* Print["System ",sgnsys]; *)
                          sgnvars   = Table[sgn[i], {i,Length[prsnew]}];
                          sgnconstr = And @@ Table[Abs[sgn[i]]==1, {i,Length[prsnew]}];
                          (* Print["To solve ",{sgnsys && sgnconstr, Join[sgnvars,wr/@legs[[1;;-2]]]}]; *)

                          subsol = If[Length[legs] >= 2,
                                      Solve[sgnsys && sgnconstr, Join[sgnvars,wr/@legs[[1;;-2]]]],
                                      Solve[sgnsys && sgnconstr, sgnvars]
                                     ];
                          (* Print["All solutions",subsol]; *)
                          
                          (* First is with the smallest sum *)
                          plusSubSol=SortBy[subsol,(-Plus @@ (sgnvars/.#))&];

                          (* Print["Most positive", First[plusSubSol]]; *)

                          (* Decripting back *)
                          
                          prsBack = prsnew;
                          MapIndexed[(prsBack[[#1]] = (prsnew[[#2[[1]]]] -> 
                                                       {p[extop[[2,#2[[1]],1]]]/sgn[#2[[1]]], (* Momenum in aux top *)
                                                        extop[[2,#2[[1]],2]]/sgn[#2[[1]]],    (* Momentum *)
                                                        extop[[2,#2[[1]],3]]                  (* Mass *)
                                                       } ))&, po];
                          
                          If[Length[legs] >= 2,
                             {top @@ prsBack, wr/@legs[[1;;-2]]},
                             {top @@ prsBack, {}}
                            ]/.First[plusSubSol]
                         ]
                   ]
                 ,
                 (* First match is ok *)
                 Print["Not Exact match"];
                 GetMassesSig[pr_]:= List @@ ((If[#[[3]] =!= 0,1,0])& /@ pr);
                 (* Get signature for massless lines m1,m2,0,m4 -> {1,1,0,1} *)
                 mvec = (If[#[[2]] =!= 0,1,0])& /@ prsnew;
                 extop=Select[topos,(GetMassesSig[#[[2]]] == mvec)&];
                 Print["Masses ",topos];

                 If[Length[extop] == 0,
                    Print["Mass distribution matching pattern not found!"];
                    Return[{Null,Null}],
                    (* ELSE *)
                    Block[{top,pv,auxpropnums,vcrules,redvcr},
                          extop = extop[[1]];
                          top = Head[extop[[2]]];
                          (* Extract numbers of propagators from AUX  top *)
                          (* We apply conservation rules only with such numbers *)
                          auxpropnums = List @@ ({First[#]}& /@ extop[[2]]);
                          vcrules = vertcons[top];
                          redvcr  = (Part[#[[1]],Flatten[auxpropnums]]==#[[2]])& /@ (Select[vcrules, (MatchQ[Union[Delete[#[[1]],auxpropnums]], {} | {0}])&]);

                          (* Print["RCVCR ",redvcr]; *)
                          subWrap = If[Length[legs] >= 2,(# -> wr[#])& /@ legs[[1;;-2]],{}];
                          
                          (* 
                             TODO: 
                             
                             Coefficients in front of loop mometum equal

                             *)
                          
                          (* Create propagator vector with sign variables *)
                          pv = Transpose[{MapIndexed[(sgn[ #2[[1]] ] * #1[[1]])&, prsnew]}];
                          sys   = (First[#[[1]].pv/.subWrap]-#[[2]])& /@ redvcr;
                          syscr = Flatten[CoefficientRules[#,ks]& /@ sys];
                          (* Print["Sys initial ",syscr]; *)
                          sgnsys    = And @@ ((#[[2]]==0)& /@ syscr);
                          (* Print["ks: ",ks]; *)
                          (* Print["System ",sgnsys]; *)
                          sgnvars   = Table[sgn[i], {i,Length[prsnew]}];
                          sgnconstr = And @@ Table[Abs[sgn[i]]==1, {i,Length[prsnew]}];
                          (* Print["To solve ",{sgnsys && sgnconstr, Join[sgnvars,wr/@legs[[1;;-2]]]}]; *)

                          subsol = If[Length[legs] >= 2,
                                      Solve[sgnsys && sgnconstr, Join[sgnvars,wr/@legs[[1;;-2]]]],
                                      Solve[sgnsys && sgnconstr, sgnvars]
                                     ];
                          (* Print["All solutions",subsol]; *)
                          
                          (* First is with the smallest sum *)
                          plusSubSol=SortBy[subsol,(-Plus @@ (sgnvars/.#))&];

                          (* Print["Most positive", First[plusSubSol]]; *)

                          (* Decripting back *)
                          
                          prsBack = prsnew;
                          MapIndexed[(prsBack[[#1]] = (prsnew[[#2[[1]]]] -> 
                                                       {p[extop[[2,#2[[1]],1]]]/sgn[#2[[1]]], (* Momenum in aux top *)
                                                        extop[[2,#2[[1]],2]]/sgn[#2[[1]]],    (* Momentum *)
                                                        extop[[2,#2[[1]],3]]                  (* Mass *)
                                                       } ))&, po];
                          
                          If[Length[legs] >= 2,
                             {top @@ prsBack, wr/@legs[[1;;-2]]},
                             {top @@ prsBack, {}}
                            ]/.First[plusSubSol]
                         ]
                   ]
                 (* TODO *)
                 (* Print[topos]; *)
                ]
              
             ]
           
          ]
    



(********************************************************************************)
(**                                                                            **)
(**   Map on AUX topology removing dots before and keep set of exernal         **)
(**   moment untuched                                                          **)
(**                                                                            **)
(********************************************************************************)

Options[MapOnAux]={
    ExactMatch   -> True,         (* Match only when all mass symbols are equal *)
    SplitMomenta -> {},           (* Keep routing of momentum specified, mapping on 
                                     topo where such momentum nullified *)
    Verbose      -> False};       (* Show more output *)

Options[MapCorrectLegs]={
    ExactMatch   -> True,         (* Match only when all mass symbols are equal *)
    SplitMomenta -> {},           (* Keep routing of momentum specified, mapping on 
                                     topo where such momentum nullified *)
    Verbose      -> False};       (* Show more output *)


MapCorrectLegs[ks_,legs_,prsWdots_,{auxtop_,vertcons_},OptionsPattern[]]:=
    Module[{rdTo,rdFrom,prsNoDots,mInt,mExt,dsub},
           (* Print[RemoveDots[prsWdots]]; *)
           (* Print[prsWdots]; *)
           
           (* Removing dots *)
           {rdTo,rdFrom} = RemoveDots[prsWdots];
           prsNoDots     = Last /@ rdFrom;
           
           (* Mapping rules *)
           {mInt,mExt} = MapOnAuxExact[ks,legs,prsNoDots,{auxtop,vertcons},ExactMatch->OptionValue[ExactMatch]];
           
           If[mInt === Null || mExt === Null,
              Return[Missing[prsNoDots]];
              ,
              
              dsub={};
              MapIndexed[AppendTo[dsub,d[#2[[1]]] -> #1[[2,1;;2]]]&, mInt];
              
              Print["dsub:",dsub];
              Return[Head[mInt] @@ (rdTo/.dsub)];
             ];
           
          ]


MapOnAux[ks_,legs_,prsWdots_,{auxtop_,vertcons_},OptionsPattern[]] :=
    Module[{mapres,subZeroSplit,splitProps,splitLHS},
           
           Print["[1] - removing dots"];
           
           
           
           If[OptionValue[SplitMomenta] === {},
              
              (* Do not split *)
              Print["Momentum splitting not needed"];
              
              mapres = MapCorrectLegs[ks,legs,prsWdots,{auxtop,vertcons},ExactMatch->OptionValue[ExactMatch]];
              Print["Mappp ",mapres];
              mapres
              ,
              
              (* Momenta splitting *)
              
              subZeroSplit = (# -> 0)& /@ OptionValue[SplitMomenta];
              Print[subZeroSplit];
              
              SplitMom[m_] := {m/.subZeroSplit, Expand[m - (m/.subZeroSplit)]};
              splitProps = Map[{SplitMom[#[[1]]],#[[2]]}&, prsWdots];
              Print[splitProps];
              
              splitLHS = ({#[[1,1]],#[[2]]})& /@ splitProps; 
              

              mapres = MapCorrectLegs[ks,DeleteCases[legs/.subZeroSplit,0],splitLHS,{auxtop,vertcons},ExactMatch->OptionValue[ExactMatch]];
              Print["MAPRES    ====  ",mapres];
              If[Head[mapres] === Missing,
                 Return[mapres];
                 ,

                 Print["Mappp ", mapres];
                 Return[Head[mapres] @@ MapThread[ {Plus @@ #1[[1]],#1[[2]]} -> {#2[[2,1]] + #1[[1,2]],#2[[2,2]]}&,{splitProps, List @@ mapres}]];
                ]
             ]
           
          ]
