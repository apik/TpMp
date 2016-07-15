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
              
              Print["points: ",rs];
              
              Print[Length[rs]," points"];
              
              (* aux function to determine rank of a set of points *)
              PRank[ps_List] := If[Length[ps] <= 1, 0, 
                                   MatrixRank @ Map[# - ps[[1]]&, Drop[ps,1]]];
              
              pr = PRank[rs];
              Print["dimensionality of the convex hull: ",pr];
              nx = Length[xs];
              (pr < nx - 1)
             ]
          ]

(* Generate all posible subtopologies from master topology *)
ToposFromAux[ks_,ds_] :=
    Module[{ufx,scf},
           ufx = {UF[ks,#,{}],#}& /@ Subsets[ds,{Length[ks],Length[ds]}];
           Print[ufx];
           (* Select only scalefull topologies *)
           scf=Select[ufx,Not[IsScaleless[#[[1,1]]*#[[1,2]],#[[1,3]]]]&];
           
           (* For each subtopo construct transformation to minimum *)
           RewireX[po_] := MapIndexed[(x[#1] -> x[#2[[1]]])&, po[[1]]];
           
           ({#[[2]],#[[1,1]],#[[1,2]]}/.RewireX[PolynomialOrderings[#[[1,1]] + #[[1,2]], #[[1,3]],1]])& /@ scf
          ]

