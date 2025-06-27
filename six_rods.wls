#!/usr/bin/env wolframscript

rod1[t_, theta_] := {1, t Cos[theta], t Sin[theta]};
rod2[t_, theta_] := {t Sin[theta], 1, t Cos[theta]};

distanceSquare[t1_, t2_, theta_] := Total[Map[#^2 &, (rod1[t1, theta] - rod2[t2, theta])]]//FullSimplify;

Print[distanceSquare[t1, t2, theta]];

(* minDist[theta_] := Minimize[distanceSquare[t1, t2, theta], {t1, t2}, Reals]; *)

sols = Solve[
D[ distanceSquare[t1, t2, theta], t1 ] == 0 && D[ distanceSquare[t1, t2, theta], t2 ] == 0, {t1, t2}
];
sol1 = sols[[1]];

minDistSq[theta_] := distanceSquare[t1, t2, theta]/.sol1;
rodDist[theta_] := FullSimplify[Sqrt[minDistSq[theta]]];
(*
Normalize rod dist to 1: always make the rods touch. Diameter of rods = 1.
inverseRod = the distance of rods to origin
*)
inverseRod[theta_] := FullSimplify[1/rodDist[theta]];

Print[inverseRod[theta]];

graph = Plot[inverseRod[theta], {theta, -Pi/4, Pi/4}];
outputFileName = "output/six_rods/test.png";
Export[outputFileName, graph];