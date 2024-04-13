#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
imageSize = 720 / 3 * {4, 3};
epsilon = 0.000000000001;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

allColors = <|
	"3_3" -> Cyan,
	"5_3" -> Red,
	"3_5" -> Green,
	"52_5" -> Blue,
	"5_52" -> Yellow,
	"52_3" -> Magenta,
	"3_52" -> Orange
|>;

allSchlaefliSymbols = <|
	"Icosahedron" -> "3_5",
	"Dodecahedron" -> "5_3",
	"GreatDodecahedron" -> "5_52",
	"GreatIcosahedron" -> "3_52", 
	"GreatStellatedDodecahedron" -> "52_3", 
	"SmallStellatedDodecahedron" -> "52_5"
|>;

colors = Join[{Red, Blue, Green, Yellow, Magenta, Cyan, Brown, Orange, Pink, Purple}, RandomColor[100]];

getFaceCenter[face_] := Simplify[Total[PolygonCoordinates[face]]/5];
getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];

(* 
	functions from:
	https://mathematica.stackexchange.com/questions/45265/distance-between-two-line-segments-in-3-space
 *)
pointsegdis[{seg_, pointlist_}] := 
       Module[{u = Subtract @@ seg, mean = Plus @@ seg/2},
       {#, (mean - u  Sign@# Min[1, Abs@#]/2) &@(-(2 ( # - mean).u )/u.u) } & 
        /@ pointlist ]
segsegdis[s1_, s2_] := Module[{u, v, w, d, t1, t2},
   {u, v, w} = Subtract @@ # & /@ {Sequence @@ #, First /@ #} &@{s1, s2};
   If[(d = u.u v.v - (v.u)^2) != 0 &&
       Abs[t2 = 2 (u.u v.w - u.w v.u)/d + 1] <= 1 &&
       Abs[t1 = 2 (v.u v.w - v.v u.w)/d + 1] <= 1,
    Plus @@ (#[[1]] (1 + {1, -1} #[[2]] )) /2 & /@ {{s1, t1}, {s2, t2}} ,
    First@SortBy[Flatten[pointsegdis /@ {{s1, s2}, {s2, s1}}, 1], 
                       Total@((Subtract @@ #)^2) &]
       ] ] ;

distSegToSeg[s1_, s2_] :=
  Module[{small = 10^(-8), p1, p2, q1, a2, u, v, w,
          a, b, c, d, e, D,
          sc, sN, sD,
          tc, tN, tD},
	p1 = s1[[1]];
	p2 = s1[[2]];
	q1 = s2[[1]];
	q2 = s2[[2]];
    u = p2 - p1;
    v = q2 - q1;
    w = p1 - q1;
    a = u.u;
    b = u.v;
    c = v.v;
    d = u.w;
    e = v.w;
    D = a*c - b*b;
    sD = D;
    tD = D;
    If[D < small, 
      sN = 0; sD = 1; tN = e; tD = c,
      sN = b*e - c*d;
      tN = (a*e - b*d);
      If[sN < 0, 
        sN = 0.0; tN = e; tD = c,
        If[sN > sD, sN = sD; tN = e + b; tD = c;]]];
    If[tN < 0, 
      tN = 0;
      If[-d < 0, 
        sN = 0,
        If[-d > a, 
          sN = sD, 
          sN = -d; sD = a]],
      If[tN > tD, 
        tN = tD; 
        If[-d + b < 0, 
          sN = 0,
          If[-d + b > a, 
            sN = sD, 
            sN = -d + b; sD = a]]]];
    sc = If[Norm[sN] < small, 0, sN/sD];
    tc = If[Norm[tN] < small, 0, tN/tD];
    N[Norm[w + sc*u - tc*v]]
];

segDistance[s1_, s2_] := Apply[EuclideanDistance, segsegdis[s1, s2]];

findMinDistance[edges_, faceCenters_, shiftFactor_, rotationAngleFactor_ ] := Module[
	{ rotationAngle, minDistance, copy, copyCenter, copyEdges, copyVertices, copy1Edges, copy1Vertices, copyId, coreEdgeId, copyEdgeId },
	
	rotationAngle = 2 Pi / 5 * rotationAngleFactor;

	minDistance = 10.0;

	For[copyId = 1, copyId <= Length[faceCenters] && copyId <= 12, copyId++,
		copy = <||>;
		copyCenter = faceCenters[[copyId]] * shiftFactor;

		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, edges, {2} ];
		copyVertices = Map[ (# + copyCenter).rotationMatrix &, vertices];

		If[copyId == 1,
			copy1Edges = copyEdges;
			copy1Vertices = copyVertices;
		];

		If[copyId >= 2,
			For[coreEdgeId = 1, coreEdgeId <= Length[copy1Edges], coreEdgeId++,
				For[copyEdgeId = 1, copyEdgeId <= Length[copyEdges], copyEdgeId++,
					distance = distSegToSeg[ copy1Edges[[coreEdgeId]], copyEdges[[copyEdgeId]] ];
					(* distance = segDistance[ copy1Edges[[coreEdgeId]], copyEdges[[copyEdgeId]] ]; *)
					
					(* minDistance = Min[distance, minDistance]; *)
					If[distance < minDistance, minDistance = distance];
					If[minDistance < epsilon,
						Return[0];
					];
				]
			];
		];
	];

	Return[minDistance];
];

exportPolyhedron[shape_] := Module[
	{outputFileName, vertices, edges, faces, thickness, lighting, graph},

	outputFileName = shape <> ".png";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	thickness = 0.089/2;
	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	color = allColors[ allSchlaefliSymbols[shape]];
	viewPoint = {0, 0, 10};

	viewCenter = {0, 0, 0};
	faceCenters = Table[ Mean[faceCoordinates[[k]] ], {k, 1, Length[faceCoordinates]}];

	elements = {};

	core = <||>;
	core["edges"] = edges;
	core["vertices"] = vertices;

	copies = {};
	
	(* shiftFactor = 0.34;
	rotationAngleFactor = 0.17; *)

	shiftFactor = 1.04;
	rotationAngleFactor = 0.27;

	range = range * (1 + shiftFactor) / 1.3;

	(* minDistance = findMinDistance[edges, faceCenters, 0.5, 0.1]; *)
	minDistance = findMinDistance[edges, faceCenters, shiftFactor, rotationAngleFactor];
	Print["minDistance, shiftFactor, rotationAngleFactor"];
	Print[{minDistance, shiftFactor, rotationAngleFactor}];

	(* maxMinDistance = 0;	
	For[shiftFactor = 1.0, shiftFactor < 1.1, shiftFactor += 0.01,
		For[angleFactor = 0.0, angleFactor < 0.5, angleFactor += 0.01,
			minDistance = findMinDistance[edges, faceCenters, shiftFactor, angleFactor];
			If[minDistance > maxMinDistance,
				maxMinDistance = minDistance;
				Print["updating maxMinDistance to:"];
				Print[{ maxMinDistance, shiftFactor, angleFactor } ];
			];
		];
	]; *)

(* 
	candidates
	{0.06340204890759395, 0.34, 0.17000000000000004}
	{0.06002275290971208, 0.81, 0.29000000000000004}
	{0.06669808021639463, 0.9500000000000002, 0.28}
	{0.07290163789019177, 0.9600000000000002, 0.29000000000000004}
	{0.08908130915292858, 1.04, 0.27}

	looks similar to original:
	0.04, 0.8, 0.3
 *)

	(* Exit[]; *)

	(* maxMinDistance = FindMaximum[ findMinDistance[edges, faceCenters, 0.5, angleFactor], {angleFactor, 0.05} ]; *)

	(* Print[maxMinDistance]; *)

	For[copyId = 1, copyId <= Length[faceCenters] && copyId <= 12, copyId++,
		copy = <||>;
		copyCenter = faceCenters[[copyId]] * shiftFactor;

		rotationAngle = 2 Pi / 5 * rotationAngleFactor;
		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, edges, {2} ];
		copyVertices = Map[ (# + copyCenter).rotationMatrix &, vertices];
		copyFaces = Map[ (# + copyCenter).rotationMatrix &, faceCoordinates, {2} ];

		If[copyId == 1,
			copy1Edges = copyEdges;
			copy1Vertices = copyVertices;
		];

		color = colors[[copyId]];

		opacity = 1.0;
		(* opacity = 0.7; *)

		AppendTo[ elements, { FaceForm[color], Map[Cylinder[#, thickness]&, copyEdges] } ];
		AppendTo[ elements, { FaceForm[color], Map[Sphere[#, thickness]&, copyVertices] } ];
		(* AppendTo[ elements, { FaceForm[color], Opacity[opacity], Map[Polygon[#]&, copyFaces] } ]; *)
	];

	graph = Graphics3D[
		elements,
		ViewPoint -> viewPoint,
		PlotRange -> {{viewCenter[[1]]-range, viewCenter[[1]]+range}, {viewCenter[[2]]-range, viewCenter[[2]]+range}, {viewCenter[[3]]-range, viewCenter[[3]]+range}},
		Lighting -> lighting
	];


	If[exportToPov,
		If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
		POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
		Run[cropCommand];
		,
		Export[outputFileName, graph]
	];
];

shapes = {
	"Dodecahedron"
};

Map[exportPolyhedron, shapes];

