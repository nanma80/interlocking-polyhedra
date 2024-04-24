#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 2;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

optimalShiftFactor = <|
	"Dodecahedron" -> 2.14,
	"Cube" -> 0.33,
	"Octahedron" -> 0.29,
	"Tetrahedron" -> 0.31,
	"Icosahedron" -> 1.19
	(* "Icosahedron" -> 0.78 *)
|>;

optimalRotationAngleFactor = <|
	"Dodecahedron" -> 0.18,
	"Cube" -> 0.17,
	"Octahedron" -> 0.18,
	"Tetrahedron" -> 0.21,
	"Icosahedron" -> 0.29
	(* "Icosahedron" -> 0.21 *)
|>;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

colors = Join[
	{
		Red,  Orange, Blue, Green, Pink, 
		Brown, Gray, Cyan, Darker[Green, 0.5], Purple,
		Yellow, White
	},
	RandomColor[100]];

getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];

(* 
	functions from:
	https://mathematica.stackexchange.com/questions/45265/distance-between-two-line-segments-in-3-space
 *)

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

findMinDistance[edges_, explosionVectors_, findMinDistance_, shiftFactor_, rotationAngleFactor_ ] := Module[
	{ rotationAngle, minDistance, copy, copyCenter, copyEdges, copyVertices, copy1Edges, copy1Vertices, copyId, copy1EdgeId, copyEdgeId },
	
	rotationAngle = 2 Pi / edgeCountPerVertex * rotationAngleFactor;

	minDistance = 10.0;

	For[copyId = 1, copyId <= Length[explosionVectors], copyId++,
		copy = <||>;
		copyCenter = explosionVectors[[copyId]] * shiftFactor;

		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, edges, {2} ];
		copyVertices = Map[ (# + copyCenter).rotationMatrix &, vertices];

		If[copyId == 1,
			copy1Edges = copyEdges;
			copy1Vertices = copyVertices;
		];

		If[copyId >= 2,
			For[copy1EdgeId = 1, copy1EdgeId <= Length[copy1Edges], copy1EdgeId++,
				For[copyEdgeId = 1, copyEdgeId <= Length[copyEdges], copyEdgeId++,
					distance = distSegToSeg[ copy1Edges[[copy1EdgeId]], copyEdges[[copyEdgeId]] ];
					If[distance < minDistance, minDistance = distance];
					(* If[distance < minDistance * 1.03, Print[{distance, copyId, copy1EdgeId, copyEdgeId}]]; *)
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

	outputFileName = "vertex_explosion_" <> shape <> "_level_" <> ToString[displayLevel] <> ".png";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	edgeCountPerVertex = Length[edges] * 2 / Length[vertices];

	
	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	color = allColors[ allSchlaefliSymbols[shape]];
	
	viewPoint = If[shape == "Dodecahedron", {0, 0, 10}, {50, -20, 30}];

	viewCenter = {0, 0, 0};
	(* explosionVectors = Table[ Mean[faceCoordinates[[k]] ], {k, 1, Length[faceCoordinates]}]; *)
	explosionVectors = vertices;
	(* Map[Print[{#, explosionVectors[[#]]//N}]&, Range[Length[explosionVectors]]]; *)

	elements = {};

	core = <||>;
	core["edges"] = edges;
	core["vertices"] = vertices;

	copies = {};

	shiftFactor = optimalShiftFactor[shape];
	rotationAngleFactor = optimalRotationAngleFactor[shape];

	range = range * (1 + shiftFactor) / 1.3;

	minDistance = findMinDistance[edges, explosionVectors, faceCoordinates, shiftFactor, rotationAngleFactor];
	Print["minDistance, shiftFactor, rotationAngleFactor"];
	Print[{minDistance, shiftFactor, rotationAngleFactor}];

	thickness = minDistance/2;

	If[searchingForOptimalFactors,
		maxMinDistance = 0;	
		For[shiftFactor = 0.2, shiftFactor < 0.4, shiftFactor += 0.01,
			Print["shiftFactor:"];
			Print[shiftFactor];
			For[angleFactor = 0.0, angleFactor <= 0.5, angleFactor += 0.01,
				minDistance = findMinDistance[edges, explosionVectors, faceCoordinates, shiftFactor, angleFactor];
				If[minDistance > maxMinDistance,
					maxMinDistance = minDistance;
					Print["updating maxMinDistance to:"];
					Print[{ maxMinDistance, shiftFactor, angleFactor } ];
				];
			];
		];
		Exit[];
	];

(* 
	dodecahedron
	{0.4644790843781216, 2.139999999999998, 0.18000000000000002}

	cube
	{0.08781383982639773, 0.33000000000000007, 0.17000000000000004}

	octahedron
	{0.0468156128801163, 0.29000000000000004, 0.18000000000000005}

	tetrahedron
	{0.1351475851690155, 0.3100000000000001, 0.21000000000000005}

	icosahedron:
	{0.07023868828041116, 1.1900000000000002, 0.2900000000000001}
 *)

	(* Exit[]; *)

	(* maxMinDistance = FindMaximum[ findMinDistance[edges, explosionVectors, findMinDistance, 0.5, angleFactor], {angleFactor, 0.05} ]; *)

	For[copyId = 1, copyId <= Length[explosionVectors], copyId++,
		copy = <||>;
		copyCenter = explosionVectors[[copyId]] * shiftFactor;

		rotationAngle = 2 Pi / edgeCountPerVertex * rotationAngleFactor;
		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, edges, {2} ];
		copyVertices = Map[ (# + copyCenter).rotationMatrix &, vertices];
		copyFaces = Map[ (# + copyCenter).rotationMatrix &, faceCoordinates, {2} ];
		copyEdgeCenters = Map[Mean, copyEdges];
		copyEdgeCenterLevels = Map[Round[Norm[#], epsilon]&, copyEdgeCenters];
		levels = copyEdgeCenterLevels//Union//Sort;
		If[displayLevel > Length[levels],
			Print["Warning: there are only " <> ToString[Length[levels]] <> " levels. Display level is reset from " <> ToString[displayLevel] <> " to the length of levels"];
			displayLevel = Length[levels];
		];
		displayEdges = Select[copyEdges, Norm[Mean[#]] < levels[[displayLevel]] + 10 epsilon & ];

		(* plot edges with min distance *)
(* 		If[copyId == 1,
			displayEdges = displayEdges[[ {4, 5, 13, 15, 26} ]];
		];

		If[copyId == 2,
			displayEdges = displayEdges[[ {1, 2, 21, 22, 24 } ]];
		];
 *)

(* 		If[copyId == 3,
			displayEdges = displayEdges[[ {11, 25 } ]];
		];

		If[copyId == 4,
			displayEdges = displayEdges[[ {23, 25 } ]];
		];

		If[copyId == 5,
			displayEdges = displayEdges[[ {8, 23 } ]];
		];

		If[copyId == 6,
			displayEdges = displayEdges[[ {3, 8} ]];
		];

		If[copyId == 7,
			displayEdges = displayEdges[[ {3, 11 } ]];
		];
 *)
		(* If[copyId > 7 || copyId == 2, *)
(* 		If[copyId > 2,
			displayEdges = {};
		];
 *)
		displayVertices = Union[Flatten[displayEdges, 1], SameTest -> ApproxSamePoint];

		color = colors[[copyId]];

		opacity = 1.0;
		(* opacity = 0.7; *)

		AppendTo[ elements, { FaceForm[color], Map[Cylinder[#, thickness]&, displayEdges] } ];
		AppendTo[ elements, { FaceForm[color], Map[Sphere[#, thickness]&, displayVertices] } ];
		(* AppendTo[ elements, { FaceForm[color], Opacity[opacity], Map[Polygon[#]&, copyFaces] } ]; *)
	];

	graph = Graphics3D[
		elements,
		ViewPoint -> viewPoint,
		(* PlotRange -> {{viewCenter[[1]]-range, viewCenter[[1]]+range}, {viewCenter[[2]]-range, viewCenter[[2]]+range}, {viewCenter[[3]]-range, viewCenter[[3]]+range}}, *)
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
	(* "Dodecahedron" *)
	(* "Cube" *)
	(* "Octahedron" *)
	"Tetrahedron"
	(* "Icosahedron" *)
};

Map[exportPolyhedron, shapes];

