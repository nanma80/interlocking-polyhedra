#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 7;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

optimalShiftFactor = <|
	"Dodecahedron" -> 1.04,
	"Cube" -> 1.42,
	"Octahedron" -> 1.14,
	"Tetrahedron" -> 0.76,
	"Icosahedron" -> 2.53
	(* "Icosahedron" -> 0.78 *)
|>;

optimalRotationAngleFactor = <|
	"Dodecahedron" -> 0.27,
	"Cube" -> 0.23,
	"Octahedron" -> 0.21,
	"Tetrahedron" -> 0.21,
	"Icosahedron" -> 0.20
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

findMinDistance[edges_, faceCenters_, findMinDistance_, shiftFactor_, rotationAngleFactor_ ] := Module[
	{ rotationAngle, minDistance, copy, copyCenter, copyEdges, copyVertices, copy1Edges, copy1Vertices, copyId, coreEdgeId, copyEdgeId },
	
	sideCountInFace = Length[faceCoordinates[[1]]];
	rotationAngle = 2 Pi / sideCountInFace * rotationAngleFactor;

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

	outputFileName = shape <> "_level_" <> ToString[displayLevel] <> ".png";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	
	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	color = allColors[ allSchlaefliSymbols[shape]];
	viewPoint = {0, 0, 10};
	viewPoint = {50, -50, 50};

	viewCenter = {0, 0, 0};
	faceCenters = Table[ Mean[faceCoordinates[[k]] ], {k, 1, Length[faceCoordinates]}];
	(* Map[Print[{#, faceCenters[[#]]//N}]&, Range[Length[faceCenters]]]; *)

	elements = {};

	core = <||>;
	core["edges"] = edges;
	core["vertices"] = vertices;

	copies = {};

	shiftFactor = optimalShiftFactor[shape];
	rotationAngleFactor = optimalRotationAngleFactor[shape];

	range = range * (1 + shiftFactor) / 1.3;

	minDistance = findMinDistance[edges, faceCenters, faceCoordinates, shiftFactor, rotationAngleFactor];
	Print["minDistance, shiftFactor, rotationAngleFactor"];
	Print[{minDistance, shiftFactor, rotationAngleFactor}];

	thickness = minDistance/2;

	If[searchingForOptimalFactors,
		maxMinDistance = 0;	
		For[shiftFactor = 2.45, shiftFactor < 2.9, shiftFactor += 0.01,
			For[angleFactor = 0.1, angleFactor <= 0.3, angleFactor += 0.01,
				minDistance = findMinDistance[edges, faceCenters, faceCoordinates, shiftFactor, angleFactor];
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
	{0.08908130915292858, 1.04, 0.27}

	cube
	{0.13997235190833174, 1.4200000000000002, 0.23000000000000007}

	octahedron
	{0.0840834722251952, 1.1400000000000001, 0.21000000000000008}

	tetrahedron
	{0.08361351956627538, 0.76, 0.21000000000000008}

	icosahedron: questionable. too loose
	{0.12892581145636603, 2.5299999999999985, 0.20000000000000007}
	local optimum:
	{0.026120905956610308, 0.78, 0.21000000000000008}

 *)

	(* Exit[]; *)

	(* maxMinDistance = FindMaximum[ findMinDistance[edges, faceCenters, findMinDistance, 0.5, angleFactor], {angleFactor, 0.05} ]; *)

	For[copyId = 1, copyId <= Length[faceCenters], copyId++,
		copy = <||>;
		copyCenter = faceCenters[[copyId]] * shiftFactor;

		sideCountInFace = Length[faceCoordinates[[1]]];
		rotationAngle = 2 Pi / sideCountInFace * rotationAngleFactor;
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
	(* "Tetrahedron" *)
	"Icosahedron"
};

Map[exportPolyhedron, shapes];

