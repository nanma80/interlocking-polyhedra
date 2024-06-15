#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 3;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;

(* withRubberBands = False; *)
withRubberBands = True;

noSkew = False;
(* noSkew = True; *)

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

(* standard pencil *)
pencilDiameter = 7;
pencilLength = 190;

(* straw *)
(* pencilDiameter = 6; *)
(* pencilLength = 210; *)

optimalShiftFactor = <|
	"Dodecahedron" -> 1.04,
	(* "Cube" -> 1.42, *)
	(* "Cube" -> 0.58, *)
	"Cube" -> 2.0,
	"Octahedron" -> 1.14,
	"Tetrahedron" -> 0.76,
	"Icosahedron" -> 2.51
|>;

optimalRotationAngleFactor = <|
	"Dodecahedron" -> 0.27,
	(* "Cube" -> 0.23, *)
	(* "Cube" -> 0.19, *)
	"Cube" -> 0.3,
	"Octahedron" -> 0.21,
	"Tetrahedron" -> 0.21,
	"Icosahedron" -> 0.39
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

randomDirection = {1, Pi, Exp[1]}//N;

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

getPentagonSideLength[furtherRotationAngle_] := Module[
	{edges, minDistance, scalingFactor, edgeId, edge, edgeCenter, edgeLength, extendedEdge},
	edges = getRotatedEdges[furtherRotationAngle];

 	minDistance = findMinDistance[furtherRotationAngle];

	scalingFactor = pencilDiameter/minDistance;
	edges = edges * scalingFactor;
	For[edgeId = 1, edgeId <= Length[edges], edgeId++,
		edge = edges[[edgeId]];
		edgeCenter = Mean[edge];
		edgeLength = EuclideanDistance[edge[[1]], edge[[2]]];
		extendedEdge = Map[(# - edgeCenter) * pencilLength/edgeLength + edgeCenter &, edge];
		edges[[edgeId]] = extendedEdge;
	];

	Return[{ EuclideanDistance[ edges[[1]][[1]], edges[[2]][[1]] ], edges }];
];

findMinDistance[furtherRotationAngle_] := Module[
	{edges, minDistance, edgeId, edgeDistance},
	edges = getRotatedEdges[furtherRotationAngle];

	minDistance = -1;

	edgeCountMatchingMinDistance = 0;

	For[edgeId = 2, edgeId <= Length[edges], edgeId++,
		edgeDistance = distSegToSeg[edges[[1]], edges[[edgeId]] ];
		If[minDistance < 0 || edgeDistance < minDistance, 
			edgeCountMatchingMinDistance = 1;
			minDistance = edgeDistance,
			If[edgeDistance < minDistance + 10 epsilon,
				edgeCountMatchingMinDistance ++;
			];
		];
	];

	Print["edge count with minDistance " <> IntegerString[edgeCountMatchingMinDistance]];
	Return[minDistance];
];

getRotatedEdges[furtherRotationAngle_] := Module[
	{rotatedAndShiftedFaces, inFaceDistance, height, shiftFactor, rotationMatrix, faceId, edges },
	rotatedAndShiftedFaces = {};

	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		rotationMatrix = RotationMatrix[furtherRotationAngle, faceCenters[[faceId]]]//N;
		rotatedAndShiftedFace = Map[ #.rotationMatrix &, rotatedFaces[[faceId]] ];
		AppendTo[rotatedAndShiftedFaces, rotatedAndShiftedFace];
	];

	edges = Map[{ rotatedAndShiftedFaces[[#[[1]][[1]]]][[ #[[1]][[2]] ]], rotatedAndShiftedFaces[[#[[2]][[1]]]][[ #[[2]][[2]] ]] }&, rodIds ];
	Return[edges];
];

exportPolyhedron[shape_] := Module[
	{outputFileName, vertices, edges, faces, thickness, lighting, graph},

	outputFileName = "rotating_edges_" <> shape <> ".png";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	faceCenters = Table[ Mean[faceCoordinates[[k]] ]//FullSimplify, {k, 1, Length[faceCoordinates]}];
	edgeCenters = Map[Mean, edges];

	lengthFactor = 6;

	For[rotationInDegree = 38, rotationInDegree <= 41, rotationInDegree+= 0.1,
		(* rotationInDegree = 30; *)
		rotationAngle = rotationInDegree /180 * Pi;

		rods = {};
		rodDistances = {};

		For[edgeId = 1, edgeId <= Length[edges], edgeId ++,
			edge = edges[[edgeId]];
			edgeCenter = Mean[edge];
			rotationMatrix = RotationMatrix[rotationAngle, edgeCenter];
			rod = Map[( (# - edgeCenter) * lengthFactor + edgeCenter).rotationMatrix &, edge];
			AppendTo[rods, rod];

			If[ edgeId > 1, 
				rodDistance = distSegToSeg[rods[[1]], rod ];
				AppendTo[rodDistances, rodDistance];
			];
		];

		Print[{rotationInDegree , Sort[Tally[Round[rodDistances, epsilon]]]} ];
		(* Print[Sort[rodDistances]]; *)

	];

	Exit[];

	lighting = {{"Point", White, {50, -50, 50}}};
	
	viewPoint = {50, -50, 50};
	If[shape == "Dodecahedron", viewPoint = {0, 0, 50}];
	If[shape == "Icosahedron", viewPoint = {0, 0, 50}];
	
	viewPoint = {50, -30, 20};
	(* viewPoint = {50, -50, 50}; *)
	(* Map[Print[{#, vertices[[#]]//N}]&, Range[20]]; *)
	(* Exit[]; *)
	(* viewPoint = vertices[[5]] * 100; *)
	(* viewPoint = {50, -50, 50}; *)

	viewCenter = {0, 0, 0};
	
	elements = {};

  thickness = 0.01;
	AppendTo[ elements, { FaceForm[Yellow], Map[Cylinder[#, thickness]&, rods] } ];
	
	graph = Graphics3D[
		elements,
		ViewPoint -> viewPoint,
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

