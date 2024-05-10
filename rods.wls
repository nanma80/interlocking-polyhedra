#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 3;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

pencilDiameter = 7;
pencilLength = 190;

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

	outputFileName = "rods_" <> shape <> ".png";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"];
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	faceCenters = Table[ Mean[faceCoordinates[[k]] ]//FullSimplify, {k, 1, Length[faceCoordinates]}];


	faceCenterIndexMap = <||>;
	oppositeFaceIdMap = <||>;
	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		faceCenterIndexMap[ Round[faceCenters[[faceId]], epsilon] ] = faceId;
	];

	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		oppositeFaceCenter = Round[ -faceCenters[[faceId]], epsilon ];
		oppositeFaceIdMap[ faceId ] = faceCenterIndexMap[oppositeFaceCenter];
	];

	Print[oppositeFaceIdMap];

	sideCountInFace = Length[faceCoordinates[[1]]];
	initialRotationAngle = -2 Pi / sideCountInFace * 1/4;

	rotatedFaces = {};
	
	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		rotationMatrix = RotationMatrix[initialRotationAngle, faceCenters[[faceId]]];
		rotatedFace = faceCoordinates[[faceId]].rotationMatrix;
		AppendTo[rotatedFaces, rotatedFace];
	];

	rods = {};
	rodIds = {};

	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		For[vertexId = 1, vertexId <= Length[rotatedFaces[[faceId]]], vertexId++,
			oppositeFaceId = oppositeFaceIdMap[faceId];
			oppositeFace = rotatedFaces[[ oppositeFaceId ]];
			distances = Table[{ovid, EuclideanDistance[ N[rotatedFaces[[faceId]][[vertexId]]], rotatedFaces[[oppositeFaceId]][[ovid]] ] },  {ovid, 1, Length[oppositeFace]}];
			orderedDistances = SortBy[ distances, #[[2]]& ];
			oppositeVertexId = orderedDistances[[1]][[1]];
			originalEdgeLength = orderedDistances[[1]][[2]];

(* 			If[faceId == 1 && vertexId == 1, 
				Print[originalEdgeLength/2//N];
			]; *)
			rodId = Sort[{ {faceId, vertexId}, {oppositeFaceId, oppositeVertexId} }];
			AppendTo[rodIds, rodId];
		];
	];

	rodIds = Union[rodIds];


	rotatedAndShiftedFaces = {};
	furtherRotationAngle = 5.596/180 * Pi;

	Print["furtherRotationAngle"];
	Print[{furtherRotationAngle, furtherRotationAngle*180./Pi}];

	(*
		0 rotation:
		0.16245984811645323 
	*)

	inRadius = Norm[faceCenters[[1]]];
	faceCircumRadius = 1/2/Sin[Pi/sideCountInFace];

	(* 
		from measuring my model. 
		length of a pencil = 190mm.
		side of a pentagon = 43mm enforced by rubber bands, measured from my model
		Assume that the outer diameter of a pencil is 7mm
	*)
	targetLength = inRadius//N;

	(* edges = getRotatedEdges[furtherRotationAngle]; *)

	pentagonSideLengthAndEdges = getPentagonSideLength[furtherRotationAngle];
	pentagonSideLength = pentagonSideLengthAndEdges[[1]];
	edges = pentagonSideLengthAndEdges[[2]];
	Print[pentagonSideLength];

	(* Map[Print, Table[{findMinDistance[angleInDeg/180*Pi], Round[angleInDeg, 0.001]}, {angleInDeg, 11.5, 11.7, .01}]]; *)

	(* Map[Print, Table[{findMinDistance[angleInDeg/180*Pi], Round[angleInDeg, 0.001]}, {angleInDeg, -20, 30, 10}]]; *)

	(* Map[Print, Table[{getPentagonSideLength[angleInDeg/180*Pi][[1]], Round[angleInDeg, 0.001]}, {angleInDeg, 5.59, 5.61, .001}]]; *)
(* 
	By angle:
	This value depends on the relative lengths of the pencil and rubber bands.
	For pencil length 19cm and pentagonal side 4.3cm, the optimal angle is 11.57

	{0.16245984811645323, 0.}
	{0.16825398343783893, 11.57}

	By side length:
	{43.08756952045353, 0.}
	{41.65868725792897, 5.596}

*)

	(* Exit[]; *)
	
	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	(* color = allColors[ allSchlaefliSymbols[shape]]; *)
	
	viewPoint = If[shape == "Dodecahedron", faceCenters[[3]] * 100, {50, -50, 50}];
	(* viewPoint = {50, -40, 20}; *)

	viewCenter = {0, 0, 0};
	
	elements = {};

  thickness = pencilDiameter/2;
	AppendTo[ elements, { FaceForm[Yellow], Map[Cylinder[#, thickness]&, edges] } ];

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
	"Dodecahedron"
	(* "Cube" *)
	(* "Octahedron" *)
	(* "Tetrahedron" *)
	(* "Icosahedron" *)
};

Map[exportPolyhedron, shapes];

