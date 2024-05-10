#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 3;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

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

findMinDistance[edges_, explosionVectors_, findMinDistance_, shiftFactor_, rotationAngleFactor_ ] := Module[
	{ rotationAngle, minDistance, copy, copyCenter, copyEdges, copyVertices, copy1Edges, copy1Vertices, copyId, copy1EdgeId, copyEdgeId },
	
	sideCountInFace = Length[faceCoordinates[[1]]];
	rotationAngle = 2 Pi / sideCountInFace * rotationAngleFactor;

	minDistance = 10.0;

	For[copyId = 1, copyId <= Length[explosionVectors], copyId++,
		copy = <||>;
		copyCenter = explosionVectors[[copyId]] * shiftFactor;

		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, edges, {2} ];

		(* copyVertices = Map[ (# + copyCenter).rotationMatrix &, vertices]; *)

		If[copyId == 1,
			copy1Edges = copyEdges;
			(* copy1Vertices = copyVertices; *)
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
		faceCenterIndexMap[ Round[faceCenters[[faceId]], epsilon] ] = faceId
	];

	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		oppositeFaceCenter = Round[ -faceCenters[[faceId]], epsilon ];
		oppositeFaceIdMap[ faceId ] = faceCenterIndexMap[oppositeFaceCenter];
	];

	Print[oppositeFaceIdMap];

	sideCountInFace = Length[faceCoordinates[[1]]];
	initialRotationAngle = 2 Pi / sideCountInFace * 1/4;

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

			If[faceId == 1 && vertexId == 1, 
				Print[originalEdgeLength/2//N];
			];
			rodId = Sort[{ {faceId, vertexId}, {oppositeFaceId, oppositeVertexId} }];
			AppendTo[rodIds, rodId];
		];
	];

	rodIds = Union[rodIds];
	Print[rodIds//Length];

	rotatedAndShiftedFaces = {};
	furtherRotationAngle = - 0.3;
	shiftFactor = 1.0;
	Print[ faceCenters[[4]][[3]]//N ];
	inRadius = Sqrt[5/8 + 11/(8*Sqrt[5])];
	Print[inRadius//N];


	For[faceId = 1, faceId <= Length[faceCenters], faceId++,
		rotationMatrix = RotationMatrix[furtherRotationAngle, faceCenters[[faceId]]];
		rotatedAndShiftedFace = Map[ (# + faceCenters[[faceId]] * shiftFactor).rotationMatrix &, rotatedFaces[[faceId]]];
		AppendTo[rotatedAndShiftedFaces, rotatedAndShiftedFace];
	];

	edges = Map[{ rotatedAndShiftedFaces[[#[[1]][[1]]]][[ #[[1]][[2]] ]], rotatedAndShiftedFaces[[#[[2]][[1]]]][[ #[[2]][[2]] ]] }&, rodIds ];

	minDistance = 100.0;

	For[edgeId = 2, edgeId <= Length[edges], edgeId++,
		edgeDistance = distSegToSeg[edges[[1]], edges[[edgeId]] ];
		If[edgeDistance < minDistance, minDistance = edgeDistance];
	];
	Print[minDistance];


	(* minDistance = findMinDistance[edges, explosionVectors, faceCoordinates, shiftFactor, rotationAngleFactor]; *)


	Exit[];
	
	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	(* color = allColors[ allSchlaefliSymbols[shape]]; *)
	
	(* viewPoint = If[shape == "Dodecahedron", {0, 0, 10}, {50, -50, 50}]; *)
	viewPoint = {50, -40, 20};
	viewCenter = {0, 0, 0};
	(* faceCenters = Table[ Mean[faceCoordinates[[k]] ], {k, 1, Length[faceCoordinates]}]; *)
	(* Map[Print[{#, explosionVectors[[#]]//N}]&, Range[Length[explosionVectors]]]; *)
	(* explosionVectors = faceCenters; *)
	
	elements = {};
(* 
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
		For[shiftFactor = 1.9, shiftFactor < 2.1, shiftFactor += 0.01,
			Print["shiftFactor:"];
			Print[shiftFactor];
			For[angleFactor = 0.1, angleFactor <= 0.5, angleFactor += 0.01,
				minDistance = findMinDistance[edges, explosionVectors, faceCoordinates, shiftFactor, angleFactor];
				If[minDistance > maxMinDistance,
					maxMinDistance = minDistance;
					Print["updating maxMinDistance to:"];
					Print[{ maxMinDistance, shiftFactor, angleFactor } ];
				];
			];
		];
		Exit[];
	]; *)

	(* Exit[]; *)
(* 
	For[copyId = 1, copyId <= Length[explosionVectors], copyId++,
		copy = <||>;
		copyCenter = explosionVectors[[copyId]] * shiftFactor;

		sideCountInFace = Length[faceCoordinates[[1]]];
		rotationAngle = 2 Pi / sideCountInFace * rotationAngleFactor;
		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, edges, {2} ];
		copyVertices = Map[ (# + copyCenter).rotationMatrix &, vertices];
		copyFaces = Map[ (# + copyCenter).rotationMatrix &, faceCoordinates, {2} ];
		copyEdgeCenters = Map[Mean, copyEdges];
		copyEdgeCenterLevels = Map[Round[Norm[#], epsilon]&, copyEdgeCenters];
		levels = copyEdgeCenterLevels//Union//Sort;
		If[copyId == 1,
			Print["levels"];
			Print[levels];
		];
		
		If[displayLevel > Length[levels],
			Print["Warning: there are only " <> ToString[Length[levels]] <> " levels. Display level is reset from " <> ToString[displayLevel] <> " to the length of levels"];
			displayLevel = Length[levels];
		];
		displayEdges = Select[copyEdges, Norm[Mean[#]] < levels[[displayLevel]] + 10 epsilon & ];

		displayVertices = Union[Flatten[displayEdges, 1], SameTest -> ApproxSamePoint];

		color = colors[[copyId]];

		opacity = 1.0;

		AppendTo[ elements, { FaceForm[color], Map[Cylinder[#, thickness]&, displayEdges] } ];
		AppendTo[ elements, { FaceForm[color], Map[Sphere[#, thickness]&, displayVertices] } ];
		(* AppendTo[ elements, { FaceForm[color], Opacity[opacity], Map[Polygon[#]&, copyFaces] } ]; *)
	];
 *)
  thickness = 0.03;
	AppendTo[ elements, { FaceForm[Yellow], Map[Cylinder[#, thickness]&, edges] } ];

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
	"Dodecahedron"
	(* "Cube" *)
	(* "Octahedron" *)
	(* "Tetrahedron" *)
	(* "Icosahedron" *)
};

Map[exportPolyhedron, shapes];

