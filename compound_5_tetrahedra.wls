#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 5;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

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

	outputFileName = "compound_5_tetra.png";
	shapeFolder = "compound_5_tetra";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shapeFolder}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"]//N;
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	faceCenters = Table[ Mean[faceCoordinates[[k]] ], {k, 1, Length[faceCoordinates]}];

(* 	verticesInverseMap = <||>;
	For[vi = 1, vi <= Length[vertices], vi++,
		vertex = vertices[[vi]];
		verticesInverseMap[Round[vertex, epsilon]] = vi;
(* 		Print[{
			vi, 
			N[Abs[2.288245611270737 - EuclideanDistance[vertices[[4]], vertices[[vi]]]]] < epsilon,
			N[Abs[2.288245611270737 - EuclideanDistance[vertices[[13]], vertices[[vi]]]]] < epsilon
			}]; *)
	];
 *)
(* 	rotate5fold = RotationMatrix[2 Pi/5, {0, 0, 1}];

	For[vi = 1, vi <= Length[vertices], vi++,
		vertex = vertices[[vi]];
		rotatedVertex = vertex.rotate5fold;
		rotatedIndex = verticesInverseMap[Round[rotatedVertex, epsilon]];
		Print[{vi, rotatedIndex}];
	];
 *)

	copyIndices = {
		{1, 11, 17, 20},
		{4, 7, 9, 2},
		{6, 16, 10, 19},
		{5, 8, 18, 14},
		{3, 12, 13, 15}
	};

(* 	For[copyId = 1, copyId <= Length[copyIndices], copyId++,
		copyIndex = copyIndices[[copyId]];
		indexPairs = Subsets[copyIndex, {2}];
		distances = Map[N[EuclideanDistance[ vertices[[#[[1]]]], vertices[[#[[2]]]] ]]&, indexPairs];
		If[Max[distances] - Min[distances] > epsilon, 
			Print["Wrong!"]; Print[copyId]
		];
	]; *)
	
	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	color = allColors[ allSchlaefliSymbols[shape]];
	
	viewPoint = If[shape == "Dodecahedron", {0, 0, 10}, {50, -50, 50}];

	viewCenter = {0, 0, 0};
	(* Map[Print[{#, faceCenters[[#]]//N}]&, Range[Length[faceCenters]]]; *)

	elements = {};

(* 	minDistance = findMinDistance[edges, faceCenters, faceCoordinates, shiftFactor, rotationAngleFactor];
	Print["minDistance, shiftFactor, rotationAngleFactor"];
	Print[{minDistance, shiftFactor, rotationAngleFactor}];

	
 *)

 (* compute min distance *)
 	minDistance = 10.0;
	copy1Edges = {};
	For[copyId = 1, copyId <= Length[copyIndices], copyId++,
		copyIndex = copyIndices[[copyId]];
		indexPairs = Subsets[copyIndex, {2}];
		copyEdges = Map[{vertices[[#[[1]]]], vertices[[#[[2]]]]}&, indexPairs];
		edgeDistances = Map[ EuclideanDistance[#[[1]], #[[2]] ]&, copyEdges ];
		edgeLength = Min[edgeDistances];
		If[copyId == 1, 
			copy1Edges = copyEdges,
			For[coreEdgeId = 1, coreEdgeId <= Length[copy1Edges], coreEdgeId++,

				For[copyEdgeId = 1, copyEdgeId <= Length[copyEdges], copyEdgeId++,
					distance = distSegToSeg[ copy1Edges[[coreEdgeId]], copyEdges[[copyEdgeId]] ];
					If[distance < minDistance, minDistance = distance];
				];
			];
		];
	];

	Print["edge length, min edge distance, ratio, 1/ratio"];
	Print[{edgeLength, minDistance, minDistance/edgeLength, edgeLength/minDistance}];
	(* 
		result:
		{2.288245611270737, 0.1689477446214834, 0.07383287169407537, 13.544102742521984}
	*)

	thickness = minDistance/2;

	For[copyId = 1, copyId <= Length[copyIndices], copyId++,
		copyIndex = copyIndices[[copyId]];
		indexPairs = Subsets[copyIndex, {2}];
		copyEdges = Map[{vertices[[#[[1]]]], vertices[[#[[2]]]]}&, indexPairs];
		edgeDistances = Map[ EuclideanDistance[#[[1]], #[[2]] ]&, copyEdges ];

		displayEdges = copyEdges;
		displayVertices = Union[Flatten[displayEdges, 1], SameTest -> ApproxSamePoint];

		color = colors[[copyId]];

		opacity = 1.0;

		AppendTo[ elements, { FaceForm[color], Map[Cylinder[#, thickness]&, displayEdges] } ];
		AppendTo[ elements, { FaceForm[color], Map[Sphere[#, thickness]&, displayVertices] } ];
	];

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
};

Map[exportPolyhedron, shapes];

