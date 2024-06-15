#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 3;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;

(* highlightingRod = False; *)
highlightingRod = True;

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

	outputFileName = "rotating_edges_" <> shape <> If[highlightingRod, "_highlighting", ""] <> ".png";
	outputPlotName = "rotating_edge_distances_" <> shape <> ".png";
	imageFolder = "output";
	outputFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	outputPlotName = FileNameJoin[{outputFolder, outputPlotName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"];
	faceCoordinates = Map[#[[1]]&, faces];
	faceCenters = Table[ Mean[faceCoordinates[[k]] ]//FullSimplify, {k, 1, Length[faceCoordinates]}];
	edgeCenters = Map[Mean, edges];
	(* Export[shape <> "_edge_coordinates.json", edges]; *)
	(* Exit[]; *)

	lengthFactor = 4;
	rods = {};
	For[edgeId = 1, edgeId <= Length[edges], edgeId ++,
		edge = edges[[edgeId]];
		edgeCenter = Mean[edge];
		rod = Map[( (# - edgeCenter) * lengthFactor + edgeCenter) &, edge];
		AppendTo[rods, rod];
	];

	rotatedRodDistance[angle_, rodId_] := Module[
		{rod1, rotationMatrix1, rotatedRod1, rod, rotationMatrix, rotatedRod},
		rod1 = rods[[1]];
		rotationMatrix1 = RotationMatrix[angle, Mean[rod1]];
		rotatedRod1 = rod1.rotationMatrix1;
		rod = rods[[rodId]];
		rotationMatrix = RotationMatrix[angle, Mean[rod]];
		rotatedRod = rod.rotationMatrix;

		distSegToSeg[rotatedRod1, rotatedRod ]
	];

	minRodDistance[angle_] := Min[Table[rotatedRodDistance[angle, k], {k, 2, 30}]];

	For[rotationInDegree = 39.8037, rotationInDegree <= 39.8038, rotationInDegree+= 0.0001,
		rotationAngle = rotationInDegree /180 * Pi;

		rodDistances = {};

		minDist = minRodDistance[rotationAngle];
		rodDistances = Map[rotatedRodDistance[rotationAngle, #]&, Range[2, Length[rods]]];
		tallyHead = Sort[Tally[ Round[rodDistances, epsilon] ] ][[ {1, 2, 3} ]];
		gap = tallyHead[[2]][[1]] - tallyHead[[1]][[1]];

		Print[{rotationInDegree, minDist, Length[ Select[rodDistances, # < minDist + 0.0003 &] ] , gap, tallyHead} ];


(* 
3 local maxima
{39.80370000000002,  0.16760835756010592, 8, 2.3700000000015375*^-6, {{0.16760836, 4}, {0.16761073, 4}, {0.36309891, 2}}}
{39.803800000000024, 0.1676075744460097,  8, 8.699999999861596*^-7, {{0.16760757, 4}, {0.16760844, 4}, {0.36309711, 2}}}

{50.11190000000003, 0.16494575463958702, 8, 8.999999999981245*^-7, {{0.16494575, 4}, {0.16494665, 4}, {0.1661902, 2}}}
{50.11200000000003, 0.1649465249998606, 8, 2.3899999999910104*^-6, {{0.16494652, 4}, {0.16494891, 4}, {0.1661882, 2}}}

{50.17830000000004, 0.16486088964901724, 6, 1.7800000000067762*^-6, {{0.16486089, 4}, {0.16486267000000002, 2}, {0.16704261, 4}}}
{50.178400000000046, 0.16486066543826744, 6, 8.999999998038355*^-8, {{0.16486067000000001, 2}, {0.16486076, 4}, {0.16704576000000002, 4}}}

*)

	];


(* 	plot = Plot[ Table[ rotatedRodDistance[angle/180*Pi, k], {k, 2, 30}], {angle, 50, 50.3}, PlotRange -> {0, 0.2} ];
	Export[outputPlotName, plot]; *)

	(* Exit[]; *)

	lighting = {{"Point", White, {50, -50, 50}}};
	
	viewPoint = {50, -50, 50};
	If[shape == "Dodecahedron", viewPoint = {0, 0, 50}];
	If[shape == "Icosahedron", viewPoint = {0, 0, 50}];
	
	(* viewPoint = {0, -50, 0}; *)
	viewPoint = {0.1314327780297834, -0.4045084971874737, 0.6881909602355868} * 100;
	(* Map[Print[{#, vertices[[#]]//N}]&, Range[20]]; *)
	(* Exit[]; *)
	(* viewPoint = vertices[[5]] * 100; *)
	(* viewPoint = {50, -50, 50}; *)

	viewCenter = {0, 0, 0};
	
	elements = {};

	(* angle = 39.8037 / 180.0 * Pi; *)
	angle = 50.111/180.0 * Pi;
	minDist = minRodDistance[angle];
	rotatedRods = {};
	For[rodId = 1, rodId <= Length[rods], rodId++,
		rod = rods[[rodId]];
		rotationMatrix = RotationMatrix[angle, Mean[rod]];
		rotatedRod = rod.rotationMatrix;
		AppendTo[rotatedRods, rotatedRod];
	];

	highlightedRod = Select[rotatedRods, Norm[ Mean[#] - {0.1314327780297834, -0.4045084971874737, 0.6881909602355868}] < 0.001 &];
	Print["Number of highlighted rod"];
	Print[Length[highlightedRod]];
	highlightedRod = highlightedRod[[1]];

	contactingRods = Select[rotatedRods, Norm[ Mean[highlightedRod] - Mean[#] ] > 0.01 && distSegToSeg[highlightedRod, # ] < minDist + 0.001 & ];
	Print["Number of contacting rod"];
	Print[Length[contactingRods]];

	otherRods = Select[rotatedRods, distSegToSeg[highlightedRod, # ] > minDist + 0.001 & ];
	Print["Number of other rod"];
	Print[Length[otherRods]];

  thickness = 0.16/2;
  If[highlightingRod,
  	AppendTo[ elements, { FaceForm[Red], Map[Cylinder[#, thickness]&, {highlightedRod} ] } ];
  	AppendTo[ elements, { FaceForm[Green], Map[Cylinder[#, thickness]&, contactingRods] } ];
  	AppendTo[ elements, { FaceForm[Yellow], Map[Cylinder[#, thickness]&, otherRods] } ];
  	,
		AppendTo[ elements, { FaceForm[Yellow], Map[Cylinder[#, thickness]&, rotatedRods] } ];
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
	(* "Dodecahedron" *)
	(* "Cube" *)
	(* "Octahedron" *)
	(* "Tetrahedron" *)
	"Icosahedron"
};

Map[exportPolyhedron, shapes];

