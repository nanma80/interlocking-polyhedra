#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 2;
(* searchingForOptimalFactors = True; *)
searchingForOptimalFactors = False;
(* halfCopies = True; *)
halfCopies = False;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

optimalShiftFactor = <|
	"Octahedron" -> 0.04,
	"Icosahedron" -> 0.06
|>;

optimalRotationAngleFactor = <|
	"Octahedron" -> 0.32,
	"Icosahedron" -> 0.30
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

findMinDistance[edges_, explosionVectors_, faceCoordinates_, shiftFactor_, rotationAngleFactor_ ] := Module[
	{ rotationAngle, minDistance, copy, copyCenter, copyEdges, copyVertices, copy1Edges, copy1Vertices, copyId, copy1EdgeId, copyEdgeId },
	
	sideCountInFace = Length[faceCoordinates[[1]]];
	rotationAngle = 2 Pi / sideCountInFace * rotationAngleFactor;

	minDistance = 10.0;

	For[copyId = 1, copyId <= Length[explosionVectors], copyId++,
		face = faceCoordinates[[copyId]];
		(* assume face is a triangle, now build a tetrahedron *)
		originalEdges = getEdgesFromFace[face];
		faceCenter = Mean[face];
		edgeLength = EuclideanDistance[originalEdges[[1]][[1]], originalEdges[[1]][[2]]];
		height = Sqrt[2/3] * edgeLength;
		apex = faceCenter * (Norm[faceCenter] + height)/Norm[faceCenter];
		originalEdges = Join[originalEdges, Map[{apex, #}&, face]];

		copyCenter = explosionVectors[[copyId]] * shiftFactor;

		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, originalEdges, {2} ];

		If[copyId == 1,
			copy1Edges = copyEdges;
		];

		If[copyId >= 2,
			For[copy1EdgeId = 1, copy1EdgeId <= Length[copy1Edges], copy1EdgeId++,
				For[copyEdgeId = 1, copyEdgeId <= Length[copyEdges], copyEdgeId++,
					distance = distSegToSeg[ copy1Edges[[copy1EdgeId]], copyEdges[[copyEdgeId]] ];
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

	outputFileName = "face_explosion_" <> shape <> "_level_" <> ToString[displayLevel] <> If[halfCopies, "_half", ""] <> ".png";
	imageFolder = "output";
	shapeFolder = "tetra_on_" <> shape;
	outputFolder = FileNameJoin[{imageFolder, shapeFolder}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = FileNameJoin[{outputFolder, outputFileName}];
	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	vertices = PolyhedronData[shape, "Vertices"];
	edges = PolyhedronData[shape, "EdgeCoordinates"]//N;
	faces = PolyhedronData[shape, "Polygons"]//N;
	faceCoordinates = Map[#[[1]]&, faces];

	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	color = allColors[ allSchlaefliSymbols[shape]];
	
	viewPoint = If[shape == "Dodecahedron" || shape == "Icosahedron", {0, 0, 50}, {50, -50, 50}];

	viewCenter = {0, 0, 0};
	faceCenters = Table[ Mean[faceCoordinates[[k]] ], {k, 1, Length[faceCoordinates]}];
	(* Map[Print[{#, explosionVectors[[#]]//N}]&, Range[Length[explosionVectors]]]; *)
	explosionVectors = faceCenters;
	If[halfCopies, explosionVectors = Select[explosionVectors, #.randomDirection > 0 & ]];

	elements = {};

	core = <||>;
	core["edges"] = edges;
	core["vertices"] = vertices;

	copies = {};

	shiftFactor = optimalShiftFactor[shape];
	rotationAngleFactor = optimalRotationAngleFactor[shape];
	If[halfCopies,
		shiftFactor = 1.0;
		rotationAngleFactor = 0.25;
	];

	range = range * (1 + shiftFactor) / 1.3;

	minDistance = findMinDistance[edges, explosionVectors, faceCoordinates, shiftFactor, rotationAngleFactor];
	Print["minDistance, shiftFactor, rotationAngleFactor"];
	Print[{minDistance, shiftFactor, rotationAngleFactor}];

	thickness = minDistance/2;

	If[searchingForOptimalFactors,
		maxMinDistance = 0;	
		For[shiftFactor = -1.0, shiftFactor < 0.1, shiftFactor += 0.01,
			(* Print["shiftFactor:"]; *)
			(* Print[shiftFactor]; *)
			For[angleFactor = 0.0, angleFactor <= 1.0, angleFactor += 0.01,
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

	sideCountInFace = Length[faceCoordinates[[1]]];
	rotationAngle = 2 Pi / sideCountInFace * rotationAngleFactor;

	For[copyId = 1, copyId <= Length[explosionVectors], copyId++,
		face = faceCoordinates[[copyId]];
		(* assume face is a triangle, now build a tetrahedron *)
		originalEdges = getEdgesFromFace[face];
		faceCenter = Mean[face];
		edgeLength = EuclideanDistance[originalEdges[[1]][[1]], originalEdges[[1]][[2]]];
		height = Sqrt[2/3] * edgeLength;
		apex = faceCenter * (Norm[faceCenter] + height)/Norm[faceCenter];
		originalEdges = Join[originalEdges, Map[{apex, #}&, face]];

		copy = <||>;
		copyCenter = explosionVectors[[copyId]] * shiftFactor;

		rotationMatrix = RotationMatrix[rotationAngle, copyCenter];

		copyEdges = Map[ (# + copyCenter).rotationMatrix &, originalEdges, {2} ];

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

(* 

Icosahedron:

{0.028904897717780167, -0.98, 0.23000000000000007}
{0.030026267731339797, -0.7499999999999998, 0.09999999999999999}
{0.059433625057294555, -0.6799999999999997, 0.20000000000000004}
{0.07830951568669882, 0.06000000000000076, 0.3000000000000001}

Octahedron:
{0.06580671416249433, -0.8899999999999999, 0.35000000000000014}
{0.09340325585715792, 0.04000000000000076, 0.3200000000000001}

 *)