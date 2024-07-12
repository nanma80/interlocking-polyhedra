#!/usr/bin/env wolframscript

exportToPov = True;
specialViewPoint = False;
displayLevel = 5;

imageSize = 720 / 3 * {4, 3};
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

(* plotChart = False; *)
plotChart = True;

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

findMinDistance[components_] := Module[
	{ minDistance, edge1Id, edge2Id, edge1, edge2 },
	
	minDistance = 10.0;

	For[edge1Id = 1, edge1Id <= Length[components[[1]]], edge1Id++,
		edge1 = components[[1]][[edge1Id]];
		For[edge2Id = 1, edge2Id <= Length[components[[2]]], edge2Id++,
			edge2 = components[[2]][[edge2Id]];

			If[MemberQ[matchingEdges, {edge1Id, edge2Id}] == False,
				(* ignore the case when two half edges come from the same long edge *)
				distance = distSegToSeg[ edge1, edge2 ];
				If[distance < minDistance, minDistance = distance];
				If[minDistance < epsilon,
					Return[0];
				];

			];
		];
	];

	Return[minDistance];
];

moveComponents[components_, moveDistance_, moveAngleInDegree_] := Module[
	{movedComponents, moveAngle, cid, component, moveDirection, rotationMatrix, movedComponent, movedMinDistance},
	movedComponents = {{}, {}};
	moveAngle = moveAngleInDegree * Pi/180.;
	For[cid = 1, cid <= Length[components], cid++,
		component = components[[cid]];
		(* component 1: bottom; component 2: top *)
		moveDirection = {0, 0, 1} * If[cid == 1, -1, 1];
		rotationMatrix = RotationMatrix[moveAngle, moveDirection];

		movedComponent = Map[ (# + moveDirection * moveDistance).rotationMatrix &, component, {2} ];
		movedComponents[[cid]] = movedComponent;
	];
	Return[movedComponents];
];

moveAndFindMinDistance[components_, moveDistance_, moveAngleInDegree_] := Module[
	{movedComponents, movedMinDistance},
	movedComponents = moveComponents[components, moveDistance, moveAngleInDegree];
	movedMinDistance = findMinDistance[movedComponents];
	Return[movedMinDistance];
];

exportPolyhedron[shape_] := Module[
	{outputFileName, vertices, edges, faces, thickness, lighting, graph},

	outputFileName = "compound_5_tetra_split_test.png";
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

	(* Print["faceCenters"]; *)
	(* Map[Print[{#, faceCenters[[#]]//N}]&, Range[Length[faceCenters]]]; *)

	copyIndices = {
		{1, 11, 17, 20},
		{4, 7, 9, 2},
		{6, 16, 10, 19},
		{5, 8, 18, 14},
		{3, 12, 13, 15}
	};

	lighting = {{"Point", White, {50, -50, 50}}};
	range = 1.0 * Norm[vertices[[1]]];
	color = allColors[ allSchlaefliSymbols[shape]];
	
	viewPoint = If[shape == "Dodecahedron", {0, 0, 10}, {50, -50, 50}];
	viewPoint = faceCenters[[2]] * 10;

	viewCenter = {0, 0, 0};

	elements = {};

	minDistance = 0.1689477446214834;
	thickness = minDistance/2;

	components = {{}, {}};

	matchingEdges = {};

	For[copyId = 1, copyId <= Length[copyIndices], copyId++,
		copyIndex = copyIndices[[copyId]];
		indexPairs = Subsets[copyIndex, {2}];
		copyEdges = Map[{vertices[[#[[1]]]], vertices[[#[[2]]]]}&, indexPairs];
		edgeDistances = Map[ EuclideanDistance[#[[1]], #[[2]] ]&, copyEdges ];
		displayComponents = {{}, {}};

		For[edgeId = 1, edgeId <= Length[copyEdges], edgeId++,
			edge = copyEdges[[edgeId]];
			edgeCenter = Mean[edge];
			edgeCenterZ = edgeCenter[[3]];
			If[Abs[edgeCenterZ] > 0.5,
				(* edge on one side *)
				side = If[edgeCenterZ < 0, 1, 2];
				AppendTo[components[[side]], edge];
				AppendTo[displayComponents[[side]], edge],
				(* edge crossing cut *)
				edge1 = {edge[[1]], edgeCenter};
				edge2 = {edge[[2]], edgeCenter};
				side = If[edge[[1]][[3]] < 0, 1, 2];
				AppendTo[components[[side]], edge1];
				AppendTo[displayComponents[[side]], edge1];
				AppendTo[components[[ 3 - side]], edge2];
				AppendTo[displayComponents[[ 3 - side]], edge2];
				AppendTo[matchingEdges, Map[Length, components]];
			];
		];


		(* displayEdges = copyEdges; *)
		(* displayVertices = Union[Flatten[displayEdges, 1], SameTest -> ApproxSamePoint]; *)

		color = colors[[copyId]];

		opacity = 1.0;

		(* AppendTo[ elements, { FaceForm[color], Map[Cylinder[#, thickness]&, displayEdges] } ]; *)
		(* AppendTo[ elements, { FaceForm[color], Map[Sphere[#, thickness]&, displayVertices] } ]; *)

(* 		componentsToDisplay = { 2 };
		For[cid = 1, cid <= Length[componentsToDisplay], cid++,
			componentId = componentsToDisplay[[cid]];

			AppendTo[ elements, { FaceForm[color], Map[Cylinder[#, thickness]&, displayComponents[[componentId]]] } ];
		]; *)
	];

	(* Print[ moveAndFindMinDistance[components, 0, 0] ]; *)

	(* Exit[]; *)

	If[plotChart,
		graph = ContourPlot[
			moveAndFindMinDistance[components, moveDistance, moveAngleInDegree],
			{moveAngleInDegree, -36, 36},
			{moveDistance, 0, 0.6}		
			, PlotPoints -> 30
			, MaxRecursion -> 2
			, Mesh -> Full
			, Contours -> {{minDistance * 0.02, Dashed}, {minDistance * 0.5, Dashed}, { minDistance * 0.95, Thick }, {minDistance * 1.5, Dashed}}
		];

		Export[outputFileName, graph],

		movedComponents = moveComponents[components, 0.1, 5];

		componentsToDisplay = { 1, 2 };
		componentColors = {Red, White};

		For[cid = 1, cid <= Length[componentsToDisplay], cid++,
			componentId = componentsToDisplay[[cid]];
			AppendTo[ elements, { FaceForm[ componentColors[[cid]] ], Map[Cylinder[#, thickness]&, movedComponents[[componentId]]] } ];
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
];

shapes = {
	"Dodecahedron"
};

Map[exportPolyhedron, shapes];

