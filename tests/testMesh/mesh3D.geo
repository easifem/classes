// Gmsh project created on Thu Jul 22 15:49:19 2021
SetFactory("OpenCASCADE");
//+
Box(1) = {0, 0, 0, 1, 1, 1};
//+
Physical Surface("top") = {6};
//+
Physical Surface("bottom") = {5};
//+
Physical Surface("front") = {3};
//+
Physical Surface("back") = {4};
//+
Physical Surface("left") = {1};
//+
Physical Surface("right") = {2};
//+
Physical Volume("omega") = {1};
