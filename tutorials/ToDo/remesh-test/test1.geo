Merge "mesh.msh";
CreateGeometry;

// Create a volume as usual
ll1 = newll; Line Loop(ll1) = Line{ : };
ps1 = news; Plane Surface(ps1) = {ll1};

// // element size imposed by a size field
// Field[1] = MathEval;
// Field[1].F = "4";
// Background Field = 1;

// funny = DefineNumber[0, Choices{0, 1}, Name "Parameters/Apply funny mesh size field?"];
// If(funny)
//     Field[1].F = "2*Sin((x+y)/5) + 3";
// EndIf

Physical Surface("new") = {ll1};
