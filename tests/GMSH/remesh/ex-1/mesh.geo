/**************************************
Geometry    :: Rectangle
Author      :: Vikas Sharma
Data        :: 04-May-2020
**************************************/

// mesh sizes
s = 1; p1 = newp; Point(p1) = {0, 0, 0, s};
s = 1; p2 = newp; Point(p2) = {5, 0, 0, s};
s = 1; p3 = newp; Point(p3) = {5, 2, 0, s};
s = 1; p4 = newp; Point(p4) = {5, 10, 0, s};
s = 1; p5 = newp; Point(p5) = {0, 10, 0, s};
s = 1; p6 = newp; Point(p6) = {2.5, 0, 0, s};
s = 1; p7 = newp; Point(p7) = {2.5, 10, 0, s};

// Lines
l1 = newl; Line(l1) = {p1,p6};
l2 = newl; Line(l2) = {p6,p2};
l3 = newl; Line(l3) = {p2,p3};
l4 = newl; Line(l4) = {p3,p4};
l5 = newl; Line(l5)={p4, p7};
l6 = newl; Line(l6) = {p7,p5};
l7 = newl; Line(l7) = {p5,p1};
l8 = newl; Line(l8) = {p6,p7};

// // Line Loops
ll1 = newll; Line Loop(ll1) = {l1,l8,l6,l7};
ll2 = newll; Line Loop(ll2) = {l2,l3,l4,l5,-l8};

// // Physical Surfaces
ps1 = news; Plane Surface(ps1) = {ll1};
ps2 = news; Plane Surface(ps2) = {ll2};

Physical Surface( "Omega_1", 1 ) = {ps1};
Physical Surface( "Omega_2", 2 ) = {ps2};
Physical Line( "Gamma_1", 1 ) = {l1, l2}; // bottom
Physical Line( "Gamma_2", 2 ) = {l3}; // right1
Physical Line( "Gamma_3", 3 ) = {l4}; // right2
Physical Line( "Gamma_4", 4 ) = {l5}; // top_r
Physical Line( "Gamma_5", 5 ) = {l6}; // top_l
Physical Line( "Gamma_6", 6 ) = {l7}; //left
Physical Line("Gamma_7", 7) = {l8}; //mid_top

// Recombine Surface {ps1};
// Mesh 2;


