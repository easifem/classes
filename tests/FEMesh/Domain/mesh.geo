/*------------------------------------

    (4) ----------------------------------(3)
    |                                     |
    |       x1,y1,r1   x2,y2,r2
    |                                     |
    (1) ---------------------------------(2)

------------------------------------*/


// Parameters
L1 = 4; L2=4; L3=4; H = 4;
s1 = 4; s2 = 4;

// Points
p1 = newp; Point(p1) = {0, 0, 0, s1};
p2 = newp; Point(p2) = {L1, 0, 0, s1};
p3 = newp; Point(p3) = {L2+L1, 0, 0, s1};
p4 = newp; Point(p4) = {L3+L2+L1, 0, 0, s1};
p5 = newp; Point(p5) = {L3+L2+L1, H, 0, s1};
p6 = newp; Point(p6) = {L2+L1, H, 0, s2};
p7 = newp; Point(p7) = {L1, H, 0, s2};
p8 = newp; Point(p8) = {0, H, 0, s1};

// Lines
l1 = newl; Line(l1) = {p1,p2};
l2 = newl; Line(l2) = {p2,p3};
l3 = newl; Line(l3) = {p3,p4};
l4 = newl; Line(l4) = {p4,p5};
l5 = newl; Line(l5) = {p5,p6};
l6 = newl; Line(l6) = {p6,p7};
l7 = newl; Line(l7) = {p7,p8};
l8 = newl; Line(l8) = {p8,p1};
l9 = newl; Line(l9) = {p2,p7};
l10 = newl; Line(l10) = {p3,p6};

// Line Loops
ll1 = newll; Line Loop(ll1) = {l1,l9,l7,l8};
ll2 = newll; Line Loop(ll2) = {l2,l10,l6,-l9};
ll3 = newll; Line Loop(ll3) = {l3,l4,l5,-l10};

// Physical Surfaces
ps1 = news; Plane Surface(ps1) = {ll1};
ps2 = news; Plane Surface(ps2) = {ll2};
ps3 = news; Plane Surface(ps3) = {ll3};

Physical Surface( "Omega_l", 1 ) = {ps1};
Physical Surface( "Omega_c", 2 ) = {ps2};
Physical Surface( "Omega_r", 3 ) = {ps3};
Physical Line( "Gamma", 1 ) = {l1:l8};
Physical Line( "Gamma_g_x", 2 ) = {l1, l2, l3, l4, l8};
Physical Line( "Gamma_g_y", 3 ) = {l1, l2, l3, l4, l8};
Physical Line( "Gamma_m", 4 ) = {l6};

// Transfinite Surface{ps1} Left;
//Recombine Surface {ps1};

//Mesh 2;