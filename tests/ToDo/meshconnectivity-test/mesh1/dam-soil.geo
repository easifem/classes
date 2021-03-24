// Gmsh project created on Thu Oct 25 22:31:24 2018
// SetFactory("");

meshCharacteristicLengthMin=0.001;
meshCharacteristicLengthMax=100.00;
smin = 6.0;
smax = 6.0;

W1 = 40; W2 = 4; H1 = 50; H2 = 5;
W3 = 160; H3 = 150;

p1 = newp; Point(p1) = {0, 0, 0, smax};
p2 = newp; Point(p2) = {W1, 0, 0, smax};
p3 = newp; Point(p3) = {W2, H1-H2, 0, smax};
p4 = newp; Point(p4) = {W2, H1, 0, 0, smax};
p5 = newp; Point(p5) = {0, H1, 0, smax};
p6 = newp; Point(p6) = {-W3, 0, 0, smax};
p7 = newp; Point(p7) = {-W3, -H3, 0, smax};
p8 = newp; Point(p8) = {W1+W3, -H3, 0, smax};
p9 = newp; Point(p9) = {W1+W3, 0, 0, smax};

l1 = newl; Line( l1 ) = { p1, p2 };
l2 = newl; Line( l2 ) = { p2, p3 };
l3 = newl; Line( l3 ) = { p3, p4 };
l4 = newl; Line( l4 ) = { p4, p5 };
l5 = newl; Line( l5 ) = { p5, p1 };
l6 = newl; Line( l6 ) = { p1, p6 };
l7 = newl; Line( l7 ) = { p6, p7 };
l8 = newl; Line( l8 ) = { p7, p8 };
l9 = newl; Line( l9 ) = { p8, p9 };
l10 = newl; Line( l10 ) = { p9, p2 };

ll1 = newll; Line Loop( ll1 ) = {-l1,l6,l7,l8,l9,l10};
ll2 = newll; Line Loop( ll2 ) = {l1,l2,l3,l4,l5};
ps1 = news; Plane Surface(ps1) = {ll1};
ps2 = news; Plane Surface(ps2) = {ll2};

// physical surface
Left = l7; Bottom = l8; Right = l9;
Physical Surface("Soil", 1) = {ps1};
Physical Surface("Dam", 2 ) = {ps2};
Physical Line("Left", 1) = {Left};
Physical Line("Bottom", 2) = {Bottom};
Physical Line("Right", 3) = {Right};
Physical Line("VerticalABC", 4) = {Left, Right};
Physical Line("HorizontalABC", 5) = {Bottom};
