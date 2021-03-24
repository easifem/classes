// Parameters
L = 13; H = 5; 
s0 = 1.0; 
n1 = 5;
n2 = 5;
x1 = 6.25; y1 = H-3.0; r1 = 0.6;
x2 = 6.75; y2 = H-4.0; r2 = 0.3;

// Points
p1 = newp; Point(p1) = {0, 0, 0, s0};
p2 = newp; Point(p2) = {L, 0, 0, s0};
p3 = newp; Point(p3) = {L, H, 0, s0};
p4 = newp; Point(p4) = {0, H, 0, s0};

// Lines
l1 = newl; Line(l1) = {p1,p2};
l2 = newl; Line(l2) = {p2,p3};
l3 = newl; Line(l3) = {p3,p4};
l4 = newl; Line(l4) = {p4,p1};

// Line Loops
ll1 = newll; Line Loop(ll1) = {l1,l2,l3,l4};

// Cavity-1
c1 = newp; Point(c1)={x1,y1,0};

p5 = newp; Point(p5)={x1,y1-r1,0};
p6 = newp; Point(p6)={x1+r1,y1,0};
p7 = newp; Point(p7)={x1,y1+r1,0};
p8 = newp; Point(p8)={x1-r1,y1,0};

c11 = newl; Circle(c11) = {p6,c1,p5};
c12 = newl; Circle(c12) = {p5,c1,p8};
c13 = newl; Circle(c13) = {p8,c1,p7};
c14 = newl; Circle(c14) = {p7,c1,p6};

ll2=newll; Curve Loop(ll2)={c11,c12,c13,c14};
Transfinite Line{c11,c12,c13,c14}=n1;

// Physical entities
ps1 = news; Plane Surface(ps1) = { ll1, ll2 };

Physical Surface( "Omega", 1 ) = {ps1};
Physical Line( "Dirichlet", 1 ) = {l1, l2, l4};
Physical Curve("Nitsche", 2) = { c11, c12, c13, c14 };
Physical Line( "Free", 3 ) = { l3 };

//Transfinite Surface{ps1} Left;
//Recombine Surface {ps1};