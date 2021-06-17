Point(1)={0.0, 0.0, 0.0};
Point(2)={1.0,0.0,0.0};
Point(3)={1.0,1.0,0.0};
Point(4)={0.0,1.0,0.0};
Line(1)={1,2};
Line(2)={2,3};
Line(3)={3,4};
Line(4)={4,1};
Line Loop(1)={1,2,3,4};
Plane Surface(1)={1};

Physical Surface("domain", 1)={1};
Physical Line("bottom",1)={1};
Physical Line("right",2)={2};
Physical Line( "top", 3)={3};
Physical Line( "left", 4)={4};
Physical Line( "dbc", 5)={1,2};


