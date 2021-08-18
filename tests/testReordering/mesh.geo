Point(1)={0.0, 0.0, 0.0};
Point(2)={2.0,0.0,0.0};
Point(3)={2.0,2.0,0.0};
Point(4)={0.0,2.0,0.0};
Point(5)={1.0,0.0,0.0};
Point(6)={1.0,2.0,0.0};

Line(1)={1,5};
Line(2)={5,2};
Line(3)={2,3};
Line(4)={3,6};
Line(5)={6,4};
Line(6)={4,1};
Line(7)={5,6};

Line Loop(1)={1,7,5,6};
Line Loop(2)={2,3,4,-7};

Plane Surface(1)={1};
Plane Surface(2)={2};

Physical Surface("region_1", 1)={1};
Physical Surface("region_2", 2)={2};
Physical Line("bottom",1)={1,2};
Physical Line("right",2)={3};
Physical Line( "top", 3)={4,5};
Physical Line( "left", 4)={6};
//Physical Line( "interface", 5)={7};


