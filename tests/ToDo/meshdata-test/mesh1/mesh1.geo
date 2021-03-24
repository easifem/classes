/**************************************
Geometry    :: Rectangle
Author      :: Vikas Sharma
Institute   :: Kyoto University, Japan
Data        :: 15-Jan-2018
**************************************/

// SetFactory( "OpenCASCADE");
// Mesh.CharacteristicLengthMin = 0.5;
// Rectangle( 1 ) = {-1,-1,0,1,1};
// h = 1.0;
// Out[] = Extrude{0,0,h}{Surface{1}; Layers{{2},{1.0}}; Recombine;};

Mesh.CharacteristicLengthMin = 0.1;
Point(1) = {0,0,0};
Point(2) = {1,0,0};
Line(1) = {1,2};
hy = 1.0; hz = 1.0;

//make surface from line extrusion
Out[] = Extrude{0,hy,0}{Line{1}; Layers{5}; Recombine;};

//make volume from surface extrusion
Out[] = Extrude{0,0,hz}{Surface{Out[1]}; Layers{5}; Recombine;};

Physical Surface( "Top", 1 ) = {27};
Physical Surface( "Bottom", 2 ) = {5};
Physical Surface( "Lateral-1", 3 ) = {18};
Physical Surface( "Lateral-2", 4 ) = {22};
Physical Surface( "Lateral-3", 5 ) = {26};
Physical Surface( "Lateral-4", 6 ) = {14};
Physical Volume( "Omega", 1 ) = {1};
