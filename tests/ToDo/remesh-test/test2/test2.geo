p1=newp; Point(p1) = {0,0,0};
p2=newp; Point(p2) = {1,0,0};
p3=newp; Point(p3) = {2,0,0};
p4=newp; Point(p4) = {2,1,0};
p5=newp; Point(p5) = {1,1,0};
p6=newp; Point(p6) = {0,1,0};

l1=newl; Line(l1) = {p1,p2};
l2=newl; Line(l2) = {p2,p3};
l3=newl; Line(l3) = {p3,p4};
l4=newl; Line(l4) = {p4,p5};
l5=newl; Line(l5) = {p5,p6};
l6=newl; Line(l6) = {p6,p1};
l7=newl; Line(l7) = {p2,p5};
l8=newl; Line(l8) = {p5,p2};
l9=newl; Line(l9) = {p1,p3};
l10=newl; Line(l10) = {p4,p6};

ll1=newll; Line Loop( ll1 )={l1,l7,l5,l6};
ll2=newll; Line Loop( ll2 )={l2,l3,l4,l8};

ps1=news; Plane Surface( ps1 ) = {ll1};
ps2=news; Plane Surface( ps2 ) = {ll2};

Physical Surface( "Omega1" )={ps1};
Physical Surface( "Omega2" )={ps2};
Physical Line( "Left" ) = {l6};
Physical Line( "Bottom" ) = {l9};
Physical Line( "Right" ) = {l3};
Physical Line( "Top" ) = {l10};

Coherence;
