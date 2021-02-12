!! we USE no penetration boundary condition

PROGRAM Main
USE easifem
IMPLICIT NONE


TYPE( gmsh_ ) :: gmsh, newgmsh
TYPE( domain_ ) :: dom
INTEGER( i4b ) :: tnodes
INTEGER( i4b ), PARAMETER :: NSD = 2
INTEGER( i4b ), PARAMETER :: NNT = 2
INTEGER( i4b ), PARAMETER :: BOTTOM=1, RIGHT1=2, RIGHT2=3, TOP_R=4, &
  & TOP_L=5, LEFT=6, MID=7
TYPE( string ), ALLOCATABLE :: facetmesh( :, : )
INTEGER( I4B ) :: ii


  ii = gmsh%initialize( NSD )
  ii = gmsh%open( "./", "mesh", ".msh" )
  tnodes = gmsh % model % mesh % totalnodes( )

  allocate( facetmesh( 2, 2 ) )
  facetmesh(1,1) = "Gamma_4"
  facetmesh(1,2) = "Omega_2"
  facetmesh(2,1) = "Gamma_5"
  facetmesh(2,2) = "Omega_1"
  CALL gmsh%model%mesh%getElements( dom, facetmesh )

  CALL gmsh%model%mesh%writeMesh("./", "deform_mesh", ".msh", dom%nodes)

  ii=gmsh%finalize()
END PROGRAM main