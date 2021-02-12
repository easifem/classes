program main
use easifem

type( gmsh_ ) :: gmsh
type( domain_ ) :: dom
integer( i4b ) :: ii

ii = gmsh%initialize(NSD)
ii = gmsh % open( "./", "mesh", ".msh" )
tnodes = gmsh%model%mesh%totalNodes()
call gmsh%model%mesh%getElements(dom)
ii=gmsh%finalize()

end program main