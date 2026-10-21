!> author: Vikas Sharma, Ph. D.
! summary: This example shows the use of `GetFacetElementType` method of
!          [[Mesh_]] class.
!
! This example is similar to example 15.
!
! In this example we do create an instance of [[Domain_]], therefore, we build
! the information about `DOMAIN_BOUNDARY_ELEMENT`.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  CLASS(Mesh_), POINTER :: obj
  TYPE(Domain_) :: dom
  TYPE(HDF5File_) :: domainFile
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./mesh.h5"

! Initiate and open the mesh file which is in [[HDF5File_]] format. Then,
! create an instance of mesh.

CALL domainFile%Initiate(FileName="./mesh.h5", MODE="READ")
CALL domainFile%Open()
CALL dom%Initiate(hdf5=domainFile, group="")
obj => dom%getMeshPointer(dim=dom%getNSD(), entityNum=1)

! Getting the facet element type of a given cell element. In this example,
! cell element is a triangle, so there are 3 faces. Each face can be
! `INTERNAL_ELEMENT`, DOMAIN_ELEMENT.
!
! In the case given below all faces are `INTERNAL_ELEMENT`

iel = 20
CALL Display(obj%getConnectivity(iel), "cellElement ( "//tostring(iel)&
  & // " ) = ")
CALL Display(obj%isBoundaryElement(iel), "isBoundaryElement=")
CALL Display(obj%getFacetElementType(iel), &
    & "facet element type=")

! In this case two faces are `INTERNAL_ELEMENT`, and one face is
! `DOMAIN_BOUNDARY_ELEMENT`.

iel = 17
CALL Display(obj%getConnectivity(iel), "cellElement ( "//tostring(iel)&
  & // " ) = ")
CALL Display(obj%isBoundaryElement(iel), "isBoundaryElement=")
CALL Display(obj%getFacetElementType(iel), &
    & "facet element type=")
!!

! In this case two faces are `INTERNAL_ELEMENT`, and one face is
! `BOUNDARY_ELEMENT`

iel = 23
CALL Display(obj%getConnectivity(iel), "cellElement ( "//tostring(iel)&
  & // " ) = ")
CALL Display(obj%getFacetElementType(iel), &
    & "facet element type=")
CALL Display(obj%isBoundaryElement(iel), "isBoundaryElement=")
  NULLIFY(obj)
  CALL dom%Deallocate()
  CALL domainFile%Close()
  CALL domainFile%Deallocate()
END PROGRAM main
