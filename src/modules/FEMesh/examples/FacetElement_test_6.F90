!> author: Vikas Sharma, Ph. D.
! summary: This example shows the use of methods for getting information of
!          `domainFacetData`.
!
! Domain facet elements have `DOMAIN_BOUNDARY_ELEMENT` type.
!
! In this example we do create an instance of [[Domain_]], therefore, we build
! the information about `DOMAIN_BOUNDARY_ELEMENT` (`domainFacetData`) and
! `BOUNDARY_ELEMENT` (`meshFacetData`).

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

CALL domainFile%Initiate(FileName=filename, MODE="READ")
CALL domainFile%Open()
CALL dom%Initiate(hdf5=domainFile, group="")
obj => dom%getMeshPointer(dim=dom%getNSD(), entityNum=1)

! Getting the total number of Domain Facet elements in the mesh.

CALL Display(obj%getTotalDomainFacetElements(), &
  & "getTotalDomainFacetElements=")

! Getting the master cell number of an domain facet element.

DO iel = 1, obj%getTotalDomainFacetElements()
  ii  = obj%getMasterCellNumber( &
    & facetElement=iel, &
    & elementType=DOMAIN_BOUNDARY_ELEMENT)
  CALL Display(ii, "masterCell( " // tostring(iel) // " ) = ")
END DO

! Getting the `masterLocalFacetID` of an domain facet element.

DO iel = 1, obj%getTotalDomainFacetElements()
  ii  = obj%getMasterCellNumber(facetElement=iel, &
    & elementType=DOMAIN_BOUNDARY_ELEMENT)
  !!
  CALL Display(obj%getConnectivity(globalElement=ii), &
    & "cellNptrs = ")
  !!
  CALL Display(obj%getFacetConnectivity(facetElement=iel, &
    & elementType=DOMAIN_BOUNDARY_ELEMENT, &
    & isMaster=.TRUE.), "facetNptrs=")
  !!
  CALL Display(obj%getLocalFacetID( &
    & facetElement=iel, &
    & elementType=DOMAIN_BOUNDARY_ELEMENT, &
    & isMaster = .TRUE.), &
    & "masterLocalFacetID( " // tostring(iel) // " ) = ")
END DO

! Getting the slave number of an internal facet element. `DomainFacetElement`
! does not have a slave.

  NULLIFY(obj)
  CALL dom%Deallocate()
  CALL domainFile%Close()
  CALL domainFile%Deallocate()
END PROGRAM main
