!> author: Vikas Sharma, Ph. D.
! summary: This example shows the use of some simple methods related to the
!          domain facet elements of the mesh.
!
! In this example we do not create a domain, therefore, we have no information
! about the MeshFacetData. This information is only avaiable when we create a
! domain. this is because `MeshFacetData` denotes the interface between two
! meshes.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./mesh.h5"

! Initiate and open the mesh file which is in [[HDF5File_]] format. Then,
! create an instance of mesh.

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%Open()
CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

! Getting the total number of Domain Facet elements in the mesh.

CALL Display(obj%getTotalDomainFacetElements(), &
  & "getTotalDomainFacetElements=")

! Getting the master cell number of an domain facet element.

DO iel = 1, obj%getTotalDomainFacetElements()
  ii  = obj%getMasterCellNumber(facetElement=iel, &
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

  CALL obj%Deallocate()
  CALL meshfile%Close()
  CALL meshfile%Deallocate()
END PROGRAM main
