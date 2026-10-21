!> author: Vikas Sharma, Ph. D.
! summary: This example shows the use of some simple methods related to the
!          internal facet elements of the mesh.
!
! - In this example we do not create a domain, therefore, we have no
! information about the MeshFacetData.
! - This information is only avaiable when we create a domain.
! - This is because `MeshFacetData` denotes the interface between two meshes.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./mesh.h5"

  ! Initiate and open the mesh file which is in [[HDF5File_]] format.
  ! Then, create an instance of mesh.

  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  ! Getting the total number of Internal Facet elements in the mesh.

  CALL Display(obj%GetTotalInternalFacetElements(), &
    & "GetTotalInternalFacetElements=")

  ! Getting the total number of boundary facet elements in the mesh.

  CALL Display(obj%getTotalBoundaryFacetElements(), &
    & "getTotalBoundaryFacetElements=")

  ! Getting the total number of Domain Facet elements in the mesh.

  CALL Display(obj%getTotalDomainFacetElements(), &
    & "getTotalDomainFacetElements=")

  ! Getting the total number of Mesh Facet elements in the mesh.

  CALL Display(obj%getTotalMeshFacetElements(), &
    & "getTotalMeshFacetElements=")

  ! Getting the master cell number of an internal facet element.

  DO iel = 1, obj%getTotalInternalFacetElements()
    CALL Display(obj%getMasterCellNumber(facetElement=iel, &
      & elementType=INTERNAL_ELEMENT), &
      & "masterCell( " // tostring(iel) // " ) = ")
  END DO

  ! Getting the slave number of an internal facet element.

  DO iel = 1, obj%getTotalInternalFacetElements()
    CALL Display(obj%getSlaveCellNumber(facetElement=iel, &
      & elementType=INTERNAL_ELEMENT), &
      & "SlaveCell( " // tostring(iel) // " ) = ")
  END DO

  ! Getting the connectivity of an internal facet element

  DO iel = 1, obj%getTotalInternalFacetElements()
    CALL Display(obj%getFacetConnectivity(facetElement=iel, &
      & elementType=INTERNAL_ELEMENT, isMaster=.TRUE.), &
      & "internalFacetElementData( " // tostring(iel) // " ) = ")
  END DO

  CALL obj%Deallocate()
  CALL meshfile%Close()
  CALL meshfile%Deallocate()
END PROGRAM main
