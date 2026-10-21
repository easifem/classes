!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to use `InitiateFacetElemSD` method to
!          initiate element shape data on facet elements.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./mesh.h5"

  ! Initiate and open the mesh file which is in [[HDF5File_]] format. Then, create an instance of mesh.
  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  ! Initiating the element shape data on cells.

  CALL obj%InitiateElemSD(orderSpace=1, linSpaceElem=obj%refelem, &
    & spaceElem=obj%refelem, &
    & quadTypeForSpace = "GaussLegendre", &
    & continuityTypeForSpace = "H1", &
    & interpolTypeForSpace = "LagrangeInterpolation")
  ! Initiating the element shape data on facet elements.
  CALL obj%InitiateFacetElemSD( &
    & orderSpace=1, &
    & linSpaceElem=obj%facetElements, &
    & spaceElem=obj%facetElements, &
    & quadTypeForSpace = "GaussLegendre", &
    & continuityTypeForSpace = "H1", &
    & interpolTypeForSpace = "LagrangeInterpolation")
  !!
  CALL obj%DisplayFacetElemSD("DisplayFacetElemSD = ")
  !!
  CALL obj%DisplayFacetElements("DisplayFacetElements = ")
  !!
  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
