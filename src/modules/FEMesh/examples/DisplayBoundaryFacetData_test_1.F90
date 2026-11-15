!> author: Vikas Sharma, Ph. D.
! date: 2025-06-02
! summary:  This example shows how to use `DisplayBoundaryFacetData`
! method to display the content of boundary facet data.
!
! This example shows how to use `DisplayBoundaryFacetData` method to display
! the content of boundary facet data.

PROGRAM main
  USE FEMesh_Class
  USE HDF5File_Class
  USE GlobalData

  IMPLICIT NONE

  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./meshdata/small_tri3_mesh.h5"

  ! Initiate and open the mesh file which is in [[HDF5File_]] format.
  ! Then, create an instance of mesh.

  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, dim=2)

  ! Displaying the node data.

  CALL obj%DisplayBoundaryFacetData("Boundary facet data")

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
