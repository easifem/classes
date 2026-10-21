!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to use `InitiateElemSD` method to initiate
!          element shape data for space-time FEM.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./mesh.h5"

  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  CALL obj%InitiateElemSD(orderSpace=1, linSpaceElem=obj%refelem, &
    & spaceElem=obj%refelem, &
    & quadTypeForSpace = "GaussLegendre", &
    & continuityTypeForSpace = "H1", &
    & interpolTypeForSpace = "LagrangeInterpolation")

  CALL Display(obj%spaceElemSD, "spaceElemSD=")
  CALL Display("OK")

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
