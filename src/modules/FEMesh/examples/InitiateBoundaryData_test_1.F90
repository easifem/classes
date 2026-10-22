!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate the boundary data
!
! InitiateBoundaryData stores, for every boundary element, the
! local facets on the boundary of the mesh. Every element of this
! mesh is a boundary element. The facets of a Quadrangle are
! [1-2], [2-3], [3-4], [4-1] in local node numbers.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: bdata(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateBoundaryData()
  CALL OK(obj%IsBoundaryData(), "IsBoundaryData")
  CALL OK(obj%GetTotalBoundaryElements() .EQ. 4_I4B, &
    "GetTotalBoundaryElements")
  ! element 13 = [1, 5, 9, 7]: facets 1-5 and 7-1 are on the boundary
  bdata = obj%GetBoundaryElementData(globalElement=13_I4B)
  CALL OK(ALL(bdata .EQ. [1, 4]), "element 13")
  bdata = obj%GetBoundaryElementData(globalElement=16_I4B)
  CALL OK(ALL(bdata .EQ. [2, 3]), "element 16")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
