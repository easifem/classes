!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Convert global and local numbers
!
! Local node numbers follow the order of the global node numbers;
! local element numbers follow the order of the elements in the
! file. In this mesh the local node number equals the global node
! number, and local elements 1 to 4 are global elements 13 to 16.
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
  INTEGER(I4B) :: buf(2)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetLocalNodeNumber(7_I4B) .EQ. 7_I4B, &
    "GetLocalNodeNumber")
  CALL OK(ALL(obj%GetLocalNodeNumber([2_I4B, 9_I4B]) .EQ. [2, 9]), &
    "GetLocalNodeNumber(:)")
  CALL obj%GetLocalNodeNumber_(globalNode=[5_I4B, 6_I4B], ans=buf)
  CALL OK(ALL(buf(1:2) .EQ. [5, 6]), "GetLocalNodeNumber_")
  CALL OK(obj%GetGlobalNodeNumber(7_I4B) .EQ. 7_I4B, &
    "GetGlobalNodeNumber")
  CALL OK(ALL(obj%GetGlobalNodeNumber([2_I4B, 9_I4B]) .EQ. [2, 9]), &
    "GetGlobalNodeNumber(:)")

  CALL OK(obj%GetLocalElemNumber(15_I4B) .EQ. 3_I4B, &
    "GetLocalElemNumber")
  CALL OK(ALL(obj%GetLocalElemNumber([13_I4B, 16_I4B]) .EQ. [1, 4]), &
    "GetLocalElemNumber(:)")
  CALL OK(obj%GetGlobalElemNumber(3_I4B) .EQ. 15_I4B, &
    "GetGlobalElemNumber")
  CALL OK(ALL(obj%GetGlobalElemNumber([1_I4B, 4_I4B]) .EQ. [13, 16]), &
    "GetGlobalElemNumber(:)")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
