!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Convert global and local numbers
!
! Local numbers are the local numbers of the mesh of dimension nsd.
! In this mesh local node numbers equal global node numbers, and
! local elements 1 to 4 are global elements 13 to 16.
!
! The mesh very_small_quad4_mesh.h5 is the unit square: four
! Quadrangle4 elements (13 to 16), eight Line2 elements (5 to 12) on
! the boundary curves, four Point elements (1 to 4), and nine nodes:
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
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(obj%GetLocalNodeNumber(7_I4B) .EQ. 7_I4B, &
    "GetLocalNodeNumber")
  CALL OK(ALL(obj%GetLocalNodeNumber([2_I4B, 3_I4B]) .EQ. [2, 3]), &
    "GetLocalNodeNumber(:)")
  CALL OK(obj%GetGlobalNodeNumber(7_I4B) .EQ. 7_I4B, &
    "GetGlobalNodeNumber")
  CALL OK(ALL(obj%GetGlobalNodeNumber([2_I4B, 3_I4B]) .EQ. [2, 3]), &
    "GetGlobalNodeNumber(:)")
  CALL OK(obj%GetLocalElemNumber(15_I4B) .EQ. 3_I4B, &
    "GetLocalElemNumber")
  CALL OK(ALL(obj%GetLocalElemNumber([13_I4B, 16_I4B]) .EQ. [1, 4]), &
    "GetLocalElemNumber(:)")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
