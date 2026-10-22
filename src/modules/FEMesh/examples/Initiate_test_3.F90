!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a mesh from selected entities
!
! With dim and entities only the given entities are read. Curve 1
! is the bottom edge [1, 5, 2] and curve 3 is the left edge
! [1, 7, 3] of the unit square.
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
  INTEGER(I4B), ALLOCATABLE :: ivec(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()

  CALL obj%Initiate(hdf5=meshfile, DIM=1, entities=[1_I4B, 3_I4B])
  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
  CALL OK(obj%GetTotalNodes() .EQ. 5_I4B, "GetTotalNodes")
  ivec = obj%GetElemNum()
  CALL OK(ALL(ivec .EQ. [5, 6, 9, 10]), "GetElemNum")
  ivec = obj%GetNptrs()
  CALL OK(ALL(ivec .EQ. [1, 2, 3, 5, 7]), "GetNptrs")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
