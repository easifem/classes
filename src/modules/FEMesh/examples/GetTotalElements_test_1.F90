!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the total number of elements
!
! GetTotalElements (also Size and GetTotalCells) returns the number
! of elements in the mesh, or in the given mesh ids (entities).
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

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "whole mesh")
  CALL OK(obj%SIZE() .EQ. 4_I4B, "Size")
  CALL OK(obj%GetTotalCells() .EQ. 4_I4B, "GetTotalCells")
  CALL OK(obj%GetTotalElements(meshid=1_I4B) .EQ. 4_I4B, "meshid=1")
  CALL OK(obj%GetTotalElements(meshid=[1_I4B]) .EQ. 4_I4B, &
    "meshid=[1]")
  CALL OK(obj%GetTotalElements(meshid=2_I4B) .EQ. 0_I4B, "meshid=2")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
