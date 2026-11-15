!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the neighbours of an element without allocation
!
! GetElementToElements_ with ans(:) returns the neighbour element
! numbers. With ans(:, :) it returns the full information (see
! GetElementToElements).
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
  INTEGER(I4B) :: elems(10), e2e(10, 3), tsize, nrow, ncol

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%GetElementToElements_(ans=elems, tsize=tsize, &
    globalElement=16_I4B)
  CALL OK(ALL(elems(1:tsize) .EQ. [15, 14]), "neighbours of 16")

  CALL obj%GetElementToElements_(ans=e2e, nrow=nrow, ncol=ncol, &
    globalElement=4_I4B, islocal=.TRUE.)
  CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 3_I4B, "nrow and ncol")
  CALL OK(ALL(e2e(1, 1:3) .EQ. [15, 1, 3]), "first neighbour")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
