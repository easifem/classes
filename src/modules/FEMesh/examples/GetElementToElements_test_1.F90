!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the neighbours of an element
!
! GetElementToElements returns one row per neighbour of an
! element: the global element number of the neighbour, the local
! facet number in the element, and the local facet number in the
! neighbour. With onlyElements only the first column is returned.
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
  INTEGER(I4B), ALLOCATABLE :: e2e(:, :)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  ! element 13 shares its facet 2 with facet 4 of element 15,
  ! and its facet 3 with facet 1 of element 14
  e2e = obj%GetElementToElements(globalElement=13_I4B)
  CALL OK(ALL(SHAPE(e2e) .EQ. [2, 3]), "shape")
  CALL OK(ALL(e2e(1, :) .EQ. [15, 2, 4]), "neighbour 15")
  CALL OK(ALL(e2e(2, :) .EQ. [14, 3, 1]), "neighbour 14")

  e2e = obj%GetElementToElements(globalElement=13_I4B, &
    onlyElements=.TRUE.)
  CALL OK(ALL(SHAPE(e2e) .EQ. [2, 1]), "onlyElements: shape")
  CALL OK(ALL(e2e(:, 1) .EQ. [15, 14]), "onlyElements")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
