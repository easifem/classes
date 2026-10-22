!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the vertex connectivity of an element
!
! GetConnectivity returns the global node numbers of an element.
! The element is given by its global element number, or by its local
! element number when islocal is true.
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
  INTEGER(I4B), ALLOCATABLE :: con(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  con = obj%GetConnectivity(globalElement=13_I4B)
  CALL OK(SIZE(con) .EQ. 4_I4B, "size of connectivity")
  CALL OK(ALL(con .EQ. [1, 5, 9, 7]), "global element 13")

  ! local element 2 is global element 14
  con = obj%GetConnectivity(globalElement=2_I4B, islocal=.TRUE.)
  CALL OK(ALL(con .EQ. [7, 9, 6, 3]), "local element 2")

  ! opt="V" (vertex) is the default
  con = obj%GetConnectivity(globalElement=16_I4B, opt="V")
  CALL OK(ALL(con .EQ. [9, 8, 4, 6]), "opt=V")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
