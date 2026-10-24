!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the node numbers of a mesh
!
! GetNptrs returns the global node numbers of the mesh of dimension
! dim. GetInternalNptrs returns the internal nodes of that mesh.
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
  INTEGER(I4B), ALLOCATABLE :: nodes(:)
  INTEGER(I4B) :: buf(20), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  nodes = obj%GetNptrs(DIM=2_I4B)
  CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8, 9]), "surface mesh")
  nodes = obj%GetNptrs(DIM=1_I4B)
  CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8]), "curve mesh")
  CALL obj%GetNptrs_(nptrs=buf, DIM=1_I4B, tsize=tsize)
  CALL OK(tsize .EQ. 8_I4B, "GetNptrs_")
  nodes = obj%GetInternalNptrs(DIM=2_I4B)
  CALL OK(ALL(nodes .EQ. [9]), "GetInternalNptrs")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
