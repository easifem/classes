!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a curve mesh (dim=1)
!
! With dim=1 the mesh contains the Line2 elements of all curve
! entities, that is the boundary of the unit square (elements 5 to
! 12). The curves form a closed loop, so every node belongs to two
! elements and no node is a boundary node of the curve mesh.
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
  INTEGER(I4B), ALLOCATABLE :: elems(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=1)

  CALL OK(obj%GetXidimension() .EQ. 1_I4B, "GetXidimension")
  CALL OK(obj%GetNSD() .EQ. 2_I4B, "GetNSD")
  CALL OK(obj%GetTotalElements() .EQ. 8_I4B, "GetTotalElements")
  CALL OK(obj%GetTotalNodes() .EQ. 8_I4B, "GetTotalNodes")
  elems = obj%GetElemNum()
  CALL OK(ALL(elems .EQ. [5, 6, 7, 8, 9, 10, 11, 12]), "GetElemNum")
  CALL OK(obj%GetTotalBoundaryNodes() .EQ. 0_I4B, "closed loop")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
