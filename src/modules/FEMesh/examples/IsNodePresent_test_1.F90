!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Check if nodes are in the mesh
!
! IsNodePresent checks one or several global node numbers.
! GetNodeMask sets mask(i) to true for every global node i of the
! mesh.
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
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  LOGICAL(LGT) :: mask(10)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%IsNodePresent(9_I4B), "node 9")
  CALL OK(.NOT. obj%IsNodePresent(10_I4B), "node 10")
  CALL OK(ALL(obj%IsNodePresent([1_I4B, 10_I4B]) .EQV. &
    [.TRUE., .FALSE.]), "nodes 1 and 10")
  CALL OK(obj%IsAnyNodePresent([10_I4B, 11_I4B, 2_I4B]), &
    "IsAnyNodePresent")
  CALL OK(.NOT. obj%IsAllNodePresent([1_I4B, 2_I4B, 10_I4B]), &
    "IsAllNodePresent")
  CALL obj%GetNodeMask(mask=mask)
  CALL OK(ALL(mask(1:9)) .AND. .NOT. mask(10), "GetNodeMask")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
