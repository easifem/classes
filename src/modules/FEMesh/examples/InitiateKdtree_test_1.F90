!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate and deallocate the kdtree
!
! The kdtree is used by GetNearestNode and GetNptrsInBox. These
! methods initiate it when needed. DeallocateKdtree frees it; it is
! initiated again on the next query.
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
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  REAL(DFP) :: x(3)
  INTEGER(I4B) :: node

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateKdtree()
  CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=x, globalNode=node)
  CALL OK(node .EQ. 2_I4B, "nearest node")

  CALL obj%DeallocateKdtree()
  CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=x, globalNode=node)
  CALL OK(node .EQ. 2_I4B, "nearest node after DeallocateKdtree")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
