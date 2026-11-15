!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the node nearest to a point
!
! GetNearestNode uses the kdtree of the node coordinates of the
! domain; it is initiated if needed. DeallocateKdtree frees it.
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
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  REAL(DFP) :: x(3), xn(3, 1)
  INTEGER(I4B) :: node, nodes(1)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL obj%GetNearestNode(qv=[0.1_DFP, 0.9_DFP], x=x, globalNode=node)
  CALL OK(node .EQ. 3_I4B, "nearest node to (0.1, 0.9)")
  CALL obj%DeallocateKdtree()
  CALL obj%InitiateKdtree()
  CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=xn, &
                          globalNode=nodes, nn=1_I4B)
  CALL OK(nodes(1) .EQ. 2_I4B, "nearest node to (0.9, 0.1)")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
