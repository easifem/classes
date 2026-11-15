!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the nodes connected to nodes without allocation
!
! GetNodeToNodes_ writes the node numbers in ans and returns their
! number in tsize. ans must be large enough.
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
  INTEGER(I4B) :: nodes(20), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateNodeToNodes()

  CALL obj%GetNodeToNodes_(globalNode=2_I4B, includeSelf=.TRUE., &
    ans=nodes, tsize=tsize)
  CALL OK(tsize .EQ. 4_I4B, "node 2: tsize")
  CALL OK(nodes(1) .EQ. 2_I4B, "node 2: first entry is node 2")

  CALL obj%GetNodeToNodes_(globalNode=[2_I4B, 3_I4B], &
    includeSelf=.FALSE., ans=nodes, tsize=tsize)
  CALL OK(ALL(nodes(1:tsize) .EQ. [5, 6, 7, 8, 9]), "nodes 2 and 3")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
