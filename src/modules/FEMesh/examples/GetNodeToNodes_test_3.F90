!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get node to nodes without calling InitiateNodeToNodes
!
! GetNodeToNodes and GetNodeToNodes_ initiate the node to nodes data
! if needed, so InitiateNodeToNodes does not have to be called first.
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
  USE SortUtility, ONLY: Sort
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nodes(:)
  INTEGER(I4B) :: buf(20), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(.NOT. obj%IsNodeToNodes(), "not initiated before")

  nodes = obj%GetNodeToNodes(globalNode=1_I4B, includeSelf=.FALSE.)
  CALL OK(obj%IsNodeToNodes(), "initiated by GetNodeToNodes")
  CALL OK(SIZE(nodes) .EQ. 3_I4B, "node 1: size")
  CALL OK(ALL([5, 7, 9] .EQ. Sort(nodes)), "node 1")

  CALL obj%DEALLOCATE()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)
  nodes = obj%GetNodeToNodes(globalNode=[1_I4B, 2_I4B], &
                             includeSelf=.FALSE.)
  CALL OK(ALL(nodes .EQ. [5, 7, 8, 9]), "list of nodes")

  CALL obj%DEALLOCATE()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)
  CALL obj%GetNodeToNodes_(globalNode=2_I4B, includeSelf=.TRUE., &
                           ans=buf, tsize=tsize)
  CALL OK(tsize .EQ. 4_I4B, "GetNodeToNodes_: node 2")

  CALL obj%DEALLOCATE()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)
  CALL obj%GetNodeToNodes_(globalNode=[2_I4B, 3_I4B], &
                           includeSelf=.FALSE., ans=buf, tsize=tsize)
  CALL OK(ALL(buf(1:tsize) .EQ. [5, 6, 7, 8, 9]), &
          "GetNodeToNodes_: nodes 2 and 3")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
