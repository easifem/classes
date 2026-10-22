!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the elements around nodes
!
! GetNodeToElements returns the elements of the mesh of dimension
! nsd around a node (or a list of nodes, sorted without
! duplicates). The node to elements data is initiated if needed.
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
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  INTEGER(I4B), ALLOCATABLE :: elems(:)
  INTEGER(I4B) :: buf(10), tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  elems = obj%GetNodeToElements(globalNode=9_I4B)
  CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "node 9")
  elems = obj%GetNodeToElements(globalNode=[1_I4B, 2_I4B])
  CALL OK(ALL(elems .EQ. [13, 15]), "nodes 1 and 2")
  CALL obj%GetNodeToElements_(ans=buf, tsize=tsize, globalNode=5_I4B)
  CALL OK(ALL(buf(1:tsize) .EQ. [13, 15]), "GetNodeToElements_")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
