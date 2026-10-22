!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the number of vertex nodes
!
! GetTotalVertexNodes works on the mesh of dimension dim (default
! nsd): for the whole mesh, one element, or a list of elements.
! GetTotalEntitiesList returns the number of nodes, edges, faces
! and 1 of an element. GetElemTopologyIndx returns the topology
! index (4 for Quadrangle).
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
  INTEGER(I4B) :: ent(4)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(obj%GetTotalVertexNodes() .EQ. 9_I4B, "mesh")
  CALL OK(obj%GetTotalVertexNodes(globalElement=13_I4B) .EQ. 4_I4B, &
    "element 13")
  CALL OK(obj%GetTotalVertexNodes(globalElement=[13_I4B, 14_I4B]) &
    .EQ. 6_I4B, "elements 13 and 14")
  ent = obj%GetTotalEntitiesList(globalElement=13_I4B)
  CALL OK(ent(1) .EQ. 4_I4B .AND. ent(4) .EQ. 1_I4B, &
    "GetTotalEntitiesList")
  CALL OK(obj%GetElemTopologyIndx(13_I4B) .EQ. 4_I4B, &
    "GetElemTopologyIndx")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
