!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the orientation of an element
!
! Element 16 = [9, 8, 4, 6]: every edge goes from a higher to a lower
! global node number, so the edge orientation is -1. The edges are
! numbered by InitiateEdgeConnectivity of the mesh.
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
  USE AbstractMesh_Class, ONLY: AbstractMesh_
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
  CLASS(AbstractMesh_), POINTER :: mesh
  INTEGER(I4B) :: cell(5), face(5, 10), edge(20), tface(2)
  INTEGER(I4B) :: tcell, tedge

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  mesh => obj%GetMeshPointer()
  CALL mesh%InitiateEdgeConnectivity()
  CALL obj%GetOrientation(cellOrient=cell, faceOrient=face, &
                          edgeOrient=edge, tCellOrient=tcell, &
                          tFaceOrient=tface, tEdgeOrient=tedge, &
                          globalElement=16_I4B)
  CALL OK(tedge .EQ. 4_I4B, "number of edges")
  CALL OK(ALL(edge(1:tedge) .EQ. -1), "edge orientation")
  mesh => NULL()

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
