!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate the edge connectivity
!
! InitiateEdgeConnectivity numbers the edges of the mesh. An edge
! shared by two elements has the same global edge number in both.
! The edges of a Quadrangle are [1-2], [4-3], [1-4], [2-3] in local
! node numbers.
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

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetTotalEdges() .EQ. 0_I4B, "no edges before")
  CALL obj%InitiateEdgeConnectivity()
  CALL OK(obj%IsEdgeConnectivity(), "IsEdgeConnectivity")
  CALL OK(obj%GetTotalEdges() .EQ. 12_I4B, "GetTotalEdges")

  ! edge [7, 9] is edge 2 of element 13 and edge 1 of element 14
  CALL OK(obj%GetGlobalEdgeNumber(globalElement=13_I4B, &
    localEdgeNumber=2_I4B) .EQ. &
    obj%GetGlobalEdgeNumber(globalElement=14_I4B, &
    localEdgeNumber=1_I4B), "edge 7-9")
  ! edge [5, 9] is edge 4 of element 13 and edge 3 of element 15
  CALL OK(obj%GetGlobalEdgeNumber(globalElement=13_I4B, &
    localEdgeNumber=4_I4B) .EQ. &
    obj%GetGlobalEdgeNumber(globalElement=15_I4B, &
    localEdgeNumber=3_I4B), "edge 5-9")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
