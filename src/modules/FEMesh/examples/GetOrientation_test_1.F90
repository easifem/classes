!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the orientation of an element
!
! The orientation of an edge is 1 when its first node has the lower
! global node number, else -1. Element 36 = [9, 2, 4, 10, 11, 6, 8,
! 12]: edge 1 [9, 2] and edge 9 [11, 6] have orientation -1. The
! edge orientation needs the edge connectivity.
!
! The mesh small_hexa8_mesh.h5 is the unit cube with two Hexahedron8
! elements: 35 = [1, 9, 10, 3, 5, 11, 12, 7] and
! 36 = [9, 2, 4, 10, 11, 6, 8, 12]. They share the face
! [9, 10, 12, 11] at x = 0.5.

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
    "./meshdata/small_hexa8_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: buf(20), face(5, 10), cell(5), tface(2)
  INTEGER(I4B) :: tsize, nrow, ncol, tcell

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=3)

  CALL obj%InitiateEdgeConnectivity()
  CALL obj%GetEdgeOrientation(ans=buf, tsize=tsize, &
    globalElement=36_I4B)
  CALL OK(tsize .EQ. 12_I4B, "number of edges")
  CALL OK(ALL(buf(1:12) .EQ. [-1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1]), &
    "GetEdgeOrientation")

  CALL obj%GetCellOrientation(ans=buf, tsize=tsize, &
    globalElement=36_I4B)
  CALL OK(ALL(buf(1:tsize) .EQ. [1, 1, 1]), "GetCellOrientation")

  CALL obj%GetFaceOrientation(ans=face, nrow=nrow, ncol=ncol, &
    globalElement=36_I4B)
  CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, "GetFaceOrientation")

  CALL obj%GetOrientation(cellOrient=cell, faceOrient=face, &
    edgeOrient=buf, tCellOrient=tcell, &
    tFaceOrient=tface, tEdgeOrient=tsize, &
    globalElement=36_I4B)
  CALL OK(tcell .EQ. 3_I4B .AND. ALL(tface .EQ. [3, 6]) .AND. &
    tsize .EQ. 12_I4B, "GetOrientation")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
