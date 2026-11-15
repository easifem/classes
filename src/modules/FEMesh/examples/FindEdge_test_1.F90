!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Find an edge in an element
!
! FindEdge checks if edgeCon is an edge of an element, and returns
! its local edge number. Use it only for volume meshes. The edges
! of a Hexahedron are [1-2], [1-4], [1-5], [2-3], [2-6], [3-4],
! [3-7], [4-8], [5-6], [5-8], [6-7], [7-8] in local node numbers.
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
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/small_hexa8_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  LOGICAL(LGT) :: isEdge
  INTEGER(I4B) :: len_

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=3)

  CALL obj%FindEdge(globalElement=35_I4B, edgeCon=[9, 10], &
    isEdge=isEdge, localEdgeNumber=len_, &
    onlyBoundaryElement=.FALSE.)
  CALL OK(isEdge .AND. len_ .EQ. 4_I4B, "edge 9-10")
  CALL obj%FindEdge(globalElement=35_I4B, edgeCon=[2, 4], &
    isEdge=isEdge, localEdgeNumber=len_, &
    onlyBoundaryElement=.FALSE.)
  CALL OK(.NOT. isEdge .AND. len_ .EQ. 0_I4B, "not an edge of 35")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
