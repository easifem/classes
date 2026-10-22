!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate the face connectivity
!
! For a volume mesh Initiate numbers the faces of the mesh. The
! shared face has the same global face number in both elements.
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

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=3)

  CALL obj%InitiateFaceConnectivity()
  CALL OK(obj%IsFaceConnectivity(), "IsFaceConnectivity")
  CALL OK(obj%GetTotalFaces() .EQ. 11_I4B, "GetTotalFaces")
  ! face [9, 10, 12, 11] is face 4 of 35 and face 3 of 36
  CALL OK(obj%GetGlobalFaceNumber(globalElement=35_I4B, &
    localFaceNumber=4_I4B) .EQ. &
    obj%GetGlobalFaceNumber(globalElement=36_I4B, &
    localFaceNumber=3_I4B), &
    "shared face")
  CALL obj%InitiateEdgeConnectivity()
  CALL OK(obj%GetTotalEdges() .EQ. 20_I4B, "GetTotalEdges")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
