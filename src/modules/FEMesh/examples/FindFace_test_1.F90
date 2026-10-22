!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Find a face in an element
!
! FindFace checks if faceCon (vertex nodes of a face, in any order)
! is a face of an element, and returns its local face number.
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
  LOGICAL(LGT) :: isFace
  INTEGER(I4B) :: lfn

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=3)

  CALL obj%FindFace(globalElement=36_I4B, faceCon=[9, 10, 12, 11], &
    isFace=isFace, localFaceNumber=lfn, &
    onlyBoundaryElement=.FALSE.)
  CALL OK(isFace .AND. lfn .EQ. 3_I4B, "shared face")
  CALL obj%FindFace(globalElement=36_I4B, faceCon=[11, 12, 10, 9], &
    isFace=isFace, localFaceNumber=lfn, &
    onlyBoundaryElement=.FALSE.)
  CALL OK(isFace .AND. lfn .EQ. 3_I4B, "any node order")
  CALL obj%FindFace(globalElement=36_I4B, faceCon=[1, 9, 11, 5], &
    isFace=isFace, localFaceNumber=lfn, &
    onlyBoundaryElement=.FALSE.)
  CALL OK(.NOT. isFace .AND. lfn .EQ. 0_I4B, "not a face of 36")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
