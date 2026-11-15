!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the number of media of the elements
!
! SetTotalMedium sets the number of media in the elements to n; it
! does not add n media to the existing ones. The material ids of the
! media that are kept do not change, and new media get material id 0.
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

  CALL obj%SetTotalMedium(n=2_I4B)
  CALL obj%SetMaterial(medium=1_I4B, material=7_I4B)
  CALL obj%SetMaterial(medium=2_I4B, material=4_I4B)

  ! more media in element 14
  CALL obj%SetTotalMedium(n=3_I4B, globalElement=14_I4B)
  CALL OK(obj%GetTotalMaterial(14_I4B) .EQ. 3_I4B, "element 14: 3 media")
  CALL OK(obj%GetTotalMaterial(13_I4B) .EQ. 2_I4B, "element 13: 2 media")
  CALL OK(obj%GetMaterial(medium=1_I4B, globalElement=14_I4B) &
          .EQ. 7_I4B, "medium 1 is kept")
  CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=14_I4B) &
          .EQ. 4_I4B, "medium 2 is kept")
  CALL OK(obj%GetMaterial(medium=3_I4B, globalElement=14_I4B) &
          .EQ. 0_I4B, "new medium 3 is 0")

  ! the same n again changes nothing
  CALL obj%SetTotalMedium(n=2_I4B)
  CALL OK(obj%GetTotalMaterial(13_I4B) .EQ. 2_I4B, "same n: 2 media")
  CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=13_I4B) &
          .EQ. 4_I4B, "same n: medium 2 is kept")

  ! a smaller n removes the last media
  CALL obj%SetTotalMedium(n=1_I4B)
  CALL OK(obj%GetTotalMaterial(14_I4B) .EQ. 1_I4B, "smaller n: 1 medium")
  CALL OK(obj%GetMaterial(medium=1_I4B, globalElement=14_I4B) &
          .EQ. 7_I4B, "smaller n: medium 1 is kept")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
