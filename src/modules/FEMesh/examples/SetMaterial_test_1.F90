!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the materials of the elements
!
! SetTotalMedium allocates space for n media in the elements.
! SetMaterial sets the material id of a medium in all elements, in
! the elements of an entity, or in one element. GetMaterial returns
! the material id of a medium of an element.
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
  CALL OK(obj%GetTotalMaterial(13_I4B) .EQ. 2_I4B, "GetTotalMaterial")

  ! all elements
  CALL obj%SetMaterial(medium=1_I4B, material=7_I4B)
  ! elements of entity 1
  CALL obj%SetMaterial(entityNum=1_I4B, medium=2_I4B, material=3_I4B)
  ! element 16 only
  CALL obj%SetMaterial(medium=2_I4B, material=5_I4B, &
    globalElement=16_I4B, islocal=.FALSE.)

  CALL OK(obj%GetMaterial(medium=1_I4B, globalElement=13_I4B) &
    .EQ. 7_I4B, "medium 1")
  CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=13_I4B) &
    .EQ. 3_I4B, "medium 2 of element 13")
  CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=16_I4B) &
    .EQ. 5_I4B, "medium 2 of element 16")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
