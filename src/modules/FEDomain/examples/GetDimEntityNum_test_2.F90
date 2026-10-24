!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the dimension and entity of an element of two regions
!
! In very_small_quad4_mesh_two_region.h5 the elements 18 and 19 are
! in surface entity 1 and 20 and 21 in surface entity 2. Line
! element 16 is on curve 6 (the interface x = 1), line element 13 on
! curve 4, and point element 5 on point 5. GetDimEntityNum returns
! the mesh id of the element, not the first entity of the mesh.
!
! The mesh very_small_quad4_mesh_two_region.h5 is [0, 2] x [0, 1] with
! two surface entities: entity 1 has elements 18 and 19, entity 2 has
! elements 20 and 21. Line element 16 [2, 5] is the interface.
!
!   4 ---- 9 ---- 5 ---- 10 ---- 6
!   |  18  |  19  |  20  |  21   |
!   1 ---- 7 ---- 2 ---- 8 ----- 3

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
    "./meshdata/very_small_quad4_mesh_two_region.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(ALL(obj%GetDimEntityNum(18_I4B) .EQ. [2, 1]), "element 18")
  CALL OK(ALL(obj%GetDimEntityNum(20_I4B) .EQ. [2, 2]), "element 20")
  CALL OK(ALL(obj%GetDimEntityNum(21_I4B) .EQ. [2, 2]), "element 21")
  CALL OK(ALL(obj%GetDimEntityNum(16_I4B) .EQ. [1, 6]), "element 16")
  CALL OK(ALL(obj%GetDimEntityNum(13_I4B) .EQ. [1, 4]), "element 13")
  CALL OK(ALL(obj%GetDimEntityNum(5_I4B) .EQ. [0, 5]), "element 5")
  CALL OK(ALL(obj%GetDimEntityNum(99_I4B) .EQ. [0, 0]), "not found")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
