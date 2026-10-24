!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a domain with two surface entities
!
! The surface mesh of the domain contains the elements of all
! surface entities. GetTotalElements with entityNum counts the
! elements of one entity.
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
  INTEGER(I4B), ALLOCATABLE :: con(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(obj%GetTotalNodes() .EQ. 10_I4B, "GetTotalNodes")
  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
  CALL OK(obj%GetTotalEntities(DIM=2_I4B) .EQ. 2_I4B, &
          "GetTotalEntities")
  CALL OK(obj%GetTotalElements(DIM=2_I4B, entityNum=2_I4B) &
          .EQ. 2_I4B, "elements of entity 2")
  con = obj%GetConnectivity(globalElement=16_I4B, DIM=1_I4B)
  CALL OK(ALL(con .EQ. [2, 5]), "interface element 16")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
