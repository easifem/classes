!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the parameters of the domain
!
! GetParam returns the meta data of the domain read from the mesh
! file. tElements(0:3) and tEntities(0:3) are the number of elements
! and entities of each dimension.
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
  INTEGER(I4B) :: nsd, tNodes, tElements(0:3), tEntities(0:3)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL obj%GetParam(nsd=nsd, tNodes=tNodes, tElements=tElements, &
                    tEntities=tEntities)
  CALL OK(nsd .EQ. 2_I4B .AND. tNodes .EQ. 9_I4B, "nsd and tNodes")
  CALL OK(ALL(tElements .EQ. [4, 8, 4, 0]), "tElements")
  CALL OK(ALL(tEntities .EQ. [4, 4, 1, 0]), "tEntities")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
