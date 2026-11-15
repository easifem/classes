!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the total number of elements
!
! GetTotalElements returns the number of elements of the mesh of
! dimension dim (default nsd), or of one entity of that mesh. The
! operator .tElements. does the same.
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

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "nsd mesh")
  CALL OK(obj%GetTotalElements(DIM=1_I4B) .EQ. 8_I4B, "curve mesh")
  CALL OK(obj%GetTotalElements(DIM=2_I4B, entityNum=1_I4B) .EQ. &
          4_I4B, "entity 1")
  CALL OK((.tElements.obj) .EQ. 4_I4B, ".tElements. obj")
  CALL OK((obj.tElements.1_I4B) .EQ. 8_I4B, "obj .tElements. dim")
  CALL OK((obj.tElements.[2_I4B, 1_I4B]) .EQ. 4_I4B, &
         "obj .tElements. [dim, entityNum]")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
