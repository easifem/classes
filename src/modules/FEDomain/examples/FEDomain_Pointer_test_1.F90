!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get a pointer to a new domain
!
! FEDomain_Pointer allocates a new FEDomain_ and initiates it.
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
  USE FEDomain_Class, ONLY: FEDomain_Pointer
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  CLASS(FEDomain_), POINTER :: ptr

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()

  ptr => FEDomain_Pointer(hdf5=meshfile, group="")
  CALL OK(ptr%IsInitiated(), "IsInitiated")
  CALL OK(ptr%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
  CALL ptr%DEALLOCATE()
  DEALLOCATE(ptr)
  CALL meshfile%DEALLOCATE()
END PROGRAM main
