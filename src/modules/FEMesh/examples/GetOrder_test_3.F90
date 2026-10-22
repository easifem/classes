!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the order of a quadratic mesh
!
! small_tri6_mesh.h5 has Triangle6 elements, so GetOrder() returns 2.
! After Deallocate and Initiate the order is computed again.

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
    "./meshdata/small_tri6_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetOrder() .EQ. 2_I4B, "GetOrder")
  CALL obj%DEALLOCATE()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)
  CALL OK(obj%GetOrder() .EQ. 2_I4B, "GetOrder after Initiate")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
