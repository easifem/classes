!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a surface mesh from a HDF5 mesh file
!
! Initiate reads all entities of dimension dim from the mesh file.
! Global element and node numbers are the numbers in the file.
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

  CALL OK(obj%IsInit(), "IsInit")
  CALL OK(.NOT. obj%IsEmpty(), "IsEmpty")
  CALL OK(obj%GetNSD() .EQ. 2_I4B, "GetNSD")
  CALL OK(obj%GetXidimension() .EQ. 2_I4B, "GetXidimension")
  CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
  CALL OK(obj%GetMaxNNE() .EQ. 4_I4B, "GetMaxNNE")
  CALL OK(obj%GetMinElemNumber() .EQ. 13_I4B, "GetMinElemNumber")
  CALL OK(obj%GetMaxElemNumber() .EQ. 16_I4B, "GetMaxElemNumber")
  CALL OK(obj%GetMinNodeNumber() .EQ. 1_I4B, "GetMinNodeNumber")
  CALL OK(obj%GetMaxNodeNumber() .EQ. 9_I4B, "GetMaxNodeNumber")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
