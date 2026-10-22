!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a mesh in a VTK file
!
! WriteData writes the mesh (and nodal data) in a .vtu file.
! ExportToVTK writes the mesh in an open VTKFile_.
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
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK
  USE VTKFile_Class, ONLY: VTKFile_

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  TYPE(VTKFile_) :: vtk
  REAL(DFP) :: dval(9)
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%WriteData(filename="WriteData_test_1.vtu")
  INQUIRE(FILE="WriteData_test_1.vtu", EXIST=isok)
  CALL OK(isok, "WriteData")

  dval = [(REAL(ii, DFP), ii=1, 9)]
  CALL obj%WriteData(filename="WriteData_test_1_data.vtu", &
    DATA=dval, dataName=String("u"))
  INQUIRE(FILE="WriteData_test_1_data.vtu", EXIST=isok)
  CALL OK(isok, "WriteData with data")

  CALL obj%ExportToVTK(vtk=vtk, filename="WriteData_test_1_export.vtu")
  CALL vtk%DEALLOCATE()
  INQUIRE(FILE="WriteData_test_1_export.vtu", EXIST=isok)
  CALL OK(isok, "ExportToVTK")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
