!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element data of an element
!
! GetElemData returns a copy of the element data, and
! GetElemDataPointer returns a pointer to it.
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
  USE ElemData_Class, ONLY: ElemData_
  USE ElemData_Class, ONLY: Elemdata_globalElemNum
  USE ElemData_Class, ONLY: Elemdata_localElemNum
  USE ElemData_Class, ONLY: Elemdata_Meshid
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
  TYPE(ElemData_) :: elemdata
  TYPE(ElemData_), POINTER :: ptr

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%GetElemData(elemdata=elemdata, globalElement=15_I4B)
  CALL OK(Elemdata_globalElemNum(elemdata) .EQ. 15_I4B, "global number")
  CALL OK(Elemdata_localElemNum(elemdata) .EQ. 3_I4B, "local number")
  CALL OK(Elemdata_Meshid(elemdata) .EQ. 1_I4B, "mesh id")

  ptr => obj%GetElemDataPointer(globalElement=3_I4B, islocal=.TRUE.)
  CALL OK(Elemdata_globalElemNum(ptr) .EQ. 15_I4B, "pointer")
  ptr => NULL()

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
