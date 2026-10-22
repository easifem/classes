!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element data of an element
!
! The element data of the mesh of dimension nsd is returned.
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
  USE ElemData_Class, ONLY: ElemData_
  USE ElemData_Class, ONLY: Elemdata_globalElemNum
  USE ElemData_Class, ONLY: Elemdata_localElemNum
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  TYPE(ElemData_) :: elemdata
  TYPE(ElemData_), POINTER :: ptr

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL obj%GetElemData(elemdata=elemdata, globalElement=15_I4B)
  CALL OK(Elemdata_localElemNum(elemdata) .EQ. 3_I4B, "GetElemData")
  ptr => obj%GetElemDataPointer(globalElement=16_I4B)
  CALL OK(Elemdata_globalElemNum(ptr) .EQ. 16_I4B, &
    "GetElemDataPointer")
  ptr => NULL()

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
