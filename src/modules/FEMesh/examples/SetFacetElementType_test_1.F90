!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the element type of an element
!
! SetFacetElementType sets the element type of an element:
! INTERNAL_ELEMENT, BOUNDARY_ELEMENT or DOMAIN_BOUNDARY_ELEMENT.
! IsBoundaryElement and IsDomainBoundaryElement read it. After
! Initiate every boundary element is a DOMAIN_BOUNDARY_ELEMENT.
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
  USE ElemData_Class, ONLY: BOUNDARY_ELEMENT
  USE ElemData_Class, ONLY: INTERNAL_ELEMENT
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

  CALL OK(obj%IsDomainBoundaryElement(13_I4B), "after Initiate")

  CALL obj%SetFacetElementType(globalElement=13_I4B, iface=1_I4B, &
    facetElementType=BOUNDARY_ELEMENT)
  CALL OK(obj%IsBoundaryElement(13_I4B), "BOUNDARY_ELEMENT")
  CALL OK(.NOT. obj%IsDomainBoundaryElement(13_I4B), &
    "not a domain boundary element")

  CALL obj%SetFacetElementType(globalElement=1_I4B, iface=1_I4B, &
    facetElementType=INTERNAL_ELEMENT, &
    islocal=.TRUE.)
  CALL OK(.NOT. obj%IsBoundaryElement(13_I4B), "INTERNAL_ELEMENT")
  CALL OK(obj%GetTotalBoundaryElements() .EQ. 3_I4B, &
    "GetTotalBoundaryElements")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
