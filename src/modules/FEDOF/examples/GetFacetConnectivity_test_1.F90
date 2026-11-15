!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the connectivity of a facet of an element
!
! GetFacetConnectivity_ writes the vertex degrees of freedom of a facet
! of an element followed by the degrees of freedom on the facet. For H1
! hierarchical basis on the quad mesh a facet (edge of the quadrangle) has
! 2 vertices and order - 1 degrees of freedom on it.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (13 to 16) and nine nodes:
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
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: ans(20), tsize, order

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  DO order = 1, 2
    CALL obj%Initiate(baseContinuity="H1", &
                      baseInterpolation="Hierarchical", order=order, &
                      dom=dom)
    CALL obj%GetFacetConnectivity_(ans=ans, tsize=tsize, globalElement=1, &
                                   localFaceNumber=1, islocal=math%yes)
    CALL OK(tsize .EQ. 2 + order - 1, "GetFacetConnectivity_: tsize")
  END DO

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
