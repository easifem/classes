!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the maximum number of quadrature points
!
! GetMaxTotalQuadraturePoints sets the quadrature order of each element
! to scaleForQuadOrder (default 2) times its cell order and returns the
! number of quadrature points. With H1 hierarchical basis on Triangle3
! elements, order 1 gives quadrature order 2 (3 points) and order 2 gives
! quadrature order 4 (6 points).
!
! The mesh small_tri3_mesh.h5 has 14 Triangle3 elements.

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
    "./meshdata/small_tri3_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=1, dom=dom)
  CALL OK(obj%GetMaxTotalQuadraturePoints() .EQ. 3_I4B, "order 1")

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=2, dom=dom)
  CALL OK(obj%GetMaxTotalQuadraturePoints() .EQ. 6_I4B, "order 2")

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
