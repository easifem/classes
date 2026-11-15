!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary:  Get the quadrature points for lagrange

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: Display
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE BaseType, ONLY: QuadraturePoint_
  USE BaseType, ONLY: quadopt => TypeQuadratureOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE AbstractFE_Class, ONLY: AbstractFE_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
    baseContinuity = "H1", &
    baseInterpolation = "Lagrange", &
    testname = baseContinuity // " " &
    // baseInterpolation &
    // " GetQuadraturePoints test:"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  TYPE(QuadraturePoint_) :: quad
  INTEGER(I4B) :: order
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CLASS(AbstractFE_), POINTER :: feptr => NULL()

  CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()

  CALL test1
  CALL test2
  CALL test3

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

CONTAINS

!---------------------------------------------------------------------------
!                                                                      test1
!---------------------------------------------------------------------------

  SUBROUTINE test1
    order = 1
    CALL obj%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=order, &
                      dom=dom, &
                      ipType=quadopt%equidistance, &
                      quadratureIsHomogeneous=math%yes, &
                      quadratureOrder=[2 * order], &
                      quadratureIsOrder=math%yes, &
                      quadratureType=[quadopt%GaussLegendre])

    CALL obj%SetFE(globalElement=1, islocal=math%yes)
    feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
    CALL feptr%GetQuadraturePoints(quad=quad)
    CALL check_quad(3_I4B, 0.5_DFP, testname // " test1")
  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2
    order = 2
    CALL obj%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=order, &
                      dom=dom, &
                      ipType=quadopt%equidistance, &
                      quadratureIsHomogeneous=math%yes, &
                      quadratureOrder=[2 * order], &
                      quadratureIsOrder=math%yes, &
                      quadratureType=[quadopt%GaussLegendre])

    CALL obj%SetFE(globalElement=1, islocal=math%yes)
    feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
    CALL feptr%GetQuadraturePoints(quad=quad)
    CALL check_quad(6_I4B, 0.5_DFP, testname // " test2")
  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                     test3
!---------------------------------------------------------------------------

  SUBROUTINE test3
    order = 3
    CALL obj%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=order, &
                      dom=dom, &
                      ipType=quadopt%equidistance, &
                      quadratureIsHomogeneous=math%yes, &
                      quadratureOrder=[2 * order], &
                      quadratureIsOrder=math%yes, &
                      quadratureType=[quadopt%GaussLegendre])

    CALL obj%SetFE(globalElement=1, islocal=math%yes)
    feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
    CALL feptr%GetQuadraturePoints(quad=quad)
    CALL check_quad(0_I4B, 0.5_DFP, testname // " test3")
  END SUBROUTINE test3

!---------------------------------------------------------------------------
!                                                                check_quad
!---------------------------------------------------------------------------

! Check the number of quadrature points (if nwant > 0) and the sum of the
! weights (the last row of quad%points), which is the area of the
! reference triangle.

  SUBROUTINE check_quad(nwant, area, msg)
    INTEGER(I4B), INTENT(IN) :: nwant
    REAL(DFP), INTENT(IN) :: area
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B) :: nips, nrow

    nrow = SIZE(quad%points, 1)
    nips = SIZE(quad%points, 2)
    IF (nwant .GT. 0) CALL OK(nips .EQ. nwant, msg // " number of points")
    CALL OK(ABS(SUM(quad%points(nrow, :)) - area) .LT. 1.0E-8_DFP, &
            msg // " sum of weights")
  END SUBROUTINE check_quad

END PROGRAM main
