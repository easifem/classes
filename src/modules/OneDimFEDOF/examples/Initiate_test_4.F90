!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate fedof for H1 and Lagrange, with different order in
! each element.
!
! baseContinuity = "H1", &
! baseInterpolation = "Lagrange", &
! feType = TypeFEVariableOpt%scalar, &
! ipType = TypeQuadratureOpt%equidistance, &
! basisType = TypePolynomialOpt%monomial, &
! quadratureType = TypeQuadratureOpt%gaussLegendre

PROGRAM main
USE OneDimFEDOF_Class, ONLY: OneDimFEDOF_
USE OneDimDomain_Class, ONLY: OneDimDomain_
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: DFP, LGT, I4B
USE Test_Method, ONLY: OK, IS
USE ReallocateUtility, ONLY: Reallocate
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType, ONLY: TypeFEVariableOpt, TypeQuadratureOpt, &
                    TypePolynomialOpt

IMPLICIT NONE

TYPE(OneDimFEDOF_) :: obj
TYPE(OneDimDomain_) :: dom
INTEGER(I4B) :: found, want, quadratureOrder
REAL(DFP), PARAMETER :: domain(2) = [0.0_DFP, 1.0_DFP]
CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                           baseInterpolation = "Lagrange", &
                           testname = baseContinuity//"_"//baseInterpolation
INTEGER(I4B), PARAMETER :: feType = TypeFEVariableOpt%scalar, &
                           ipType = TypeQuadratureOpt%equidistance, &
                           basisType = TypePolynomialOpt%monomial, &
                           quadratureType = TypeQuadratureOpt%gaussLegendre
INTEGER(I4B), ALLOCATABLE :: order(:)

CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)

CALL dom%Initiate(domain=domain, totalElements=2)
! CALL dom%Display("domain: ")

CALL test1

CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE test1
  CALL Reallocate(order, 2)
  order(1) = 1
  order(2) = 2
  quadratureOrder = 2
  CALL obj%Initiate(order=order, mesh=dom, &
                    baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    fetype=feType, ipType=ipType, &
                    basisType=basisType, &
                    quadratureType=quadratureType, &
                    quadratureOrder=quadratureOrder)
  ! CALL obj%Display("FEDOF:")
  found = obj%GetTotalDOF()
  want = dom%GetTotalVertexNodes() + SUM(order - 1)
  CALL IS(found, want, testname//" test 1: ")
END SUBROUTINE test1

END PROGRAM main

