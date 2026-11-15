!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: Initiate one dimensional fedof for for H1 and lagrange basis,
! for order 1 and order 2.
!
! baseContinuity = "H1", &
! baseInterpolation = "Lagrange"
! feType = TypeFEVariableOpt%scalar, &
! ipType = TypeQuadratureOpt%equidistance, &
! basisType = TypePolynomialOpt%monomial, &
! quadratureType = TypeQuadratureOpt%gaussLegendre

PROGRAM main
USE OneDimFEDOF_Class, ONLY: OneDimFEDOF_
USE OneDimDomain_Class, ONLY: OneDimDomain_
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: I4B, DFP, LGT
USE Test_Method, ONLY: IS
USE Test_Method, ONLY: OK
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeQuadratureOpt
USE BaseType, ONLY: TypePolynomialOpt
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE

TYPE(OneDimFEDOF_) :: obj
TYPE(OneDimDomain_) :: dom
INTEGER(I4B) :: found, want
REAL(DFP), PARAMETER :: domain(2) = [math%zero, math%one]
CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                           baseInterpolation = "Lagrange"
INTEGER(I4B), PARAMETER :: &
  feType = TypeFEVariableOpt%scalar, &
  ipType = TypeQuadratureOpt%equidistance, &
  basisType = TypePolynomialOpt%monomial, &
  quadratureType = TypeQuadratureOpt%gaussLegendre, &
  order = 2

CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL dom%Initiate(domain=domain, totalElements=2)

CALL obj%Initiate(order=order, mesh=dom, &
                  baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, &
                  fetype=feType, &
                  ipType=ipType, &
                  basisType=basisType, &
                  quadratureType=quadratureType, &
                  quadratureOrder=order)

! Get the total cell degree of freedom in an element
! These are internal degree of freedoms
found = obj%GetTotalCellDOF(globalElement=1, islocal=math%yes)
want = math%one_i
CALL IS(found, want, "order=2, GetTotalCellDOF: ")

! Get the total degree of freedom in an element
! These are internal degree and vertex degree of freedoms
found = obj%GetTotalDOF(globalElement=1, islocal=math%yes)
want = math%three_i
CALL IS(found, want, "order=2, GetTotalDOF: ")

! Get the total degree of freedom in an element by specifying the
! options ("V", "C", "A") for vertex, cell, and all.
! These are internal degree and vertex degree of freedoms
found = obj%GetTotalDOF(globalElement=1, opt="V", islocal=math%yes)
want = math%two_i
CALL IS(found, want, "order=2, GetTotalDOF(opt=V): ")

found = obj%GetTotalDOF(globalElement=1, opt="C", islocal=math%yes)
want = math%one_i
CALL IS(found, want, "order=2, GetTotalDOF(opt=C): ")

found = obj%GetTotalDOF(globalElement=1, opt="A", islocal=math%yes)
want = math%three_i
CALL IS(found, want, "order=2, GetTotalDOF(opt=A): ")

CALL dom%DEALLOCATE()
END PROGRAM main
