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
INTEGER(I4B) :: tsize, found(100), want(100)
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

! Getting cell degree of freedoms in an element
CALL obj%GetCellDOF(globalElement=1, ans=found, tsize=tsize, &
                    islocal=math%yes)
want(1:tsize) = 4
CALL OK(ALL(found(1:tsize) .EQ. want(1:tsize)), "order=2, GetCellDOF: ")

! Getting vertex connectivity
CALL obj%GetConnectivity_(globalElement=1, ans=found, tsize=tsize, &
                          islocal=math%yes, opt="V")
want(1:tsize) = [1, 2]
CALL OK(ALL(found(1:tsize) .EQ. want(1:tsize)), &
        "order=2, GetConnectivity_: ")

! Getting cell connectivity
CALL obj%GetConnectivity_(globalElement=1, ans=found, tsize=tsize, &
                          islocal=math%yes, opt="C")
want(1:tsize) = [4]
CALL OK(ALL(found(1:tsize) .EQ. want(1:tsize)), &
        "order=2, GetConnectivity_: ")

! Getting all connectivity
CALL obj%GetConnectivity_(globalElement=1, ans=found, tsize=tsize, &
                          islocal=math%yes, opt="A")
want(1:tsize) = [1, 2, 4]
CALL OK(ALL(found(1:tsize) .EQ. want(1:tsize)), &
        "order=2, GetConnectivity_: ")

CALL dom%DEALLOCATE()
END PROGRAM main
