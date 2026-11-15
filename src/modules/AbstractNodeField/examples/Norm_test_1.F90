!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Norms of a node field
!
! Norm1 returns the sum of absolute values, Norm2 the Euclidean norm,
! and Normi the largest absolute value of the values of obj. Norm
! returns one of them by normType (TypeConvergenceOpt%normL1, normL2
! or normInfinity). For x = [1, -2, 3, -4, 5, -6] the norms are 21,
! SQRT(91) and 6.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: TypeConvergenceOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: x
  REAL(DFP) :: src(6)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, -2.0_DFP, 3.0_DFP, -4.0_DFP, 5.0_DFP, -6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

  CALL OK(ABS(x%Norm1() - 21.0_DFP) .LT. tol, "Norm1")
  CALL OK(ABS(x%Norm2() - SQRT(91.0_DFP)) .LT. tol, "Norm2")
  CALL OK(ABS(x%Normi() - 6.0_DFP) .LT. tol, "Normi")
  CALL OK(ABS(x%Norm(TypeConvergenceOpt%normL1) - 21.0_DFP) .LT. tol, &
          "Norm: L1")
  CALL OK(ABS(x%Norm(TypeConvergenceOpt%normL2) - SQRT(91.0_DFP)) &
          .LT. tol, "Norm: L2")
  CALL OK(ABS(x%Norm(TypeConvergenceOpt%normInfinity) - 6.0_DFP) &
          .LT. tol, "Norm: infinity")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
