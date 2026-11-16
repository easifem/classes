!> author: Vikas Sharma, Ph. D.
! date: 2026-11-18
! summary: Norm(normType) of a LIS scalar field
!
! Norm returns Norm1, Norm2 or Normi by normType
! (TypeConvergenceOpt%normL1, normL2 or normInfinity). For
! x = [1, -2, 3, -4, 5, -6] the norms are 21, SQRT(91) and 6. The native
! version is src/modules/AbstractNodeField/examples/Norm_test_1.F90.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeConvergenceOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: x
REAL(DFP) :: src(6)

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

src = [1.0_DFP, -2.0_DFP, 3.0_DFP, -4.0_DFP, 5.0_DFP, -6.0_DFP]
CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

! Test 1: Norm by normType
BLOCK
  CALL OK(ABS(x%Norm(TypeConvergenceOpt%normL1) - 21.0_DFP) .LT. tol, &
          "Norm: L1")
  CALL OK(ABS(x%Norm(TypeConvergenceOpt%normL2) - SQRT(91.0_DFP)) &
          .LT. tol, "Norm: L2")
  CALL OK(ABS(x%Norm(TypeConvergenceOpt%normInfinity) - 6.0_DFP) &
          .LT. tol, "Norm: infinity")
END BLOCK

CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
