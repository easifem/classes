!> author: Shion Shimizu
! date: 2026-11-12
! summary: Testing ImportFromToml of SDAlgorithm2_ with form,
!   forceEvaluation and dt keys, and the generalized-alpha method

PROGRAM main
USE GlobalData, ONLY: DFP
USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(SDAlgorithm2_) :: obj
CHARACTER(*), PARAMETER :: filename = "ImportFromToml_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

CALL obj%ImportFromToml(tomlName="timeIntegration", filename=filename)

CALL OK(obj%name .EQ. "HHTA", "name is HHTA")
CALL OK(obj%GetForm() .EQ. "A", "form is A")
CALL OK(obj%GetForceEvaluation() .EQ. "E", "forceEvaluation is E")
CALL OK(ABS(obj%forceTime - 0.9_DFP) .LT. tol, "HHT forceTime = 1+alpha")
CALL OK(obj%IsInitiated(), "initiated because dt is given")
CALL OK(ABS(obj%alphaF - 0.1_DFP) .LT. tol, "alphaF = -alpha")
CALL OK(ABS(obj%alphaM) .LT. tol, "alphaM = 0 for HHT")
! beta = (1-alpha)^2/4 = 0.3025, tanmat(3) = (1+alpha) beta dt^2
CALL OK(ABS(obj%beta - 0.3025_DFP) .LT. tol, &
        "default beta follows the alpha read")
CALL OK(ABS(obj%tanmat(3) - 0.9_DFP * 0.3025_DFP * 0.01_DFP) .LT. tol, &
        "a-form tanmat(3)")

CALL obj%ImportFromToml(tomlName="timeIntegration2", filename=filename)

CALL OK(obj%name .EQ. "NEWM", "name is NEWM")
CALL OK(obj%GetForm() .EQ. "U", "default form is U")
CALL OK(obj%GetForceEvaluation() .EQ. "I", &
        "default forceEvaluation is I")
CALL OK(.NOT. obj%IsInitiated(), "not initiated without dt")

! generalized-alpha: rhoInf = 0.5 gives alphaM = 0, alphaF = 1/3, and
! alphaF = 0.4 replaces it; beta and gamma follow alphaM = 0, alphaF = 0.4
CALL obj%ImportFromToml(tomlName="timeIntegration3", filename=filename)

CALL OK(obj%name .EQ. "GENA", "name is GENA")
CALL OK(ABS(obj%alphaM) .LT. tol, "alphaM from rhoInf")
CALL OK(ABS(obj%alphaF - 0.4_DFP) .LT. tol, "alphaF from the table")
CALL OK(ABS(obj%gamma - 0.9_DFP) .LT. tol, "gamma = 1/2 - alphaM + alphaF")
CALL OK(ABS(obj%beta - 0.49_DFP) .LT. tol, &
        "beta = (1 - alphaM + alphaF)^2/4")
CALL OK(ABS(obj%forceTime - 0.6_DFP) .LT. tol, "forceTime = 1 - alphaF")

END PROGRAM main
