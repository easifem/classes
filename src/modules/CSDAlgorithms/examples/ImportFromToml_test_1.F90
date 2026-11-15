!> author: Shion Shimizu
! date: 2026-11-13
! summary: Testing ImportFromToml of CSDAlgorithm2_

PROGRAM main
USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(CSDAlgorithm2_) :: obj
CHARACTER(*), PARAMETER :: filename = "ImportFromToml_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! old key gamma is the splitting ratio, gamma-Bathe
CALL obj%ImportFromToml(tomlName="oldBathe", filename=filename)
CALL OK(obj%name .EQ. "BATH", "oldBathe: name")
CALL OK(ABS(obj%splittingRatio - 0.6_DFP) .LT. tol, &
        "oldBathe: gamma is read as splittingRatio")
CALL OK(ABS(obj%b2 - 0.4_DFP / 1.4_DFP) .LT. tol, "oldBathe: b2")
CALL OK(.NOT. obj%IsInitiated(), "oldBathe: not initiated without dt")

! RhoBathe without rhoInf uses rhoInf = 0
CALL obj%ImportFromToml(tomlName="rhoBathe", filename=filename)
CALL OK(ABS(obj%b1 - 1.0_DFP / 3.0_DFP) .LT. tol, &
        "rhoBathe: rhoInf = 0 by default")
CALL OK(obj%GetForm() .EQ. "A", "rhoBathe: form")
CALL OK(obj%GetForceEvaluation() .EQ. "E", "rhoBathe: forceEvaluation")
CALL OK(obj%IsInitiated(), "rhoBathe: initiated with dt")

! beta1 and beta2 select beta1/beta2-Bathe
CALL obj%ImportFromToml(tomlName="betaBathe", filename=filename)
CALL OK(ABS(obj%b0 - 0.35_DFP) .LT. tol, "betaBathe: b0 = (1-beta1) s")
CALL OK(ABS(obj%b2 - 0.3_DFP) .LT. tol, "betaBathe: b2 = (1-s) beta2")

! Newmark with one substep
CALL obj%ImportFromToml(tomlName="newmark", filename=filename)
CALL OK(obj%name .EQ. "NEWM", "newmark: name")
CALL OK(ABS(obj%beta - 0.3_DFP) .LT. tol, "newmark: beta")
CALL OK(ABS(obj%gamma - 0.5_DFP) .LT. tol, "newmark: default gamma")
CALL OK(obj%GetTotalSubstep() .EQ. 1, "newmark: one substep")

END PROGRAM main
