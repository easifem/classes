!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set a single entry and all entries of a scalar field
!
! Set(globalNode, islocal, VALUE) sets one entry, and Set(VALUE) sets
! all entries. Without addContribution the entry becomes
! scale * VALUE; with addContribution = .TRUE. scale * VALUE is added.
! The entries are read back with Get(VALUE, globalNode, islocal).

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: u
  REAL(DFP) :: val, found(6)
  INTEGER(I4B) :: ii

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  ! all entries
  CALL u%Set(VALUE=1.0_DFP)
  DO ii = 1, 6
    CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
  END DO
  CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Set all")

  CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes)
  CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set all: add 2 * 1")

  ! single entry
  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP)
  CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
  CALL OK(ABS(val - 5.0_DFP) .LT. tol, "Set single")

  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP, &
             scale=2.0_DFP)
  CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
  CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: scale")

  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=1.0_DFP, &
             scale=3.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
  CALL OK(ABS(val - 13.0_DFP) .LT. tol, "Set single: add 3 * 1")

  CALL u%Get(VALUE=val, globalNode=1, islocal=math%yes)
  CALL OK(ABS(val - 3.0_DFP) .LT. tol, "other entries unchanged")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
