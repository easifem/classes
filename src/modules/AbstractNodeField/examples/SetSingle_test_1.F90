!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get a single entry, set all entries
!
! SetSingle and GetSingle access one entry of the vector of values by
! its index. SetAll sets (or adds to) all entries. Without
! addContribution the entry becomes scale * VALUE; with
! addContribution = .TRUE. scale * VALUE is added. The field x is a
! ScalarField_ with six DOFs (order 1 Lagrange on square_3x2.h5).

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

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: x
  REAL(DFP) :: val

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  CALL x%SetAll(VALUE=1.0_DFP)
  CALL x%GetSingle(indx=6, VALUE=val)
  CALL OK(ABS(val - 1.0_DFP) .LT. tol, "SetAll")

  CALL x%SetAll(VALUE=2.0_DFP, scale=0.5_DFP, addContribution=math%yes)
  CALL x%GetSingle(indx=1, VALUE=val)
  CALL OK(ABS(val - 2.0_DFP) .LT. tol, "SetAll: add 0.5 * 2")

  CALL x%SetSingle(indx=3, VALUE=4.0_DFP, scale=2.0_DFP)
  CALL x%GetSingle(indx=3, VALUE=val)
  CALL OK(ABS(val - 8.0_DFP) .LT. tol, "SetSingle: scale")

  CALL x%SetSingle(indx=3, VALUE=1.0_DFP, addContribution=math%yes)
  CALL x%GetSingle(indx=3, VALUE=val)
  CALL OK(ABS(val - 9.0_DFP) .LT. tol, "SetSingle: add")

  CALL x%GetSingle(indx=4, VALUE=val)
  CALL OK(ABS(val - 2.0_DFP) .LT. tol, "other entries unchanged")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
