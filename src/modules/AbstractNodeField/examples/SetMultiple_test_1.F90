!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get multiple entries
!
! SetMultiple and GetMultiple access several entries of the vector of
! values, given by indices (indx), or by a range istart:iend:stride.
! A range of VALUE can be selected with istart_value:iend_value:
! stride_value. SetMultiple with a scalar VALUE sets the range to one
! value. The field x is a ScalarField_ with six DOFs.

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
  REAL(DFP) :: found(6), src(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetAll(VALUE=0.0_DFP)

  ! indices
  CALL x%SetMultiple(indx=[2, 4], VALUE=[20.0_DFP, 40.0_DFP])
  CALL x%GetMultiple(indx=[4, 2], VALUE=found, tsize=tsize)
  CALL OK(tsize .EQ. 2, "GetMultiple indx: tsize")
  CALL OK(ALL(ABS(found(1:2) - [40.0_DFP, 20.0_DFP]) .LT. tol), &
          "SetMultiple indx")

  CALL x%SetMultiple(indx=[2, 4], VALUE=[1.0_DFP, 1.0_DFP], &
                     scale=2.0_DFP, addContribution=math%yes)
  CALL x%GetMultiple(indx=[2, 4], VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found(1:2) - [22.0_DFP, 42.0_DFP]) .LT. tol), &
          "SetMultiple indx: add 2 * value")

  ! range, vector value
  CALL x%SetMultiple(istart=1, iend=5, stride=2, &
                     VALUE=[1.0_DFP, 3.0_DFP, 5.0_DFP])
  CALL x%GetMultiple(istart=1, iend=5, stride=2, VALUE=found, &
                     tsize=tsize)
  CALL OK(tsize .EQ. 3, "GetMultiple range: tsize")
  CALL OK(ALL(ABS(found(1:3) - [1.0_DFP, 3.0_DFP, 5.0_DFP]) .LT. tol), &
          "SetMultiple range")

  ! range, scalar value
  CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=0.0_DFP)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - [1.0_DFP, 0.0_DFP, 3.0_DFP, 0.0_DFP, &
                           5.0_DFP, 0.0_DFP]) .LT. tol), &
          "SetMultiple range: one value")

  ! range, range of value: x(2:6:2) = src(1:3)
  CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
                     istart_value=1, iend_value=3, stride_value=1)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - [1.0_DFP, 1.0_DFP, 3.0_DFP, 2.0_DFP, &
                           5.0_DFP, 3.0_DFP]) .LT. tol), &
          "SetMultiple range: range of value")

  ! range, range of value: found(4:6) = x(1:3)
  found = 0.0_DFP
  CALL x%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
                     istart_value=4, iend_value=6, stride_value=1, &
                     tsize=tsize)
  CALL OK(tsize .EQ. 3, "GetMultiple range of value: tsize")
  CALL OK(ALL(ABS(found(4:6) - [1.0_DFP, 1.0_DFP, 3.0_DFP]) .LT. tol), &
          "GetMultiple range of value")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
