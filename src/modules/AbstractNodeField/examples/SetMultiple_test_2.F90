!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: SetMultiple with scale and without addContribution
!
! Without addContribution, SetMultiple sets the entries to scale * VALUE
! (as SetSingle and SetAll do). This is checked for the four forms:
! indices, range with a vector VALUE, range with a range of VALUE, and
! range with one value. The entries which are not selected keep their
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

! indices: x(2) = 2 * 10, x(4) = 2 * 20
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(indx=[2, 4], VALUE=[10.0_DFP, 20.0_DFP], &
                   scale=2.0_DFP)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [7.0_DFP, 20.0_DFP, 7.0_DFP, 40.0_DFP, &
                         7.0_DFP, 7.0_DFP]) .LT. tol), &
        "indx: scale * value")

! range, vector value: x(1:5:2) = 3 * [1, 2, 3]
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=1, iend=5, stride=2, &
                   VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], scale=3.0_DFP)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [3.0_DFP, 7.0_DFP, 6.0_DFP, 7.0_DFP, &
                         9.0_DFP, 7.0_DFP]) .LT. tol), &
        "range: scale * value")

! range, range of value: x(2:6:2) = -1 * src(4:6)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   scale=-1.0_DFP)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [7.0_DFP, -4.0_DFP, 7.0_DFP, -5.0_DFP, &
                         7.0_DFP, -6.0_DFP]) .LT. tol), &
        "range of value: scale * value")

! range, one value: x(1:6:3) = 0.5 * 8
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=1, iend=6, stride=3, VALUE=8.0_DFP, &
                   scale=0.5_DFP)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [4.0_DFP, 7.0_DFP, 7.0_DFP, 4.0_DFP, &
                         7.0_DFP, 7.0_DFP]) .LT. tol), &
        "one value: scale * value")

! without scale the values are set as they are
CALL x%SetMultiple(indx=[2, 4], VALUE=[10.0_DFP, 20.0_DFP])
CALL x%GetMultiple(indx=[2, 4], VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:2) - [10.0_DFP, 20.0_DFP]) .LT. tol), &
        "no scale: value")

CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
