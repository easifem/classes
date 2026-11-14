!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: GetMultiple with scale and addContribution, SetMultiple from a
! range of another field
!
! GetMultiple(istart, iend, stride, VALUE, istart_value, iend_value,
! stride_value, scale, addContribution) sets (or adds) scale times the
! range of the field in the range of the array VALUE.
!
! SetMultiple(istart, iend, stride, VALUE, istart_value, iend_value,
! stride_value, scale, addContribution) with a node field VALUE sets (or
! adds) scale times the range of VALUE in the range of obj. VALUE can
! be obj itself when the two ranges do not overlap. The fields x, y are
! ScalarField_ with six DOFs.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
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
TYPE(ScalarField_) :: x, y
REAL(DFP) :: found(6), src(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL y%Initiate(NAME="y", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL y%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

! Test 1: found(2:6:2) = found(2:6:2) + 2 * y(1:5:2)
found = 1.0_DFP
CALL y%GetMultiple(istart=1, iend=5, stride=2, VALUE=found, &
                   istart_value=2, iend_value=6, stride_value=2, &
                   scale=2.0_DFP, addContribution=math%yes)
CALL OK(ALL(ABS(found - [1.0_DFP, 3.0_DFP, 1.0_DFP, 7.0_DFP, &
                         1.0_DFP, 11.0_DFP]) .LT. tol), &
        "GetMultiple: add")

! Test 2: found(4:6) = -1 * y(1:3)
found = 7.0_DFP
CALL y%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   scale=-1.0_DFP)
CALL OK(ALL(ABS(found - [7.0_DFP, 7.0_DFP, 7.0_DFP, -1.0_DFP, &
                         -2.0_DFP, -3.0_DFP]) .LT. tol), &
        "GetMultiple: set")

! Test 3: x(2:6:2) = 3 * y(1:3)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=y, &
                   istart_value=1, iend_value=3, stride_value=1, &
                   scale=3.0_DFP)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [7.0_DFP, 3.0_DFP, 7.0_DFP, 6.0_DFP, &
                         7.0_DFP, 9.0_DFP]) .LT. tol), &
        "SetMultiple field: set")

! Test 4: x(1:3) = x(1:3) + y(4:6)
CALL x%SetAll(VALUE=1.0_DFP)
CALL x%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   addContribution=math%yes)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [5.0_DFP, 6.0_DFP, 7.0_DFP, 1.0_DFP, &
                         1.0_DFP, 1.0_DFP]) .LT. tol), &
        "SetMultiple field: add")

! Test 5: same field, y(1:3) = y(4:6)
CALL y%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=4, iend_value=6, stride_value=1)
CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [4.0_DFP, 5.0_DFP, 6.0_DFP, 4.0_DFP, &
                         5.0_DFP, 6.0_DFP]) .LT. tol), &
        "SetMultiple field: same field")

CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
