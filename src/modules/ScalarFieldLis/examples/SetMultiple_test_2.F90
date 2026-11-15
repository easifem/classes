!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: GetMultiple with scale and SetMultiple from another field,
! LIS and native engines
!
! GetMultiple(istart, iend, stride, VALUE, istart_value, iend_value,
! stride_value, scale, addContribution) sets (or adds) scale times a
! range of the LIS vector in a range of the array VALUE.
!
! SetMultiple(istart, iend, stride, VALUE, istart_value, iend_value,
! stride_value, scale, addContribution) with a node field VALUE sets (or
! adds) scale times a range of VALUE in a range of obj. The tests cover
! LIS <- native, LIS <- LIS (copy and add), native <- LIS and LIS <- the
! same LIS field. x, y are ScalarFieldLis_ and n is a native
! ScalarField_, all with six DOFs.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: x, y
TYPE(ScalarField_) :: n
REAL(DFP) :: found(6), src(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL y%Initiate(NAME="y", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL y%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
CALL n%SetMultiple(istart=1, iend=6, stride=1, VALUE=10.0_DFP * src)

! Test 1: found(2:6:2) = found(2:6:2) + 2 * y(1:5:2)
found = 1.0_DFP
CALL y%GetMultiple(istart=1, iend=5, stride=2, VALUE=found, &
                   istart_value=2, iend_value=6, stride_value=2, &
                   scale=2.0_DFP, addContribution=math%yes)
CALL OK(ALL(ABS(found - [1.0_DFP, 3.0_DFP, 1.0_DFP, 7.0_DFP, &
                         1.0_DFP, 11.0_DFP]) .LT. tol), &
        "GetMultiple: add")

! Test 2: found(4:6) = y(1:3)
found = 7.0_DFP
CALL y%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   scale=1.0_DFP)
CALL OK(ALL(ABS(found - [7.0_DFP, 7.0_DFP, 7.0_DFP, 1.0_DFP, &
                         2.0_DFP, 3.0_DFP]) .LT. tol), &
        "GetMultiple: set")

! Test 3: LIS <- native, x(2:6:2) = 0.5 * n(1:3)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=n, &
                   istart_value=1, iend_value=3, stride_value=1, &
                   scale=0.5_DFP)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [7.0_DFP, 5.0_DFP, 7.0_DFP, 10.0_DFP, &
                         7.0_DFP, 15.0_DFP]) .LT. tol), &
        "LIS <- native")

! Test 4: LIS <- LIS, copy, x(1:3) = y(4:6)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=4, iend_value=6, stride_value=1)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [4.0_DFP, 5.0_DFP, 6.0_DFP, 7.0_DFP, &
                         7.0_DFP, 7.0_DFP]) .LT. tol), &
        "LIS <- LIS: copy")

! Test 5: LIS <- LIS, add, x(4:6) = x(4:6) + 2 * y(1:5:2)
CALL x%SetAll(VALUE=1.0_DFP)
CALL x%SetMultiple(istart=4, iend=6, stride=1, VALUE=y, &
                   istart_value=1, iend_value=5, stride_value=2, &
                   scale=2.0_DFP, addContribution=math%yes)
CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [1.0_DFP, 1.0_DFP, 1.0_DFP, 3.0_DFP, &
                         7.0_DFP, 11.0_DFP]) .LT. tol), &
        "LIS <- LIS: add")

! Test 6: native <- LIS, n(1:3) = n(1:3) + y(4:6)
CALL n%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   addContribution=math%yes)
CALL n%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [14.0_DFP, 25.0_DFP, 36.0_DFP, 40.0_DFP, &
                         50.0_DFP, 60.0_DFP]) .LT. tol), &
        "native <- LIS")

! Test 7: same LIS field, y(1:3) = y(4:6)
CALL y%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=4, iend_value=6, stride_value=1)
CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - [4.0_DFP, 5.0_DFP, 6.0_DFP, 4.0_DFP, &
                         5.0_DFP, 6.0_DFP]) .LT. tol), &
        "LIS <- same LIS field")

CALL n%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
