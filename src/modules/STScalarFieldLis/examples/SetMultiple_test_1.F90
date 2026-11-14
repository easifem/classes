!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set and Get methods of a LIS space-time scalar field
!
! SetSingle, SetAll, SetMultiple and GetSingle, GetMultiple work on the
! LIS vector. The Set methods of STScalarField_ (Set2,
! SetFromScalarField, SetToScalarField) call them, so they work on the
! LIS vector too. u, v are STScalarFieldLis_ with 12 values
! (6 space DOFs, timeCompo = 2), n is a native STScalarField_ and s is
! a ScalarFieldLis_ with 6 values. Value (I, a) has the index
! (a-1)*6 + I.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarFieldLis_) :: u, v
TYPE(STScalarField_) :: n
TYPE(ScalarFieldLis_) :: s
REAL(DFP) :: found(12), want(12), areal, s6(6)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)
CALL s%Initiate(NAME="s", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)
want = [(REAL(ii, DFP), ii=1, 12)]

! Test 1: SetSingle and GetSingle, set and add
CALL u%SetAll(VALUE=0.0_DFP)
CALL u%SetSingle(indx=8, VALUE=3.0_DFP)
CALL u%SetSingle(indx=8, VALUE=1.0_DFP, scale=2.0_DFP, &
                 addContribution=math%yes)
CALL u%GetSingle(indx=8, VALUE=areal)
CALL OK(ABS(areal - 5.0_DFP) .LT. tol, "SetSingle, GetSingle")

! Test 2: SetMultiple and GetMultiple over all values
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(tsize .EQ. 12, "GetMultiple: tsize")
CALL OK(ALL(ABS(found - want) .LT. tol), "SetMultiple: range")

! Test 3: SetMultiple and GetMultiple at given indices, add
CALL u%SetMultiple(indx=[2, 7, 12], VALUE=[1.0_DFP, 1.0_DFP, 1.0_DFP], &
                   scale=10.0_DFP, addContribution=math%yes)
CALL u%GetMultiple(indx=[2, 7, 12], VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:3) - [12.0_DFP, 17.0_DFP, 22.0_DFP]) .LT. tol), &
        "SetMultiple, GetMultiple: indices")

! Test 4: one value in the range of time component 2
CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=-1.0_DFP)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(7:12) + 1.0_DFP) .LT. tol) .AND. &
        ABS(found(2) - 12.0_DFP) .LT. tol, "SetMultiple: one value")

! Test 5: SetAll with add
CALL u%SetAll(VALUE=1.0_DFP)
CALL u%SetAll(VALUE=2.0_DFP, addContribution=math%yes)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 3.0_DFP) .LT. tol), "SetAll: add")

! Test 6: Set2 of STScalarField_, time component a set to VALUE(a)
CALL u%Set(VALUE=[4.0_DFP, 5.0_DFP])
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - 4.0_DFP) .LT. tol) .AND. &
        ALL(ABS(found(7:12) - 5.0_DFP) .LT. tol), "Set2: time components")

! Test 7: SetMultiple from a native field, v(1:6) = 2 * n(7:12)
CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
CALL v%SetAll(VALUE=0.0_DFP)
CALL v%SetMultiple(istart=1, iend=6, stride=1, VALUE=n, istart_value=7, &
                   iend_value=12, stride_value=1, scale=2.0_DFP)
CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - 2.0_DFP * want(7:12)) .LT. tol) .AND. &
        ALL(ABS(found(7:12)) .LT. tol), "LIS <- native")

! Test 8: SetMultiple from a LIS field, v(7:12) = u(1:6)
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
CALL v%SetMultiple(istart=7, iend=12, stride=1, VALUE=u, istart_value=1, &
                   iend_value=6, stride_value=1)
CALL v%GetMultiple(istart=7, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - want(1:6)) .LT. tol), "LIS <- LIS")

! Test 9: SetFromScalarField and SetToScalarField with a LIS field
s6 = [10.0_DFP, 20.0_DFP, 30.0_DFP, 40.0_DFP, 50.0_DFP, 60.0_DFP]
CALL s%SetMultiple(istart=1, iend=6, stride=1, VALUE=s6)
CALL u%SetFromScalarField(timeCompo=2, VALUE=s)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - want(1:6)) .LT. tol) .AND. &
        ALL(ABS(found(7:12) - s6) .LT. tol), "SetFromScalarField")
CALL u%SetToScalarField(timeCompo=1, VALUE=s)
CALL s%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - want(1:6)) .LT. tol), "SetToScalarField")

CALL s%DEALLOCATE()
CALL n%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
