!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Set and Get methods of a LIS space-time vector field
!
! SetSingle, SetAll, SetMultiple and GetSingle, GetMultiple work on the
! LIS vector. u, v are STVectorFieldLis_ with 24 values (6 nodes,
! spaceCompo = 2, timeCompo = 2), n is a native STVectorField_. The
! value of node I, space component s and time component a has the
! index ((a-1)*2 + (s-1))*6 + I, so 7:12 is space 2 and time 1.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE STVectorField_Class, ONLY: STVectorField_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorFieldLis_) :: u, v
TYPE(STVectorField_) :: n
REAL(DFP) :: found(24), want(24), areal
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL v%Initiate(NAME="v", engine="LIS_OMP", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
want = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: SetSingle and GetSingle, set and add
CALL u%SetAll(VALUE=0.0_DFP)
CALL u%SetSingle(indx=8, VALUE=3.0_DFP)
CALL u%SetSingle(indx=8, VALUE=1.0_DFP, scale=2.0_DFP, &
                 addContribution=math%yes)
CALL u%GetSingle(indx=8, VALUE=areal)
CALL OK(ABS(areal - 5.0_DFP) .LT. tol, "SetSingle, GetSingle")

! Test 2: SetMultiple and GetMultiple over all values
CALL u%SetMultiple(istart=1, iend=24, stride=1, VALUE=want)
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(tsize .EQ. 24, "GetMultiple: tsize")
CALL OK(ALL(ABS(found - want) .LT. tol), "SetMultiple: range")

! Test 3: SetMultiple and GetMultiple at given indices, add
CALL u%SetMultiple(indx=[2, 7, 12], VALUE=[1.0_DFP, 1.0_DFP, 1.0_DFP], &
                   scale=10.0_DFP, addContribution=math%yes)
CALL u%GetMultiple(indx=[2, 7, 12], VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:3) - [12.0_DFP, 17.0_DFP, 22.0_DFP]) .LT. tol), &
        "SetMultiple, GetMultiple: indices")

! Test 4: one value in the range of space 2, time 1
CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=-1.0_DFP)
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(7:12) + 1.0_DFP) .LT. tol) .AND. &
        ABS(found(2) - 12.0_DFP) .LT. tol .AND. &
        ALL(ABS(found(13:24) - want(13:24)) .LT. tol), &
        "SetMultiple: one value")

! Test 5: SetAll with add
CALL u%SetAll(VALUE=1.0_DFP)
CALL u%SetAll(VALUE=2.0_DFP, addContribution=math%yes)
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 3.0_DFP) .LT. tol), "SetAll: add")

! Test 6: a range of an array, u(7:12) = want(1:6)
CALL u%SetAll(VALUE=0.0_DFP)
CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=want, &
                   istart_value=1, iend_value=6, stride_value=1)
CALL u%GetMultiple(istart=7, iend=12, stride=1, VALUE=found, &
                   istart_value=1, iend_value=6, stride_value=1, &
                   tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - want(1:6)) .LT. tol), &
        "SetMultiple3, GetMultiple3")

! Test 7: SetMultiple from a native field, v(1:6) = 2 * n(7:12)
CALL n%SetMultiple(istart=1, iend=24, stride=1, VALUE=want)
CALL v%SetAll(VALUE=0.0_DFP)
CALL v%SetMultiple(istart=1, iend=6, stride=1, VALUE=n, istart_value=7, &
                   iend_value=12, stride_value=1, scale=2.0_DFP)
CALL v%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - 2.0_DFP * want(7:12)) .LT. tol) .AND. &
        ALL(ABS(found(7:24)) .LT. tol), "LIS <- native")

! Test 8: SetMultiple from a LIS field, v(19:24) = u(7:12)
CALL u%SetMultiple(istart=1, iend=24, stride=1, VALUE=want)
CALL v%SetMultiple(istart=19, iend=24, stride=1, VALUE=u, &
                   istart_value=7, iend_value=12, stride_value=1)
CALL v%GetMultiple(istart=19, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:6) - want(7:12)) .LT. tol), "LIS <- LIS")

! Test 9: GetMultiple4, found(7:12) = found(7:12) + 2 * u(1:6)
found = 1.0_DFP
CALL u%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                   istart_value=7, iend_value=12, stride_value=1, &
                   scale=2.0_DFP, addContribution=math%yes)
CALL OK(ALL(ABS(found(7:12) - (1.0_DFP + 2.0_DFP * want(1:6))) .LT. tol) &
        .AND. ALL(ABS(found(1:6) - 1.0_DFP) .LT. tol), "GetMultiple4: add")

CALL n%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
