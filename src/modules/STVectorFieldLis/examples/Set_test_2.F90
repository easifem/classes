!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Copy between LIS and native space-time and vector fields
!
! u, w are STVectorFieldLis_ and n is a native STVectorField_, all
! with 24 values (6 nodes, spaceCompo = 2, timeCompo = 2). a is a
! native VectorField_ and b a VectorFieldLis_ (12 values).
!
!- Set(VALUE=field) (Set14) copies a native field into a LIS field,
!  a LIS field into a native field and a LIS field into a LIS field,
!- SetFromVectorField sets one time component from a LIS vector field,
!- SetToVectorField sets a native and a LIS vector field from one time
!  component of a LIS space-time field.

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
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorFieldLis_) :: u, w
TYPE(STVectorField_) :: n
TYPE(VectorField_) :: a
TYPE(VectorFieldLis_) :: b
REAL(DFP) :: found(24), want(24)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL w%Initiate(NAME="w", engine="LIS_OMP", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
CALL a%Initiate(NAME="a", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof)
CALL b%Initiate(NAME="b", engine="LIS_OMP", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof)
want = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: Set14, LIS <- native
CALL n%SetMultiple(istart=1, iend=24, stride=1, VALUE=want)
CALL u%Set(VALUE=n)
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "Set14: LIS <- native")

! Test 2: Set14, native <- LIS
CALL u%SetMultiple(istart=1, iend=24, stride=1, VALUE=-want)
CALL n%Set(VALUE=u)
CALL n%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found + want) .LT. tol), "Set14: native <- LIS")

! Test 3: Set14, LIS <- LIS
CALL w%Set(VALUE=u)
CALL w%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found + want) .LT. tol), "Set14: LIS <- LIS")

! Test 4: SetFromVectorField, time 2 of u from a LIS vector field
CALL u%SetAll(VALUE=0.0_DFP)
CALL b%SetMultiple(istart=1, iend=12, stride=1, VALUE=want(1:12))
CALL u%SetFromVectorField(VALUE=b, timeCompo=2)
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(13:24) - want(1:12)) .LT. tol) .AND. &
        ALL(ABS(found(1:12)) .LT. tol), "SetFromVectorField: LIS")

! Test 5: SetToVectorField, native and LIS vector fields from time 2
CALL u%SetMultiple(istart=1, iend=24, stride=1, VALUE=want)
CALL a%SetAll(VALUE=0.0_DFP)
CALL u%SetToVectorField(VALUE=a, timeCompo=2)
CALL a%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:12) - want(13:24)) .LT. tol), &
        "SetToVectorField: native")
CALL b%SetAll(VALUE=0.0_DFP)
CALL u%SetToVectorField(VALUE=b, timeCompo=2)
CALL b%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:12) - want(13:24)) .LT. tol), &
        "SetToVectorField: LIS")

CALL b%DEALLOCATE()
CALL a%DEALLOCATE()
CALL n%DEALLOCATE()
CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
