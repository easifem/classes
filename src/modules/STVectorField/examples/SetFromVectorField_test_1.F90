!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Copy one time component between a space-time vector field
!   and a vector field
!
! u is a space-time vector field (two space components, timeCompo = 2)
! and v, w are vector fields with two components on the same fedof.
!
!- u%SetFromVectorField(VALUE=v, timeCompo) sets time component
!  timeCompo of u to v (all space components),
!- u%SetToVectorField(VALUE=w, timeCompo) sets w to time component
!  timeCompo of u.
!
! Both accept scale and addContribution.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
TYPE(VectorField_) :: v, w
REAL(DFP) :: a(6), b(6), found(12)
INTEGER(I4B) :: tsize, ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)
CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

a = [(REAL(ii, DFP), ii=1, 6)]
b = 10.0_DFP * a
CALL v%Set(VALUE=a, spaceCompo=1)
CALL v%Set(VALUE=b, spaceCompo=2)

! Test 1: SetFromVectorField
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetFromVectorField(VALUE=v, timeCompo=2)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "SetFromVectorField: x")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - b) .LT. tol), "SetFromVectorField: y")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6)) .LT. tol), &
          "SetFromVectorField: time 1 unchanged")

  CALL u%SetFromVectorField(VALUE=v, timeCompo=2, scale=2.0_DFP, &
                            addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * b) .LT. tol), &
          "SetFromVectorField: add 2 * value")
END BLOCK

! Test 2: SetToVectorField
BLOCK
  CALL w%Set(VALUE=0.0_DFP)
  CALL u%SetToVectorField(VALUE=w, timeCompo=2)
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * a) .LT. tol), &
          "SetToVectorField: x")
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * b) .LT. tol), &
          "SetToVectorField: y")

  CALL u%SetToVectorField(VALUE=w, timeCompo=1, scale=1.0_DFP, &
                          addContribution=math%yes)
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * a) .LT. tol), &
          "SetToVectorField: add time 1 (zero)")
END BLOCK

CALL w%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
