!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set one time component from a time component of another field
!
! The fields u and v are order 1 Lagrange in space (six nodes) and in
! time (timeCompo = 2). In DOF storage format, entries 1 to 6 are time
! component 1 and entries 7 to 12 are time component 2.
!
! Set(objTimeCompo, VALUE, valueTimeCompo, scale, addContribution) sets
! time component objTimeCompo of u to scale * (time component
! valueTimeCompo of v), or adds it when addContribution is true. The
! other time component of u is not changed.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u, v
REAL(DFP) :: found(12), src(12)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)

src = [(REAL(ii, DFP), ii=1, 12)]
CALL v%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)

! Test 1: u(time 1) = v(time 2)
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(objTimeCompo=1, VALUE=v, valueTimeCompo=2)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - src(7:12)) .LT. tol), "set: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6)) .LT. tol), "set: time 2 not changed")
END BLOCK

! Test 2: u(time 2) = u(time 2) + 2 * v(time 1)
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%Set(objTimeCompo=2, VALUE=v, valueTimeCompo=1, scale=2.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 1.0_DFP) .LT. tol), &
          "add: time 1 not changed")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 2.0_DFP * src(1:6))) .LT. tol), &
          "add: time 2")
END BLOCK

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
