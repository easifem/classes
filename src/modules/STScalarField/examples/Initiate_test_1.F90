!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Initiate a space-time scalar field
!
! The field u is made by ImportFromToml (STScalarField_test.toml):
! order 1 Lagrange in space (six DOFs) and in time (two DOFs), so
! timeCompo = 2 and the field has 6 * 2 = 12 values.
!
!- Initiate(name, engine, fedof, geofedof, timefedof) takes timeCompo
!  from the total DOF of timefedof,
!- Initiate(name, engine, fedof, geofedof, timeCompo) takes it from
!  timeCompo(1) when there is no timefedof,
!- Initiate(obj2, copyFull) copies another space-time field.

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
TYPE(STScalarField_) :: u, v, w, c
REAL(DFP) :: found(12), want(12)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: field from the toml file
CALL OK(u%IsInitiated(), "u: IsInitiated")
CALL OK(u%timeCompo .EQ. 2, "u: timeCompo")
CALL OK(u%SIZE() .EQ. 12, "u: Size")
CALL OK(u%GetTotalPhysicalVars() .EQ. 1, "u: one physical variable")

! Test 2: timeCompo from timefedof
BLOCK
  CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof, timefedof=timefedof)
  CALL OK(v%timeCompo .EQ. timefedof%GetTotalDOF(), &
          "v: timeCompo = time DOF")
  CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF() * v%timeCompo, &
          "v: Size = space DOF * timeCompo")
END BLOCK

! Test 3: timeCompo given without timefedof
BLOCK
  CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof, timeCompo=[3])
  CALL OK(w%timeCompo .EQ. 3, "w: timeCompo")
  CALL OK(w%SIZE() .EQ. 18, "w: Size")
END BLOCK

! Test 4: copy of u, with its values
BLOCK
  want = [(REAL(ii, DFP), ii=1, 12)]
  CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
  CALL c%Initiate(obj2=u, copyFull=math%yes)
  CALL OK(c%timeCompo .EQ. u%timeCompo, "copy: timeCompo")
  CALL OK(c%SIZE() .EQ. u%SIZE(), "copy: Size")
  CALL c%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "copy: values")
END BLOCK

CALL c%DEALLOCATE()
CALL w%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
