!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Copy another space-time scalar field, v = VALUE (Set13)
!
! Set(VALUE=field) copies all values of a native STScalarField_ or of
! another STScalarFieldLis_ into the LIS vector of v. u, v are
! STScalarFieldLis_ and n is a native STScalarField_, all with 12
! values (6 space DOFs, timeCompo = 2).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
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
REAL(DFP) :: found(12), want(12)
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
want = [(REAL(ii, DFP), ii=1, 12)]

! Test 1: v = n, all values of a native field (not one value for each
! time component)
BLOCK
  CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
  CALL v%SetAll(VALUE=0.0_DFP)
  CALL v%Set(VALUE=n)
  CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "Set13: LIS <- native")
END BLOCK

! Test 2: v = u, LIS vector copied
BLOCK
  CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=2.0_DFP * want)
  CALL v%SetAll(VALUE=0.0_DFP)
  CALL v%Set(VALUE=u)
  CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 2.0_DFP * want) .LT. tol), "Set13: LIS <- LIS")
END BLOCK

CALL n%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
