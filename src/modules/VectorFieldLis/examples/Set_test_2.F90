!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Copy a vector field when one of the fields uses LIS
!
! u and w are VectorFieldLis_, n is a native VectorField_. Set(VALUE)
! (Set13 of VectorField_) copies all the values of VALUE into obj, for
! every combination of engines: native from LIS (n = u), LIS from LIS
! (w = u) and LIS from native (w = n).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u, w
TYPE(VectorField_) :: n
REAL(DFP) :: want(12), found(12)
INTEGER(I4B) :: tsize, ii

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL w%Initiate(NAME="w", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

want = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)

! Test 1: native from LIS
BLOCK
  CALL n%Set(VALUE=0.0_DFP)
  CALL n%Set(VALUE=u)
  CALL n%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "n = u")
END BLOCK

! Test 2: LIS from LIS
BLOCK
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%Set(VALUE=u)
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "w = u")
END BLOCK

! Test 3: LIS from native
BLOCK
  CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)
  CALL w%Set(VALUE=n)
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found + want) .LT. tol), "w = n")
END BLOCK

! Test 4: u is not changed
BLOCK
  CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
END BLOCK

CALL n%DEALLOCATE()
CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
