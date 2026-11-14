!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Initiate a LIS space-time scalar field
!
! The field u is made by ImportFromToml (STScalarFieldLis_test.toml,
! engine LIS_OMP): order 1 Lagrange in space (six DOFs) and in time
! (two DOFs), so timeCompo = 2 and the field has 6 * 2 = 12 values.
!
!- Initiate and ImportFromToml create the LIS vector,
!- each field has its own LIS vector,
!- Initiate(obj2, copyFull) copies the values of a LIS or a native
!  space-time field into a new LIS vector.

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
TYPE(STScalarFieldLis_) :: u, v, c, c2
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

! Test 1: fields from the toml file and from Initiate
CALL OK(u%IsInitiated(), "u: IsInitiated")
CALL OK(u%timeCompo .EQ. 2, "u: timeCompo")
CALL OK(u%SIZE() .EQ. 12, "u: Size")
CALL OK(u%lis_ptr .NE. 0, "u: LIS vector created")
CALL OK(v%SIZE() .EQ. 12, "v: Size")
CALL OK(v%lis_ptr .NE. 0, "v: LIS vector created")

! Test 2: u and v have their own LIS vectors
CALL u%SetAll(VALUE=1.0_DFP)
CALL v%SetAll(VALUE=2.0_DFP)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "u: values")
CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "v: values")

! Test 3: copy of a LIS field, with its own LIS vector
want = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
CALL c%Initiate(obj2=u, copyFull=math%yes)
CALL OK(c%timeCompo .EQ. u%timeCompo, "copy: timeCompo")
CALL c%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "copy: values")
CALL c%SetAll(VALUE=0.0_DFP)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "copy: u unchanged")

! Test 4: copy of a native space-time field
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)
CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)
CALL c2%Initiate(obj2=n, copyFull=math%yes)
CALL OK(c2%lis_ptr .NE. 0, "native copy: LIS vector created")
CALL OK(c2%GetEngineName() .EQ. "LIS_OMP", "native copy: engine")
CALL c2%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found + want) .LT. tol), "native copy: values")

CALL c2%DEALLOCATE()
CALL n%DEALLOCATE()
CALL c%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
