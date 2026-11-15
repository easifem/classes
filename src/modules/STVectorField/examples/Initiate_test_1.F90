!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Initiate a space-time vector field
!
! u is read from STVectorField_test.toml (ImportFromToml): order 1
! Lagrange in space (six DOFs) and time (two DOFs), spaceCompo = 2. So
! timeCompo = 2 and u%SIZE() = 6 * 2 * 2 = 24.
!
! v is initiated by arguments with the same fedof, geofedof and
! timefedof; timeCompo is taken from timefedof. w is a copy of v
! (Initiate(obj2)). DEALLOCATE resets spaceCompo and timeCompo.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u, v, w
CHARACTER(1) :: names(1)
REAL(DFP) :: found(24)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: ImportFromToml
CALL OK(u%IsInitiated(), "ImportFromToml: IsInitiated")
CALL OK(u%spaceCompo .EQ. 2, "ImportFromToml: spaceCompo")
CALL OK(u%timeCompo .EQ. 2, "ImportFromToml: timeCompo")
CALL OK(u%SIZE() .EQ. 24, "ImportFromToml: Size = 24")

! Test 2: Initiate by arguments
CALL v%Initiate(name="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
CALL OK(v%IsInitiated(), "Initiate: IsInitiated")
CALL OK(v%spaceCompo .EQ. 2, "Initiate: spaceCompo")
CALL OK(v%timeCompo .EQ. timefedof%GetTotalDOF(), &
        "Initiate: timeCompo = total time DOF")
CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF() * 2 * 2, &
        "Initiate: Size = space DOF * spaceCompo * timeCompo")
CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "GetTotalPhysicalVars")
CALL v%GetPhysicalNames(names)
CALL OK(names(1) .EQ. "v", "GetPhysicalNames")
CALL OK(ALL(v%GetSpaceCompo(1) .EQ. [2]), "GetSpaceCompo")
CALL OK(ALL(v%GetTimeCompo(1) .EQ. [2]), "GetTimeCompo")
CALL OK(v%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
        "GetStorageFMT")

! Test 3: copy
CALL v%Set(VALUE=3.0_DFP)
CALL w%Initiate(obj2=v)
CALL OK(w%spaceCompo .EQ. 2, "Initiate(obj2): spaceCompo")
CALL OK(w%timeCompo .EQ. 2, "Initiate(obj2): timeCompo")
CALL w%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(tsize .EQ. 24, "Initiate(obj2): Size")
CALL OK(ALL(ABS(found - 3.0_DFP) .LT. tol), "Initiate(obj2): values")

! Test 4: DEALLOCATE
CALL w%DEALLOCATE()
CALL OK(.NOT. w%IsInitiated(), "DEALLOCATE: not initiated")
CALL OK(w%spaceCompo .EQ. 0, "DEALLOCATE: spaceCompo = 0")
CALL OK(w%timeCompo .EQ. 0, "DEALLOCATE: timeCompo = 0")

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
