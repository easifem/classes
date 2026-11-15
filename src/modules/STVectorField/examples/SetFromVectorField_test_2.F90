!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set one time component of a space-time vector field from a
!   LIS vector field
!
! u is a native space-time vector field (two space components,
! timeCompo = 2) and v is a VectorFieldLis_ with two components on the
! same fedof. SetFromVectorField(VALUE=v, timeCompo) reads the values
! of v from its LIS vector (v has no Fortran vector to point to).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
TYPE(VectorFieldLis_) :: v
REAL(DFP) :: a(6), b(6), found(12)
INTEGER(I4B) :: tsize, ii

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL v%Initiate(NAME="v", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

a = [(REAL(ii, DFP), ii=1, 6)]
b = 10.0_DFP * a
CALL v%Set(VALUE=a, spaceCompo=1)
CALL v%Set(VALUE=b, spaceCompo=2)

! Test 1: set time component 2 from v
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
END BLOCK

! Test 2: add a scaled v
BLOCK
  CALL u%SetFromVectorField(VALUE=v, timeCompo=2, scale=2.0_DFP, &
                            addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * b) .LT. tol), &
          "SetFromVectorField: add 2 * value")
END BLOCK

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
