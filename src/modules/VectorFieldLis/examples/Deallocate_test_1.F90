!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Deallocate a LIS vector field
!
! DEALLOCATE destroys the LIS vector and then deallocates the data of
! VectorField_. Afterwards the field is not initiated, lis_ptr and
! spaceCompo are zero, and the field can be initiated again.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: INT64
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

! Test 1: the field is empty after DEALLOCATE
CALL u%DEALLOCATE()
CALL OK(.NOT. u%IsInitiated(), "DEALLOCATE: not initiated")
CALL OK(u%lis_ptr .EQ. 0_INT64, "DEALLOCATE: lis_ptr = 0")
CALL OK(u%spaceCompo .EQ. 0, "DEALLOCATE: spaceCompo = 0")
CALL OK(u%SIZE() .EQ. 0, "DEALLOCATE: Size = 0")

! Test 2: DEALLOCATE of an empty field does nothing
CALL u%DEALLOCATE()
CALL OK(.NOT. u%IsInitiated(), "DEALLOCATE twice: not initiated")

! Test 3: the field can be initiated again
CALL u%Initiate(NAME="u", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)
CALL OK(u%IsInitiated(), "Initiate again: IsInitiated")
CALL OK(u%SIZE() .EQ. 12, "Initiate again: Size")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
