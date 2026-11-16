!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Deallocate a LIS space-time vector field
!
! After DEALLOCATE the field is not initiated, lis_ptr, spaceCompo and
! timeCompo are zero; DEALLOCATE of an empty field does nothing and
! the field can be initiated again.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorFieldLis_test.toml"
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorFieldLis_) :: u

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: the field is empty after DEALLOCATE
CALL u%DEALLOCATE()
CALL OK(.NOT. u%IsInitiated(), "Deallocate: not initiated")
CALL OK(u%lis_ptr .EQ. 0, "Deallocate: lis_ptr = 0")
CALL OK(u%spaceCompo .EQ. 0 .AND. u%timeCompo .EQ. 0, &
        "Deallocate: components = 0")
CALL OK(u%SIZE() .EQ. 0, "Deallocate: Size = 0")

! Test 2: DEALLOCATE of an empty field does nothing
CALL u%DEALLOCATE()
CALL OK(.NOT. u%IsInitiated(), "Deallocate twice: not initiated")

! Test 3: the field can be initiated again
CALL u%Initiate(NAME="u", engine="LIS_OMP", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
CALL OK(u%IsInitiated() .AND. u%SIZE() .EQ. 24, "Initiate again")
CALL OK(u%lis_ptr .NE. 0, "Initiate again: LIS vector")

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
