!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Export a LIS space-time vector field to HDF5 and import it
!
! u (STVectorFieldLis_test.toml, engine LIS_OMP, spaceCompo = 2,
! timeCompo = 2) is set to f(s, t, i) = 100 s + 10 t + i and exported
! to the group "u" of Export_test_1.h5. Export gathers the LIS vector
! into realVec first. v is imported from the same group; Import must
! create the LIS vector of v and copy the values into it.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorFieldLis_test.toml"
CHARACTER(*), PARAMETER :: h5FileName = "Export_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorFieldLis_) :: u, v
TYPE(HDF5File_) :: h5
REAL(DFP) :: found(24), found2(24)
INTEGER(I4B) :: tsize, ii, s, t

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
DO t = 1, 2
  DO s = 1, 2
    CALL u%Set(VALUE=[(100.0_DFP * s + 10.0_DFP * t + ii, ii=1, 6)], &
               spaceCompo=s, timeCompo=t)
  END DO
END DO

! Test 1: Export
CALL h5%Initiate(h5FileName, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="u")
CALL h5%DEALLOCATE()
CALL OK(.TRUE., "Export")

! Test 2: Import
CALL h5%Initiate(h5FileName, mode="READ")
CALL h5%OPEN()
CALL v%IMPORT(hdf5=h5, group="u", fedof=fedof, geofedof=geofedof, &
              timefedof=timefedof)
CALL h5%DEALLOCATE()

CALL OK(v%IsInitiated(), "Import: IsInitiated")
CALL OK(v%spaceCompo .EQ. 2 .AND. v%timeCompo .EQ. 2, &
        "Import: components")
CALL OK(v%SIZE() .EQ. 24, "Import: Size")
CALL OK(v%lis_ptr .NE. 0 .AND. v%lis_ptr .NE. u%lis_ptr, &
        "Import: own LIS vector")
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                   tsize=tsize)
CALL v%GetMultiple(istart=1, iend=24, stride=1, VALUE=found2, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - found2) .LT. tol), "Import: values")
CALL v%Get(VALUE=found2, tsize=tsize, spaceCompo=2, timeCompo=1)
CALL OK(ALL(ABS(found2(1:6) - [(210.0_DFP + ii, ii=1, 6)]) .LT. tol), &
        "Import: Get component")

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
