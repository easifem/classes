!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Export a space-time vector field to HDF5 and import it back
!
! u (two space components, timeCompo = 2) is set to
! f(s, t, i) = 100 s + 10 t + i and exported to the group "u" of
! Export_test_1.h5. v is imported from the same group with the same
! fedof, geofedof and timefedof; it must have the same spaceCompo,
! timeCompo, size and values as u.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
CHARACTER(*), PARAMETER :: h5FileName = "Export_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u, v
TYPE(HDF5File_) :: h5
REAL(DFP) :: found(24), found2(24)
INTEGER(I4B) :: tsize, ii, s, t

CALL e%SetQuietMode(math%yes)
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

! Export
CALL h5%Initiate(h5FileName, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="u")
CALL h5%DEALLOCATE()
CALL OK(.TRUE., "Export")

! Import
CALL h5%Initiate(h5FileName, mode="READ")
CALL h5%OPEN()
CALL v%IMPORT(hdf5=h5, group="u", fedof=fedof, geofedof=geofedof, &
              timefedof=timefedof)
CALL h5%DEALLOCATE()

CALL OK(v%IsInitiated(), "Import: IsInitiated")
CALL OK(v%spaceCompo .EQ. 2, "Import: spaceCompo")
CALL OK(v%timeCompo .EQ. 2, "Import: timeCompo")
CALL OK(v%SIZE() .EQ. 24, "Import: Size")
CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                   tsize=tsize)
CALL v%GetMultiple(istart=1, iend=24, stride=1, VALUE=found2, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - found2) .LT. tol), "Import: values")
CALL v%Get(VALUE=found2, tsize=tsize, spaceCompo=2, timeCompo=1)
CALL OK(ALL(ABS(found2(1:6) - found(7:12)) .LT. tol), &
        "Import: Get component")

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
