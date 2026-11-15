!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Export a space-time scalar field to HDF5 and import it back
!
! Export writes the node field data (AbstractNodeField_ Export) and
! timeCompo in a group of an HDF5 file. Import reads them back into
! another field; the FEDOFs and the TimeFEDOF are given as arguments.
! The imported field must have the same timeCompo, size and values,
! and Get by node must work on it.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u, y
TYPE(HDF5File_) :: h5
REAL(DFP) :: src(12), found(12)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

src = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="/u")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()
CALL y%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof, &
              timefedof=timefedof)
CALL h5%DEALLOCATE()

! Test 1: data of the field
CALL OK(y%IsInitiated(), "Import: IsInitiated")
CALL OK(y%timeCompo .EQ. 2, "Import: timeCompo")
CALL OK(y%SIZE() .EQ. 12, "Import: Size")

! Test 2: values
BLOCK
  CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")
END BLOCK

! Test 3: Get by node on the imported field
BLOCK
  found = 0.0_DFP
  CALL y%Get(VALUE=found, tsize=tsize, globalNode=3)
  CALL OK(tsize .EQ. 2, "Import: Get node, tsize")
  CALL OK(ALL(ABS(found(1:2) - [3.0_DFP, 9.0_DFP]) .LT. tol), &
          "Import: Get node, values")
END BLOCK

CALL y%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
