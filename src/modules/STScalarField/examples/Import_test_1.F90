!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Import a space-time scalar field from an HDF5 file
!
! Import reads a field written by Export: the node field data,
! timeCompo and idofs; the FEDOFs and the TimeFEDOF are given as
! arguments. u and z (12 values, 6 space DOFs, timeCompo = 2) are
! exported in two groups of one file. y is imported from the group of
! u, then imported again from the group of z: the second import
! replaces the data of the first one.

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
CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u, y, z
TYPE(HDF5File_) :: h5
REAL(DFP) :: src(12), found(12)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL z%Initiate(NAME="z", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)

src = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)
CALL z%SetMultiple(istart=1, iend=12, stride=1, VALUE=-src)

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="/u")
CALL z%Export(hdf5=h5, group="/z")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()

! Test 1: timeCompo, idofs and size of the imported field
BLOCK
  CALL y%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof, &
                timefedof=timefedof)
  CALL OK(y%IsInitiated(), "Import: IsInitiated")
  CALL OK(y%timeCompo .EQ. 2, "Import: timeCompo")
  CALL OK(ALL(y%idofs .EQ. [1, 2]), "Import: idofs")
  CALL OK(y%SIZE() .EQ. 12, "Import: Size")
END BLOCK

! Test 2: values, and Get by node (value (I, a) has index (a-1)*6 + I)
BLOCK
  CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")
  CALL y%Get(VALUE=found, tsize=tsize, globalNode=3)
  CALL OK(tsize .EQ. 2 .AND. &
          ALL(ABS(found(1:2) - [3.0_DFP, 9.0_DFP]) .LT. tol), &
          "Import: Get node")
END BLOCK

! Test 3: import into an initiated field, from another group
BLOCK
  CALL y%IMPORT(hdf5=h5, group="/z", fedof=fedof, geofedof=geofedof, &
                timefedof=timefedof)
  CALL OK(y%timeCompo .EQ. 2 .AND. y%SIZE() .EQ. 12, &
          "Import again: timeCompo, Size")
  CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found + src) .LT. tol), "Import again: values")
END BLOCK

CALL h5%DEALLOCATE()

CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
