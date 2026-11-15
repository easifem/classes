!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Import a scalar field from an HDF5 file
!
! Import reads a field written by Export; the FEDOFs are given as
! arguments. x and z are exported in two groups of one file. y is
! imported from the group of x, then imported again from the group of
! z: the second import replaces the data of the first one.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: x, y, z
TYPE(HDF5File_) :: h5
REAL(DFP) :: src(6), found(6), areal
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL z%Initiate(NAME="z", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

src = [(REAL(ii, DFP), ii=1, 6)]
CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
CALL z%SetMultiple(istart=1, iend=6, stride=1, VALUE=10.0_DFP * src)

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL x%Export(hdf5=h5, group="/x")
CALL z%Export(hdf5=h5, group="/z")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()

! Test 1: data and values of the imported field
BLOCK
  CALL y%IMPORT(hdf5=h5, group="/x", fedof=fedof, geofedof=geofedof)
  CALL OK(y%IsInitiated(), "Import: IsInitiated")
  CALL OK(y%SIZE() .EQ. 6, "Import: Size")
  CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")
END BLOCK

! Test 2: Get by node on the imported field
BLOCK
  CALL y%Get(VALUE=areal, globalNode=3, islocal=math%yes)
  CALL OK(ABS(areal - 3.0_DFP) .LT. tol, "Import: Get node")
END BLOCK

! Test 3: import into an initiated field, from another group
BLOCK
  CALL y%IMPORT(hdf5=h5, group="/z", fedof=fedof, geofedof=geofedof)
  CALL OK(y%SIZE() .EQ. 6, "Import again: Size")
  CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 10.0_DFP * src) .LT. tol), &
          "Import again: values")
END BLOCK

CALL h5%DEALLOCATE()

CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
