!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Export a LIS vector field to HDF5 and import it
!
! Import reads the field written by Export (of VectorFieldLis_ or of
! a native VectorField_), creates the LIS vector and copies the
! values into it. u is a VectorFieldLis_ and n a native
! VectorField_, both with 12 values (6 nodes, spaceCompo = 2).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u, w
TYPE(VectorField_) :: n
TYPE(HDF5File_) :: h5
REAL(DFP) :: found(12), want(12)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof)

want = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="/u")
CALL n%Export(hdf5=h5, group="/n")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()

! Test 1: import a field exported by a VectorFieldLis_
BLOCK
  CALL w%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
  CALL OK(w%IsInitiated(), "Import LIS: IsInitiated")
  CALL OK(w%spaceCompo .EQ. 2, "Import LIS: spaceCompo")
  CALL OK(w%SIZE() .EQ. 12, "Import LIS: Size")
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "Import LIS: values")
END BLOCK

! Test 2: import again into w (old LIS vector is destroyed), the file
! comes from a native VectorField_
BLOCK
  CALL w%IMPORT(hdf5=h5, group="/n", fedof=fedof, geofedof=geofedof)
  CALL OK(w%SIZE() .EQ. 12, "Import native: Size")
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found + want) .LT. tol), "Import native: values")
END BLOCK

CALL h5%DEALLOCATE()

CALL w%DEALLOCATE()
CALL n%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
