!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Export a LIS vector field to HDF5
!
! Export gathers the values of the LIS vector into the Fortran vector
! and then writes the field (node field data, spaceCompo and values) in
! a group of an HDF5 file. The file is read back into a native
! VectorField_ (Import of VectorFieldLis_ is not yet available), whose
! values must be the values of u.

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
CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u
TYPE(VectorField_) :: y
TYPE(HDF5File_) :: h5
REAL(DFP) :: want(12), found(12)
INTEGER(I4B) :: tsize, ii

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

want = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="/u")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()
CALL y%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
CALL h5%DEALLOCATE()

! Test 1: the exported data is read back
CALL OK(y%IsInitiated(), "Import: IsInitiated")
CALL OK(y%spaceCompo .EQ. 2, "Import: spaceCompo")
CALL OK(y%SIZE() .EQ. 12, "Import: Size")

! Test 2: the exported values are the values of the LIS vector
BLOCK
  CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "Export: values")
END BLOCK

CALL y%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
