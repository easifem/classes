!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Export a vector field to HDF5 and import it back
!
! Export writes the data of the node field and spaceCompo and idofs in
! the group of an HDF5 file. Import reads them back into another field;
! the FEDOFs are given as arguments.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
  TYPE(VectorField_) :: u, v
  TYPE(HDF5File_) :: h5
  REAL(DFP) :: raw(12), want(12)
  INTEGER(I4B) :: tsize, ii

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
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
  CALL v%Import(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
  CALL h5%DEALLOCATE()

  CALL OK(v%IsInitiated(), "Import: IsInitiated")
  CALL OK(v%spaceCompo .EQ. 2, "Import: spaceCompo")
  CALL OK(ALL(v%idofs .EQ. [1, 2]), "Import: idofs")
  CALL OK(v%Size() .EQ. 12, "Import: Size")
  CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ALL(ABS(raw - want) .LT. tol), "Import: values")

  CALL v%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
