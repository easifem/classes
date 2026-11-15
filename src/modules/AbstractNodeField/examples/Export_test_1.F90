!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Export a node field to HDF5 and import it back
!
! Export writes the data of obj (AbstractField_ data, DOF data and the
! values) in the group of an HDF5 file. Import reads them back into
! another field; the FEDOFs are given as arguments.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
  TYPE(ScalarField_) :: x, y
  TYPE(HDF5File_) :: h5
  REAL(DFP) :: src(6), found(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

  CALL h5%Initiate(filename=filename, mode="NEW")
  CALL h5%OPEN()
  CALL x%Export(hdf5=h5, group="/x")
  CALL h5%DEALLOCATE()

  CALL h5%Initiate(filename=filename, mode="READ")
  CALL h5%OPEN()
  CALL y%Import(hdf5=h5, group="/x", fedof=fedof, geofedof=geofedof)
  CALL h5%DEALLOCATE()

  CALL OK(y%IsInitiated(), "Import: IsInitiated")
  CALL OK(y%Size() .EQ. 6, "Import: Size")
  CALL OK(y%GetTotalPhysicalVars() .EQ. 1, "Import: physical vars")
  CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")

  CALL y%DEALLOCATE()
  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
