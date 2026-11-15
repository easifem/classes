!> author: Vikas Sharma, Ph. D.
! date: 2026-11-13
! summary: Import a LIS scalar field from HDF5
!
! A LIS field u and a native field n are exported to an HDF5 file. Both
! groups are read back into LIS fields (v from u, w from n). Import
! creates a new LIS vector and copies the values read from the file
! into it. The engine of the imported field is always LIS_OMP.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: u, v, w
TYPE(ScalarField_) :: n
TYPE(HDF5File_) :: h5
REAL(DFP) :: want(6), found(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL u%Set(VALUE=want)
CALL n%Set(VALUE=-want)

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL u%Export(hdf5=h5, group="/u")
CALL n%Export(hdf5=h5, group="/n")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()
CALL v%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
CALL w%IMPORT(hdf5=h5, group="/n", fedof=fedof, geofedof=geofedof)
CALL h5%DEALLOCATE()

! Test 1: field exported by a LIS field
CALL OK(v%IsInitiated(), "v: IsInitiated")
CALL OK(v%SIZE() .EQ. 6, "v: Size")
CALL OK(v%GetEngineName() .EQ. "LIS_OMP", "v: engine")

! Test 2: values of v are the values of u
BLOCK
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "v: values of u")
END BLOCK

! Test 3: v has its own LIS vector
BLOCK
  CALL v%Set(VALUE=0.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
END BLOCK

! Test 4: field exported by a native field
BLOCK
  CALL OK(w%IsInitiated(), "w: IsInitiated")
  CALL OK(w%SIZE() .EQ. 6, "w: Size")
  CALL OK(w%GetEngineName() .EQ. "LIS_OMP", "w: engine")
  CALL w%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + want) .LT. tol), "w: values of n")
END BLOCK

CALL w%DEALLOCATE()
CALL v%DEALLOCATE()
CALL n%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
