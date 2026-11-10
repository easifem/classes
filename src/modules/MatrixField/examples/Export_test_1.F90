!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Export a matrix field to HDF5 and import it back
!
! Export writes the field header and the CSR matrix to the group "/K" of
! an HDF5 file. Import reads them into another matrix field that uses
! the same FEDOF. Both matrices then have the same shape, sparsity and
! values, and the imported DOF gives the same entry for a node pair.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
CHARACTER(*), PARAMETER :: filename = "./Export_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj, obj2
TYPE(HDF5File_) :: h5
REAL(DFP) :: val1, val2
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)
DO ii = 1, SIZE(obj%mat%A)
  obj%mat%A(ii) = REAL(ii, DFP)
END DO

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL obj%Export(hdf5=h5, group="/K")
CALL h5%DEALLOCATE()

CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()
CALL obj2%Import(hdf5=h5, group="/K", fedof=fedof, geofedof=geofedof)
CALL h5%DEALLOCATE()

CALL OK(obj2%IsInitiated(), "Import: IsInitiated")
CALL OK(ALL(obj2%SHAPE() .EQ. [6, 6]), "Import: Shape")
CALL OK(obj2%mat%csr%nnz .EQ. 28, "Import: nnz")
CALL OK(ALL(obj2%mat%csr%JA .EQ. obj%mat%csr%JA), "Import: sparsity")
CALL OK(ALL(ABS(obj2%mat%A - obj%mat%A) .LT. tol), "Import: values")
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, jdof=1, &
             VALUE=val1)
CALL obj2%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, jdof=1, &
              VALUE=val2)
CALL OK(ABS(val1 - val2) .LT. tol, "Import: dof, Get(5, 6)")

CALL obj2%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
