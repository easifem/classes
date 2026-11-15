!> author: Vikas Sharma, Ph. D.
! date: 2026-11-10
! summary: Export a block matrix field to HDF5 and import it back
!
! A block matrix field initiated from the vector field v is exported to
! the group "/K" of an HDF5 file (Export of MatrixField_). Import reads
! it into another block matrix field with the same FEDOF. Both have the
! same name, engine, shape, sparsity and values. The imported DOF gives
! the same entry for a node pair.
!
!- Test 1 passes one fedof, which is used for every physical variable.
!- Test 2 passes fedofs, one FEDOF pointer per physical variable.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockMatrixField_Class, ONLY: BlockMatrixField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockMatrixField_test.toml"
CHARACTER(*), PARAMETER :: filename = "./Import_test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: v
TYPE(BlockMatrixField_) :: obj, obj2, obj3
TYPE(FEDOFPointer_) :: fedofs(1)
TYPE(HDF5File_) :: h5
REAL(DFP) :: val1, val2
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")
CALL obj%Initiate(obj2=v)
DO ii = 1, SIZE(obj%mat%A)
  obj%mat%A(ii) = REAL(ii, DFP)
END DO

CALL h5%Initiate(filename=filename, mode="NEW")
CALL h5%OPEN()
CALL obj%Export(hdf5=h5, group="/K")
CALL h5%DEALLOCATE()

! Test 1: one fedof for all physical variables
CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()
CALL obj2%Import(hdf5=h5, group="/K", rowFedof=fedof, &
                  rowGeoFedof=geofedof)
CALL h5%DEALLOCATE()

CALL OK(obj2%IsInitiated(), "Import: IsInitiated")
CALL OK(obj2%GetName() .EQ. "v", "Import: name")
CALL OK(obj2%GetEngineName() .EQ. "NATIVE_SERIAL", "Import: engine")
CALL OK(ALL(obj2%SHAPE() .EQ. [12, 12]), "Import: Shape")
CALL OK(obj2%mat%csr%nnz .EQ. 112, "Import: nnz")
CALL OK(ALL(obj2%mat%csr%JA .EQ. obj%mat%csr%JA), "Import: sparsity")
CALL OK(ALL(ABS(obj2%mat%A - obj%mat%A) .LT. tol), "Import: values")
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, jdof=1, &
             VALUE=val1)
CALL obj2%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, jdof=1, &
              VALUE=val2)
CALL OK(ABS(val1 - val2) .LT. tol, "Import: dof, Get(5, 6)")
CALL OK(SIZE(obj2%rowFedofs) .EQ. 1, "Import: one fedof per variable")
CALL OK(ASSOCIATED(obj2%rowFedofs(1)%ptr, fedof), "Import: rowFedofs(1)")
CALL OK(ASSOCIATED(obj2%colFedofs(1)%ptr, fedof), "Import: colFedofs(1)")

! Test 2: one fedof pointer per physical variable
fedofs(1)%ptr => fedof
CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()
CALL obj3%Import(hdf5=h5, group="/K", rowFedofs=fedofs)
CALL h5%DEALLOCATE()

CALL OK(obj3%IsInitiated(), "Import fedofs: IsInitiated")
CALL OK(ALL(ABS(obj3%mat%A - obj%mat%A) .LT. tol), "Import fedofs: values")
CALL OK(SIZE(obj3%rowFedofs) .EQ. 1, "Import fedofs: SIZE(rowFedofs)")
CALL OK(ASSOCIATED(obj3%rowFedofs(1)%ptr, fedof), "Import fedofs: row")
CALL OK(ASSOCIATED(obj3%colFedofs(1)%ptr, fedof), "Import fedofs: col")

NULLIFY (fedofs(1)%ptr)
CALL obj3%DEALLOCATE()
CALL obj2%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
