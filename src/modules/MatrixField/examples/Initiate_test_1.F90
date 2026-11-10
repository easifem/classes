!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Initiate a matrix field from a scalar field
!
! The scalar field u (order 1 Lagrange on square_3x2.h5, six DOFs) is
! imported from MatrixField_test.toml.
!
! The matrix field obj is then initiated from u.
!
! The result is a 6 x 6 unsymmetric CSR matrix whose
! sparsity comes from the FEDOF of u: the corner nodes 1, 2, 3, 4 touch
! four nodes, the middle nodes 5 and 6 touch six, so nnz = 28.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

CALL obj%Initiate(obj2=u)

CALL OK(obj%IsInitiated(), "IsInitiated")
CALL OK(obj%SIZE(1) .EQ. 6, "Size(1) = 6")
CALL OK(obj%SIZE(2) .EQ. 6, "Size(2) = 6")
CALL OK(obj%SIZE() .EQ. 36, "Size() = 36")
CALL OK(ALL(obj%SHAPE() .EQ. [6, 6]), "Shape = [6, 6]")
CALL OK(obj%mat%csr%nnz .EQ. 28, "nnz = 28")
CALL OK(TRIM(obj%mat%matrixProp) .EQ. "UNSYM", "matrixProp = UNSYM")
CALL OK(obj%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
        "GetStorageFMT")
CALL OK(.NOT. obj%isRectangle, "not rectangle")
CALL OK(.NOT. obj%IsSubmatInitiated(), "IsSubmatInitiated is false")

CALL obj%DEALLOCATE()
CALL OK(.NOT. obj%IsInitiated(), "DEALLOCATE")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
