!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Initiate a matrix field from a vector field and by copying
!
! The vector field v (two space components on square_3x2.h5) is imported
! from MatrixField_test.toml.
!
! The matrix field obj is initiated from v which gives a 12 x 12 matrix
! with 28 x 4 = 112 nonzero entries.
!
! A second matrix field obj2 is then initiated by copying obj; it gets
! the same structure and the same values.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE MatrixField_Class, ONLY: MatrixField_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: v
TYPE(MatrixField_) :: obj, obj2
REAL(DFP) :: val

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")

! Test 1: initiate from a vector field
CALL obj%Initiate(obj2=v)
CALL OK(obj%IsInitiated(), "from vector field: IsInitiated")
CALL OK(ALL(obj%SHAPE() .EQ. [12, 12]), "from vector field: Shape")
CALL OK(obj%mat%csr%nnz .EQ. 112, "from vector field: nnz = 112")

! Test 2: initiate by copying another matrix field
CALL obj%Set(VALUE=5.0_DFP)
CALL obj2%Initiate(obj2=obj)
CALL OK(obj2%IsInitiated(), "copy: IsInitiated")
CALL OK(ALL(obj2%SHAPE() .EQ. [12, 12]), "copy: Shape")
CALL OK(obj2%mat%csr%nnz .EQ. 112, "copy: nnz")
CALL obj2%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, &
              jdof=1, VALUE=val)
CALL OK(ABS(val - 5.0_DFP) .LT. tol, "copy: values")

CALL obj2%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
