!> author: Vikas Sharma, Ph. D.
! date: 2026-11-10
! summary: Initiate a block matrix field from a vector field
!
! The vector field v (two space components on square_3x2.h5) is imported
! from BlockMatrixField_test.toml. The block matrix field obj is
! initiated from v by the Initiate method of MatrixField_. This gives a
! 12 x 12 matrix with 112 nonzero entries. The Set and Get methods of
! MatrixField_ work on obj.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockMatrixField_Class, ONLY: BlockMatrixField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockMatrixField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: v
TYPE(BlockMatrixField_) :: obj
REAL(DFP) :: val

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")

! Test 1: initiate from a vector field
CALL obj%Initiate(obj2=v)
CALL OK(obj%IsInitiated(), "IsInitiated")
CALL OK(ALL(obj%SHAPE() .EQ. [12, 12]), "Shape")
CALL OK(obj%mat%csr%nnz .EQ. 112, "nnz = 112")
CALL OK(obj%GetName() .EQ. "v", "name of v")

! Test 2: set and get through the methods of MatrixField_
CALL obj%Set(VALUE=1.0_DFP)
CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, jdof=1, &
             VALUE=7.0_DFP)
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=2, jdof=1, &
             VALUE=val)
CALL OK(ABS(val - 7.0_DFP) .LT. tol, "Set/Get (5,6), idof=2, jdof=1")

! Test 3: deallocate
CALL obj%DEALLOCATE()
CALL OK(.NOT. obj%IsInitiated(), "DEALLOCATE")

CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
