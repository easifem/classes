!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set and get a row of a matrix field built on a scalar field
!
! The matrix field is initiated from the scalar field u (6 x 6). Node 1
! is connected to nodes 1, 4, 5, 6; node 5 to all nodes. Only the
! entries in the sparsity pattern are written; the other entries of a
! returned row are zero.
!
!- SetRow(globalNode, islocal, idof, scalarVal | vecVal | nodeFieldVal)
!- GetRow(globalNode, islocal, idof, VALUE | nodeFieldVal, scale,
!  addContribution)

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj
REAL(DFP) :: row(6), vec(6), found(6)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)
CALL obj%Set(VALUE=1.0_DFP)
vec = [(REAL(ii, DFP), ii=1, 6)]

! Test 1: set a row to a scalar
CALL obj%SetRow(globalNode=1, islocal=math%yes, idof=1, scalarVal=2.0_DFP)
CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, VALUE=row)
CALL OK(ALL(ABS(row - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
                       2.0_DFP]) .LT. tol), "SetRow scalarVal")

! Test 2: set a row from a vector
CALL obj%SetRow(globalNode=5, islocal=math%yes, idof=1, vecVal=vec)
CALL obj%GetRow(globalNode=5, islocal=math%yes, idof=1, VALUE=row)
CALL OK(ALL(ABS(row - vec) .LT. tol), "SetRow vecVal: full row")
CALL obj%SetRow(globalNode=2, islocal=math%yes, idof=1, vecVal=vec)
CALL obj%GetRow(globalNode=2, islocal=math%yes, idof=1, VALUE=row)
CALL OK(ALL(ABS(row - [0.0_DFP, 2.0_DFP, 3.0_DFP, 0.0_DFP, 5.0_DFP, &
                       6.0_DFP]) .LT. tol), "SetRow vecVal: sparsity")

! Test 3: add scale * row to VALUE
row = 1.0_DFP
CALL obj%GetRow(globalNode=5, islocal=math%yes, idof=1, VALUE=row, &
                scale=2.0_DFP, addContribution=math%yes)
CALL OK(ALL(ABS(row - (1.0_DFP + 2.0_DFP * vec)) .LT. tol), &
        "GetRow: add 2 * row")

! Test 4: set a row from a node field
DO ii = 1, 6
  CALL u%Set(globalNode=ii, islocal=math%yes, VALUE=vec(ii))
END DO
CALL obj%SetRow(globalNode=6, islocal=math%yes, idof=1, nodeFieldVal=u)
CALL obj%GetRow(globalNode=6, islocal=math%yes, idof=1, VALUE=row)
CALL OK(ALL(ABS(row - vec) .LT. tol), "SetRow nodeFieldVal")

! Test 5: get a row into a node field
CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, nodeFieldVal=u)
DO ii = 1, 6
  CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
END DO
CALL OK(ALL(ABS(found - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
                         2.0_DFP]) .LT. tol), "GetRow nodeFieldVal")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
