!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set and get a column of a matrix field built on a scalar field
!
! The matrix field is initiated from the scalar field u (6 x 6). Node 1
! is connected to nodes 1, 4, 5, 6; node 5 to all nodes. Only the
! entries in the sparsity pattern are written; the other entries of a
! returned column are zero.
!
!- SetColumn(globalNode, islocal, idof, scalarVal | vecVal | nodeFieldVal)
!- GetColumn(globalNode, islocal, idof, VALUE | nodeFieldVal, scale,
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
REAL(DFP) :: col(6), vec(6), found(6)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)
CALL obj%Set(VALUE=1.0_DFP)
vec = [(REAL(ii, DFP), ii=1, 6)]

! Test 1: set a column to a scalar
CALL obj%SetColumn(globalNode=1, islocal=math%yes, idof=1, &
                   scalarVal=2.0_DFP)
CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, VALUE=col)
CALL OK(ALL(ABS(col - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
                       2.0_DFP]) .LT. tol), "SetColumn scalarVal")

! Test 2: set a column from a vector
CALL obj%SetColumn(globalNode=5, islocal=math%yes, idof=1, vecVal=vec)
CALL obj%GetColumn(globalNode=5, islocal=math%yes, idof=1, VALUE=col)
CALL OK(ALL(ABS(col - vec) .LT. tol), "SetColumn vecVal: full column")
CALL obj%SetColumn(globalNode=2, islocal=math%yes, idof=1, vecVal=vec)
CALL obj%GetColumn(globalNode=2, islocal=math%yes, idof=1, VALUE=col)
CALL OK(ALL(ABS(col - [0.0_DFP, 2.0_DFP, 3.0_DFP, 0.0_DFP, 5.0_DFP, &
                       6.0_DFP]) .LT. tol), "SetColumn vecVal: sparsity")

! Test 3: add scale * column to VALUE
col = 1.0_DFP
CALL obj%GetColumn(globalNode=5, islocal=math%yes, idof=1, VALUE=col, &
                scale=2.0_DFP, addContribution=math%yes)
CALL OK(ALL(ABS(col - (1.0_DFP + 2.0_DFP * vec)) .LT. tol), &
        "GetColumn: add 2 * column")

! Test 4: set a column from a node field
DO ii = 1, 6
  CALL u%Set(globalNode=ii, islocal=math%yes, VALUE=vec(ii))
END DO
CALL obj%SetColumn(globalNode=6, islocal=math%yes, idof=1, nodeFieldVal=u)
CALL obj%GetColumn(globalNode=6, islocal=math%yes, idof=1, VALUE=col)
CALL OK(ALL(ABS(col - vec) .LT. tol), "SetColumn nodeFieldVal")

! Test 5: get a column into a node field
CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, nodeFieldVal=u)
DO ii = 1, 6
  CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
END DO
CALL OK(ALL(ABS(found - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
                         2.0_DFP]) .LT. tol), "GetColumn nodeFieldVal")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
