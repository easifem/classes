!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Apply Dirichlet boundary conditions to a matrix field
!
! A unit element matrix is assembled on both elements of square_3x2.h5.
! Nodes 1 and 2 are Dirichlet nodes, dbcPtrs = [1, 2].
!
!- ApplyDirichletBC(dbcPtrs) stores the columns dbcPtrs of A in a
!  submatrix, then sets the rows and columns dbcPtrs of A to zero and
!  their diagonal entries to one.
!- ApplyDirichletBCToRHS(x, y, scale, addContribution) computes
!  y = y + scale * submat * x, the RHS correction -A(:, dbc) u_dbc.
!- ApplyDirichletBC() without dbcPtrs repeats both steps for the stored
!  dbcPtrs, after A is assembled again.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: rhs(6) = [1.0_DFP, 2.0_DFP, 2.0_DFP, 1.0_DFP, &
                                  3.0_DFP, 3.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: x, y
TYPE(MatrixField_) :: obj
REAL(DFP) :: val, found(6)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL y%Initiate(NAME="y", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)
CALL obj%Initiate(obj2=x)
DO ii = 1, 6
  CALL x%Set(globalNode=ii, islocal=math%yes, VALUE=REAL(ii, DFP))
END DO

! Test 1: apply DBC to the matrix
CALL Assemble(obj, 1.0_DFP)
CALL obj%ApplyDirichletBC(dbcPtrs=[1, 2])
CALL OK(obj%IsSubmatInitiated(), "IsSubmatInitiated")
CALL OK(obj%tdbcPtrs .EQ. 2, "tdbcPtrs = 2")
CALL obj%Get(iNodeNum=1, jNodeNum=1, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP) .LT. tol, "diagonal (1,1) = 1")
CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val) .LT. tol, "row 1 is zero: (1,5) = 0")
CALL obj%Get(iNodeNum=5, jNodeNum=2, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val) .LT. tol, "column 2 is zero: (5,2) = 0")
CALL obj%Get(iNodeNum=5, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 2.0_DFP) .LT. tol, "other entries kept: (5,5) = 2")

! Test 2: RHS correction y = 0 - submat * x
CALL y%Set(VALUE=0.0_DFP)
CALL obj%ApplyDirichletBCToRHS(x=x, y=y, scale=-1.0_DFP, &
                               addContribution=math%yes)
DO ii = 1, 6
  CALL y%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
END DO
CALL OK(ALL(ABS(found + rhs) .LT. tol), "ApplyDirichletBCToRHS")

! Test 3: assemble 2 A again and apply the stored DBC
CALL Assemble(obj, 2.0_DFP)
CALL obj%ApplyDirichletBC()
CALL obj%Get(iNodeNum=5, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 4.0_DFP) .LT. tol, "stored DBC: (5,5) = 4")
CALL obj%Get(iNodeNum=2, jNodeNum=2, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP) .LT. tol, "stored DBC: (2,2) = 1")
CALL obj%ApplyDirichletBCToRHS(x=x, y=y)
DO ii = 1, 6
  CALL y%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
END DO
CALL OK(ALL(ABS(found - 2.0_DFP * rhs) .LT. tol), &
        "stored DBC: submat updated")

CALL obj%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE Assemble(mat, s)
  TYPE(MatrixField_), INTENT(INOUT) :: mat
  REAL(DFP), INTENT(IN) :: s
  REAL(DFP) :: ke(4, 4)
  ke = 1.0_DFP
  CALL mat%Set(VALUE=0.0_DFP)
  CALL mat%Set(globalNode=[1, 5, 6, 4], islocal=math%yes, VALUE=ke, &
               storageFMT=TypeFieldOpt%storageFormatDOF, scale=s, &
               addContribution=math%yes)
  CALL mat%Set(globalNode=[5, 2, 3, 6], islocal=math%yes, VALUE=ke, &
               storageFMT=TypeFieldOpt%storageFormatDOF, scale=s, &
               addContribution=math%yes)
END SUBROUTINE Assemble

END PROGRAM main
