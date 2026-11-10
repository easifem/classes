!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Get the diagonal and scale a matrix field by a diagonal
!
! A unit element matrix is assembled on both elements of square_3x2.h5,
! so A(i,j) is the number of elements that share nodes i and j and the
! diagonal is d = [1, 1, 1, 1, 2, 2].
!
!- GetDiagonal(diag) returns d.
!- DiagonalScaling(side="BOTH") uses d and the default operator "SQRT":
!  A(i,j) = A(i,j) / SQRT(|d(i) d(j)|).
!- DiagonalScaling(side="LEFT", diag, OPERATOR="NONE") uses the given
!  diagonal: A(i,j) = A(i,j) / diag(i).

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
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj
REAL(DFP), ALLOCATABLE :: diag(:)
REAL(DFP) :: val
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)

! Test 1: GetDiagonal
CALL Assemble(obj)
CALL obj%GetDiagonal(diag=diag)
CALL OK(SIZE(diag) .EQ. 6, "GetDiagonal: size")
CALL OK(ALL(ABS(diag - [1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 2.0_DFP, &
                        2.0_DFP]) .LT. tol), "GetDiagonal: values")

! Test 2: symmetric scaling by the own diagonal
CALL obj%DiagonalScaling(side="BOTH")
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP) .LT. tol, "BOTH: (5,6) = 2 / 2")
CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP / SQRT(2.0_DFP)) .LT. tol, &
        "BOTH: (1,5) = 1 / sqrt(2)")

! Test 3: left scaling by a given diagonal
CALL Assemble(obj)
diag = [(REAL(ii, DFP), ii=1, 6)]
CALL obj%DiagonalScaling(side="LEFT", diag=diag, OPERATOR="NONE")
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 2.0_DFP / 5.0_DFP) .LT. tol, "LEFT: (5,6) = 2 / 5")
CALL obj%Get(iNodeNum=6, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 2.0_DFP / 6.0_DFP) .LT. tol, "LEFT: (6,5) = 2 / 6")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE Assemble(mat)
  TYPE(MatrixField_), INTENT(INOUT) :: mat
  REAL(DFP) :: ke(4, 4)
  ke = 1.0_DFP
  CALL mat%Set(VALUE=0.0_DFP)
  CALL mat%Set(globalNode=[1, 5, 6, 4], islocal=math%yes, VALUE=ke, &
               storageFMT=TypeFieldOpt%storageFormatDOF, &
               addContribution=math%yes)
  CALL mat%Set(globalNode=[5, 2, 3, 6], islocal=math%yes, VALUE=ke, &
               storageFMT=TypeFieldOpt%storageFormatDOF, &
               addContribution=math%yes)
END SUBROUTINE Assemble

END PROGRAM main
