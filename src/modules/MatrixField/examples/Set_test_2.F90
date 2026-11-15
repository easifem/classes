!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set and assemble element matrices
!
! The matrix field is initiated from the scalar field u (6 x 6). The
! mesh has two elements with nodes [1, 5, 6, 4] and [5, 2, 3, 6].
!
!- Set(globalNode, islocal, VALUE, storageFMT) writes an element matrix
!  to obj(globalNode, globalNode); Get(globalNode, islocal, VALUE, nrow,
!  ncol, storageFMT) reads it back.
!- With addContribution = .TRUE. the element matrices are assembled:
!  entries shared by both elements get both contributions.

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
INTEGER(I4B), PARAMETER :: elem1(4) = [1, 5, 6, 4]
INTEGER(I4B), PARAMETER :: elem2(4) = [5, 2, 3, 6]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj
REAL(DFP) :: ke(4, 4), found(4, 4), val
INTEGER(I4B) :: ii, jj, nrow, ncol, fmt

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)
fmt = TypeFieldOpt%storageFormatDOF

! Test 1: set an element matrix and read it back
DO jj = 1, 4
  DO ii = 1, 4
    ke(ii, jj) = REAL(10 * ii + jj, DFP)
  END DO
END DO
CALL obj%Set(VALUE=0.0_DFP)
CALL obj%Set(globalNode=elem1, islocal=math%yes, VALUE=ke, &
             storageFMT=fmt)
CALL obj%Get(globalNode=elem1, islocal=math%yes, VALUE=found, &
             nrow=nrow, ncol=ncol, storageFMT=fmt)
CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "Get: nrow, ncol")
CALL OK(ALL(ABS(found - ke) .LT. tol), "Set element matrix")
CALL obj%Get(iNodeNum=6, jNodeNum=4, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 34.0_DFP) .LT. tol, "Set element matrix: (6,4)")

! Test 2: assemble ke1 = 1 and 2 * ke2 with ke2 = 1
CALL obj%Set(VALUE=0.0_DFP)
ke = 1.0_DFP
CALL obj%Set(globalNode=elem1, islocal=math%yes, VALUE=ke, &
             storageFMT=fmt, addContribution=math%yes)
CALL obj%Set(globalNode=elem2, islocal=math%yes, VALUE=ke, &
             storageFMT=fmt, scale=2.0_DFP, addContribution=math%yes)
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 3.0_DFP) .LT. tol, "assemble: shared (5,6) = 3")
CALL obj%Get(iNodeNum=1, jNodeNum=4, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP) .LT. tol, "assemble: (1,4) = 1")
CALL obj%Get(iNodeNum=2, jNodeNum=3, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 2.0_DFP) .LT. tol, "assemble: (2,3) = 2")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
