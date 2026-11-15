!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Matrix vector multiplication with arrays and node fields
!
! A unit element matrix is assembled on both elements of square_3x2.h5,
! so A(i,j) is the number of elements that share nodes i and j. Then 10
! is added to A(1,5) to make A unsymmetric. With x = 1:
!
!- y = A x       = [14, 4, 4, 4, 8, 8]
!- y = A^T x     = [4, 4, 4, 4, 18, 8]
!- y = y + 2 A x when addContribution = .TRUE. and scale = 2
!
! Matvec(x, y) works with REAL(DFP) arrays and with node fields.

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
REAL(DFP), PARAMETER :: ax(6) = [14.0_DFP, 4.0_DFP, 4.0_DFP, 4.0_DFP, &
                                 8.0_DFP, 8.0_DFP]
REAL(DFP), PARAMETER :: atx(6) = [4.0_DFP, 4.0_DFP, 4.0_DFP, 4.0_DFP, &
                                  18.0_DFP, 8.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u, w
TYPE(MatrixField_) :: obj
REAL(DFP) :: x(6), y(6), ke(4, 4), found(6)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)
CALL obj%Initiate(obj2=u)

CALL obj%Set(VALUE=0.0_DFP)
ke = 1.0_DFP
CALL obj%Set(globalNode=[1, 5, 6, 4], islocal=math%yes, VALUE=ke, &
             storageFMT=TypeFieldOpt%storageFormatDOF, &
             addContribution=math%yes)
CALL obj%Set(globalNode=[5, 2, 3, 6], islocal=math%yes, VALUE=ke, &
             storageFMT=TypeFieldOpt%storageFormatDOF, &
             addContribution=math%yes)
CALL obj%Set(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, jdof=1, &
             VALUE=10.0_DFP, addContribution=math%yes)
x = 1.0_DFP

! Test 1: y = A x
CALL obj%Matvec(x=x, y=y)
CALL OK(ALL(ABS(y - ax) .LT. tol), "y = A x")

! Test 2: y = A^T x
CALL obj%Matvec(x=x, y=y, isTranspose=math%yes)
CALL OK(ALL(ABS(y - atx) .LT. tol), "y = A^T x")

! Test 3: y = y + 2 A x
y = 1.0_DFP
CALL obj%Matvec(x=x, y=y, addContribution=math%yes, scale=2.0_DFP)
CALL OK(ALL(ABS(y - (1.0_DFP + 2.0_DFP * ax)) .LT. tol), "y = y + 2 A x")

! Test 4: node fields, w = A u
CALL u%Set(VALUE=1.0_DFP)
CALL obj%Matvec(x=u, y=w)
DO ii = 1, 6
  CALL w%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
END DO
CALL OK(ALL(ABS(found - ax) .LT. tol), "node fields: w = A u")

CALL obj%DEALLOCATE()
CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
