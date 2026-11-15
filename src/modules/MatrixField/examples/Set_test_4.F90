!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set or add another matrix field
!
! obj is initiated from the scalar field u and obj2 is a copy of obj,
! so both have the same sparsity.
!
!- Set(VALUE=obj2, scale, addContribution=.TRUE.) gives
!  obj = obj + scale * obj2.
!- Set(VALUE=obj2) gives obj = obj2.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj, obj2

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)
CALL obj2%Initiate(obj2=obj)

CALL obj%Set(VALUE=1.0_DFP)
CALL obj2%Set(VALUE=2.0_DFP)

! Test 1: obj = obj + 3 * obj2
CALL obj%Set(VALUE=obj2, scale=3.0_DFP, addContribution=math%yes)
CALL OK(ALL(ABS(obj%mat%A - 7.0_DFP) .LT. tol), "add 3 * obj2")

! Test 2: obj = obj2
CALL obj%Set(VALUE=obj2)
CALL OK(ALL(ABS(obj%mat%A - 2.0_DFP) .LT. tol), "set obj2")

CALL obj2%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
