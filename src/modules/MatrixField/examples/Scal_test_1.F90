!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Scale all entries of a matrix field
!
! Scal(a) multiplies every nonzero entry by a: obj = a * obj.

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
TYPE(MatrixField_) :: obj
REAL(DFP) :: val

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL obj%Initiate(obj2=u)

CALL obj%Set(VALUE=2.0_DFP)
CALL obj%Scal(a=3.0_DFP)
CALL OK(ALL(ABS(obj%mat%A - 6.0_DFP) .LT. tol), "Scal: all entries")
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 6.0_DFP) .LT. tol, "Scal: (5,6) = 3 * 2")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
