!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set all entries, a block of nodes and a single entry
!
! The matrix field is initiated from the scalar field u (6 x 6).
!
!- Set(VALUE) sets every nonzero entry.
!- Set(VALUE, globalNode, islocal) sets the entries obj(globalNode,
!  globalNode).
!- Set(iNodeNum, jNodeNum, islocal, idof, jdof, VALUE) sets one entry.
!
! With addContribution = .TRUE. scale * VALUE is added instead.
! The entries are read back with Get(iNodeNum, jNodeNum, islocal, idof,
! jdof, VALUE).

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

! Test 1: set all entries
CALL obj%Set(VALUE=1.0_DFP)
CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set all")

! Test 2: add to all entries
CALL obj%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
CALL obj%Get(iNodeNum=6, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set all: add 2 * 1")

! Test 3: set a single entry
CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=7.0_DFP)
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 7.0_DFP) .LT. tol, "Set single")
CALL obj%Get(iNodeNum=6, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set single: (6,5) unchanged")

! Test 4: add to a single entry
CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=1.0_DFP, scale=3.0_DFP, &
             addContribution=math%yes)
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: add 3 * 1")

! Test 5: set the block obj([1,5], [1,5])
CALL obj%Set(VALUE=0.0_DFP, globalNode=[1, 5], islocal=math%yes)
CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val) .LT. tol, "Set block: (1,5) = 0")
CALL obj%Get(iNodeNum=1, jNodeNum=4, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set block: (1,4) unchanged")

! Test 6: add to the block obj([1,5], [1,5])
CALL obj%Set(VALUE=2.0_DFP, globalNode=[1, 5], islocal=math%yes, &
             scale=0.5_DFP, addContribution=math%yes)
CALL obj%Get(iNodeNum=5, jNodeNum=5, islocal=math%yes, idof=1, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set block: add 0.5 * 2")

CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
