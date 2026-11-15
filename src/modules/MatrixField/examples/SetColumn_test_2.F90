!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set and get columns by physical variable and components
!
! The matrix field is initiated from the vector field v (two space
! components, 12 x 12). The column of (node, component) is
! (component - 1) * 6 + node. Node 1 is connected to nodes 1, 4, 5, 6,
! so its columns have nonzeros in the rows 1, 4, 5, 6, 7, 10, 11, 12.
!
! When a component or node argument is a vector, SetColumn writes every
! selected column. GetColumn with several columns should be called with
! addContribution = .TRUE.; then the selected columns are summed into VALUE.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatrixField_Class, ONLY: MatrixField_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: v
TYPE(MatrixField_) :: obj
REAL(DFP) :: col(12), vec(12), want(12)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")
CALL obj%Initiate(obj2=v)
CALL obj%Set(VALUE=1.0_DFP)
vec = [(REAL(ii, DFP), ii=1, 12)]

! Test 1: column by ivar and idof
CALL obj%SetColumn(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
                vecVal=vec)
CALL obj%GetColumn(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
                VALUE=col)
CALL OK(ALL(ABS(col - vec) .LT. tol), "column by ivar, idof")
CALL obj%GetColumn(globalNode=5, islocal=math%yes, idof=2, VALUE=col)
CALL OK(ALL(ABS(col - vec) .LT. tol), "column by idof")

! Test 2: column by space and time component
CALL obj%SetColumn(globalNode=1, islocal=math%yes, ivar=1, spacecompo=2, &
                timecompo=1, scalarVal=3.0_DFP)
CALL obj%GetColumn(globalNode=1, islocal=math%yes, ivar=1, spacecompo=2, &
                timecompo=1, VALUE=col)
want = 0.0_DFP
want([1, 4, 5, 6, 7, 10, 11, 12]) = 3.0_DFP
CALL OK(ALL(ABS(col - want) .LT. tol), "column by components")

! Test 3: timecompo(:) selects the same column here (one time component)
CALL obj%SetColumn(globalNode=1, islocal=math%yes, ivar=1, spacecompo=1, &
                timecompo=[1], scalarVal=4.0_DFP)
col = 0.0_DFP
CALL obj%GetColumn(globalNode=1, islocal=math%yes, ivar=1, spacecompo=1, &
                timecompo=[1], VALUE=col, addContribution=math%yes)
want = 0.0_DFP
want([1, 4, 5, 6, 7, 10, 11, 12]) = 4.0_DFP
CALL OK(ALL(ABS(col - want) .LT. tol), "timecompo(:)")

! Test 4: spacecompo(:) selects both columns of node 6
CALL obj%SetColumn(globalNode=6, islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, scalarVal=2.0_DFP)
col = 0.0_DFP
CALL obj%GetColumn(globalNode=6, islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, VALUE=col, &
                addContribution=math%yes)
CALL OK(ALL(ABS(col - 4.0_DFP) .LT. tol), "spacecompo(:): sum of columns")

! Test 5: globalNode(:) with spacecompo(:)
CALL obj%SetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
col = 0.0_DFP
CALL obj%GetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, VALUE=col, &
                addContribution=math%yes)
CALL OK(ALL(ABS(col - 4.0_DFP) .LT. tol), "globalNode(:): sum of columns")

! Test 6: globalNode(:) with timecompo(:)
CALL obj%SetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=2, timecompo=[1], vecVal=vec)
col = 0.0_DFP
CALL obj%GetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=2, timecompo=[1], VALUE=col, &
                addContribution=math%yes)
CALL OK(ALL(ABS(col - 2.0_DFP * vec) .LT. tol), "timecompo(:), nodes")

CALL obj%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
