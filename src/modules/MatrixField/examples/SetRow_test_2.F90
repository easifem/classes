!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set and get rows by physical variable and components
!
! The matrix field is initiated from the vector field v (two space
! components, 12 x 12). The row of (node, component) is
! (component - 1) * 6 + node. Node 1 is connected to nodes 1, 4, 5, 6,
! so its rows have nonzeros in the columns 1, 4, 5, 6, 7, 10, 11, 12.
!
! When a component or node argument is a vector, SetRow writes every
! selected row. GetRow with several rows should be called with
! addContribution = .TRUE.; then the selected rows are summed into VALUE.

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
REAL(DFP) :: row(12), vec(12), want(12)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")
CALL obj%Initiate(obj2=v)
CALL obj%Set(VALUE=1.0_DFP)
vec = [(REAL(ii, DFP), ii=1, 12)]

! Test 1: row by ivar and idof
CALL obj%SetRow(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
                vecVal=vec)
CALL obj%GetRow(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
                VALUE=row)
CALL OK(ALL(ABS(row - vec) .LT. tol), "row by ivar, idof")
CALL obj%GetRow(globalNode=5, islocal=math%yes, idof=2, VALUE=row)
CALL OK(ALL(ABS(row - vec) .LT. tol), "row by idof")

! Test 2: row by space and time component
CALL obj%SetRow(globalNode=1, islocal=math%yes, ivar=1, spacecompo=2, &
                timecompo=1, scalarVal=3.0_DFP)
CALL obj%GetRow(globalNode=1, islocal=math%yes, ivar=1, spacecompo=2, &
                timecompo=1, VALUE=row)
want = 0.0_DFP
want([1, 4, 5, 6, 7, 10, 11, 12]) = 3.0_DFP
CALL OK(ALL(ABS(row - want) .LT. tol), "row by components")

! Test 3: timecompo(:) selects the same row here (one time component)
CALL obj%SetRow(globalNode=1, islocal=math%yes, ivar=1, spacecompo=1, &
                timecompo=[1], scalarVal=4.0_DFP)
row = 0.0_DFP
CALL obj%GetRow(globalNode=1, islocal=math%yes, ivar=1, spacecompo=1, &
                timecompo=[1], VALUE=row, addContribution=math%yes)
want = 0.0_DFP
want([1, 4, 5, 6, 7, 10, 11, 12]) = 4.0_DFP
CALL OK(ALL(ABS(row - want) .LT. tol), "timecompo(:)")

! Test 4: spacecompo(:) selects both rows of node 6
CALL obj%SetRow(globalNode=6, islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, scalarVal=2.0_DFP)
row = 0.0_DFP
CALL obj%GetRow(globalNode=6, islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, VALUE=row, &
                addContribution=math%yes)
CALL OK(ALL(ABS(row - 4.0_DFP) .LT. tol), "spacecompo(:): sum of rows")

! Test 5: globalNode(:) with spacecompo(:)
CALL obj%SetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
row = 0.0_DFP
CALL obj%GetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=[1, 2], timecompo=1, VALUE=row, &
                addContribution=math%yes)
CALL OK(ALL(ABS(row - 4.0_DFP) .LT. tol), "globalNode(:): sum of rows")

! Test 6: globalNode(:) with timecompo(:)
CALL obj%SetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=2, timecompo=[1], vecVal=vec)
row = 0.0_DFP
CALL obj%GetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
                spacecompo=2, timecompo=[1], VALUE=row, &
                addContribution=math%yes)
CALL OK(ALL(ABS(row - 2.0_DFP * vec) .LT. tol), "timecompo(:), nodes")

CALL obj%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
