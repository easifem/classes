!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Set and Get by physical variable, DOF and component
!
! The matrix field is initiated from the vector field v (two space
! components, 12 x 12). The matrix uses DOF storage: the row of
! (node, component) is (component - 1) * 6 + node. A block of nodes
! [1, 5] for one physical variable is therefore ordered
! (1, c1), (5, c1), (1, c2), (5, c2).
!
! Every Set call is checked with the Get overload that takes the same
! filter.

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
INTEGER(I4B), PARAMETER :: nodes(2) = [1, 5]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: v
TYPE(MatrixField_) :: obj
REAL(DFP) :: val, a(4, 4), found(4, 4), b(2, 2), found2(2, 2)
INTEGER(I4B) :: ii, jj, nrow, ncol

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL v%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="v")
CALL obj%Initiate(obj2=v)
CALL obj%Set(VALUE=0.0_DFP)

! Test 1: single entry by ivar, jvar, idof, jdof
CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, jvar=1, &
             idof=1, jdof=2, VALUE=4.0_DFP)
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, jvar=1, &
             idof=1, jdof=2, VALUE=val)
CALL OK(ABS(val - 4.0_DFP) .LT. tol, "single by ivar, idof")
CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, jvar=1, &
             idof=1, jdof=2, VALUE=1.0_DFP, scale=2.0_DFP, &
             addContribution=math%yes)
CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
             jdof=2, VALUE=val)
CALL OK(ABS(val - 6.0_DFP) .LT. tol, "single by ivar, idof: add")

! Test 2: single entry by space and time components
CALL obj%Set(iNodeNum=1, jNodeNum=4, islocal=math%yes, ivar=1, jvar=1, &
             ispacecompo=2, itimecompo=1, jspacecompo=1, jtimecompo=1, &
             VALUE=5.0_DFP)
CALL obj%Get(iNodeNum=1, jNodeNum=4, islocal=math%yes, ivar=1, jvar=1, &
             ispacecompo=2, itimecompo=1, jspacecompo=1, jtimecompo=1, &
             VALUE=val)
CALL OK(ABS(val - 5.0_DFP) .LT. tol, "single by components")
CALL obj%Get(iNodeNum=1, jNodeNum=4, islocal=math%yes, idof=2, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 5.0_DFP) .LT. tol, "single by components: idof=2")

! Test 3: block of one physical variable, value(4, 4)
DO jj = 1, 4
  DO ii = 1, 4
    a(ii, jj) = REAL(10 * ii + jj, DFP)
  END DO
END DO
CALL obj%Set(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, VALUE=a)
CALL obj%Get(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, VALUE=found, nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "block by ivar: nrow, ncol")
CALL OK(ALL(ABS(found - a) .LT. tol), "block by ivar")
CALL obj%Get(iNodeNum=5, jNodeNum=1, islocal=math%yes, idof=2, &
             jdof=1, VALUE=val)
CALL OK(ABS(val - 41.0_DFP) .LT. tol, "block by ivar: DOF order")

! Test 4: block of one DOF pair, value(2, 2)
b = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
CALL obj%Set(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, idof=2, jdof=2, VALUE=b)
CALL obj%Get(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, idof=2, jdof=2, VALUE=found2, nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "block by idof: nrow, ncol")
CALL OK(ALL(ABS(found2 - b) .LT. tol), "block by idof")

! Test 5: one scalar to the block of one component pair
CALL obj%Set(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, ispacecompo=1, itimecompo=1, jspacecompo=1, &
             jtimecompo=1, VALUE=9.0_DFP)
CALL obj%Get(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, ispacecompo=1, itimecompo=1, jspacecompo=1, &
             jtimecompo=1, VALUE=found2, nrow=nrow, ncol=ncol)
CALL OK(ALL(ABS(found2 - 9.0_DFP) .LT. tol), "block by components")
CALL obj%Get(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, idof=2, jdof=2, VALUE=found2, nrow=nrow, ncol=ncol)
CALL OK(ALL(ABS(found2 - b) .LT. tol), &
        "block by components: other block unchanged")

! Test 6: time components given as a vector
CALL obj%Set(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, ispacecompo=2, itimecompo=[1], jspacecompo=2, &
             jtimecompo=[1], VALUE=6.0_DFP)
CALL obj%Get(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, idof=2, jdof=2, VALUE=found2, nrow=nrow, ncol=ncol)
CALL OK(ALL(ABS(found2 - 6.0_DFP) .LT. tol), "timecompo(:)")

! Test 7: space components given as a vector
CALL obj%Set(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, ispacecompo=[1, 2], itimecompo=1, &
             jspacecompo=[1, 2], jtimecompo=1, VALUE=8.0_DFP)
CALL obj%Get(iNodeNum=nodes, jNodeNum=nodes, islocal=math%yes, ivar=1, &
             jvar=1, VALUE=found, nrow=nrow, ncol=ncol)
CALL OK(ALL(ABS(found - 8.0_DFP) .LT. tol), "spacecompo(:)")

CALL obj%DEALLOCATE()
CALL v%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
