!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set a scalar field by a user function
!
! The field u (order 1 Lagrange on square_3x2.h5) is set by the user
! function f(x, y) = 1 + 2 x + 3 y. Since f is linear, the nodal
! values of u are equal to f at the nodes of the mesh. For order 1
! Lagrange the DOF number is the local node number.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "./toml/SetByFunction_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
CLASS(AbstractMesh_), POINTER :: mesh
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP) :: found, want
INTEGER(I4B) :: ii, nrow, ncol
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL func%Initiate(NAME="func", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=1)
procptr => linearFunc
CALL func%SetScalarFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

CALL u%Set(VALUE=0.0_DFP)
CALL u%SetByFunction(func=func)

mesh => dom%GetMeshPointer()
ALLOCATE (xij(3, mesh%GetTotalNodes()))
CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)

isok = .TRUE.
DO ii = 1, ncol
  CALL u%Get(VALUE=found, globalNode=ii, islocal=math%yes)
  CALL linearFunc(xij(:, ii), 3, want)
  isok = isok .AND. ABS(found - want) .LT. tol
END DO
CALL OK(ncol .EQ. 6, "number of nodes")
CALL OK(isok, "u = f at the nodes")

NULLIFY (mesh)
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE linearFunc(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = 1.0_DFP + 2.0_DFP * args(1) + 3.0_DFP * args(2)
END SUBROUTINE linearFunc

END PROGRAM main
