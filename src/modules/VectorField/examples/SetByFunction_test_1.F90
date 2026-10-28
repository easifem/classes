!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set a vector field by user functions
!
! The field u (order 1 Lagrange, two components) is set by the vector
! function f(x, y) = [1 + 2 x + 3 y, 4 - x + y]. Then component 2 is set
! by the scalar function g(x, y) = x + y with spaceCompo = 2. Since the
! functions are linear, the nodal values are equal to the functions at
! the nodes. For order 1 Lagrange the DOF number is the node number.

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: TypeFEVariableOpt
  USE BaseType, ONLY: InterfaceScalarSubroutine
  USE BaseType, ONLY: InterfaceVectorSubroutine
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE Test_Method, ONLY: OK
  USE UserFunction_Class, ONLY: UserFunction_
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(VectorField_) :: u
  TYPE(UserFunction_) :: vfunc, sfunc
  PROCEDURE(InterfaceVectorSubroutine), POINTER :: vptr
  PROCEDURE(InterfaceScalarSubroutine), POINTER :: sptr
  CLASS(AbstractMesh_), POINTER :: mesh
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP) :: found(6), want(2), g
  INTEGER(I4B) :: ii, nrow, ncol, tsize
  LOGICAL(LGT) :: isok1, isok2

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL vfunc%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
                      argType=TypeFEVariableOpt%space, numArgs=4, &
                      numReturns=2)
  vptr => vectorFunc
  CALL vfunc%SetVectorFunctionPointer(func=vptr)

  CALL sfunc%Initiate(NAME="g", returnType=TypeFEVariableOpt%scalar, &
                      argType=TypeFEVariableOpt%space, numArgs=4, &
                      numReturns=1)
  sptr => scalarFunc
  CALL sfunc%SetScalarFunctionPointer(func=sptr)

  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  mesh => dom%GetMeshPointer()
  ALLOCATE(xij(3, mesh%GetTotalNodes()))
  CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)

  ! vector function
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=vfunc)
  isok1 = .TRUE.
  DO ii = 1, ncol
    CALL u%Get(VALUE=found, tsize=tsize, globalNode=ii)
    CALL vectorFunc(xij(:, ii), 3, want, tsize)
    isok1 = isok1 .AND. ALL(ABS(found(1:2) - want) .LT. 1.0E-8_DFP)
  END DO
  CALL OK(isok1, "vector function: u = f at the nodes")

  ! scalar function for component 2
  CALL u%SetByFunction(func=sfunc, spaceCompo=2)
  isok1 = .TRUE.
  isok2 = .TRUE.
  DO ii = 1, ncol
    CALL u%Get(VALUE=found, tsize=tsize, globalNode=ii)
    CALL vectorFunc(xij(:, ii), 3, want, tsize)
    CALL scalarFunc(xij(:, ii), 3, g)
    isok1 = isok1 .AND. ABS(found(1) - want(1)) .LT. 1.0E-8_DFP
    isok2 = isok2 .AND. ABS(found(2) - g) .LT. 1.0E-8_DFP
  END DO
  CALL OK(isok1, "scalar function: component 1 unchanged")
  CALL OK(isok2, "scalar function: component 2 = g")

  NULLIFY(mesh)
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()

CONTAINS

  SUBROUTINE vectorFunc(args, nargs, ans, tsize)
    REAL(DFP), INTENT(IN) :: args(:)
    INTEGER(I4B), INTENT(IN) :: nargs
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    tsize = 2
    ans(1) = 1.0_DFP + 2.0_DFP * args(1) + 3.0_DFP * args(2)
    ans(2) = 4.0_DFP - args(1) + args(2)
  END SUBROUTINE vectorFunc

  SUBROUTINE scalarFunc(args, nargs, ans)
    REAL(DFP), INTENT(IN) :: args(:)
    INTEGER(I4B), INTENT(IN) :: nargs
    REAL(DFP), INTENT(INOUT) :: ans
    ans = args(1) + args(2)
  END SUBROUTINE scalarFunc

END PROGRAM main
