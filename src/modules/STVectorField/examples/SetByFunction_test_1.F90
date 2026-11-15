!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a space-time vector field by a user function
!
! The field u (order 1 Lagrange in space and time, two space
! components, timeCompo = 2) is set by the vector function
! f(x, y, t) = [1 + x + 2 y + 3 t, x - y + t].
!
!- SetByFunction(func, times) sets time component a of node I to
!  f(x_I, times(a)), SIZE(times) = timeCompo,
!- SetByFunction(func) evaluates f at t = 0 and sets the same value at
!  all time components.
!
! For order 1 Lagrange the DOF number is the node number.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceVectorSubroutine
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
CLASS(AbstractMesh_), POINTER :: mesh
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP) :: mat(2, 2), want(2), xt(4)
INTEGER(I4B) :: ii, itime, nrow, ncol, tsize
LOGICAL(LGT) :: isok1, isok2

CALL e%SetQuietMode(math%yes)
CALL func%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=2)
procptr => vectorFunc
CALL func%SetVectorFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

mesh => dom%GetMeshPointer()
ALLOCATE (xij(3, mesh%GetTotalNodes()))
CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)

! Test 1: with times
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=func, times=times)
  isok1 = .TRUE.
  DO ii = 1, ncol
    CALL u%Get(VALUE=mat, nrow=nrow, ncol=tsize, globalNode=ii, &
               islocal=math%yes)
    DO itime = 1, 2
      xt = [xij(1, ii), xij(2, ii), 0.0_DFP, times(itime)]
      CALL vectorFunc(xt, 4, want, tsize)
      isok1 = isok1 .AND. ALL(ABS(mat(:, itime) - want) .LT. tol)
    END DO
  END DO
  CALL OK(isok1, "with times: u = f(x, times(a))")
END BLOCK

! Test 2: without times, f at t = 0 for all time components
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=func)
  isok1 = .TRUE.
  isok2 = .TRUE.
  DO ii = 1, ncol
    CALL u%Get(VALUE=mat, nrow=nrow, ncol=tsize, globalNode=ii, &
               islocal=math%yes)
    xt = [xij(1, ii), xij(2, ii), 0.0_DFP, 0.0_DFP]
    CALL vectorFunc(xt, 4, want, tsize)
    isok1 = isok1 .AND. ALL(ABS(mat(:, 1) - want) .LT. tol)
    isok2 = isok2 .AND. ALL(ABS(mat(:, 2) - want) .LT. tol)
  END DO
  CALL OK(isok1, "without times: time 1 = f(x, 0)")
  CALL OK(isok2, "without times: time 2 = f(x, 0)")
END BLOCK

NULLIFY (mesh)
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
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
  ans(1) = 1.0_DFP + args(1) + 2.0_DFP * args(2) + 3.0_DFP * args(4)
  ans(2) = args(1) - args(2) + args(4)
END SUBROUTINE vectorFunc

END PROGRAM main
