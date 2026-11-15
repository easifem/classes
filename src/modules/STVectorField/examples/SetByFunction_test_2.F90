!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Set a space-time vector field of order 2 by a user function
!
! The field u is order 2 Lagrange in space (15 DOFs on square_3x2.h5)
! with two components and order 1 Lagrange in time (timeCompo = 2).
! SetByFunction interpolates f element by element, so all DOFs are set,
! not only the mesh nodes. f(x, y, t) = [x**2 + y t, x y - t**2] is
! quadratic in space.
!
! For each time a, the space values of u at time a must be equal to the
! values of a VectorField_ v on the same FEDOF set by
! v%SetByFunction(func, times=[times(a)]).

PROGRAM main
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
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "SetByFunction_test_2.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
REAL(DFP), PARAMETER :: times(2) = [0.5_DFP, 2.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
TYPE(VectorField_) :: v
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
REAL(DFP) :: found(64), want(64)
INTEGER(I4B) :: itime, ispace, usize, vsize
LOGICAL(LGT) :: isok

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
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

! Test 1: with times, time a is f(., times(a)) at all space DOFs
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=func, times=times)
  isok = .TRUE.
  DO itime = 1, 2
    CALL v%Set(VALUE=0.0_DFP)
    CALL v%SetByFunction(func=func, times=times(itime:itime))
    DO ispace = 1, 2
      CALL u%Get(VALUE=found, tsize=usize, spaceCompo=ispace, &
                 timeCompo=itime)
      CALL v%Get(VALUE=want, tsize=vsize, spaceCompo=ispace)
      isok = isok .AND. usize .EQ. 15 .AND. vsize .EQ. 15
      isok = isok .AND. ALL(ABS(found(1:15) - want(1:15)) .LT. tol)
    END DO
  END DO
  CALL OK(isok, "with times: all DOFs of order 2")
END BLOCK

! Test 2: without times, f(., 0) at all time components
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=func)
  CALL v%Set(VALUE=0.0_DFP)
  CALL v%SetByFunction(func=func)
  isok = .TRUE.
  DO itime = 1, 2
    DO ispace = 1, 2
      CALL u%Get(VALUE=found, tsize=usize, spaceCompo=ispace, &
                 timeCompo=itime)
      CALL v%Get(VALUE=want, tsize=vsize, spaceCompo=ispace)
      isok = isok .AND. ALL(ABS(found(1:15) - want(1:15)) .LT. tol)
    END DO
  END DO
  CALL OK(isok, "without times: f(., 0) at all times")
END BLOCK

CALL v%DEALLOCATE()
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
  ans(1) = args(1)**2 + args(2) * args(4)
  ans(2) = args(1) * args(2) - args(4)**2
END SUBROUTINE vectorFunc

END PROGRAM main
