!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Add the body source given by a space-time vector field
!
! u is order 1 Lagrange in space and time on square_3x2.h5, with two
! components and timeCompo = 2. The body source
!
!   g(x, y, t) = [1 + x + 2 t, x y t]
!
! is exact in the space of u (Q1 in space, linear in time) and in the
! space of f (order 2 in space). ApplyBodySource(bodySource=field)
! interpolates the field at the quadrature points of u, so it must give
! the same values as ApplyBodySource(bodySource=g) with the user
! function g (reference r):
!
!- s: the field on the FEDOF and TimeFEDOF of u (same FEDOF),
!- f: the field on its own FEDOF (order 2) and TimeFEDOF.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceVectorSubroutine
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ApplyBodySource_test_3.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
REAL(DFP), PARAMETER :: times(2) = [0.5_DFP, 1.5_DFP]
REAL(DFP), PARAMETER :: scale = 2.0_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof, fedofB, geofedofB
TYPE(TimeFEDOF_), TARGET :: timefedof, timefedofB
TYPE(STVectorField_) :: u, r, s, f
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
REAL(DFP) :: found(24), want(24)
INTEGER(I4B) :: usize, rsize

CALL e%SetQuietMode(math%yes)
CALL func%Initiate(NAME="g", returnType=TypeFEVariableOpt%vector, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=2)
procptr => vectorFunc
CALL func%SetVectorFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL f%ImportFromToml(tomlName="f", filename=tomlFileName, &
                      fedof=fedofB, geofedof=geofedofB, &
                      timefedof=timefedofB, dom=dom)
CALL r%Initiate(NAME="r", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)
CALL s%Initiate(NAME="s", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof, timefedof=timefedof)

! reference: body source given by the user function g
CALL r%Set(VALUE=0.0_DFP)
CALL r%ApplyBodySource(bodySource=func, scale=scale, times=times)
CALL r%GetMultiple(istart=1, iend=24, stride=1, VALUE=want, tsize=rsize)

CALL s%SetByFunction(func=func, times=times)
CALL f%SetByFunction(func=func, times=times)

! Test 1: body source on the same FEDOF and TimeFEDOF as u
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=s, scale=scale, times=times)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=usize)
  CALL OK(usize .EQ. 24 .AND. rsize .EQ. 24, "same FEDOF: tsize")
  CALL OK(ALL(ABS(found - want) .LT. tol), "same FEDOF: u = r")
END BLOCK

! Test 2: the source is added to the values of u
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=s, scale=scale, times=times)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=usize)
  CALL OK(ALL(ABS(found - (1.0_DFP + want)) .LT. tol), &
          "same FEDOF: added to u")
END BLOCK

! Test 3: body source on another FEDOF (order 2) and TimeFEDOF
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=f, scale=scale, times=times)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=usize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "other FEDOF: u = r")
END BLOCK

CALL f%DEALLOCATE()
CALL s%DEALLOCATE()
CALL r%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedofB%DEALLOCATE()
CALL fedofB%DEALLOCATE()
CALL geofedofB%DEALLOCATE()
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
  ans(1) = 1.0_DFP + args(1) + 2.0_DFP * args(4)
  ans(2) = args(1) * args(2) * args(4)
END SUBROUTINE vectorFunc

END PROGRAM main
