!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Add the point Neumann boundary conditions of a space-time
!   vector field
!
! The field u (two space components, order 1 Lagrange in time on
! [0, 1], timeCompo = 2) has two point Neumann BCs, constant in time
! (see ApplyPointNeumannBC_test_1.toml): f_x = 2 at node 1 and f_y = 4
! at node 3. The load of time component a is integral(T_a(t) f(t) dt):
! the nodal values f_b are multiplied by the time mass matrix
! ct(a, b) = integral(T_a T_b dt) = [1/3, 1/6; 1/6, 1/3]. For a
! constant f each time component gets f / 2.
!
! ApplyPointNeumannBC(scale, times) adds scale times this load to the
! space component idof of the BC; all other entries are unchanged.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "ApplyPointNeumannBC_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
REAL(DFP) :: found(24), want(6)
INTEGER(I4B) :: tsize, itime

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: number of point Neumann BCs
CALL OK(u%GetTotalPointNBC() .EQ. 2, "GetTotalPointNBC")

! Test 2: scale = 1
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyPointNeumannBC(scale=1.0_DFP, times=times)
  DO itime = 1, 2
    want = [1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
    CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=itime)
    CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
            "scale = 1: x, time "//CHAR(48 + itime))
    want = [0.0_DFP, 0.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
    CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=itime)
    CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
            "scale = 1: y, time "//CHAR(48 + itime))
  END DO
END BLOCK

! Test 3: added to the current values, scale = 0.5
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyPointNeumannBC(scale=0.5_DFP, times=times)
  want = [1.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "add with scale = 0.5: x")
  want = [1.0_DFP, 1.0_DFP, 2.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "add with scale = 0.5: y")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
