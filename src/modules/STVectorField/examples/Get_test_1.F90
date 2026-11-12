!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Get values of a space-time vector field in a vector
!
! Get(VALUE(:), tsize, globalNode, spaceCompo, timeCompo) returns,
! depending on the optional arguments that are present:
!
!- node, space and time: one value (tsize = 1),
!- node: all values of the node, in the order
!  (s, t) = (1, 1), (2, 1), (1, 2), (2, 2) (tsize = 4),
!- node and space: the time components of (node, s) (tsize = 2),
!- node and time: the space components of (node, t) (tsize = 2),
!- space and time: component (s, t) of all nodes (tsize = 6),
!- space: all time components of s, time 1 first (tsize = 12),
!- time: all space components of t, space 1 first (tsize = 12).
!
! The test values are f(s, t, i) = 100 s + 10 t + i.

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

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
REAL(DFP) :: found(24), want(12)
INTEGER(I4B) :: tsize, ii, s, t

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

DO t = 1, 2
  DO s = 1, 2
    CALL u%Set(VALUE=[(f(s, t, ii), ii=1, 6)], spaceCompo=s, timeCompo=t)
  END DO
END DO

! Test 1: node, space and time
CALL u%Get(VALUE=found, tsize=tsize, globalNode=3, spaceCompo=2, &
           timeCompo=1)
CALL OK(tsize .EQ. 1 .AND. ABS(found(1) - f(2, 1, 3)) .LT. tol, &
        "node, space, time")

! Test 2: node
CALL u%Get(VALUE=found, tsize=tsize, globalNode=3)
CALL OK(tsize .EQ. 4, "node: tsize")
CALL OK(ALL(ABS(found(1:4) - [f(1, 1, 3), f(2, 1, 3), f(1, 2, 3), &
                              f(2, 2, 3)]) .LT. tol), "node: values")

! Test 3: node and space
CALL u%Get(VALUE=found, tsize=tsize, globalNode=3, spaceCompo=2)
CALL OK(tsize .EQ. 2, "node, space: tsize")
CALL OK(ALL(ABS(found(1:2) - [f(2, 1, 3), f(2, 2, 3)]) .LT. tol), &
        "node, space: values")

! Test 4: node and time
CALL u%Get(VALUE=found, tsize=tsize, globalNode=3, timeCompo=1)
CALL OK(tsize .EQ. 2, "node, time: tsize")
CALL OK(ALL(ABS(found(1:2) - [f(1, 1, 3), f(2, 1, 3)]) .LT. tol), &
        "node, time: values")

! Test 5: space and time
CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
CALL OK(tsize .EQ. 6, "space, time: tsize")
CALL OK(ALL(ABS(found(1:6) - [(f(1, 2, ii), ii=1, 6)]) .LT. tol), &
        "space, time: values")

! Test 6: space
CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
CALL OK(tsize .EQ. 12, "space: tsize")
want = [[(f(1, 1, ii), ii=1, 6)], [(f(1, 2, ii), ii=1, 6)]]
CALL OK(ALL(ABS(found(1:12) - want) .LT. tol), "space: values")

! Test 7: time
CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
CALL OK(tsize .EQ. 12, "time: tsize")
want = [[(f(1, 2, ii), ii=1, 6)], [(f(2, 2, ii), ii=1, 6)]]
CALL OK(ALL(ABS(found(1:12) - want) .LT. tol), "time: values")

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

PURE FUNCTION f(sc, tc, nd) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: sc, tc, nd
  REAL(DFP) :: ans
  ans = 100.0_DFP * sc + 10.0_DFP * tc + nd
END FUNCTION f

END PROGRAM main
