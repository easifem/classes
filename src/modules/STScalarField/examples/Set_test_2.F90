!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set one time component and selected nodes
!
!- Set(VALUE, timeCompo) sets time component timeCompo of all nodes to
!  one value,
!- Set(VALUE(:), timeCompo) sets it from a vector (one value for each
!  space DOF),
!- Set(VALUE(:), globalNode(:), islocal, timeCompo) sets it at the
!  selected nodes,
!- Set(VALUE(:), globalNode(:), islocal) sets all time components of
!  the selected nodes, VALUE(a) for time component a.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u
REAL(DFP) :: found(12), want(6)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

want = [(REAL(ii, DFP), ii=1, 6)]
CALL u%Set(VALUE=0.0_DFP)

! Test 1: one time component to one value
BLOCK
  CALL u%Set(VALUE=4.0_DFP, timeCompo=1)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 4.0_DFP) .LT. tol), "timeCompo = 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6)) .LT. tol), "time 2 unchanged")
END BLOCK

! Test 2: one time component from a vector
BLOCK
  CALL u%Set(VALUE=want, timeCompo=2)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "timeCompo = 2: vector")

  CALL u%Set(VALUE=want, timeCompo=2, scale=-1.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6)) .LT. tol), "timeCompo = 2: add -1 * vector")
END BLOCK

! Test 3: one time component at selected nodes
BLOCK
  CALL u%Set(VALUE=[10.0_DFP, 20.0_DFP], globalNode=[1, 3], &
             islocal=math%yes, timeCompo=1)
  CALL u%Get(VALUE=found, globalNode=[1, 3], tsize=tsize, &
             islocal=math%yes, timeCompo=1)
  CALL OK(ALL(ABS(found(1:2) - [10.0_DFP, 20.0_DFP]) .LT. tol), &
          "selected nodes, timeCompo")
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
  CALL OK(ALL(ABS(found(1:2) - [4.0_DFP, 0.0_DFP]) .LT. tol), &
          "other nodes unchanged")
END BLOCK

! Test 4: all time components at selected nodes
BLOCK
  CALL u%Set(VALUE=[-1.0_DFP, -2.0_DFP], globalNode=[5, 6], &
             islocal=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=5)
  CALL OK(ALL(ABS(found(1:2) - [-1.0_DFP, -2.0_DFP]) .LT. tol), &
          "selected nodes: node 5")
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=6)
  CALL OK(ALL(ABS(found(1:2) - [-1.0_DFP, -2.0_DFP]) .LT. tol), &
          "selected nodes: node 6")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
