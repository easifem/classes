!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set space-time components of all nodes
!
!- Set(VALUE, spaceCompo, timeCompo) sets component (s, t) of all nodes
!  to one value,
!- Set(VALUE(:), spaceCompo, timeCompo) sets component (s, t) of all
!  nodes from a vector, SIZE(VALUE) = tNodes,
!- Set(VALUE(:, :), spaceCompo(:), timeCompo, storageFMT) sets several
!  space components at one time component,
!- Set(VALUE(:, :), spaceCompo, timeCompo(:), storageFMT) sets several
!  time components of one space component.
!
! With storageFMT = NODES_FMT the rows of VALUE are the components and
! the columns the nodes; with DOF_FMT it is the transpose.
! Get(VALUE, tsize, spaceCompo, timeCompo) returns component (s, t) of
! all nodes.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: NODES_FMT
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
REAL(DFP) :: found(24), a(6), b(6), m(2, 6)
INTEGER(I4B) :: tsize, ii

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

a = [(REAL(ii, DFP), ii=1, 6)]
b = 10.0_DFP * a

! Test 1: one component to one value
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=4.0_DFP, spaceCompo=2, timeCompo=1)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(tsize .EQ. 6, "Get component: tsize")
  CALL OK(ALL(ABS(found(1:6) - 4.0_DFP) .LT. tol), "Set component")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6)) .LT. tol), "Set component: others zero")
END BLOCK

! Test 2: one component from a vector
BLOCK
  CALL u%Set(VALUE=a, spaceCompo=1, timeCompo=2)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "Set component vector")
  ! DOF format: (s, t) = (1, 2) is entries 13 to 18
  CALL u%GetMultiple(istart=13, iend=18, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), &
          "Set component vector: DOF format")
  CALL u%Set(VALUE=a, spaceCompo=1, timeCompo=2, scale=2.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * a) .LT. tol), &
          "Set component vector: add 2 * value")
END BLOCK

! Test 3: several space components, one time component
BLOCK
  m(1, :) = a
  m(2, :) = b
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=m, spaceCompo=[1, 2], timeCompo=2, &
             storageFMT=NODES_FMT)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "spaceCompo(:), NODES: x")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - b) .LT. tol), "spaceCompo(:), NODES: y")

  CALL u%Set(VALUE=TRANSPOSE(m), spaceCompo=[2, 1], timeCompo=1, &
             storageFMT=DOF_FMT)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "spaceCompo(:), DOF: y")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - b) .LT. tol), "spaceCompo(:), DOF: x")
END BLOCK

! Test 4: several time components, one space component
BLOCK
  m(1, :) = a
  m(2, :) = b
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=m, spaceCompo=2, timeCompo=[1, 2], &
             storageFMT=NODES_FMT)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "timeCompo(:), NODES: t1")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - b) .LT. tol), "timeCompo(:), NODES: t2")

  CALL u%Set(VALUE=TRANSPOSE(m), spaceCompo=1, timeCompo=[2, 1], &
             storageFMT=DOF_FMT)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "timeCompo(:), DOF: t2")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - b) .LT. tol), "timeCompo(:), DOF: t1")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
