!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set all entries and single entries of a space-time field
!
!- Set(VALUE) sets all entries to one value,
!- Set(globalNode, islocal, VALUE(:)) sets all time components of one
!  node, SIZE(VALUE) = timeCompo,
!- Set(VALUE, globalNode, islocal, timeCompo) sets one entry,
!- Set(VALUE(:)) sets time component a of all nodes to VALUE(a).
!
! Without addContribution the entries become scale * VALUE; with
! addContribution = .TRUE. scale * VALUE is added.

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
REAL(DFP) :: found(12), val
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: all entries to one value
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Set all")
END BLOCK

! Test 2: all time components of one node
BLOCK
  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=[5.0_DFP, 6.0_DFP])
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
  CALL OK(ALL(ABS(found(1:2) - [5.0_DFP, 6.0_DFP]) .LT. tol), &
          "Set node")
  ! DOF format: node 2 is entry 2 (time 1) and entry 8 (time 2)
  CALL u%GetMultiple(indx=[2, 8], VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found(1:2) - [5.0_DFP, 6.0_DFP]) .LT. tol), &
          "Set node: DOF format")

  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=[1.0_DFP, 1.0_DFP], &
             scale=2.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
  CALL OK(ALL(ABS(found(1:2) - [7.0_DFP, 8.0_DFP]) .LT. tol), &
          "Set node: add 2 * value")
END BLOCK

! Test 3: one entry
BLOCK
  CALL u%Set(VALUE=9.0_DFP, globalNode=3, islocal=math%yes, timeCompo=2)
  CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, timeCompo=2)
  CALL OK(ABS(val - 9.0_DFP) .LT. tol, "Set single")
  CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, timeCompo=1)
  CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set single: other time")
END BLOCK

! Test 4: one value for each time component, all nodes
BLOCK
  CALL u%Set(VALUE=[2.0_DFP, 3.0_DFP])
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 2.0_DFP) .LT. tol), &
          "Set time values: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 3.0_DFP) .LT. tol), &
          "Set time values: time 2")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
