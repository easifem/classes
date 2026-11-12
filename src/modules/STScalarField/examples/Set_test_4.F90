!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a space-time field from an FEVariable and other fields
!
!- Set(VALUE=fevar, globalNode(:), islocal) sets the selected nodes
!  from a nodal, space-time, scalar FEVariable whose values are stored
!  as val(a, j): time component a of node globalNode(j),
!- GetFEVariable(globalNode(:), islocal, VALUE) returns them in the
!  same layout,
!- Set(VALUE=v) copies another space-time field (Set13),
!- SetFromScalarField(timeCompo, VALUE=s) sets one time component
!  from a scalar field s,
!- SetToScalarField(timeCompo, VALUE=s) sets s from one time
!  component.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ScalarField_Class, ONLY: ScalarField_
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u, v
TYPE(ScalarField_) :: s
TYPE(FEVariable_) :: fevar
REAL(DFP) :: found(12), src(12), want(6), vals(2, 2)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)
CALL s%Initiate(NAME="s", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

! Test 1: FEVariable, vals(a, j) for node j = 1, 2
BLOCK
  vals(1:2, 1) = [1.0_DFP, 2.0_DFP]
  vals(1:2, 2) = [3.0_DFP, 4.0_DFP]
  fevar = NodalVariable(vals, TypeFEVariableScalar, &
                        TypeFEVariableSpaceTime)
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=fevar, globalNode=[1, 2], islocal=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
  CALL OK(ALL(ABS(found(1:2) - [3.0_DFP, 4.0_DFP]) .LT. tol), &
          "Set FEVariable")

  CALL u%GetFEVariable(globalNode=[2, 1], islocal=math%yes, VALUE=fevar)
  CALL OK(ALL(fevar%s(1:2) .EQ. [2, 2]), "GetFEVariable: shape")
  CALL OK(ALL(ABS(fevar%val(1:4) - [3.0_DFP, 4.0_DFP, 1.0_DFP, &
                                    2.0_DFP]) .LT. tol), &
          "GetFEVariable: values")
END BLOCK

! Test 2: copy another space-time field
BLOCK
  src = [(REAL(ii, DFP), ii=1, 12)]
  CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)
  CALL v%Set(VALUE=u)
  CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Set(VALUE=u): copy")
END BLOCK

! Test 3: one time component from a scalar field
BLOCK
  want = [(10.0_DFP * ii, ii=1, 6)]
  CALL s%Set(VALUE=want)
  CALL u%SetFromScalarField(timeCompo=2, VALUE=s)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "SetFromScalarField")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - src(1:6)) .LT. tol), &
          "SetFromScalarField: time 1 unchanged")

  CALL u%SetFromScalarField(timeCompo=2, VALUE=s, scale=0.5_DFP, &
                            addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 1.5_DFP * want) .LT. tol), &
          "SetFromScalarField: add 0.5 * s")
END BLOCK

! Test 4: a scalar field from one time component
BLOCK
  CALL s%Set(VALUE=0.0_DFP)
  CALL u%SetToScalarField(timeCompo=1, VALUE=s)
  CALL s%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found(1:6) - src(1:6)) .LT. tol), "SetToScalarField")
END BLOCK

CALL s%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
