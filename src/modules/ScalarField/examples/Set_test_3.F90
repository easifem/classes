!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get selected entries with FEVariable
!
! Set(globalNode(:), islocal, VALUE) with a nodal FEVariable sets the
! selected entries: a constant FEVariable sets all of them to one
! value, a space FEVariable gives one value for each node. Get with an
! FEVariable (and GetFEVariable) returns the selected entries as a
! nodal, space FEVariable.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: FEVariable_
  USE BaseType, ONLY: TypeFEVariableScalar
  USE BaseType, ONLY: TypeFEVariableSpace
  USE BaseType, ONLY: TypeFEVariableConstant
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE FEVariable_Method, ONLY: NodalVariable
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: u
  TYPE(FEVariable_) :: fevar
  REAL(DFP) :: found(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL u%Set(VALUE=0.0_DFP)

  ! constant FEVariable: one value for all selected nodes
  fevar = NodalVariable(7.0_DFP, TypeFEVariableScalar, &
                        TypeFEVariableConstant)
  CALL u%Set(globalNode=[1, 2], islocal=math%yes, VALUE=fevar)

  ! space FEVariable: one value for each selected node
  fevar = NodalVariable([3.0_DFP, 5.0_DFP], TypeFEVariableScalar, &
                       TypeFEVariableSpace)
  CALL u%Set(globalNode=[5, 6], islocal=math%yes, VALUE=fevar)

  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - [7.0_DFP, 7.0_DFP, 0.0_DFP, 0.0_DFP, &
                           3.0_DFP, 5.0_DFP]) .LT. tol), &
          "Set with FEVariable")

  ! get the selected entries in an FEVariable
  fevar = NodalVariable(6, TypeFEVariableScalar, TypeFEVariableSpace)
  CALL u%Get(VALUE=fevar, globalNode=[6, 1], islocal=math%yes)
  CALL OK(fevar%len .EQ. 2, "Get FEVariable: len")
  CALL OK(ALL(ABS(fevar%val(1:2) - [5.0_DFP, 7.0_DFP]) .LT. tol), &
          "Get FEVariable: values")

  CALL u%GetFEVariable(globalNode=[5], islocal=math%yes, VALUE=fevar)
  CALL OK(ABS(fevar%val(1) - 3.0_DFP) .LT. tol, "GetFEVariable")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
