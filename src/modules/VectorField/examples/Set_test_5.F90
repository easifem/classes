!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get selected nodes with FEVariable
!
! Set(VALUE, globalNode(:), islocal) with a vector, nodal FEVariable
! sets the vectors of the selected nodes: a constant FEVariable gives
! one vector for all nodes, a space FEVariable one vector for each node
! (shape (spaceCompo, SIZE(globalNode))). Get with an FEVariable (and
! GetFEVariable) returns the vectors of the selected nodes as a vector,
! nodal, space FEVariable.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: FEVariable_
  USE BaseType, ONLY: TypeFEVariableVector
  USE BaseType, ONLY: TypeFEVariableSpace
  USE BaseType, ONLY: TypeFEVariableConstant
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE FEVariable_Method, ONLY: NodalVariable
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: OK
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(VectorField_) :: u
  TYPE(FEVariable_) :: fevar
  REAL(DFP) :: vec(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL u%Set(VALUE=0.0_DFP)

  ! constant FEVariable: one vector for all selected nodes
  fevar = NodalVariable([1.0_DFP, 2.0_DFP], TypeFEVariableVector, &
                       TypeFEVariableConstant)
  CALL u%Set(VALUE=fevar, globalNode=[3, 4], islocal=math%yes)

  ! space FEVariable: one vector for each selected node
  fevar = NodalVariable(RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], &
                               [2, 2]), TypeFEVariableVector, &
                        TypeFEVariableSpace)
  CALL u%Set(VALUE=fevar, globalNode=[5, 6], islocal=math%yes)

  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec - [0.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, 5.0_DFP, &
                         7.0_DFP]) .LT. tol), "Set FEVariable: compo 1")
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(vec - [0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, 6.0_DFP, &
                         8.0_DFP]) .LT. tol), "Set FEVariable: compo 2")

  ! get the vectors of the selected nodes in an FEVariable
  CALL u%Get(VALUE=fevar, globalNode=[6, 3], islocal=math%yes)
  CALL OK(ALL(fevar%s(1:2) .EQ. [2, 2]), "Get FEVariable: shape")
  CALL OK(ALL(ABS(fevar%val(1:4) - [7.0_DFP, 8.0_DFP, 1.0_DFP, &
                                    2.0_DFP]) .LT. tol), &
          "Get FEVariable: values")

  CALL u%GetFEVariable(globalNode=[5], islocal=math%yes, VALUE=fevar)
  CALL OK(ALL(ABS(fevar%val(1:2) - [5.0_DFP, 6.0_DFP]) .LT. tol), &
          "GetFEVariable")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
