!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: DOF data of a node field
!
! GetTotalPhysicalVars returns the number of physical variables,
! GetPhysicalNames their one character names, GetSpaceCompo and
! GetTimeCompo their space and time components, GetStorageFMT the
! storage format (DOF format), and Size the number of values. A
! ScalarField_ named "u" has one physical variable "u" with one space
! and one time component.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE FieldOpt_Class, ONLY: TypeFieldOpt
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: x
  CHARACTER(1), ALLOCATABLE :: names(:)
  INTEGER(I4B) :: tvar

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  tvar = x%GetTotalPhysicalVars()
  CALL OK(tvar .EQ. 1, "GetTotalPhysicalVars")
  ALLOCATE(names(tvar))
  CALL x%GetPhysicalNames(names)
  CALL OK(names(1) .EQ. "u", "GetPhysicalNames")
  CALL OK(ALL(x%GetSpaceCompo(tvar) .EQ. [1]), "GetSpaceCompo")
  CALL OK(ALL(x%GetTimeCompo(tvar) .EQ. [1]), "GetTimeCompo")
  CALL OK(x%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
          "GetStorageFMT")
  CALL OK(x%Size() .EQ. 6, "Size")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
