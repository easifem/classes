!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Initiate a scalar field by passing arguments
!
! The FEDOFs (order 1 Lagrange on square_3x2.h5, six DOFs) are made by
! importing the field u from ScalarField_test.toml. A second field v is
! then initiated with Initiate by passing the name, the engine and the
! same FEDOFs. A scalar field has one physical variable, named by the
! first character of the name, with one space and one time component.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE FieldOpt_Class, ONLY: TypeFieldOpt
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: u, v
  CHARACTER(1) :: names(1)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)

  CALL OK(v%IsInitiated(), "IsInitiated")
  CALL OK(v%Size() .EQ. fedof%GetTotalDOF(), "Size = total DOF")
  CALL OK(v%Size() .EQ. 6, "Size = 6")
  CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "GetTotalPhysicalVars")
  CALL v%GetPhysicalNames(names)
  CALL OK(names(1) .EQ. "v", "GetPhysicalNames")
  CALL OK(ALL(v%GetSpaceCompo(1) .EQ. [1]), "GetSpaceCompo")
  CALL OK(ALL(v%GetTimeCompo(1) .EQ. [1]), "GetTimeCompo")
  CALL OK(v%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
          "GetStorageFMT")

  CALL v%DEALLOCATE()
  CALL OK(.NOT. v%IsInitiated(), "DEALLOCATE")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
