!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Import a domain from a toml file
!
! The table [domain] of FEDomain.toml gives the mesh file
! (filename), the group (default is the root) and the number of
! media of the elements (totalMedium, default is 1):
!
!   [domain]
!   filename = "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   totalMedium = 2

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  TYPE(FEDomain_) :: obj

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%ImportFromToml(tomlName="domain", filename="FEDomain.toml")
  CALL OK(obj%IsInitiated(), "IsInitiated")
  CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
  CALL OK(obj%GetTotalMaterial(DIM=2_I4B, globalElement=13_I4B) &
    .EQ. 2_I4B, "totalMedium")
  CALL obj%DEALLOCATE()
END PROGRAM main
