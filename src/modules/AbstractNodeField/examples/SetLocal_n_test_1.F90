!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Set the local and global sizes of a node field
!
! The scalar field x (6 DOFs) is imported from
! AbstractNodeField_test.toml. Initiate sets local_n and global_n to
! the size of the field, 6. SetLocal_n and SetGlobal_n change them.
! SIZE of a scalar field returns local_n.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: x

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  CALL OK(x%local_n .EQ. 6, "after Initiate: local_n = 6")
  CALL OK(x%global_n .EQ. 6, "after Initiate: global_n = 6")

  CALL x%SetLocal_n(local_n=4)
  CALL OK(x%local_n .EQ. 4, "SetLocal_n: local_n = 4")
  CALL OK(x%SIZE() .EQ. 4, "SetLocal_n: SIZE = 4")
  CALL OK(x%global_n .EQ. 6, "SetLocal_n: global_n unchanged")

  CALL x%SetGlobal_n(global_n=10)
  CALL OK(x%global_n .EQ. 10, "SetGlobal_n: global_n = 10")
  CALL OK(x%local_n .EQ. 4, "SetGlobal_n: local_n unchanged")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
