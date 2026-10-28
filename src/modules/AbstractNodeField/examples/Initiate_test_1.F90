!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Initiate a node field from another node field
!
! Initiate(obj2) makes obj a copy of obj2: the data of AbstractField_
! (name, engine, FEDOF pointers, ...), the DOF data and the values.
! DEALLOCATE removes all data of obj; obj2 is not changed.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: x, y
  REAL(DFP) :: src(6), found(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

  CALL y%Initiate(obj2=x)
  CALL OK(y%IsInitiated(), "Initiate: IsInitiated")
  CALL OK(y%Size() .EQ. x%Size(), "Initiate: Size")
  CALL OK(y%GetTotalPhysicalVars() .EQ. 1, "Initiate: physical vars")
  CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Initiate: values")

  CALL y%DEALLOCATE()
  CALL OK(.NOT. y%IsInitiated(), "DEALLOCATE: not initiated")
  CALL OK(y%Size() .EQ. 0, "DEALLOCATE: Size = 0")
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "DEALLOCATE: x unchanged")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
