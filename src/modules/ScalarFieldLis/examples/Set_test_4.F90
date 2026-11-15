!> author: Vikas Sharma, Ph. D.
! date: 2026-11-13
! summary: Set a native scalar field from a LIS scalar field
!
! n is a native ScalarField_ and u is a ScalarFieldLis_. The assignment
! n = u and n%Set(VALUE=u, scale, addContribution) read the values of u
! from its LIS vector (u has no Fortran vector to point to).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: u
TYPE(ScalarField_) :: n, m
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)
CALL m%Initiate(NAME="m", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL u%Set(VALUE=want)

! Test 1: assignment from a LIS field
BLOCK
  CALL n%Set(VALUE=0.0_DFP)
  n = u
  CALL n%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "n = u")
END BLOCK

! Test 2: LIS field, set and add
BLOCK
  CALL n%Set(VALUE=9.0_DFP)
  CALL n%Set(VALUE=u, scale=-3.0_DFP, addContribution=math%no)
  CALL n%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + 3.0_DFP * want) .LT. tol), "n = -3 * u")

  CALL n%Set(VALUE=u, scale=2.0_DFP, addContribution=math%yes)
  CALL n%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + want) .LT. tol), "n = n + 2 * u")
END BLOCK

! Test 3: native field (unchanged path)
BLOCK
  CALL m%Set(VALUE=10.0_DFP * want)
  CALL n%Set(VALUE=m, scale=0.5_DFP, addContribution=math%no)
  CALL n%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - 5.0_DFP * want) .LT. tol), "n = 0.5 * m")
END BLOCK

! Test 4: u is not changed
BLOCK
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
END BLOCK

CALL m%DEALLOCATE()
CALL n%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
