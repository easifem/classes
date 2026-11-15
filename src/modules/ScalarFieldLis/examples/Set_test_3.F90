!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a LIS scalar field from another scalar field (Set8)
!
! Set(VALUE, scale, addContribution) sets v = scale * VALUE, or adds
! scale * VALUE to v when addContribution is true. VALUE can be a
! ScalarFieldLis_ (copied LIS vector to LIS vector) or a ScalarField_
! with the native engine (copied from its Fortran vector). The
! assignment v = u copies the values of u. All fields use the same
! FEDOF.

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
TYPE(ScalarFieldLis_) :: u, v
TYPE(ScalarField_) :: n
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL u%Set(VALUE=want)

! Test 1: assignment from a LIS field
BLOCK
  CALL v%Set(VALUE=0.0_DFP)
  v = u
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "v = u")
END BLOCK

! Test 2: LIS field, set and add
BLOCK
  CALL v%Set(VALUE=9.0_DFP)
  CALL v%Set(VALUE=u, scale=-3.0_DFP, addContribution=math%no)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + 3.0_DFP * want) .LT. tol), "v = -3 * u")

  CALL v%Set(VALUE=u, scale=2.0_DFP, addContribution=math%yes)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + want) .LT. tol), "v = v + 2 * u")
END BLOCK

! Test 3: native field, set and add
BLOCK
  CALL n%Set(VALUE=10.0_DFP * want)
  CALL v%Set(VALUE=n, scale=0.5_DFP, addContribution=math%no)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - 5.0_DFP * want) .LT. tol), "v = 0.5 * n")

  CALL v%Set(VALUE=n, scale=-1.0_DFP, addContribution=math%yes)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + 5.0_DFP * want) .LT. tol), "v = v - n")
END BLOCK

! Test 4: u is not changed
BLOCK
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
END BLOCK

CALL n%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
