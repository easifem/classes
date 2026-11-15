!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a scalar field to scale * another field, and Size
!
!- Size() returns the number of values (six DOFs for order 1 on
!  square_3x2.h5).
!- Set(VALUE=u, scale, addContribution=.FALSE.) sets v = scale * u.
!- Set(VALUE=u, scale, addContribution=.TRUE.) adds scale * u to v;
!  scale = -1 subtracts u.
!
! See Set_test_4 for v = u and v = v + 2 * u.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u, v
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL u%Set(VALUE=want)

! Test 1: Size
BLOCK
  CALL OK(u%SIZE() .EQ. 6, "u%Size()")
  CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF(), "v%Size() = total DOF")
END BLOCK

! Test 2: v = scale * u, the old values of v are discarded
BLOCK
  CALL v%Set(VALUE=9.0_DFP)
  CALL v%Set(VALUE=u, scale=-3.0_DFP, addContribution=math%no)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(tsize .EQ. 6, "tsize")
  CALL OK(ALL(ABS(found + 3.0_DFP * want) .LT. tol), "v = -3 * u")
END BLOCK

! Test 3: v = v - u
BLOCK
  CALL v%Set(VALUE=u, scale=-1.0_DFP, addContribution=math%yes)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + 4.0_DFP * want) .LT. tol), "v = v - u")
END BLOCK

! Test 4: u is not changed
BLOCK
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
END BLOCK

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
