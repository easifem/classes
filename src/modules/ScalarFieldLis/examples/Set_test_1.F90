!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a single entry and all entries of a LIS scalar field
!
!- Set(globalNode, islocal, VALUE) sets one entry (SetSingle),
!- Set(VALUE) sets all entries (SetAll).
!
! Without addContribution the entry becomes scale * VALUE; with
! addContribution = .TRUE. scale * VALUE is added. The entries are
! read back with Get(VALUE, globalNode, islocal) (GetSingle).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: u
REAL(DFP) :: val, found(6)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

! Test 1: all entries
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  DO ii = 1, 6
    CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
  END DO
  CALL OK(ALL(ABS(found - 1.0_DFP) .LE. tol), "Set all")

  CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes)
  CALL OK(ABS(val - 3.0_DFP) .LE. tol, "Set all: add 2 * 1")
END BLOCK

! Test 2: single entry
BLOCK
  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP)
  CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
  CALL OK(ABS(val - 5.0_DFP) .LT. tol, "Set single")

  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP, &
             scale=2.0_DFP)
  CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
  CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: scale")

  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=1.0_DFP, &
             scale=3.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
  CALL OK(ABS(val - 13.0_DFP) .LT. tol, "Set single: add 3 * 1")

  CALL u%Get(VALUE=val, globalNode=1, islocal=math%yes)
  CALL OK(ABS(val - 3.0_DFP) .LT. tol, "other entries unchanged")
END BLOCK

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
