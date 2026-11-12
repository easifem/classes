!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set all entries from a vector and selected entries (LIS)
!
!- Set(VALUE(:)) sets all entries from a vector (SetMultiple, range),
!- Set(globalNode(:), islocal, VALUE) sets the selected entries to one
!  value (SetSingle for each entry),
!- Set(globalNode(:), islocal, VALUE(:)) sets the selected entries to
!  the given values (SetMultiple, indices).
!
!- Get(VALUE(:), tsize) returns all entries (GetMultiple, range),
!- Get(VALUE(:), globalNode(:), islocal, tsize) the selected ones
!  (GetMultiple, indices).

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
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]

! Test 1: all entries from a vector
BLOCK
  CALL u%Set(VALUE=want)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(tsize .EQ. 6, "Get all: tsize")
  CALL OK(ALL(ABS(found - want) .LT. tol), "Set vector")

  CALL u%Set(VALUE=want, scale=2.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - 3.0_DFP * want) .LT. tol), &
          "Set vector: add 2 * value")
END BLOCK

! Test 2: selected entries to one value
BLOCK
  CALL u%Set(VALUE=want)
  CALL u%Set(globalNode=[1, 3, 5], islocal=math%yes, VALUE=0.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - [0.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, &
                           0.0_DFP, 6.0_DFP]) .LT. tol), &
          "Set selected: one value")
END BLOCK

! Test 3: selected entries to given values
BLOCK
  CALL u%Set(globalNode=[2, 4], islocal=math%yes, &
             VALUE=[20.0_DFP, 40.0_DFP])
  CALL u%Get(VALUE=found, globalNode=[2, 4], islocal=math%yes, &
             tsize=tsize)
  CALL OK(tsize .EQ. 2, "Get selected: tsize")
  CALL OK(ALL(ABS(found(1:2) - [20.0_DFP, 40.0_DFP]) .LT. tol), &
          "Set selected: values")

  CALL u%Set(globalNode=[2, 4], islocal=math%yes, &
             VALUE=[1.0_DFP, 1.0_DFP], scale=-1.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=found, globalNode=[2, 4], islocal=math%yes, &
             tsize=tsize)
  CALL OK(ALL(ABS(found(1:2) - [19.0_DFP, 39.0_DFP]) .LT. tol), &
          "Set selected: add -1 * values")
END BLOCK

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
