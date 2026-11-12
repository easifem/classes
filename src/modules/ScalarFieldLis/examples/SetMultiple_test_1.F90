!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set and get entries of a LIS scalar field by index
!
! SetMultiple and GetMultiple work on the indices of the LIS vector:
!
!- indx(:) with VALUE(:): entries indx(i) = scale * VALUE(i),
!- istart, iend, stride with VALUE(:): one value for each index,
!- istart, iend, stride with a scalar VALUE: one value for all,
!- istart, iend, stride with a range (istart_value, iend_value,
!  stride_value) of VALUE.
!
! SetSingle and GetSingle set and get one entry. With addContribution
! the values are added.

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
TYPE(ScalarFieldLis_) :: x
REAL(DFP) :: found(6), src(6), val
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]

! Test 1: SetSingle and GetSingle
BLOCK
  CALL x%SetAll(VALUE=0.0_DFP)
  CALL x%SetSingle(indx=3, VALUE=2.0_DFP, scale=4.0_DFP)
  CALL x%GetSingle(indx=3, VALUE=val)
  CALL OK(ABS(val - 8.0_DFP) .LT. tol, "SetSingle: scale * value")
  CALL x%SetSingle(indx=3, VALUE=1.0_DFP, addContribution=math%yes)
  CALL x%GetSingle(indx=3, VALUE=val)
  CALL OK(ABS(val - 9.0_DFP) .LT. tol, "SetSingle: add")
END BLOCK

! Test 2: indices
BLOCK
  CALL x%SetAll(VALUE=7.0_DFP)
  CALL x%SetMultiple(indx=[2, 4], VALUE=[10.0_DFP, 20.0_DFP], &
                     scale=2.0_DFP)
  CALL x%GetMultiple(indx=[4, 2], VALUE=found, tsize=tsize)
  CALL OK(tsize .EQ. 2, "GetMultiple indx: tsize")
  CALL OK(ALL(ABS(found(1:2) - [40.0_DFP, 20.0_DFP]) .LT. tol), &
          "SetMultiple indx: scale * value")

  CALL x%SetMultiple(indx=[2, 4], VALUE=[1.0_DFP, 1.0_DFP], &
                     scale=2.0_DFP, addContribution=math%yes)
  CALL x%GetMultiple(indx=[2, 4], VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found(1:2) - [22.0_DFP, 42.0_DFP]) .LT. tol), &
          "SetMultiple indx: add 2 * value")
END BLOCK

! Test 3: range, vector value: x(1:5:2) = 3 * [1, 2, 3]
BLOCK
  CALL x%SetAll(VALUE=7.0_DFP)
  CALL x%SetMultiple(istart=1, iend=5, stride=2, &
                     VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], scale=3.0_DFP)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(tsize .EQ. 6, "GetMultiple range: tsize")
  CALL OK(ALL(ABS(found - [3.0_DFP, 7.0_DFP, 6.0_DFP, 7.0_DFP, &
                           9.0_DFP, 7.0_DFP]) .LT. tol), &
          "SetMultiple range")
END BLOCK

! Test 4: range, one value: x(1:6:3) = 0.5 * 8
BLOCK
  CALL x%SetAll(VALUE=7.0_DFP)
  CALL x%SetMultiple(istart=1, iend=6, stride=3, VALUE=8.0_DFP, &
                     scale=0.5_DFP)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - [4.0_DFP, 7.0_DFP, 7.0_DFP, 4.0_DFP, &
                           7.0_DFP, 7.0_DFP]) .LT. tol), &
          "SetMultiple range: one value")
END BLOCK

! Test 5: range of value: x(2:6:2) = -1 * src(4:6)
BLOCK
  CALL x%SetAll(VALUE=7.0_DFP)
  CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
                     istart_value=4, iend_value=6, stride_value=1, &
                     scale=-1.0_DFP)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - [7.0_DFP, -4.0_DFP, 7.0_DFP, -5.0_DFP, &
                           7.0_DFP, -6.0_DFP]) .LT. tol), &
          "SetMultiple range of value")
END BLOCK

! Test 6: GetMultiple into a range of value: found(4:6) = x(1:3)
BLOCK
  found = 0.0_DFP
  CALL x%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
                     istart_value=4, iend_value=6, stride_value=1, &
                     tsize=tsize)
  CALL OK(tsize .EQ. 3, "GetMultiple range of value: tsize")
  CALL OK(ALL(ABS(found(4:6) - [7.0_DFP, -4.0_DFP, 7.0_DFP]) &
              .LT. tol), "GetMultiple range of value")
END BLOCK

CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
