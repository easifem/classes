!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: COPY between LIS fields, Set between engines
!
! x, y are ScalarFieldLis_ and n is a native ScalarField_, all with six
! DOFs. The BLAS methods (COPY, AXPY, ...) need fields of the same
! engine: COPY(obj2) copies a LIS field into a LIS field. Values are
! moved between engines by Set(VALUE=field):
!
!- LIS <- LIS: x%COPY(obj2=y),
!- LIS <- native: x%Set(VALUE=n),
!- native <- LIS: n%Set(VALUE=x), the values of the LIS vector of x
!  (its realVec is not up to date).

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
TYPE(ScalarFieldLis_) :: x, y
TYPE(ScalarField_) :: n
REAL(DFP) :: src(6), found(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL y%Initiate(NAME="y", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)
CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)
src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]

! Test 1: COPY, LIS <- LIS
BLOCK
  CALL y%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
  CALL x%SetAll(VALUE=0.0_DFP)
  CALL x%COPY(obj2=y)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "COPY: LIS <- LIS")
END BLOCK

! Test 2: Set, LIS <- native
BLOCK
  CALL n%SetMultiple(istart=1, iend=6, stride=1, VALUE=-src)
  CALL x%Set(VALUE=n)
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found + src) .LT. tol), "Set: LIS <- native")
END BLOCK

! Test 3: Set, native <- LIS
BLOCK
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=2.0_DFP * src)
  CALL n%SetAll(VALUE=0.0_DFP)
  CALL n%Set(VALUE=x)
  CALL n%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "Set: native <- LIS")
END BLOCK

CALL n%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
