!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: AXPY, SCAL, COPY and Swap of LIS scalar fields
!
!- AXPY(x, scale): w = w + scale * x,
!- AXPY(x1, x2, a1, a2): w = w + a1 * x1 + a2 * x2,
!- AXPY(x1, x2, x3, a1, a2, a3): w = w + a1 x1 + a2 x2 + a3 x3,
!- SCAL(scale): w = scale * w,
!- COPY(obj2): w = obj2,
!- Swap(obj2): the values of w and obj2 are exchanged.
!
! All fields must use the LIS engine.

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
TYPE(ScalarFieldLis_) :: x, y, z, w
REAL(DFP) :: src(6), found(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL y%Initiate(NAME="y", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)
CALL z%Initiate(NAME="z", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)
CALL w%Initiate(NAME="w", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)

src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
CALL y%SetAll(VALUE=1.0_DFP)
CALL z%SetAll(VALUE=10.0_DFP)

! Test 1: AXPY with one, two and three fields
BLOCK
  CALL w%SetAll(VALUE=0.0_DFP)
  CALL w%AXPY(x=x, scale=2.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")

  CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - (3.0_DFP * src - 1.0_DFP)) .LT. tol), &
          "AXPY: w + a1 * x1 + a2 * x2")

  CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - (2.0_DFP * src + 1.0_DFP)) .LT. tol), &
          "AXPY: w + a1 * x1 + a2 * x2 + a3 * x3")
END BLOCK

! Test 2: SCAL
BLOCK
  CALL w%COPY(obj2=x)
  CALL w%SCAL(scale=-2.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found + 2.0_DFP * src) .LT. tol), "SCAL")
END BLOCK

! Test 3: COPY
BLOCK
  CALL w%COPY(obj2=x)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "COPY: w = x")
END BLOCK

! Test 4: Swap
BLOCK
  CALL w%Swap(obj2=y)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Swap: w = old y")
  CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src) .LT. tol), "Swap: y = old w")
END BLOCK

CALL w%DEALLOCATE()
CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
