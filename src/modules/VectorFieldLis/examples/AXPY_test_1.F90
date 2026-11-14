!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: BLAS methods of LIS vector fields
!
!- AXPY(x, scale), AXPY(x1, x2, a1, a2), AXPY(x1, x2, x3, a1, a2, a3),
!- SCAL, COPY, Swap,
!- Norm1, Norm2, Normi, DOT_PRODUCT, PMUL, Reciprocal.
!
! All fields use the LIS engine and have 12 values (6 nodes,
! spaceCompo = 2).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: x, y, z, w
REAL(DFP) :: src(12), found(12)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL y%Initiate(NAME="y", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)
CALL z%Initiate(NAME="z", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)
CALL w%Initiate(NAME="w", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

src = [(REAL(ii, DFP), ii=1, 12)]
CALL x%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)
CALL y%SetAll(VALUE=1.0_DFP)
CALL z%SetAll(VALUE=10.0_DFP)

! Test 1: AXPY with one, two and three fields
CALL w%SetAll(VALUE=0.0_DFP)
CALL w%AXPY(x=x, scale=2.0_DFP)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")

CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - (3.0_DFP * src - 1.0_DFP)) .LT. tol), &
        "AXPY: w + a1 * x1 + a2 * x2")

CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - (2.0_DFP * src + 1.0_DFP)) .LT. tol), &
        "AXPY: w + a1 * x1 + a2 * x2 + a3 * x3")

! Test 2: COPY and SCAL
CALL w%COPY(obj2=x)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "COPY: w = x")
CALL w%SCAL(scale=-2.0_DFP)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found + 2.0_DFP * src) .LT. tol), "SCAL")

! Test 3: Swap
CALL w%COPY(obj2=x)
CALL w%Swap(obj2=y)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Swap: w = old y")
CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "Swap: y = old w")

! Test 4: norms and dot product, y = src, z = 10
CALL OK(ABS(x%Norm1() - 78.0_DFP) .LT. tol, "Norm1")
CALL OK(ABS(x%Norm2() - SQRT(650.0_DFP)) .LT. tol, "Norm2")
CALL OK(ABS(x%Normi() - 12.0_DFP) .LT. tol, "Normi")
CALL OK(ABS(x%DOT_PRODUCT(obj2=z) - 780.0_DFP) .LT. tol, "DOT_PRODUCT")

! Test 5: PMUL, w = x * z, then Reciprocal, w = 1 / w
CALL w%PMUL(obj1=x, obj2=z)
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 10.0_DFP * src) .LT. tol), "PMUL")
CALL w%Reciprocal()
CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 0.1_DFP / src) .LT. tol), "Reciprocal")

CALL w%DEALLOCATE()
CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
