!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Norms, DOT_PRODUCT, PMUL and Reciprocal of LIS fields
!
! For x = [1, -2, 3, -4, 5, -6]:
!   Norm1 = 21, Norm2 = SQRT(91), Normi = 6.
! For y = [1, 1, 1, 2, 2, 2]: DOT_PRODUCT(x, y) = 2 - 10 = -8.
! PMUL(obj1, obj2) sets z(i) = obj1(i) * obj2(i), and Reciprocal sets
! z(i) = 1 / z(i). All fields must use the LIS engine.

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
TYPE(ScalarFieldLis_) :: x, y, z
REAL(DFP) :: src(6), ysrc(6), found(6)
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

src = [1.0_DFP, -2.0_DFP, 3.0_DFP, -4.0_DFP, 5.0_DFP, -6.0_DFP]
ysrc = [1.0_DFP, 1.0_DFP, 1.0_DFP, 2.0_DFP, 2.0_DFP, 2.0_DFP]
CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
CALL y%SetMultiple(istart=1, iend=6, stride=1, VALUE=ysrc)

! Test 1: norms
CALL OK(ABS(x%Norm1() - 21.0_DFP) .LT. tol, "Norm1")
CALL OK(ABS(x%Norm2() - SQRT(91.0_DFP)) .LT. tol, "Norm2")
CALL OK(ABS(x%Normi() - 6.0_DFP) .LT. tol, "Normi")

! Test 2: dot product
CALL OK(ABS(x%DOT_PRODUCT(obj2=y) + 8.0_DFP) .LT. tol, "DOT_PRODUCT")

! Test 3: PMUL, z = x * y
BLOCK
  CALL z%PMUL(obj1=x, obj2=y)
  CALL z%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - src * ysrc) .LT. tol), "PMUL")
END BLOCK

! Test 4: Reciprocal, z = 1 / z
BLOCK
  CALL z%Reciprocal()
  CALL z%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 1.0_DFP / (src * ysrc)) .LT. tol), &
          "Reciprocal")
END BLOCK

CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
