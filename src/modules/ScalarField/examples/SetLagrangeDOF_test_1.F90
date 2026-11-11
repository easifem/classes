!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set the Lagrange DOFs of a scalar field from another field
!
! u is order 1 Lagrange (six DOFs) and w is order 2 Lagrange (15
! DOFs) on square_3x2.h5 (see SetLagrangeDOF_test_1.toml). Each field
! has its own FEDOF. w%SetLagrangeDOF(u) interpolates u at the
! interpolation points of w.
!
! u is set to f(x, y) = 1 + 2 x + 3 y + x y / 10 at the nodes. f is in
! the space of u, so w = f exactly; the values of w at its VTK points
! (GetVtkUnstructuredValue) must be equal to f at these points.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "SetLagrangeDOF_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof, fedof2, geofedof2
TYPE(ScalarField_) :: u, w
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), found(:)
REAL(DFP) :: xij(2, 6), uval(6)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL w%ImportFromToml(fedof=fedof2, geofedof=geofedof2, dom=dom, &
                      filename=tomlFileName, tomlName="w")

! Test 1: sizes of the two fields
CALL OK(u%SIZE() .EQ. 6, "size of u")
CALL OK(w%SIZE() .EQ. 15, "size of w")

! Test 2: a constant field gives constant Lagrange DOFs
BLOCK
  CALL u%Set(VALUE=4.0_DFP)
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%SetLagrangeDOF(obj2=u)
  ALLOCATE (found(w%SIZE()))
  CALL w%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found(1:tsize) - 4.0_DFP) .LT. tol), "constant")
END BLOCK

! Test 3: bilinear field, compared at the VTK points of w
BLOCK
  xij(1, :) = [0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 5.0_DFP, 5.0_DFP]
  xij(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 10.0_DFP]
  DO ii = 1, 6
    uval(ii) = f(xij(:, ii))
  END DO
  CALL u%Set(VALUE=uval)
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%SetLagrangeDOF(obj2=u)

  CALL w%GetVtkUnstructuredGridData(points=points, &
                                    connectivity=connectivity, &
                                    offsets=offsets, types=types)
  CALL w%GetVtkUnstructuredValue(VALUE=VALUE)
  CALL OK(SIZE(VALUE) .EQ. SIZE(points, 2), "one value for each point")
  DO ii = 1, SIZE(VALUE)
    VALUE(ii) = VALUE(ii) - f(points(1:2, ii))
  END DO
  CALL OK(MAXVAL(ABS(VALUE)) .LT. tol, "w = f at VTK points")
END BLOCK

CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof2%DEALLOCATE()
CALL geofedof2%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

PURE FUNCTION f(x) RESULT(ans)
  REAL(DFP), INTENT(IN) :: x(2)
  REAL(DFP) :: ans
  ans = 1.0_DFP + 2.0_DFP * x(1) + 3.0_DFP * x(2) + 0.1_DFP * x(1) * x(2)
END FUNCTION f

END PROGRAM main
