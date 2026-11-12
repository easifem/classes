!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set the Lagrange DOFs of a vector field from another field
!
! u is order 1 Lagrange (six DOFs, from VectorField_test.toml) and w is
! order 2 Lagrange (15 DOFs, fedof2) on square_3x2.h5, both with two
! components. w%SetLagrangeDOF(u) interpolates u at the interpolation
! points of w.
!
! u is set to f(x, y) = [1 + 2 x + 3 y + x y / 10, 4 - x + y] at the
! nodes. f is in the space of u, so w = f exactly; the vectors of w at
! its VTK points (GetVtkUnstructuredValue) must be equal to f there.
!
! The last test does the same with three components (u3, w3) on the
! same two-dimensional mesh, f3 = [f, 5].

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeQuadratureOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof, fedof2
TYPE(VectorField_) :: u, w, u3, w3
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:, :), found(:)
REAL(DFP) :: xij(2, 6), uval(3)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL fedof2%Initiate(order=2, dom=dom, baseContinuity="H1", &
                     baseInterpolation="Lagrange", &
                     ipType=TypeQuadratureOpt%equidistance)
CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof2, geofedof=geofedof)

xij(1, :) = [0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 5.0_DFP, 5.0_DFP]
xij(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 10.0_DFP]

! Test 1: sizes of the two fields
CALL OK(u%SIZE() .EQ. 12, "size of u")
CALL OK(w%SIZE() .EQ. 30, "size of w")

! Test 2: a constant field gives constant Lagrange DOFs
BLOCK
  CALL u%Set(VALUE=4.0_DFP)
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%SetLagrangeDOF(obj2=u)
  ALLOCATE (found(w%SIZE()))
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found(1:tsize) - 4.0_DFP) .LT. tol), "constant: x")
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found(1:tsize) - 4.0_DFP) .LT. tol), "constant: y")
END BLOCK

! Test 3: bilinear field, compared at the VTK points of w
BLOCK
  DO ii = 1, 6
    uval = f(xij(:, ii))
    CALL u%Set(VALUE=uval(1:2), globalNode=ii, islocal=math%yes)
  END DO
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%SetLagrangeDOF(obj2=u)

  CALL w%GetVtkUnstructuredGridData(points=points, &
                                    connectivity=connectivity, &
                                    offsets=offsets, types=types)
  CALL w%GetVtkUnstructuredValue(VALUE=VALUE)
  CALL OK(SIZE(VALUE, 2) .EQ. SIZE(points, 2), &
          "one vector for each point")
  DO ii = 1, SIZE(VALUE, 2)
    uval = f(points(1:2, ii))
    VALUE(1:2, ii) = VALUE(1:2, ii) - uval(1:2)
  END DO
  CALL OK(MAXVAL(ABS(VALUE(1:2, :))) .LT. tol, "w = f at VTK points")
END BLOCK

! Test 4: three components on a 2D mesh
BLOCK
  CALL u3%Initiate(NAME="u", engine="NATIVE_SERIAL", spaceCompo=[3], &
                   fedof=fedof, geofedof=geofedof)
  CALL w3%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[3], &
                   fedof=fedof2, geofedof=geofedof)
  DO ii = 1, 6
    uval = f(xij(:, ii))
    CALL u3%Set(VALUE=uval, globalNode=ii, islocal=math%yes)
  END DO
  CALL w3%Set(VALUE=0.0_DFP)
  CALL w3%SetLagrangeDOF(obj2=u3)

  CALL w3%GetVtkUnstructuredValue(VALUE=VALUE)
  DO ii = 1, SIZE(VALUE, 2)
    VALUE(:, ii) = VALUE(:, ii) - f(points(1:2, ii))
  END DO
  CALL OK(MAXVAL(ABS(VALUE)) .LT. tol, "spaceCompo = 3: w3 = f3")
END BLOCK

CALL w3%DEALLOCATE()
CALL u3%DEALLOCATE()
CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof2%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

PURE FUNCTION f(x) RESULT(ans)
  REAL(DFP), INTENT(IN) :: x(2)
  REAL(DFP) :: ans(3)
  ans(1) = 1.0_DFP + 2.0_DFP * x(1) + 3.0_DFP * x(2) + &
           0.1_DFP * x(1) * x(2)
  ans(2) = 4.0_DFP - x(1) + x(2)
  ans(3) = 5.0_DFP
END FUNCTION f

END PROGRAM main
