!> author: Shion Shimizu
! date: 2026-11-02
! summary: Apply surface Neumann BCs of a vector field
!
! The field u (order 1 Lagrange on square_3x2.h5, [0, 10] x [0, 10]) has
! three Neumann BCs, see ApplySurfaceNeumannBC_test_1.toml: tx = 2 and
! ty = 3 on the right edge (line 2, nodes 2 and 3), and tx = 5 on the
! bottom edge (line 1, nodes 1, 5, 2). Node 2 is shared by both edges.
!
! For a constant traction t on an edge of length L with linear shape
! functions, each end node of the edge gets t * L / 2. The right edge
! has length 10, the bottom edge two segments of length 5. So the
! expected right hand side is
!   node 1: (12.5, 0), node 5: (25, 0), node 2: (22.5, 15),
!   node 3: (10, 15), nodes 4 and 6: (0, 0).
! The x and y BCs on the right edge must not be added twice, and the
! corner node 2 gets only one-sided contributions of each edge.
!
! Changes:
!   - 2026-11-12: expected values for the [0, 10] x [0, 10] mesh (they
!     were for a unit square).

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: OK
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = &
                             "ApplySurfaceNeumannBC_test_1.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(VectorField_) :: u, nbcField
  REAL(DFP) :: vec(6), expected(2, 6)
  INTEGER(I4B) :: tsize, inode

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL nbcField%Initiate(NAME="nbcField", engine="NATIVE_SERIAL", &
                         spaceCompo=[2], fedof=fedof, geofedof=geofedof)

  CALL OK(u%GetTotalNBC() .EQ. 3, "GetTotalNBC")

  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
                               times=0.0_DFP)

  expected(:, 1) = [12.5_DFP, 0.0_DFP]
  expected(:, 2) = [22.5_DFP, 15.0_DFP]
  expected(:, 3) = [10.0_DFP, 15.0_DFP]
  expected(:, 4) = [0.0_DFP, 0.0_DFP]
  expected(:, 5) = [25.0_DFP, 0.0_DFP]
  expected(:, 6) = [0.0_DFP, 0.0_DFP]

  DO inode = 1, 6
    CALL u%Get(VALUE=vec, tsize=tsize, globalNode=inode)
    CALL OK(ALL(ABS(vec(1:2) - expected(:, inode)) .LT. tol), &
            "node "//CHAR(48 + inode))
  END DO

  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ABS(SUM(vec(1:tsize)) - 70.0_DFP) .LT. tol, &
          "total x force = (2 + 5) * 10")
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
  CALL OK(ABS(SUM(vec(1:tsize)) - 30.0_DFP) .LT. tol, &
          "total y force = 3 * 10")

  ! scale and addContribution: applying again with scale -1 gives zero
  CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=-1.0_DFP, &
                               times=0.0_DFP)
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec(1:tsize)) .LT. tol), "scale = -1: x is zero")

  CALL nbcField%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
