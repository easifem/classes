!> author: Shion Shimizu
! date: 2026-11-02
! summary: Apply surface Neumann BCs of a scalar field
!
! The field u (order 1 Lagrange on the unit square square_3x2.h5) has
! two Neumann BCs, see ApplySurfaceNeumannBC_test_1.toml: q = 2 on the
! right edge (line 2, nodes 2 and 3), and q = 5 on the bottom edge
! (line 1, nodes 1, 5, 2). Node 2 is shared by both edges.
!
! For a constant flux q on an edge of length L with linear shape
! functions, each end node of the edge gets q * L / 2. So the expected
! right hand side is
!   node 1: 1.25, node 5: 2.5, node 2: 1 + 1.25 = 2.25, node 3: 1,
!   nodes 4 and 6: 0.
! The facets of each edge use only the value of their own BC, also at
! the corner node 2. For this mesh the local node numbers are the same
! as the global node numbers.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = &
                             "ApplySurfaceNeumannBC_test_1.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: u, nbcField
  REAL(DFP) :: val, total, expected(6)
  INTEGER(I4B) :: inode

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL nbcField%Initiate(NAME="nbcField", engine="NATIVE_SERIAL", &
                         fedof=fedof, geofedof=geofedof)

  CALL OK(u%GetTotalNBC() .EQ. 2, "GetTotalNBC")

  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
                               times=0.0_DFP)

  expected = [1.25_DFP, 2.25_DFP, 1.0_DFP, 0.0_DFP, 2.5_DFP, 0.0_DFP]

  total = 0.0_DFP
  DO inode = 1, 6
    CALL u%Get(VALUE=val, globalNode=inode, islocal=math%yes)
    CALL OK(ABS(val - expected(inode)) .LT. tol, &
            "node "//CHAR(48 + inode))
    total = total + val
  END DO
  CALL OK(ABS(total - 7.0_DFP) .LT. tol, "total flux = 2 + 5")

  CALL nbcField%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
