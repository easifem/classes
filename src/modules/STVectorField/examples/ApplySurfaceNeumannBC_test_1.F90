!> author: Shion Shimizu
! date: 2026-11-03
! summary: Apply a space varying surface Neumann BC of a space-time
!   vector field
!
! The field u (order 1 Lagrange in space on square_3x2.h5, [0, 10] x
! [0, 10], order 1 Lagrange in time) has one Neumann BC, see
! ApplySurfaceNeumannBC_test_1.toml: tx = x on the bottom edge (line 1,
! nodes 1 (0,0), 5 (5,0), 2 (10,0)). The time interval is [0, 1].
!
! For a time node a, the right hand side at a space node i is
!   int_0^1 T_a dt * int N_i x ds = 0.5 * int N_i x ds,
! that is node 1: 25/12, node 5: 25/2, node 2: 125/12 for both time
! components, and zero for the y component and the other nodes. The
! two time components must be the same, because tx does not depend on
! time (if space and time nodes were swapped, they would differ).
! For this mesh the local node numbers are the same as the global node
! numbers.
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
  USE STVectorField_Class, ONLY: STVectorField_
  USE Test_Method, ONLY: OK
  USE TimeFEDOF_Class, ONLY: TimeFEDOF_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = &
                             "ApplySurfaceNeumannBC_test_1.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  INTEGER(I4B), PARAMETER :: nodes(6) = [1, 2, 3, 4, 5, 6]
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(TimeFEDOF_), TARGET :: timefedof
  TYPE(STVectorField_) :: u, nbcField
  REAL(DFP) :: val(6), expected(6)
  INTEGER(I4B) :: tsize, itime
  CHARACTER(1) :: ch

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                        fedof=fedof, geofedof=geofedof, &
                        timefedof=timefedof, dom=dom)
  CALL nbcField%Initiate(obj2=u, copyFull=math%no, &
                         copyStructure=math%yes, usePointer=math%yes)

  CALL OK(u%GetTotalNBC() .EQ. 1, "GetTotalNBC")
  CALL OK(u%timeCompo .EQ. 2, "timeCompo")

  CALL u%SetAll(VALUE=0.0_DFP)
  CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
                               times=[0.0_DFP, 1.0_DFP])

  ! node:       1             2             3        4        5
  expected = [25.0_DFP / 12.0_DFP, 125.0_DFP / 12.0_DFP, 0.0_DFP, &
              0.0_DFP, 12.5_DFP, 0.0_DFP]

  DO itime = 1, 2
    ch = CHAR(48 + itime)
    CALL u%Get(VALUE=val, tsize=tsize, globalNode=nodes, &
               islocal=math%yes, spaceCompo=1, timeCompo=itime)
    CALL OK(ALL(ABS(val(1:tsize) - expected) .LT. tol), &
            "x component, time component "//ch)
    CALL u%Get(VALUE=val, tsize=tsize, globalNode=nodes, &
               islocal=math%yes, spaceCompo=2, timeCompo=itime)
    CALL OK(ALL(ABS(val(1:tsize)) .LT. tol), &
            "y component, time component "//ch)
  END DO

  CALL nbcField%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL timefedof%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
