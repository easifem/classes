!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Add the point Neumann boundary conditions of a vector field
!
! The field u (order 1 Lagrange, two components) has two point Neumann
! BCs (see ApplyPointNeumannBC_test_1.toml): f_x = 2 at node 1 and
! f_y = 4 at node 3.
!
! ApplyPointNeumannBC(scale, times) adds scale * f to the component
! idof of these nodes; all other entries are unchanged.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "ApplyPointNeumannBC_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: u
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

! Test 1: number of point Neumann BCs
CALL OK(u%GetTotalPointNBC() .EQ. 2, "GetTotalPointNBC")

! Test 2: scale = 1
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyPointNeumannBC(scale=1.0_DFP, times=0.0_DFP)
  want = [2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - want) .LT. tol), "scale = 1: x")
  want = [0.0_DFP, 0.0_DFP, 4.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - want) .LT. tol), "scale = 1: y")
END BLOCK

! Test 3: added to the current values, scale = 0.5
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyPointNeumannBC(scale=0.5_DFP)
  want = [2.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - want) .LT. tol), "add with scale = 0.5: x")
  want = [1.0_DFP, 1.0_DFP, 3.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - want) .LT. tol), "add with scale = 0.5: y")
END BLOCK

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
