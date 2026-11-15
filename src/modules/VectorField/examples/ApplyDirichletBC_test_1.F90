!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Apply Dirichlet boundary conditions to a vector field
!
! The field u (order 1 Lagrange, two components) has two Dirichlet BCs
! (see ApplyDirichletBC_test_1.toml): u_x = 1 on the left edge (nodes
! 1, 4) and u_y = 2 on the right edge (nodes 2, 3). The external list
! extDBC has u_y = 7 on the bottom edge (nodes 1, 5, 2).
!
!- ApplyDirichletBC(times) uses the internal list of u,
!- ApplyDirichletBC(dbc(:), times) uses a vector of DirichletBCPointer_,
!- ApplyDirichletBC(dbc, times) uses one DirichletBC_.
!
! Each BC sets only its own component (idof) of its boundary nodes;
! all other entries are unchanged.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE DirichletBC_Class, ONLY: DirichletBCDeallocate
USE DirichletBC_Class, ONLY: DirichletBCImportFromToml
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ApplyDirichletBC_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: u
TYPE(DirichletBCPointer_), ALLOCATABLE :: extDBC(:)
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL DirichletBCImportFromToml(obj=extDBC, dom=dom, tomlName="extDBC", &
                               filename=tomlFileName)

! Test 1: internal list of Dirichlet BCs
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyDirichletBC(times=0.0_DFP)
  want = [1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - want) .LT. tol), "internal dbc: x")
  want = [0.0_DFP, 2.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - want) .LT. tol), "internal dbc: y")
END BLOCK

! Test 2: vector of DirichletBCPointer_
BLOCK
  CALL u%Set(VALUE=-1.0_DFP)
  CALL u%ApplyDirichletBC(dbc=extDBC, times=0.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found + 1.0_DFP) .LT. tol), "dbc vector: x unchanged")
  want = [7.0_DFP, 7.0_DFP, -1.0_DFP, -1.0_DFP, 7.0_DFP, -1.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - want) .LT. tol), "dbc vector: y")
END BLOCK

! Test 3: single DirichletBC_
BLOCK
  CALL u%Set(VALUE=3.0_DFP)
  CALL u%ApplyDirichletBC(dbc=extDBC(1)%ptr)
  want = [7.0_DFP, 7.0_DFP, 3.0_DFP, 3.0_DFP, 7.0_DFP, 3.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - want) .LT. tol), "single dbc: y")
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=5)
  CALL OK(ALL(ABS(found(1:2) - [3.0_DFP, 7.0_DFP]) .LT. tol), &
          "single dbc: node 5")
END BLOCK

CALL DirichletBCDeallocate(extDBC)
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
