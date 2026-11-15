!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Apply Dirichlet boundary conditions to a space-time vector
!   field
!
! The field u (two space components, timeCompo = 2) has two Dirichlet
! BCs (see ApplyDirichletBC_test_1.toml): u_x = 1 on the left edge
! (nodes 1, 4) and u_y = 2 on the right edge (nodes 2, 3). The external
! list extDBC has u_y = 7 on the bottom edge (nodes 1, 5, 2). The
! values are constant, so both time components get the same value.
!
!- ApplyDirichletBC(times) uses the internal list of u,
!- ApplyDirichletBC(dbc(:), times) uses a vector of DirichletBCPointer_,
!- ApplyDirichletBC(dbc, times) uses one DirichletBC_.
!
! times(:) are the times of the time nodes. Each BC sets only its own
! space component (idof) of its boundary nodes.

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
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ApplyDirichletBC_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
TYPE(DirichletBCPointer_), ALLOCATABLE :: extDBC(:)
REAL(DFP) :: found(24), want(6)
INTEGER(I4B) :: tsize, itime

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL DirichletBCImportFromToml(obj=extDBC, dom=dom, tomlName="extDBC", &
                               filename=tomlFileName)

! Test 1: internal list of Dirichlet BCs
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyDirichletBC(times=times)
  DO itime = 1, 2
    want = [1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP]
    CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=itime)
    CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
            "internal dbc: x, time "//CHAR(48 + itime))
    want = [0.0_DFP, 2.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
    CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=itime)
    CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
            "internal dbc: y, time "//CHAR(48 + itime))
  END DO
END BLOCK

! Test 2: vector of DirichletBCPointer_
BLOCK
  CALL u%Set(VALUE=-1.0_DFP)
  CALL u%ApplyDirichletBC(dbc=extDBC, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) + 1.0_DFP) .LT. tol), &
          "dbc vector: x unchanged")
  want = [7.0_DFP, 7.0_DFP, -1.0_DFP, -1.0_DFP, 7.0_DFP, -1.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "dbc vector: y")
END BLOCK

! Test 3: single DirichletBC_
BLOCK
  CALL u%Set(VALUE=3.0_DFP)
  CALL u%ApplyDirichletBC(dbc=extDBC(1)%ptr, times=times)
  want = [7.0_DFP, 7.0_DFP, 3.0_DFP, 3.0_DFP, 7.0_DFP, 3.0_DFP]
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "single dbc: y")
END BLOCK

CALL DirichletBCDeallocate(extDBC)
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
