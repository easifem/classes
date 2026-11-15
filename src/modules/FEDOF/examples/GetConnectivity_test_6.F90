!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing the allocating GetConnectivity in broken spaces
!
! GetConnectivity allocates ans with the size given by GetTotalDOF for opt.
! In a broken space this size is zero for "Vertex", "Edge" and "Face", so
! nothing may be written. "All" and "Cell" give the block of the cell.
! GetConnectivity_test_5 tests the non-allocating GetConnectivity_.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: order = 2, nloc = 6
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                  isBrokenSpace=math%yes, order=order, dom=dom)

! Test 1: vertices, edges and faces own no DOFs
BLOCK
  INTEGER(I4B), ALLOCATABLE :: con(:)
  con = obj%GetConnectivity(opt="Vertex", globalElement=1, islocal=math%yes)
  CALL OK(SIZE(con) .EQ. 0, "Vertex: empty")
  con = obj%GetConnectivity(opt="Edge", globalElement=1, islocal=math%yes)
  CALL OK(SIZE(con) .EQ. 0, "Edge: empty")
  con = obj%GetConnectivity(opt="Face", globalElement=1, islocal=math%yes)
  CALL OK(SIZE(con) .EQ. 0, "Face: empty")
END BLOCK

! Test 2: All and Cell give the block of the cell
BLOCK
  INTEGER(I4B), ALLOCATABLE :: con(:), con2(:)
  con = obj%GetConnectivity(opt="All", globalElement=1, islocal=math%yes)
  con2 = obj%GetConnectivity(opt="Cell", globalElement=1, islocal=math%yes)
  CALL OK(SIZE(con) .EQ. nloc, "All: size of the cell block")
  CALL OK(SIZE(con2) .EQ. nloc, "Cell: size of the cell block")
  IF (SIZE(con) .EQ. SIZE(con2)) CALL OK(ALL(con .EQ. con2), &
                                         "Cell: same DOFs as All")
END BLOCK

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
