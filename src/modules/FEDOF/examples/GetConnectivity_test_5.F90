!> author: Vikas Sharma, Ph. D.
! date: 2026-11-02
! summary: Testing GetConnectivity_ in broken spaces
!
! In a broken space each cell owns a contiguous block of DOFs. With opt
! "All" or "Cell" GetConnectivity_ returns this block; with "Vertex",
! "Edge" or "Face" it returns nothing. The blocks of two cells are
! disjoint, and together the blocks number the DOFs from 1 to
! GetTotalDOF() without gaps.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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

! Test 1: All and Cell give the same contiguous block of the cell
BLOCK
  INTEGER(I4B) :: con(20), con2(20), tsize, tsize2, ii
  LOGICAL(LGT) :: isok
  CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="All", &
                            globalElement=1, islocal=math%yes)
  CALL obj%GetConnectivity_(ans=con2, tsize=tsize2, opt="Cell", &
                            globalElement=1, islocal=math%yes)
  CALL OK(tsize .EQ. nloc, "All: size of the cell block")
  CALL OK(tsize2 .EQ. tsize, "Cell: same size as All")
  isok = ALL(con(1:tsize) .EQ. con2(1:tsize))
  CALL OK(isok, "Cell: same DOFs as All")
  isok = ALL([(con(ii + 1) - con(ii), ii=1, tsize - 1)] .EQ. 1)
  CALL OK(isok, "All: the block is contiguous")
END BLOCK

! Test 2: vertices, edges and faces own no DOFs
BLOCK
  INTEGER(I4B) :: con(20), tsize
  CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="Vertex", &
                            globalElement=1, islocal=math%yes)
  CALL OK(tsize .EQ. 0, "Vertex: no DOFs")
  CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="Edge", &
                            globalElement=1, islocal=math%yes)
  CALL OK(tsize .EQ. 0, "Edge: no DOFs")
  CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="Face", &
                            globalElement=1, islocal=math%yes)
  CALL OK(tsize .EQ. 0, "Face: no DOFs")
END BLOCK

! Test 3: the cell blocks cover 1:GetTotalDOF() once (disjoint, no gap)
BLOCK
  INTEGER(I4B) :: con(20), tsize, iel, tdof
  INTEGER(I4B), ALLOCATABLE :: counts(:)
  tdof = obj%GetTotalDOF()
  ALLOCATE (counts(tdof))
  counts = 0
  DO iel = 1, obj%GetTotalCells()
    CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="All", &
                              globalElement=iel, islocal=math%yes)
    counts(con(1:tsize)) = counts(con(1:tsize)) + 1
  END DO
  CALL OK(ALL(counts .EQ. 1), "each DOF belongs to exactly one cell")
END BLOCK

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
