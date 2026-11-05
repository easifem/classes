!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Measure the time of SetSparsity for H1 and broken spaces
!
! This program measures the wall time of obj%SetSparsity(mat) on the mesh
! big_tri3_mesh.h5 (about 23000 Triangle3 cells). It runs the following
! cases for order 1 to maxOrder:
!
! - H1: continuous space
! - DG-cell: broken space, onlyCell=true (cell blocks only)
! - DG-flux: broken space, onlyCell=false (cell and neighbour blocks)
!
! Each case is repeated nrep times. The matrix is initiated before each
! repetition, and only the call of SetSparsity is timed. The first
! repetition also includes one-off work (GetMaxTotalConnectivity, and the
! element to element data of the mesh for broken spaces). The table shows,
! in seconds:
!
! - first: time of the first repetition of SetSparsity;
! - min: smallest time of SetSparsity;
! - loop: smallest time of the element loop of SetSparsity without the
!   calls of CSRMatrix SetSparsity (LoopOnly), that is the cost of the
!   connectivity calls of FEDOF and the mesh;
! - csr: min - loop, the cost of CSRMatrix SetSparsity (the row calls and
!   the final call which builds IA and JA).
!
! npairs is the number of (row, col) pairs passed to CSRMatrix
! SetSparsity, duplicates included.
!
! The checks verify the number of nonzeros:
!
! - DG-cell: every cell has n = (p+1)(p+2)/2 DOFs, so nnz = tdof * n;
! - DG-flux: a triangle has at most three neighbours, so
!   nnz(DG-cell) < nnz(DG-flux) <= 4 * nnz(DG-cell);
! - DG-cell and DG-flux: cells do not share DOFs, so npairs = nnz;
! - H1: nnz > tdof, and npairs >= nnz (shared DOFs give duplicates).
!
! Build the library and this program in Release mode to get timings that
! are useful; in Debug mode the bounds checks and the debug messages
! dominate the time.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeDOFOpt
USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
USE CSRMatrix_Method, ONLY: CSRMatrixDeallocate => DEALLOCATE
USE CSRMatrix_Method, ONLY: GetNNZ
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: DOFInitiate => Initiate
USE DOF_Method, ONLY: DOFDeallocate => DEALLOCATE
USE DOF_Method, ONLY: DOFSize => SIZE
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE ISO_FORTRAN_ENV, ONLY: INT64
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/big_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: maxOrder = 3
INTEGER(I4B), PARAMETER :: nrep = 3
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: order

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, "")

WRITE (*, "(A)") "case       p    tdof      nnz    npairs"// &
  "    first      min     loop      csr"

DO order = 1, maxOrder

  ! Test 1: continuous H1 space
  BLOCK
    INTEGER(I4B) :: tdof, nnz, npairs
    REAL(DFP) :: tfirst, tmin

    CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                      isBrokenSpace=math%no, order=order, dom=dom)
    CALL RunCase("H1", order, math%no, math%no, tdof, nnz, npairs, &
                 tfirst, tmin)
  END BLOCK

  ! Test 2 and 3: broken space, without and with neighbour couplings
  BLOCK
    INTEGER(I4B) :: tdof, nnzCell, nnzFlux, npairs, n
    REAL(DFP) :: tfirst, tmin
    LOGICAL(LGT) :: isok

    CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                      isBrokenSpace=math%yes, order=order, dom=dom)

    CALL RunCase("DG-cell", order, math%yes, math%no, tdof, nnzCell, &
                 npairs, tfirst, tmin)
    n = (order + 1) * (order + 2) / 2
    CALL RunCase("DG-flux", order, math%no, math%yes, tdof, nnzFlux, &
                 npairs, tfirst, tmin)
    isok = (nnzFlux .GT. nnzCell) .AND. (nnzFlux .LE. 4 * nnzCell)
  END BLOCK

END DO

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CONTAINS

!----------------------------------------------------------------------------
!                                                                    RunCase
!----------------------------------------------------------------------------

! Initiate a matrix for obj and time obj%SetSparsity nrep times.
SUBROUTINE RunCase(caseName, order, onlyCell, isFlux, tdof, nnz, &
                   npairs, tfirst, tmin)
  CHARACTER(*), INTENT(IN) :: caseName
  !! name of the case, printed in the table
  INTEGER(I4B), INTENT(IN) :: order
  !! order of the space, printed in the table
  LOGICAL(LGT), INTENT(IN) :: onlyCell
  !! onlyCell argument of SetSparsity
  LOGICAL(LGT), INTENT(IN) :: isFlux
  !! true for a broken space with neighbour couplings (see LoopOnly)
  INTEGER(I4B), INTENT(OUT) :: npairs
  !! number of (row, col) pairs of the element loop (see LoopOnly)
  INTEGER(I4B), INTENT(OUT) :: tdof
  !! total number of DOFs
  INTEGER(I4B), INTENT(OUT) :: nnz
  !! number of nonzeros of the pattern (last repetition)
  REAL(DFP), INTENT(OUT) :: tfirst
  !! wall time of the first repetition in seconds
  REAL(DFP), INTENT(OUT) :: tmin
  !! smallest wall time of all repetitions in seconds

  TYPE(CSRMatrix_) :: mat
  TYPE(DOF_) :: dofobj
  INTEGER(INT64) :: t0, t1, rate
  INTEGER(I4B) :: irep
  REAL(DFP) :: t, tloop

  tdof = obj%GetTotalDOF()
  CALL DOFInitiate(obj=dofobj, tNodes=[tdof], names=["K"], &
                   spaceCompo=[1], timeCompo=[1], &
                   storageFMT=TypeDOFOpt%storageFormatDOF)

  tmin = HUGE(1.0_DFP)
  tfirst = 0.0_DFP

  DO irep = 1, nrep
    CALL CSRMatrixDeallocate(mat)
    CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
                           nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)

    CALL SYSTEM_CLOCK(t0, rate)
    CALL obj%SetSparsity(mat=mat, onlyCell=onlyCell)
    CALL SYSTEM_CLOCK(t1)

    t = REAL(t1 - t0, DFP) / REAL(rate, DFP)
    IF (irep .EQ. 1) tfirst = t
    tmin = MIN(tmin, t)
  END DO

  nnz = GetNNZ(mat)

  tloop = HUGE(1.0_DFP)
  DO irep = 1, nrep
    CALL SYSTEM_CLOCK(t0, rate)
    CALL LoopOnly(isFlux, npairs)
    CALL SYSTEM_CLOCK(t1)
    t = REAL(t1 - t0, DFP) / REAL(rate, DFP)
    tloop = MIN(tloop, t)
  END DO

  WRITE (*, "(A8, I4, I8, I9, I10, 4F9.3)") caseName, order, tdof, nnz, &
    npairs, tfirst, tmin, tloop, tmin - tloop

  CALL CSRMatrixDeallocate(mat)
  CALL DOFDeallocate(dofobj)
END SUBROUTINE RunCase

!----------------------------------------------------------------------------
!                                                                   LoopOnly
!----------------------------------------------------------------------------

! Run the element loop of obj_SetSparsity1 without CSRMatrix SetSparsity:
! get the connectivity of each active cell and, if isFlux is true, the
! connectivity of its active neighbours. npairs counts the (row, col)
! pairs which SetSparsity would pass to CSRMatrix SetSparsity.
SUBROUTINE LoopOnly(isFlux, npairs)
  LOGICAL(LGT), INTENT(IN) :: isFlux
  !! if true, also get the connectivity of the neighbour cells
  INTEGER(I4B), INTENT(OUT) :: npairs
  !! number of (row, col) pairs, duplicates included

  CLASS(AbstractMesh_), POINTER :: mesh
  INTEGER(I4B), ALLOCATABLE :: conn(:), e2e(:), e2eConn(:)
  INTEGER(I4B) :: iel, ii, jj, tconn, te2e, te2eConn, telements, maxCon
  LOGICAL(LGT) :: isok

  mesh => obj%GetMeshPointer()
  telements = mesh%GetTotalElements()
  maxCon = obj%GetMaxTotalConnectivity()
  ALLOCATE (conn(maxCon))
  IF (isFlux) THEN
    ALLOCATE (e2e(mesh%GetMaxElementToElements()))
    ALLOCATE (e2eConn((SIZE(e2e) + 1) * maxCon))
  END IF

  npairs = 0
  DO iel = 1, telements
    isok = mesh%IsElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE
    CALL obj%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                              ans=conn, tsize=tconn, opt="A")
    te2eConn = tconn
    IF (isFlux) THEN
      CALL mesh%GetElementToElements_(ans=e2e, tsize=te2e, &
                                      globalElement=iel, islocal=math%yes)
      DO ii = 1, te2e
        isok = mesh%IsElementActive(globalElement=e2e(ii), &
                                    islocal=math%no)
        IF (.NOT. isok) CYCLE
        CALL obj%GetConnectivity_(globalElement=e2e(ii), &
                                  islocal=math%no, &
                                  ans=e2eConn(te2eConn + 1:), tsize=jj, &
                                  opt="A")
        te2eConn = te2eConn + jj
      END DO
    END IF
    npairs = npairs + tconn * te2eConn
  END DO

  NULLIFY (mesh)
END SUBROUTINE LoopOnly

END PROGRAM main
