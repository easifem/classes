!> author: Vikas Sharma, Ph. D.
! date: 2025-06-10
! update:
!   - 2026-10-24: Use FEDomain_ (dom=) instead of FEMesh_ (mesh=)
! summary: Set the sparsity pattern of a matrix from the FEDOF
!
! SetSparsity sets the nonzero pattern of mat: two degrees of freedom are
! connected if they belong to the same element. For H1 hierarchical basis
! of order 1 the degrees of freedom are the nodes, so row i has the nodes
! of the elements around node i. For order 2 one degree of freedom is added
! on each edge.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE BaseType, ONLY: CSRMatrix_
  USE BaseType, ONLY: DOF_
  USE BaseType, ONLY: math => TypeMathOpt
  USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
  USE CSRMatrix_Method, ONLY: ASSIGNMENT(=)
  USE DOF_Method, ONLY: DOFInitiate => Initiate
  USE DOF_Method, ONLY: DOFSize => SIZE
  USE Display_Method, ONLY: Display
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE GlobalData, ONLY: NODES_FMT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
    baseInterpolation = "Hierarchical", &
    testname = baseContinuity // " " // &
    baseInterpolation // " SetSparsity "
  INTEGER(I4B) :: ii, jj, order
  TYPE(CSRMatrix_) :: mat
  TYPE(DOF_) :: dofobj
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  CALL test1
  CALL test2

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

CONTAINS

!---------------------------------------------------------------------------
!                                                                     test1
!---------------------------------------------------------------------------

  SUBROUTINE test1
    order = 1
    CALL obj%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=order, dom=dom)
    CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
                     names=["K"], spaceCompo=[1], timeCompo=[1], &
                     storageFMT=NODES_FMT)
    CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
                           nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
    CALL obj%SetSparsity(mat=mat)

    ! set all entries of the pattern to 1, and convert to a dense matrix
    mat = 1.0_DFP
    found = mat

    IF (ALLOCATED(want)) DEALLOCATE(want)
    ALLOCATE(want(9, 9))
    want = 0.0_DFP
    want(1, [1, 5, 7, 9]) = 1.0_DFP
    want(2, [2, 5, 8, 9]) = 1.0_DFP
    want(3, [3, 6, 7, 9]) = 1.0_DFP
    want(4, [4, 6, 8, 9]) = 1.0_DFP
    want(5, [5, 1, 2, 7, 8, 9]) = 1.0_DFP
    want(6, [6, 3, 4, 7, 8, 9]) = 1.0_DFP
    want(7, [7, 1, 3, 5, 6, 9]) = 1.0_DFP
    want(8, [8, 2, 4, 5, 6, 9]) = 1.0_DFP
    want(9, [9, 1, 2, 3, 4, 5, 6, 7, 8]) = 1.0_DFP

    isok = ALL(SHAPE(found) .EQ. [9, 9])
    IF (isok) THEN
      do_jj: DO jj = 1, 9
        DO ii = 1, 9
          isok = found(ii, jj) .EQ. want(ii, jj)
          IF (.NOT. isok) THEN
            CALL Display([ii, jj], "ii, jj: ")
            EXIT do_jj
          END IF
        END DO
      END DO do_jj
    END IF
    CALL OK(isok, testname // "order 1, all rows")
  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                     test2
!---------------------------------------------------------------------------

  SUBROUTINE test2
    order = 2
    CALL obj%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=order, dom=dom)
    CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
                     names=["K"], spaceCompo=[1], timeCompo=[1], &
                     storageFMT=NODES_FMT)
    CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
                           nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
    CALL obj%SetSparsity(mat=mat)

    mat = 1.0_DFP
    found = mat
    want = found
    want = 0.0_DFP

    want(1, [1, 5, 7, 9, 10, 11, 12, 13, 22]) = 1.0_DFP
    CALL check_row(1, "order 2, row 1")

    want(5, [5, 1, 2, 7, 8, 9, 10, 11, 12, 13, 22, 17, 18, 19, 24]) = &
      1.0_DFP
    CALL check_row(5, "order 2, row 5")

    want(11, [11, 1, 2, 5, 7, 8, 9, 10, 12, 13, 22, 17, 18, 19, 24]) = &
      1.0_DFP
    CALL check_row(11, "order 2, row 11")
  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                 check_row
!---------------------------------------------------------------------------

  SUBROUTINE check_row(irow, msg)
    INTEGER(I4B), INTENT(IN) :: irow
    CHARACTER(*), INTENT(IN) :: msg

    DO jj = 1, SIZE(want, 2)
      isok = found(irow, jj) .EQ. want(irow, jj)
      IF (.NOT. isok) THEN
        CALL Display([irow, jj], "ii, jj: ")
        EXIT
      END IF
    END DO
    CALL OK(isok, testname // msg)
  END SUBROUTINE check_row

END PROGRAM main
