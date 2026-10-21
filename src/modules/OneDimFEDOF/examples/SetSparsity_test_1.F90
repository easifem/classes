!> author: Vikas Sharma, Ph. D.
! date: 2025-06-10
! summary:  Testing SetSparsity method of FEDOF class.

PROGRAM main
USE FEDOF_Class
USE FEMesh_Class
USE AbstractMesh_Class
USE HDF5File_Class
USE Display_Method
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType
USE DOF_Method
USE CSRMatrix_Method
USE ApproxUtility

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = &
                     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"

CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                           baseInterpolation = "Heirarchical", &
            testname = baseContinuity//" "//baseInterpolation//" SetSparsity "

INTEGER(I4B), PARAMETER :: nsd = 2

INTEGER(I4B) :: ii, jj, order

TYPE(CSRMatrix_) :: mat
TYPE(DOF_) :: dofobj
TYPE(FEDOF_) :: obj
TYPE(FEMesh_) :: mesh
TYPE(HDF5File_) :: meshfile
REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)

CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL mesh%Initiate(meshfile, dim=nsd)

CALL test1
CALL test2

!CALL mesh%Display("domain:")
CALL mesh%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CONTAINS

SUBROUTINE test1

  order = 1
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=mesh)

  CALL Initiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
                names=['K'], spaceCompo=[1], timeCompo=[1], &
                storageFMT=NODES_FMT)

  CALL Initiate(mat, ncol=SIZE(dofobj), nrow=SIZE(dofobj), &
                idof=dofobj, jdof=dofobj)

  CALL obj%SetSparsity(mat=mat)

  !! setting all entries to 1.0
  mat = 1.0_DFP

  !! converting sparse matrix to dense matrix
  !! for testing
  found = mat

  ALLOCATE (want(9, 9))
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

  do_jj: DO jj = 1, SIZE(want, 2)
    DO ii = 1, SIZE(want, 1)
      isok = found(ii, jj) .EQ. want(ii, jj)
      IF (.NOT. isok) THEN
        CALL Display([ii, jj], "ii, jj: ")
        EXIT do_jj

      END IF
    END DO
  END DO do_jj

  CALL OK(isok, testname//" test1")

END SUBROUTINE test1

!----------------------------------------------------------------------------
!                                                                 test2
!----------------------------------------------------------------------------

SUBROUTINE test2

  order = 2

  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=mesh)

  CALL Initiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
                names=['K'], spaceCompo=[1], timeCompo=[1], &
                storageFMT=NODES_FMT)

  CALL Initiate(mat, ncol=SIZE(dofobj), nrow=SIZE(dofobj), &
                idof=dofobj, jdof=dofobj)

  CALL obj%SetSparsity(mat=mat)

  mat = 1.0_DFP

  found = mat

  want = found
  want = 0.0_DFP

  want(1, [1, 5, 7, 9, 10, 11, 12, 13, 22]) = 1.0_DFP
  ii = 1
  DO jj = 1, SIZE(want, 2)
    isok = found(ii, jj) .EQ. want(ii, jj)
    IF (.NOT. isok) THEN
      CALL Display([ii, jj], "ii, jj: ")
      EXIT
    END IF
  END DO

  CALL OK(isok, testname//" test2")

  want(5, [5, 1, 2, 7, 8, 9, 10, 11, 12, 13, 22, 17, 18, 19, 24]) = 1.0_DFP
  ii = 5
  DO jj = 1, SIZE(want, 2)
    isok = found(ii, jj) .EQ. want(ii, jj)
    IF (.NOT. isok) THEN
      CALL Display([ii, jj], "ii, jj: ")
      EXIT
    END IF
  END DO
  CALL OK(isok, testname//" test2")

  want(11, [11, 1, 2, 5, 7, 8, 9, 10, 12, 13, 22, 17, 18, 19, 24]) = 1.0_DFP
  ii = 11
  DO jj = 1, SIZE(want, 2)
    isok = found(ii, jj) .EQ. want(ii, jj)
    IF (.NOT. isok) THEN
      CALL Display([ii, jj], "ii, jj: ")
      EXIT
    END IF
  END DO
  CALL OK(isok, testname//" test2")

END SUBROUTINE test2

!----------------------------------------------------------------------------
!                                                                     test3
!----------------------------------------------------------------------------

END PROGRAM main

! mesh info:
! ==============================
! total nodes: 9
! total elements: 4
! tEdges: 0
! tFaces: 12
