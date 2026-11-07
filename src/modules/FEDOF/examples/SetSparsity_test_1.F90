!> author: Vikas Sharma, Ph. D.
! date: 2025-06-10
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
!
! Changes:
!   - 2026-10-24: Use FEDomain_ (dom=) instead of FEMesh_ (mesh=)

PROGRAM main
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeDOFOpt
USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
USE CSRMatrix_Method, ONLY: ASSIGNMENT(=)
USE Display_Method, ONLY: Display
USE DOF_Method, ONLY: DOFInitiate => Initiate
USE DOF_Method, ONLY: DOFSize => SIZE
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = &
                           "./meshdata/very_small_quad4_mesh.h5"
INTEGER(I4B) :: ii, jj, order
TYPE(CSRMatrix_) :: mat
TYPE(DOF_) :: dofobj
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, "")

! Test1: H1, HIER, no brokenSpace
BLOCK
  REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
  LOGICAL(LGT) :: isok
  order = 1
  CALL obj%Initiate(baseContinuity="H1", &
                    baseInterpolation="HIER", &
                    isBrokenSpace=math%no, &
                    order=order, dom=dom)

  CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
                   names=["K"], spaceCompo=[1], timeCompo=[1], &
                   storageFMT=TypeDOFOpt%storageFormatDOF)

  CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
                         nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)

  CALL OK(math%yes, "debug1")
  CALL obj%SetSparsity(mat=mat)
  CALL OK(math%yes, "debug2")

  ! set all entries of the pattern to 1, and convert to a dense matrix
  mat = 1.0_DFP
  found = mat

  IF (ALLOCATED(want)) DEALLOCATE (want)
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

  isok = ALL(SHAPE(found) .EQ. [9, 9])
  IF (isok) THEN
    do_jj: DO jj = 1, 9
      do_ii: DO ii = 1, 9
        isok = found(ii, jj) .EQ. want(ii, jj)
        IF (.NOT. isok) THEN
          ! CALL Display([ii, jj], "ii, jj: ")
          EXIT do_jj
        END IF
      END DO do_ii
    END DO do_jj
  END IF
  CALL OK(isok, obj%GetCaseName()//" order 1, all rows")
END BLOCK

! BLOCK
!   REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
!   INTEGER(I4B) :: irow
!   LOGICAL(LGT) :: isok
!   order = 2
!   CALL obj%Initiate(baseContinuity="H1", &
!                     baseInterpolation="HIER", &
!                     isBrokenSpace=math%no, &
!                     order=order, dom=dom)
!   CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
!                    names=["K"], spaceCompo=[1], timeCompo=[1], &
!                    storageFMT=TypeDOFOpt%storageFormatNodes)
!   CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
!                          nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
!   CALL obj%SetSparsity(mat=mat)

!   mat = 1.0_DFP
!   found = mat
!   want = found
!   want = 0.0_DFP

!   irow = 1
!   want(irow, [1, 5, 7, 9, 10, 11, 12, 13, 22]) = 1.0_DFP
!   DO jj = 1, SIZE(want, 2)
!     isok = found(irow, jj) .EQ. want(irow, jj)
!     IF (.NOT. isok) THEN
!       EXIT
!     END IF
!   END DO
!   CALL OK(isok, obj%GetCaseName()//" order=2 ")

!   irow = 5
!   want(irow, [5, 1, 2, 7, 8, 9, 10, 11, 12, 13, 22, 17, 18, 19, 24]) = 1.0_DFP
!   DO jj = 1, SIZE(want, 2)
!     isok = found(irow, jj) .EQ. want(irow, jj)
!     IF (.NOT. isok) THEN
!       EXIT
!     END IF
!   END DO
!   CALL OK(isok, obj%GetCaseName()//" order=2 ")

!   want(11, [11, 1, 2, 5, 7, 8, 9, 10, 12, 13, 22, 17, 18, 19, 24]) = 1.0_DFP
!   DO jj = 1, SIZE(want, 2)
!     isok = found(irow, jj) .EQ. want(irow, jj)
!     IF (.NOT. isok) THEN
!       EXIT
!     END IF
!   END DO
!   CALL OK(isok, obj%GetCaseName()//" order=2 ")
! END BLOCK

! testing brokenSpace onlyCell=true
! BLOCK
!   REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
!   LOGICAL(LGT) :: isok
!   order = 1
!   CALL obj%Initiate(baseContinuity="H1", &
!                     baseInterpolation="HIER", &
!                     isBrokenSpace=math%yes, &
!                     order=order, dom=dom)

!   CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
!                    names=["K"], spaceCompo=[1], timeCompo=[1], &
!                    storageFMT=TypeDOFOpt%storageFormatDOF)

!   CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), &
!                          nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
!   CALL obj%SetSparsity(mat=mat, onlyCell=math%yes)

!   ! set all entries of the pattern to 1, and convert to a dense matrix
!   mat = 1.0_DFP
!   found = mat

!   CALL Reallocate(want, 16, 16)
!   want(1, [1, 2, 3, 4]) = 1.0_DFP
!   want(2, [1, 2, 3, 4]) = 1.0_DFP
!   want(3, [1, 2, 3, 4]) = 1.0_DFP
!   want(4, [1, 2, 3, 4]) = 1.0_DFP
!   want(5, [5, 6, 7, 8]) = 1.0_DFP
!   want(6, [5, 6, 7, 8]) = 1.0_DFP
!   want(7, [5, 6, 7, 8]) = 1.0_DFP
!   want(8, [5, 6, 7, 8]) = 1.0_DFP
!   want(9, [9, 10, 11, 12]) = 1.0_DFP
!   want(10, [9, 10, 11, 12]) = 1.0_DFP
!   want(11, [9, 10, 11, 12]) = 1.0_DFP
!   want(12, [9, 10, 11, 12]) = 1.0_DFP
!   want(13, [13, 14, 15, 16]) = 1.0_DFP
!   want(14, [13, 14, 15, 16]) = 1.0_DFP
!   want(15, [13, 14, 15, 16]) = 1.0_DFP
!   want(16, [13, 14, 15, 16]) = 1.0_DFP

!   isok = ALL(SHAPE(found) .EQ. [16, 16])
!   CALL OK(isok, &
!           obj%GetCaseName()//" BrokenSpaces Shape of sparse matrix is correct")
!   IF (isok) THEN
!     do_jj: DO jj = 1, SIZE(want, 2)
!       do_ii: DO ii = 1, SIZE(want, 1)
!         isok = found(ii, jj) .EQ. want(ii, jj)
!         IF (.NOT. isok) EXIT do_jj
!       END DO do_ii
!     END DO do_jj
!   END IF
!   CALL OK(isok, obj%GetCaseName()//" BrokenSpaces order 1, SetSparsity")
! END BLOCK

! testing brokenSpace onlyCell=false
! BLOCK
!   REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
!   LOGICAL(LGT) :: isok
!   order = 1
!   CALL obj%Initiate(baseContinuity="H1", &
!                     baseInterpolation="HIER", &
!                     isBrokenSpace=math%yes, &
!                     order=order, dom=dom)

!   CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
!                    names=["K"], spaceCompo=[1], timeCompo=[1], &
!                    storageFMT=TypeDOFOpt%storageFormatDOF)

!   CALL CSRMatrixInitiate(mat, ncol=DOFSize(dofobj), nrow=DOFSize(dofobj), &
!                          idof=dofobj, jdof=dofobj)

!   CALL obj%SetSparsity(mat=mat, onlyCell=math%no)

!   ! set all entries of the pattern to 1, and convert to a dense matrix
!   mat = 1.0_DFP
!   found = mat

!   CALL Reallocate(want, 16, 16)
!   want(Arange(1, 4), Arange(1, 4)) = math%one
!   want(Arange(1, 4), Arange(5, 8)) = math%one
!   want(Arange(1, 4), Arange(9, 12)) = math%one
!   want(Arange(5, 8), Arange(1, 4)) = math%one
!   want(Arange(5, 8), Arange(5, 8)) = math%one
!   want(Arange(5, 8), Arange(13, 16)) = math%one
!   want(Arange(9, 12), Arange(1, 4)) = math%one
!   want(Arange(9, 12), Arange(9, 12)) = math%one
!   want(Arange(9, 12), Arange(13, 16)) = math%one
!   want(Arange(13, 16), Arange(5, 8)) = math%one
!   want(Arange(13, 16), Arange(9, 12)) = math%one
!   want(Arange(13, 16), Arange(13, 16)) = math%one

!   isok = ALL(SHAPE(found) .EQ. [16, 16])
!   CALL OK(isok, &
!           obj%GetCaseName()//", BrokenSpaces, onlyCell=false, Shape of sparse matrix is correct")
!   IF (isok) THEN
!     do_jj: DO jj = 1, SIZE(want, 2)
!       do_ii: DO ii = 1, SIZE(want, 1)
!         isok = found(ii, jj) .EQ. want(ii, jj)
!         IF (.NOT. isok) EXIT do_jj
!       END DO do_ii
!     END DO do_jj
!   END IF
!   CALL OK(isok, &
!           obj%GetCaseName()//", BrokenSpaces, onlyCell=false, order 1, SetSparsity")
! END BLOCK

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
