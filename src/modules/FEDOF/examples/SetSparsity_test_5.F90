!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Set the sparsity pattern between two FEDOF on the same mesh
!
! obj%SetSparsity(col_fedof, mat, ivar, jvar) connects the DOFs of each
! element of obj (rows) to the DOFs of the same element of col_fedof
! (columns). It does not finish the pattern, so the caller calls
! CSRMatrix SetSparsity(mat) at the end. The checks are:
!
! - same FEDOF in rows and columns: same pattern as obj%SetSparsity(mat)
!   for a continuous and a broken space;
! - two FEDOF (order 2 rows, order 1 columns) in a rectangle matrix: same
!   pattern as FEDOFSetSparsity, which uses the cell to cell map.
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
USE CSRMatrix_Method, ONLY: CSRMatrixSetSparsity => SetSparsity
USE CSRMatrix_Method, ONLY: GetNNZ
USE DOF_Method, ONLY: DOFInitiate => Initiate
USE DOF_Method, ONLY: DOFSize => SIZE
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDOF_Class, ONLY: FEDOFSetSparsity
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: FMT_DOF
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5"
TYPE(FEDOF_), TARGET :: obj, col_fedof
TYPE(FEDOFPointer_) :: fedofs(2)
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
TYPE(DOF_) :: idof, jdof
TYPE(CSRMatrix_) :: mat1, mat2

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, "")

! same FEDOF in rows and columns
CALL CheckSame(isBrokenSpace=math%no, order=1, nnz=49_I4B)
CALL CheckSame(isBrokenSpace=math%no, order=2, nnz=-1_I4B)
CALL CheckSame(isBrokenSpace=math%yes, order=1, nnz=12_I4B * 2**4)
CALL CheckSame(isBrokenSpace=math%yes, order=2, nnz=12_I4B * 3**4)

! two FEDOF on the same mesh: order 2 rows, order 1 columns
CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                  order=2, dom=dom)
CALL col_fedof%Initiate(baseContinuity="H1", &
                        baseInterpolation="Hierarchical", order=1, dom=dom)
CALL DOFInitiate(obj=idof, tNodes=[obj%GetTotalDOF()], names=["V"], &
                 spaceCompo=[1], timeCompo=[1], storageFMT=FMT_DOF)
CALL DOFInitiate(obj=jdof, tNodes=[col_fedof%GetTotalDOF()], names=["P"], &
                 spaceCompo=[1], timeCompo=[1], storageFMT=FMT_DOF)

CALL CSRMatrixInitiate(mat1, nrow=DOFSize(idof), ncol=DOFSize(jdof), &
                       idof=idof, jdof=jdof, matrixProp="RECTANGLE")
CALL obj%SetSparsity(col_fedof=col_fedof, mat=mat1, ivar=1, jvar=1)
CALL CSRMatrixSetSparsity(mat1)

CALL CSRMatrixInitiate(mat2, nrow=DOFSize(idof), ncol=DOFSize(jdof), &
                       idof=idof, jdof=jdof, matrixProp="RECTANGLE")
fedofs(1)%ptr => obj
fedofs(2)%ptr => col_fedof
CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat2)
fedofs(1)%ptr => NULL()
fedofs(2)%ptr => NULL()

CALL OK(GetNNZ(mat1) .EQ. GetNNZ(mat2), &
        "rectangle: same nonzeros as FEDOFSetSparsity")
! each element: 9 row DOFs times 4 column DOFs; rows of shared DOFs
! give duplicates, so nnz is smaller than 4 * 9 * 4
CALL OK(GetNNZ(mat1) .GT. 0_I4B .AND. GetNNZ(mat1) .LT. 144_I4B, &
        "rectangle: nonzeros between 0 and 4 * 9 * 4")

CALL obj%DEALLOCATE()
CALL col_fedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CONTAINS

!----------------------------------------------------------------------------
!                                                                 CheckSame
!----------------------------------------------------------------------------

! Compare obj%SetSparsity(col_fedof=obj, ...) with obj%SetSparsity(mat).
! If nnz is positive, also check the number of nonzeros.

SUBROUTINE CheckSame(isBrokenSpace, order, nnz)
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  !! broken space or not
  INTEGER(I4B), INTENT(IN) :: order
  !! order of the space
  INTEGER(I4B), INTENT(IN) :: nnz
  !! expected number of nonzeros; not checked if negative

  CHARACTER(:), ALLOCATABLE :: label
  TYPE(DOF_) :: dofobj
  TYPE(CSRMatrix_) :: matA, matB

  label = "H1"
  IF (isBrokenSpace) label = "broken"
  label = label//" order "//CHAR(ICHAR("0") + order)

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    isBrokenSpace=isBrokenSpace, order=order, dom=dom)
  CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], names=["K"], &
                   spaceCompo=[1], timeCompo=[1], storageFMT=FMT_DOF)

  CALL CSRMatrixInitiate(matA, ncol=DOFSize(dofobj), nrow=DOFSize(dofobj), &
                         idof=dofobj, jdof=dofobj)
  CALL obj%SetSparsity(mat=matA)

  CALL CSRMatrixInitiate(matB, ncol=DOFSize(dofobj), nrow=DOFSize(dofobj), &
                         idof=dofobj, jdof=dofobj)
  CALL obj%SetSparsity(col_fedof=obj, mat=matB, ivar=1, jvar=1)
  CALL CSRMatrixSetSparsity(matB)

  CALL OK(GetNNZ(matB) .EQ. GetNNZ(matA), &
          label//": same nonzeros as SetSparsity(mat)")
  IF (nnz .GT. 0) CALL OK(GetNNZ(matB) .EQ. nnz, &
                          label//": "//"expected nonzeros")
END SUBROUTINE CheckSame

END PROGRAM main
