!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing FEDOFSetSparsity for a broken space
!
! For one FEDOF of a broken space, FEDOFSetSparsity must give the same
! pattern as SetSparsity: a cell is coupled to itself and to its neighbour
! cells.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four Quadrangle4
! elements. Every cell has two neighbours, and a cell of order p owns
! (p+1)**2 DOFs, so the pattern has 4 * 3 * (p+1)**4 nonzeros.

PROGRAM main
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: math => TypeMathOpt
USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
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
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5"
TYPE(FEDOF_), TARGET :: obj
TYPE(FEDOFPointer_) :: fedofs(1)
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
TYPE(DOF_) :: dofobj
TYPE(CSRMatrix_) :: mat1, mat2
INTEGER(I4B) :: order

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, "")

DO order = 1, 2
  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    isBrokenSpace=math%yes, order=order, dom=dom)
  CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalDOF()], &
                   names=["K"], spaceCompo=[1], timeCompo=[1], &
                   storageFMT=FMT_DOF)

  ! one FEDOF: type-bound SetSparsity
  CALL CSRMatrixInitiate(mat1, ncol=DOFSize(dofobj), &
                         nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
  CALL obj%SetSparsity(mat=mat1)

  ! list of FEDOF: FEDOFSetSparsity
  CALL CSRMatrixInitiate(mat2, ncol=DOFSize(dofobj), &
                         nrow=DOFSize(dofobj), idof=dofobj, jdof=dofobj)
  fedofs(1)%ptr => obj
  CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat2)
  fedofs(1)%ptr => NULL()

  CALL OK(GetNNZ(mat2) .EQ. GetNNZ(mat1), &
          "FEDOFSetSparsity: same nonzeros as SetSparsity")
  CALL OK(GetNNZ(mat2) .EQ. 12_I4B * (order + 1)**4, &
          "FEDOFSetSparsity: nonzeros of a cell and its neighbours")
END DO

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
