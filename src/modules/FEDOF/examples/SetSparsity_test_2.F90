!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Set the sparsity pattern of a matrix for a list of FEDOF
!
! FEDOFSetSparsity(fedofs, mat) sets the sparsity pattern of mat for a list
! of FEDOF (one per physical variable). With one FEDOF it gives the same
! pattern as obj%SetSparsity(mat). For H1 hierarchical basis of order 1 on
! the quad mesh the pattern has 4*4 + 4*6 + 9 = 49 nonzeros.
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

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
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
    CALL obj%Initiate(baseContinuity="H1", &
                      baseInterpolation="Hierarchical", order=order, &
                      dom=dom)
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
    IF (order .EQ. 1) CALL OK(GetNNZ(mat1) .EQ. 49_I4B, &
                              "order 1: 49 nonzeros")
  END DO

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
