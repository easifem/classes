!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the sparsity pattern with a node number map
!
! With localNodeNumber, row and column i of the matrix is the node
! with localNodeNumber(node) = i. Here the map is the identity.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
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
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: NODES_FMT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  TYPE(DOF_) :: dofobj
  TYPE(CSRMatrix_) :: mat
  INTEGER(I4B) :: nmap(9)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalNodes()], &
    names=["K"], spacecompo=[1_I4B], &
    timecompo=[1_I4B], storagefmt=NODES_FMT)
  CALL CSRMatrixInitiate(obj=mat, ncol=DOFSize(dofobj), &
    nrow=DOFSize(dofobj), idof=dofobj, &
    jdof=dofobj)
  nmap = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  CALL obj%SetSparsity(mat=mat, localNodeNumber=nmap, LBOUND=1_I4B, &
    UBOUND=9_I4B)
  CALL CSRMatrixSetSparsity(mat)
  CALL OK(GetNNZ(mat) .EQ. 49_I4B, "number of nonzeros")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
