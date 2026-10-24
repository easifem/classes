!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the sparsity pattern of a matrix
!
! SetSparsity sets the sparsity pattern of mat from the mesh of
! dimension nsd and finishes the pattern. The quad mesh gives
! 4*4 + 4*6 + 9 = 49 nonzeros.
!
! The mesh very_small_quad4_mesh.h5 is the unit square: four
! Quadrangle4 elements (13 to 16), eight Line2 elements (5 to 12) on
! the boundary curves, four Point elements (1 to 4), and nine nodes:
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
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: NODES_FMT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  TYPE(DOF_) :: dofobj
  TYPE(CSRMatrix_) :: mat

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalNodes()], &
                   names=["K"], spacecompo=[1_I4B], &
                   timecompo=[1_I4B], storagefmt=NODES_FMT)
  CALL CSRMatrixInitiate(obj=mat, ncol=DOFSize(dofobj), &
                         nrow=DOFSize(dofobj), idof=dofobj, &
                         jdof=dofobj)
  CALL obj%SetSparsity(mat=mat)
  CALL OK(GetNNZ(mat) .EQ. 49_I4B, "number of nonzeros")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
