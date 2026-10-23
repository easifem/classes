!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the sparsity pattern of a matrix for a list of domains
!
! AbstractDomainSetSparsity sets the sparsity pattern of mat for a
! list of domains (one per physical variable). It uses the node to
! node map between the domains (FEDomainConnectivity_). With one
! domain it gives the same 49 nonzeros as SetSparsity.
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
  USE AbstractDomain_Class, ONLY: AbstractDomainPointer_
  USE AbstractDomain_Class, ONLY: AbstractDomainSetSparsity
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
  USE BaseType, ONLY: TypeDOFOpt
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_), TARGET :: obj
  TYPE(DOF_) :: dofobj
  TYPE(CSRMatrix_) :: mat
  TYPE(AbstractDomainPointer_) :: domains(1)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalNodes()], &
                   names=["K"], spacecompo=[1_I4B], &
                   timecompo=[1_I4B], storagefmt=TypeDOFOpt%storageFormatDOF)
  CALL CSRMatrixInitiate(obj=mat, ncol=DOFSize(dofobj), &
                         nrow=DOFSize(dofobj), idof=dofobj, &
                         jdof=dofobj)
  domains(1)%ptr => obj
  CALL AbstractDomainSetSparsity(domains=domains, mat=mat)
  CALL OK(GetNNZ(mat) .EQ. 49_I4B, "number of nonzeros")
  domains(1)%ptr => NULL()

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
