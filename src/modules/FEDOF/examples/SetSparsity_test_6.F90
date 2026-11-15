!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: FEDOFSetSparsity for two FEDOF on two meshes
!
! FEDOFSetSparsity(fedofs, mat) uses SetSparsity4 when two FEDOF are on the
! same mesh, and the cell to cell map of FEDomainConnectivity_ with
! SetSparsity2 when they are on two meshes. This test reads the same mesh
! file into two domains, dom1 and dom2. The two meshes are different
! objects with the same elements, so the cell to cell map is the identity
! and both ways must give the same pattern (same IA and JA).
!
! The FEDOF are u (order 2, on dom1) and p (order 1, on dom1 or dom2):
!
! - rectangle matrix: rows of u, columns of p;
! - block matrix with two physical variables u and p: the blocks (1, 2)
!   and (2, 1) use the cell to cell map when p is on dom2.
!
! Each case is run for a continuous and a broken space.
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
USE AbstractMesh_Class, ONLY: AbstractMesh_
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
USE GlobalData, ONLY: FMT_DOF
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5"
TYPE(FEDOF_), TARGET :: u, p1, p2
TYPE(FEDomain_), TARGET :: dom1, dom2
TYPE(HDF5File_) :: meshfile
CLASS(AbstractMesh_), POINTER :: mesh1, mesh2

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom1%Initiate(meshfile, "")
CALL dom2%Initiate(meshfile, "")

CALL RunCase(isBrokenSpace=math%no)
CALL RunCase(isBrokenSpace=math%yes)

CALL u%DEALLOCATE()
CALL p1%DEALLOCATE()
CALL p2%DEALLOCATE()
CALL dom1%DEALLOCATE()
CALL dom2%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CONTAINS

!----------------------------------------------------------------------------
!                                                                   RunCase
!----------------------------------------------------------------------------

! u is on dom1; p1 is on dom1 (same mesh as u) and p2 on dom2. Compare the
! patterns of (u, p1) and (u, p2) for a rectangle and a block matrix.

SUBROUTINE RunCase(isBrokenSpace)
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  !! broken space or not

  CHARACTER(:), ALLOCATABLE :: label
  TYPE(DOF_) :: idof, jdof, dofobj
  TYPE(CSRMatrix_) :: mat1, mat2
  TYPE(FEDOFPointer_) :: fedofs(2)

  label = "H1"
  IF (isBrokenSpace) label = "broken"

  CALL u%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                  isBrokenSpace=isBrokenSpace, order=2, dom=dom1)
  CALL p1%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                   isBrokenSpace=isBrokenSpace, order=1, dom=dom1)
  CALL p2%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                   isBrokenSpace=isBrokenSpace, order=1, dom=dom2)

  mesh1 => p1%GetMeshPointer()
  mesh2 => p2%GetMeshPointer()
  CALL OK(.NOT. ASSOCIATED(mesh1, mesh2), &
          label//": p1 and p2 are on two meshes")
  CALL OK(p1%GetTotalDOF() .EQ. p2%GetTotalDOF(), &
          label//": p1 and p2 have the same number of DOFs")
  mesh1 => NULL()
  mesh2 => NULL()

  ! rectangle matrix: rows of u, columns of p
  CALL DOFInitiate(obj=idof, tNodes=[u%GetTotalDOF()], names=["U"], &
                   spaceCompo=[1], timeCompo=[1], storageFMT=FMT_DOF)
  CALL DOFInitiate(obj=jdof, tNodes=[p1%GetTotalDOF()], names=["P"], &
                   spaceCompo=[1], timeCompo=[1], storageFMT=FMT_DOF)

  CALL CSRMatrixInitiate(mat1, nrow=DOFSize(idof), ncol=DOFSize(jdof), &
                         idof=idof, jdof=jdof, matrixProp="RECTANGLE")
  fedofs(1)%ptr => u
  fedofs(2)%ptr => p1
  CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat1)

  CALL CSRMatrixInitiate(mat2, nrow=DOFSize(idof), ncol=DOFSize(jdof), &
                         idof=idof, jdof=jdof, matrixProp="RECTANGLE")
  fedofs(2)%ptr => p2
  CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat2)

  CALL CheckSamePattern(mat1, mat2, label//" rectangle")

  ! block matrix with two physical variables u and p
  CALL DOFInitiate(obj=dofobj, &
                   tNodes=[u%GetTotalDOF(), p1%GetTotalDOF()], &
                   names=["U", "P"], spaceCompo=[1, 1], timeCompo=[1, 1], &
                   storageFMT=FMT_DOF)

  CALL CSRMatrixInitiate(mat1, nrow=DOFSize(dofobj), ncol=DOFSize(dofobj), &
                         idof=dofobj, jdof=dofobj)
  fedofs(2)%ptr => p1
  CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat1)

  CALL CSRMatrixInitiate(mat2, nrow=DOFSize(dofobj), ncol=DOFSize(dofobj), &
                         idof=dofobj, jdof=dofobj)
  fedofs(2)%ptr => p2
  CALL FEDOFSetSparsity(fedofs=fedofs, mat=mat2)

  CALL CheckSamePattern(mat1, mat2, label//" block")

  fedofs(1)%ptr => NULL()
  fedofs(2)%ptr => NULL()
END SUBROUTINE RunCase

!----------------------------------------------------------------------------
!                                                          CheckSamePattern
!----------------------------------------------------------------------------

! mat1 is made on one mesh, mat2 on two meshes; check they have the same
! number of nonzeros and the same IA and JA.

SUBROUTINE CheckSamePattern(mat1, mat2, label)
  TYPE(CSRMatrix_), INTENT(IN) :: mat1
  !! pattern made on one mesh
  TYPE(CSRMatrix_), INTENT(IN) :: mat2
  !! pattern made on two meshes
  CHARACTER(*), INTENT(IN) :: label
  !! label of the case

  LOGICAL(LGT) :: isok

  CALL OK(GetNNZ(mat1) .GT. 0_I4B, label//": nonzeros found")
  CALL OK(GetNNZ(mat2) .EQ. GetNNZ(mat1), label//": same nonzeros")

  isok = SIZE(mat2%csr%IA) .EQ. SIZE(mat1%csr%IA)
  IF (isok) isok = ALL(mat2%csr%IA .EQ. mat1%csr%IA)
  CALL OK(isok, label//": same IA")

  isok = SIZE(mat2%csr%JA) .EQ. SIZE(mat1%csr%JA)
  IF (isok) isok = ALL(mat2%csr%JA .EQ. mat1%csr%JA)
  CALL OK(isok, label//": same JA")
END SUBROUTINE CheckSamePattern

END PROGRAM main
