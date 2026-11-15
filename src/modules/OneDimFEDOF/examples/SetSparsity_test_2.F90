!> author: Vikas Sharma, Ph. D.
! date: 2025-06-10
! summary:  This subroutine tests the SetSparsity method of the FEDOF class.

PROGRAM main
USE FEDOF_Class
USE FEMesh_Class
USE AbstractMesh_Class
USE HDF5File_Class
USE Display_Method
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class
USE BaseType
USE DOF_Method
USE CSRMatrix_Method
USE ApproxUtility

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = &
                     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"

CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                           baseInterpolation = "Heirarchical"

INTEGER(I4B), PARAMETER :: order = 2, nsd = 2

INTEGER(I4B) :: ii, jj

TYPE(CSRMatrix_) :: mat
TYPE(DOF_) :: dofobj
TYPE(FEDOF_) :: obj
TYPE(FEMesh_) :: mesh
TYPE(HDF5File_) :: meshfile
REAL(DFP), ALLOCATABLE :: found(:, :), want(:, :)
LOGICAL(LGT) :: isok
INTEGER(I4B), ALLOCATABLE :: nptrs(:)

CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()

CALL mesh%Initiate(meshfile, dim=nsd)

CALL mesh%DisplayMeshInfo("mesh info:")

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

CALL OK(isok, "CSRMatrix_Method: SetSparsity")

want(5, [5, 1, 2, 7, 8, 9, 10, 11, 12, 13, 22, 17, 18, 19, 24]) = 1.0_DFP
ii = 5
DO jj = 1, SIZE(want, 2)
  isok = found(ii, jj) .EQ. want(ii, jj)
  IF (.NOT. isok) THEN
    CALL Display([ii, jj], "ii, jj: ")
    EXIT
  END IF
END DO
CALL OK(isok, "CSRMatrix_Method: SetSparsity")

want(11, [11, 1, 2, 5, 7, 8, 9, 10, 12, 13, 22, 17, 18, 19, 24]) = 1.0_DFP
ii = 11
DO jj = 1, SIZE(want, 2)
  isok = found(ii, jj) .EQ. want(ii, jj)
  IF (.NOT. isok) THEN
    CALL Display([ii, jj], "ii, jj: ")
    EXIT
  END IF
END DO
CALL OK(isok, "CSRMatrix_Method: SetSparsity")

! CALL Display(INT(found), "sparse matrix:", full=.TRUE.)
! DO ii = 1, mesh%GetTotalElements()
!   nptrs = obj%GetConnectivity(globalElement=ii, islocal=.TRUE., opt="A")
!   CALL Display(nptrs, "connectivity for iel ("//tostring(ii)//"): ", orient="ROW", full=.TRUE.)
! END DO

!CALL mesh%Display("domain:")
CALL mesh%DEALLOCATE()
CALL meshfile%DEALLOCATE()

END PROGRAM main
