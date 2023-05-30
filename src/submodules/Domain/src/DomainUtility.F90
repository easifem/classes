! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE DomainUtility
USE BaseType
USE BaseMethod
USE Mesh_Class
USE Domain_Class
USE DomainConnectivity_Class
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
IMPLICIT NONE
PRIVATE

PUBLIC :: SetSparsity1
PUBLIC :: SetSparsity2
PUBLIC :: SetSparsity3

CHARACTER(*), PARAMETER :: modName = "DomainUtility"

INTERFACE Initiate
  MODULE PROCEDURE InitiateDomainPointer1
END INTERFACE Initiate

PUBLIC :: Initiate

CONTAINS

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

SUBROUTINE SetSparsity1(obj, mat)
  !
  CLASS(Domain_), INTENT(IN) :: obj
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  !
  INTEGER(I4B) :: imesh, dim, tmesh, lb, ub
  CLASS(Mesh_), POINTER :: meshobj
  !
  ! main
  !
  meshobj => NULL()
  lb = LBOUND(obj%local_nptrs, 1)
  ub = UBOUND(obj%local_nptrs, 1)
  !
  DO dim = 1, 3
    tmesh = obj%getTotalMesh(dim=dim)
    DO imesh = 1, tmesh
      meshobj => obj%getMeshPointer(dim=dim, entityNum=imesh)
      IF (ASSOCIATED(meshobj)) &
      & CALL meshobj%setSparsity(mat=mat, &
      & localNodeNumber=obj%local_nptrs, lbound=lb, ubound=ub)
    END DO
  END DO
  !
  CALL setSparsity(mat)
  !
  NULLIFY (meshobj)
  !
END SUBROUTINE SetSparsity1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary:  Set sparsity for sparse matrix of BlockMatrixField

SUBROUTINE SetSparsity2(domains, mat)
  CLASS(DomainPointer_), INTENT(IN) :: domains(:)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  INTEGER(I4B) :: rowMeshID, colMeshID, rowMeshSize, colMeshSize,  &
    & ivar, jvar, nsd(SIZE(domains))
  CLASS(Mesh_), POINTER :: rowMesh, colMesh
  CLASS(Domain_), POINTER :: rowDomain, colDomain
  TYPE(DomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  CHARACTER(*), PARAMETER :: myName = "SetSparsity2"
  TYPE(BoundingBox_) :: row_box, col_box
  LOGICAL(LGT) :: is_intersect
  !
  ! check
  !

  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & '[START] SetSparsity2()')

  DO ivar = 1, SIZE(domains)
    nsd(ivar) = domains(ivar)%ptr%getNSD()
  END DO
  !
  ! nullify first for safety
  !
  rowMesh => NULL(); 
  colMesh => NULL(); 
  rowDomain => NULL(); 
  colDomain => NULL()
  !
  DO ivar = 1, SIZE(domains)

    CALL Display("row domain = "//tostring(ivar))

    rowDomain => domains(ivar)%ptr
    rowMeshSize = rowDomain%getTotalMesh(dim=nsd(ivar))

    DO jvar = 1, SIZE(domains)

      CALL Display("col domain = "//tostring(jvar))

      colDomain => domains(jvar)%ptr

      CALL Display("calling DomainConnectivity_::domainConn%Deallocate()")

      CALL domainConn%DEALLOCATE()

      CALL Display("calling DomainConnectivity_::domainConn%InitiateNodeToNodeData()")

      CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
        & domain2=colDomain)

      nodeToNode => domainConn%getNodeToNodePointer()
      colMeshSize = colDomain%getTotalMesh(dim=nsd(jvar))

      DO rowMeshID = 1, rowMeshSize

        rowMesh => rowDomain%getMeshPointer(dim=nsd(ivar), &
          & entityNum=rowMeshID)

        IF (.NOT. ASSOCIATED(rowMesh)) CYCLE

        row_box = rowMesh%getBoundingBox()

        DO colMeshID = 1, colMeshSize

          colMesh => colDomain%getMeshPointer(dim=nsd(jvar), &
            & entityNum=colMeshID)

          IF (ASSOCIATED(colMesh)) THEN
            col_box = colMesh%getBoundingBox()

            is_intersect = row_box.isIntersect.col_box

            IF (is_intersect) THEN
              CALL rowMesh%SetSparsity(mat=mat, &
               & colMesh=colMesh, &
               & nodeToNode=nodeToNode, &
               & rowGlobalToLocalNodeNum=rowDomain%local_nptrs, &
               & rowLBOUND=LBOUND(rowDomain%local_nptrs, 1), &
               & rowUBOUND=UBOUND(rowDomain%local_nptrs, 1), &
               & colGlobalToLocalNodeNum=colDomain%local_nptrs, &
               & colLBOUND=LBOUND(colDomain%local_nptrs, 1), &
               & colUBOUND=UBOUND(colDomain%local_nptrs, 1), &
               & ivar=ivar, jvar=jvar)
            END IF
          END IF

        END DO
      END DO
    END DO
  END DO
  !
  CALL Display("Calling SetSparsity()")
  CALL setSparsity(mat)
  !
  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)
  !
  CALL Display("Calling DomainConnectivity_::domainConn%Deallocate()")
  CALL domainConn%DEALLOCATE()
  !
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & '[END] SetSparsity2()')
END SUBROUTINE SetSparsity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary:  Set sparsity for sparse matrix of MatrixField, which is rectangle

SUBROUTINE SetSparsity3(domains, mat)
  CLASS(DomainPointer_), INTENT(IN) :: domains(2)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  !
  INTEGER(I4B), PARAMETER :: tvar = 2, ivar = 1, jvar = 1
  INTEGER(I4B) :: rowMeshID, colMeshID, rowMeshSize, colMeshSize,  &
    & nsd(tvar), ii
  CLASS(Mesh_), POINTER :: rowMesh, colMesh
  CLASS(Domain_), POINTER :: rowDomain, colDomain
  TYPE(DomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  !
  ! check
  !
  DO ii = 1, tvar
    nsd(ii) = domains(ii)%ptr%getNSD()
  END DO
  !
  ! nullify first for safety
  !
  rowMesh => NULL(); 
  colMesh => NULL(); 
  rowDomain => domains(1)%ptr
  colDomain => domains(2)%ptr
  !
  rowMeshSize = rowDomain%getTotalMesh(dim=nsd(ivar))
  colMeshSize = colDomain%getTotalMesh(dim=nsd(jvar))
  !
  CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
    & domain2=colDomain)
  !
  nodeToNode => domainConn%getNodeToNodePointer()
  !
  DO rowMeshID = 1, rowMeshSize
    !
    rowMesh => rowDomain%getMeshPointer(dim=nsd(ivar), &
      & entityNum=rowMeshID)
    !
    IF (.NOT. ASSOCIATED(rowMesh)) CYCLE
    !
    DO colMeshID = 1, colMeshSize
      !
      colMesh => colDomain%getMeshPointer(dim=nsd(jvar), &
        & entityNum=colMeshID)
      !
      IF (ASSOCIATED(colMesh)) &
        & CALL rowMesh%SetSparsity(mat=mat, &
        & colMesh=colMesh, &
        & nodeToNode=nodeToNode, &
        & rowGlobalToLocalNodeNum=rowDomain%local_nptrs, &
        & rowLBOUND=LBOUND(rowDomain%local_nptrs, 1), &
        & rowUBOUND=UBOUND(rowDomain%local_nptrs, 1), &
        & colGlobalToLocalNodeNum=colDomain%local_nptrs, &
        & colLBOUND=LBOUND(colDomain%local_nptrs, 1), &
        & colUBOUND=UBOUND(colDomain%local_nptrs, 1), &
        & ivar=ivar, jvar=jvar)
      !
    END DO
  END DO
  !
  CALL setSparsity(mat)
  !
  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)
  !
  CALL domainConn%DEALLOCATE()
  !
END SUBROUTINE SetSparsity3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         2022-12-02
! summary:         Initiate domain pointer from file names
!
!# Introduction
!
!- Initiate domain pointer from the file names.
!- Create HDF5 file object, open it, read data and close those files.
!- Please allocate obj before using it
!- The size of `obj` should be same as the size of filename.

SUBROUTINE InitiateDomainPointer1(domains, filename, group)
  CLASS(DomainPointer_), INTENT(INOUT) :: domains(:)
  ! domain pointers to be created
  TYPE(String), INTENT(IN) :: filename(:)
  ! filenames for domain
  TYPE(String), OPTIONAL, INTENT(IN) :: group(:)
  ! Path (address) in filename
  !
  ! Internal variables
  !
  CHARACTER(*), PARAMETER :: myName = "InitiateDomainPointer1"
  TYPE(HDF5File_) :: domainFile
  INTEGER(I4B) :: ii, n
  !
  ! check
  !
  IF (SIZE(filename) .NE. SIZE(domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'SIZE of domains should be same as the size of filename')
  END IF
  !
  ! check
  !
  IF (PRESENT(group)) THEN
    IF (SIZE(group) .NE. SIZE(filename)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'SIZE of group should be same as the size of filename')
    END IF
  END IF
  !
  ! check
  !
  n = SIZE(filename)
  !
  DO ii = 1, n
    IF (ASSOCIATED(domains(ii)%ptr)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'domains( '//tostring(ii)//' ) is already associated.')
    END IF
  END DO
  !
  ! compute
  !
  DO ii = 1, SIZE(filename)
    CALL domainFile%Initiate(filename=filename(ii)%chars(), MODE="READ")
    CALL domainFile%OPEN()
    IF (PRESENT(group)) THEN
      domains(ii)%ptr => Domain_Pointer(hdf5=domainFile, &
        & group=group(ii)%chars())
    ELSE
      domains(ii)%ptr => Domain_Pointer(hdf5=domainFile, group="")
    END IF
    CALL domainFile%DEALLOCATE()
  END DO
  !
END SUBROUTINE InitiateDomainPointer1

END MODULE DomainUtility
