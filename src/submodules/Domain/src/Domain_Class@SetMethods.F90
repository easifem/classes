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

SUBMODULE(Domain_Class) SetMethods
USE BaseMethod
USE DomainConnectivity_Class
USE DomainUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetSparsity1
INTEGER(I4B) :: imesh, dim, tmesh, lb, ub
CLASS(Mesh_), POINTER :: meshobj

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Domain_SetSparsity1()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Domain is not initiated, first initiate")
END IF
#endif

! Call SetSparsity1 from DomainUtility
! CALL SetSparsity1(obj=obj, mat=mat)

meshobj => NULL()
lb = LBOUND(obj%local_nptrs, 1)
ub = UBOUND(obj%local_nptrs, 1)

DO dim = 1, 3
  tmesh = obj%getTotalMesh(dim=dim)
  DO imesh = 1, tmesh
    meshobj => obj%getMeshPointer(dim=dim, entityNum=imesh)
    IF (ASSOCIATED(meshobj)) &
      CALL meshobj%setSparsity(mat=mat, &
                        localNodeNumber=obj%local_nptrs, lbound=lb, ubound=ub)
  END DO
END DO

CALL SetSparsity(mat)

NULLIFY (meshobj)

END PROCEDURE Domain_SetSparsity1

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "Domain_SetSparsity2()"
INTEGER(I4B) :: ivar, nsd(SIZE(domains))
CHARACTER(20) :: matProp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

DO ivar = 1, SIZE(domains)

  IF (.NOT. ASSOCIATED(domains(ivar)%ptr)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & 'DOMAINS( '//TOSTRING(ivar)//' ) NOT ASSOCIATED')
  ELSE
    IF (.NOT. domains(ivar)%ptr%isInitiated)  &
      & CALL e%raiseError(modName//"::"//myName//" - "// &
      & 'DOMAINS( '//TOSTRING(ivar)//' )%ptr NOT INITIATED')
  END IF

  nsd(ivar) = domains(ivar)%ptr%getNSD()

END DO

IF (ANY(nsd .NE. nsd(1))) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
  & 'It seems that NSD (number of spatial dimensions) of domains are &
  & not identical')
END IF

CALL Display("Calling SetSparsity2 or SetSpartsity3 from DomainUtility")
matProp = GetMatrixProp(mat)

IF (TRIM(matProp) .EQ. "RECTANGLE") THEN
  CALL SetSparsity3(domains=domains, mat=mat)
ELSE
  CALL SetSparsity2(domains=domains, mat=mat)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE Domain_SetSparsity2

!----------------------------------------------------------------------------
!                                                          SetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetTotalMaterial
INTEGER(I4B) :: ii
CLASS(mesh_), POINTER :: meshptr

DO ii = 1, obj%getTotalMesh(dim=dim)
  meshptr => obj%getMeshPointer(dim=dim, entityNum=ii)
  CALL meshptr%SetTotalMaterial(n)
END DO
meshptr => NULL()
END PROCEDURE Domain_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                          SetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetMaterial
CLASS(mesh_), POINTER :: meshptr

meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
CALL meshptr%SetMaterial(medium=medium, material=material)
meshptr => NULL()
END PROCEDURE Domain_SetMaterial

!----------------------------------------------------------------------------
!                                                           SetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetNodeCoord1
CHARACTER(*), PARAMETER :: myName = "Domain_SetNodeCoord1"
REAL(DFP) :: scale0

IF (.NOT. ALLOCATED(obj%nodeCoord)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Domain_::obj%nodeCoord not allocated')
END IF

IF (SIZE(nodeCoord, 1) .NE. SIZE(obj%nodeCoord, 1) &
  & .OR. SIZE(nodeCoord, 2) .NE. SIZE(obj%nodeCoord, 2)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Size of nodeCoord does not match with Domain_::obj%nodeCoord')
END IF

scale0 = input(option=scale, default=1.0_DFP)

IF (PRESENT(addContribution)) THEN
  obj%nodeCoord = obj%nodeCoord + scale * nodeCoord
ELSE
  obj%nodeCoord = nodeCoord
END IF

END PROCEDURE Domain_SetNodeCoord1

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetQuality
CLASS(Mesh_), POINTER :: meshptr
CHARACTER(*), PARAMETER :: myName = "Domain_SetQuality"
REAL(DFP), ALLOCATABLE :: max_(:, :), min_(:, :)
INTEGER(I4B) :: tmesh, imesh

IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
  meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  IF (meshptr%getTotalElements() .EQ. 0) THEN
    CALL e%raiseWarning(modName//'::'//myName//' - '// &
    & 'mesh if empty')
  ELSE
    CALL meshptr%SetQuality(&
      & measures=measures, &
      & max_measures=max_measures, &
      & min_measures=min_measures, &
      & nodeCoord=obj%nodeCoord, &
      & local_nptrs=obj%local_nptrs &
      & )
  END IF
  NULLIFY (meshptr)
  RETURN
END IF

IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
  tmesh = obj%getTotalMesh(dim=dim)
  CALL Reallocate(max_, SIZE(measures), tmesh)
  CALL Reallocate(min_, SIZE(measures), tmesh)
  min_(:, :) = max_(:, :)

  DO imesh = 1, tmesh
    meshptr => obj%getMeshPointer(dim=dim, entityNum=imesh)
    IF (meshptr%getTotalElements() .EQ. 0) THEN
      max_(:, imesh) = -1 * MaxDFP
      min_(:, imesh) = MaxDFP
    ELSE
      CALL meshptr%SetQuality(&
        & measures=measures, &
        & max_measures=max_(:, imesh), &
        & min_measures=min_(:, imesh), &
        & nodeCoord=obj%nodeCoord, &
        & local_nptrs=obj%local_nptrs &
        & )
    END IF
  END DO

  max_measures = MAXVAL(max_, dim=2)
  min_measures = MINVAL(min_, dim=2)
  NULLIFY (meshptr)
  DEALLOCATE (max_, min_)
  RETURN
END IF

CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found')

END PROCEDURE Domain_SetQuality

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
  LOGICAL(LGT) :: is_intersect0

#ifdef DEBUG_VER
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & '[START] SetSparsity2()')
#endif

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

            is_intersect0 = row_box.isIntersect.col_box

            IF (is_intersect0) THEN
              CALL rowMesh%SetSparsity(mat=mat, &
                                       colMesh=colMesh, &
                                       nodeToNode=nodeToNode, &
                              rowGlobalToLocalNodeNum=rowDomain%local_nptrs, &
                                 rowLBOUND=LBOUND(rowDomain%local_nptrs, 1), &
                                 rowUBOUND=UBOUND(rowDomain%local_nptrs, 1), &
                              colGlobalToLocalNodeNum=colDomain%local_nptrs, &
                                 colLBOUND=LBOUND(colDomain%local_nptrs, 1), &
                                 colUBOUND=UBOUND(colDomain%local_nptrs, 1), &
                                       ivar=ivar, jvar=jvar)
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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

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

  ! check

  DO ii = 1, tvar
    nsd(ii) = domains(ii)%ptr%getNSD()
  END DO

  ! nullify first for safety

  rowMesh => NULL(); 
  colMesh => NULL(); 
  rowDomain => domains(1)%ptr
  colDomain => domains(2)%ptr

  rowMeshSize = rowDomain%getTotalMesh(dim=nsd(ivar))
  colMeshSize = colDomain%getTotalMesh(dim=nsd(jvar))

  CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
    & domain2=colDomain)

  nodeToNode => domainConn%getNodeToNodePointer()

  DO rowMeshID = 1, rowMeshSize

    rowMesh => rowDomain%getMeshPointer(dim=nsd(ivar), &
      & entityNum=rowMeshID)

    IF (.NOT. ASSOCIATED(rowMesh)) CYCLE

    DO colMeshID = 1, colMeshSize

      colMesh => colDomain%getMeshPointer(dim=nsd(jvar), &
        & entityNum=colMeshID)

      IF (ASSOCIATED(colMesh)) &
        CALL rowMesh%SetSparsity(mat=mat, &
                                 colMesh=colMesh, &
                                 nodeToNode=nodeToNode, &
                              rowGlobalToLocalNodeNum=rowDomain%local_nptrs, &
                                 rowLBOUND=LBOUND(rowDomain%local_nptrs, 1), &
                                 rowUBOUND=UBOUND(rowDomain%local_nptrs, 1), &
                              colGlobalToLocalNodeNum=colDomain%local_nptrs, &
                                 colLBOUND=LBOUND(colDomain%local_nptrs, 1), &
                                 colUBOUND=UBOUND(colDomain%local_nptrs, 1), &
                                 ivar=ivar, jvar=jvar)

    END DO
  END DO

  CALL SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)

  CALL domainConn%DEALLOCATE()

END SUBROUTINE SetSparsity3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
