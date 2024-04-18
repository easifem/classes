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
USE BaseMethod, ONLY: MaxDFP, MinDFP
USE BoundingBox_Method
USE Display_Method
USE DomainConnectivity_Class
USE ReallocateUtility
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_GetMatrixProp => GetMatrixProp
USE Display_Method, ONLY: Tostring
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
INTEGER(I4B) :: imesh, dim, tmesh, lb, ub
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Domain is not initiated, first initiate")
  RETURN
END IF
#endif

! Call SetSparsity1 from DomainUtility
! CALL SetSparsity1(obj=obj, mat=mat)

lb = LBOUND(obj%local_nptrs, 1)
ub = UBOUND(obj%local_nptrs, 1)

DO dim = 1, 3

  tmesh = obj%GetTotalMesh(dim=dim)
  DO imesh = 1, tmesh
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=imesh)
    isok = ASSOCIATED(meshptr)
    IF (isok) &
      CALL meshptr%SetSparsity(mat=mat, &
                        localNodeNumber=obj%local_nptrs, lbound=lb, ubound=ub)
  END DO

END DO

CALL CSRMatrix_SetSparsity(mat)

meshptr => NULL()

END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
INTEGER(I4B) :: ivar, nsd(SIZE(domains))
CHARACTER(:), ALLOCATABLE :: matprop
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

DO ivar = 1, SIZE(domains)

  isok = ASSOCIATED(domains(ivar)%ptr)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
      & '[INTERNAL ERROR] :: DOMAINS('//Tostring(ivar)//') NOT ASSOCIATED')
    RETURN
  END IF

  isok = domains(ivar)%ptr%isInit()
  IF (.NOT. isok)  &
    & CALL e%RaiseError(modName//"::"//myName//" - "// &
    & '[INTERNAL ERROR] :: DOMAINS('//Tostring(ivar)//')%ptr NOT INITIATED')

  nsd(ivar) = domains(ivar)%ptr%GetNSD()

END DO

isok = ALL(nsd .EQ. nsd(1))
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    '[INTERNAL ERROR] :: It seems that NSD (number of spatial dimensions)'// &
                    'of domains are not identical')
  RETURN
END IF

! CALL Display("Calling SetSparsity2 or SetSpartsity3 from DomainUtility")
matprop = CSRMatrix_GetMatrixProp(mat)

IF (matprop .EQ. "RECTANGLE") THEN
  CALL SetSparsity3(domains=domains, mat=mat)
ELSE
  CALL SetSparsity2(domains=domains, mat=mat)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary:  Set sparsity for sparse matrix of BlockMatrixField

SUBROUTINE SetSparsity2(domains, mat)
  CLASS(AbstractDomainPointer_), INTENT(IN) :: domains(:)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "SetSparsity2"

  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  CLASS(AbstractDomain_), POINTER :: rowDomain, colDomain

  TYPE(DomainConnectivity_) :: domainConn
  TYPE(BoundingBox_) :: row_box, col_box

  LOGICAL(LGT) :: is_intersect0

#ifdef DEBUG_VER
  LOGICAL(LGT), PARAMETER :: verbose = .TRUE.
#else
  LOGICAL(LGT), PARAMETER :: verbose = .FALSE.
#endif

  INTEGER(I4B) :: rowMeshID, colMeshID, rowMeshSize, colMeshSize, &
                 ivar, jvar, tsize, rowLBOUND, rowUBOUND, colLBOUND, colUBOUND

  INTEGER(I4B), POINTER :: nodeToNode(:), rowGlobalToLocalNodeNum(:), &
                           colGlobalToLocalNodeNum(:)

  INTEGER(I4B), DIMENSION(SIZE(domains)) :: nsd

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] SetSparsity2()')
#endif

  tsize = SIZE(domains)
  DO ivar = 1, tsize; nsd(ivar) = domains(ivar)%ptr%GetNSD(); END DO

  ! nullify first for safety
  DO ivar = 1, tsize

    IF (verbose) CALL Display("row domain = "//Tostring(ivar))

    rowDomain => domains(ivar)%ptr
    rowMeshSize = rowDomain%GetTotalMesh(dim=nsd(ivar))

    SELECT TYPE (rowDomain)
    CLASS IS (Domain_)
      rowLBOUND = LBOUND(rowDomain%local_nptrs, 1)
      rowUBOUND = UBOUND(rowDomain%local_nptrs, 1)
      rowGlobalToLocalNodeNum => rowDomain%local_nptrs

    CLASS DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: No case found')
    END SELECT

    DO jvar = 1, SIZE(domains)

      IF (verbose) CALL Display("col domain = "//Tostring(jvar))

      colDomain => domains(jvar)%ptr
      colMeshSize = colDomain%GetTotalEntities(dim=nsd(jvar))

      SELECT TYPE (colDomain)
      CLASS IS (Domain_)
        colLBOUND = LBOUND(colDomain%local_nptrs, 1)
        colUBOUND = UBOUND(colDomain%local_nptrs, 1)
        colGlobalToLocalNodeNum => colDomain%local_nptrs

      CLASS DEFAULT
        CALL e%RaiseError(modName//'::'//myName//' - '// &
                          '[INTERNAL ERROR] :: No case found')
      END SELECT

      IF (verbose) CALL Display( &
        "calling DomainConnectivity_::domainConn%Deallocate()")

      CALL domainConn%DEALLOCATE()

      IF (verbose) CALL Display( &
        "calling DomainConnectivity_::domainConn%InitiateNodeToNodeData()")

      CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
                                             domain2=colDomain)

      nodeToNode => domainConn%GetNodeToNodePointer()

      DO rowMeshID = 1, rowMeshSize

        rowMesh => rowDomain%GetMeshPointer(dim=nsd(ivar), &
                                            entityNum=rowMeshID)

        IF (.NOT. ASSOCIATED(rowMesh)) CYCLE

        row_box = rowMesh%GetBoundingBox()

        DO colMeshID = 1, colMeshSize

          colMesh => colDomain%GetMeshPointer(dim=nsd(jvar), &
                                              entityNum=colMeshID)

          IF (ASSOCIATED(colMesh)) THEN
            col_box = colMesh%GetBoundingBox()

            is_intersect0 = row_box.isIntersect.col_box

            IF (is_intersect0) THEN
              CALL rowMesh%SetSparsity(mat=mat, &
                                       colMesh=colMesh, &
                                       nodeToNode=nodeToNode, &
                            rowGlobalToLocalNodeNum=rowGlobalToLocalNodeNum, &
                                       rowLBOUND=rowLBOUND, &
                                       rowUBOUND=rowUBOUND, &
                            colGlobalToLocalNodeNum=colGlobalToLocalNodeNum, &
                                       colLBOUND=colLBOUND, &
                                       colUBOUND=colUBOUND, &
                                       ivar=ivar, jvar=jvar)
            END IF
          END IF

        END DO
      END DO
    END DO
  END DO

  IF (verbose) CALL Display("Calling SetSparsity()")
  CALL CSRMatrix_SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode, &
           rowGlobalToLocalNodeNum, colGlobalToLocalNodeNum)

  if(verbose) CALL Display("Calling DomainConnectivity_::domainConn%Deallocate()")
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
  CLASS(AbstractDomainPointer_), INTENT(IN) :: domains(2)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "SetSparsity3()"
  INTEGER(I4B), PARAMETER :: tvar = 2, ivar = 1, jvar = 1
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  CLASS(AbstractDomain_), POINTER :: rowDomain, colDomain
  TYPE(DomainConnectivity_) :: domainConn

  INTEGER(I4B), POINTER :: nodeToNode(:), rowGlobalToLocalNodeNum(:), &
                           colGlobalToLocalNodeNum(:)

  INTEGER(I4B) :: rowMeshID, colMeshID, rowMeshSize, colMeshSize,  &
    & nsd(tvar), ii, rowLBOUND, rowUBOUND, colLBOUND, colUBOUND

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  DO ii = 1, tvar
    nsd(ii) = domains(ii)%ptr%GetNSD()
  END DO

  ! nullify first for safety

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)
  rowDomain => domains(1)%ptr
  colDomain => domains(2)%ptr

  SELECT TYPE (rowDomain)
  CLASS IS (Domain_)
    rowLBOUND = LBOUND(rowDomain%local_nptrs, 1)
    rowUBOUND = UBOUND(rowDomain%local_nptrs, 1)
    rowGlobalToLocalNodeNum => rowDomain%local_nptrs

  CLASS DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: No case found for rowDomain')
  END SELECT

  SELECT TYPE (colDomain)
  CLASS IS (Domain_)
    colLBOUND = LBOUND(colDomain%local_nptrs, 1)
    colUBOUND = UBOUND(colDomain%local_nptrs, 1)
    colGlobalToLocalNodeNum => colDomain%local_nptrs

  CLASS DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: No case found for colDomain')
  END SELECT

  rowMeshSize = rowDomain%GetTotalMesh(dim=nsd(ivar))
  colMeshSize = colDomain%GetTotalMesh(dim=nsd(jvar))

  CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
    & domain2=colDomain)

  nodeToNode => domainConn%GetNodeToNodePointer()

  DO rowMeshID = 1, rowMeshSize

    rowMesh => rowDomain%GetMeshPointer(dim=nsd(ivar), &
      & entityNum=rowMeshID)

    IF (.NOT. ASSOCIATED(rowMesh)) CYCLE

    DO colMeshID = 1, colMeshSize

      colMesh => colDomain%GetMeshPointer(dim=nsd(jvar), &
        & entityNum=colMeshID)

      IF (ASSOCIATED(colMesh)) &
        CALL rowMesh%SetSparsity(mat=mat, &
                                 colMesh=colMesh, &
                                 nodeToNode=nodeToNode, &
                            rowGlobalToLocalNodeNum=rowGlobalToLocalNodeNum, &
                                 rowLBOUND=rowLBOUND, &
                                 rowUBOUND=rowUBOUND, &
                            colGlobalToLocalNodeNum=colGlobalToLocalNodeNum, &
                                 colLBOUND=colLBOUND, &
                                 colUBOUND=colUBOUND, &
                                 ivar=ivar, jvar=jvar)

    END DO
  END DO

  CALL CSRMatrix_SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode, &
           rowGlobalToLocalNodeNum, colGlobalToLocalNodeNum)

  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE SetSparsity3

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
LOGICAL(LGT) :: acase

acase = PRESENT(dim) .AND. PRESENT(entityNum)
IF (acase) THEN; CALL case1; RETURN; END IF

acase = PRESENT(dim) .AND. .NOT. PRESENT(entityNum)
IF (acase) THEN; CALL case2; RETURN; END IF

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: No case found')

CONTAINS
SUBROUTINE case1
  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: meshptr

  meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

  isok = meshptr%GetTotalElements() .EQ. 0

  IF (.NOT. isok) THEN

    CALL meshptr%SetQuality(measures=measures, max_measures=max_measures, &
                         min_measures=min_measures, nodeCoord=obj%nodeCoord, &
                            local_nptrs=obj%local_nptrs)

  END IF

  NULLIFY (meshptr)
END SUBROUTINE case1

SUBROUTINE case2
  CLASS(AbstractMesh_), POINTER :: meshptr
  INTEGER(I4B) :: tmesh, imesh
  REAL(DFP), ALLOCATABLE :: max_(:, :), min_(:, :)

  tmesh = obj%GetTotalMesh(dim=dim)
  CALL Reallocate(max_, SIZE(measures), tmesh)
  CALL Reallocate(min_, SIZE(measures), tmesh)
  min_(:, :) = max_(:, :)

  DO imesh = 1, tmesh
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=imesh)
    IF (meshptr%GetTotalElements() .EQ. 0) THEN
      max_(:, imesh) = -1 * MaxDFP
      min_(:, imesh) = MaxDFP
    ELSE
     CALL meshptr%SetQuality(measures=measures, max_measures=max_(:, imesh), &
                       min_measures=min_(:, imesh), nodeCoord=obj%nodeCoord, &
                              local_nptrs=obj%local_nptrs)

    END IF
  END DO

  max_measures = MAXVAL(max_, dim=2)
  min_measures = MINVAL(min_, dim=2)
  NULLIFY (meshptr)
  DEALLOCATE (max_, min_)
END SUBROUTINE case2

END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
