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

SUBMODULE(AbstractDomain_Class) SetMethods
USE FEDomainConnectivity_Class, ONLY: FEDomainConnectivity_
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_GetMatrixProp => GetMatrixProp
USE Display_Method, ONLY: ToString, Display
USE InputUtility, ONLY: Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetShowTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetShowTime
obj%showTime = VALUE
END PROCEDURE obj_SetShowTime

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (.NOT. obj%isInit) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                "[INTERNAL ERROR] :: Domain is not initiated, first initiate")
  RETURN
END IF
#endif

meshptr => obj%GetMeshPointer()
CALL meshptr%SetSparsity(mat)
CALL CSRMatrix_SetSparsity(mat)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
INTEGER(I4B) :: ivar, nsd(SIZE(domains))
LOGICAL(LGT) :: problem
#endif

CHARACTER(:), ALLOCATABLE :: matProp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
! Check if domains are associated and initiated

DO ivar = 1, SIZE(domains)

  problem = .NOT. ASSOCIATED(domains(ivar)%ptr)
  IF (problem) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
           '[INTERNAL ERROR] :: domains('//Tostring(ivar)//') NOT ASSOCIATED')
    RETURN
  END IF

  problem = .NOT. domains(ivar)%ptr%isInit
  IF (problem) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
        '[INTERNAL ERROR] :: domains('//Tostring(ivar)//')%ptr NOT INITIATED')
    RETURN
  END IF

  nsd(ivar) = domains(ivar)%ptr%GetNSD()

END DO

! NSD of all domains should be identical
problem = ANY(nsd .NE. nsd(1))
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
        '[INTERNAL ERROR] :: It seems that NSD of domains are not identical.')
  RETURN
END IF

#endif

matProp = CSRMatrix_GetMatrixProp(mat)

IF (matProp .EQ. "RECTANGLE") THEN
  CALL part2_obj_Set_sparsity2(domains=domains, mat=mat)
ELSE
  CALL part1_obj_Set_sparsity2(domains=domains, mat=mat)
END IF

matProp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                   part1_obj_Set_sparsity2
!----------------------------------------------------------------------------

SUBROUTINE part1_obj_Set_sparsity2(domains, mat)
  CLASS(AbstractDomainPointer_), INTENT(IN) :: domains(:)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "part1_obj_Set_sparsity2()"
  INTEGER(I4B) :: ivar, jvar
  CLASS(AbstractDomain_), POINTER :: rowDomain, colDomain
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  TYPE(FEDomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  LOGICAL(LGT) :: isdebug

  isdebug = .FALSE.

#ifdef DEBUG_VER
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
  isdebug = .TRUE.
#endif

  ! nullify first for safety
  rowMesh => NULL()
  colMesh => NULL()
  rowDomain => NULL()
  colDomain => NULL()

  DO ivar = 1, SIZE(domains)

    IF (isdebug) CALL Display("row domain = "//ToString(ivar))

    rowDomain => domains(ivar)%ptr
    IF (.NOT. ASSOCIATED(rowDomain)) CYCLE

    rowMesh => rowDomain%GetMeshPointer(dim=rowDomain%GetNSD())
    IF (.NOT. ASSOCIATED(rowMesh)) CYCLE
    IF (rowMesh%isEmpty()) CYCLE

    DO jvar = 1, SIZE(domains)

      IF (isdebug) CALL Display("col domain = "//ToString(jvar))

      colDomain => domains(jvar)%ptr
      IF (.NOT. ASSOCIATED(colDomain)) CYCLE

      colMesh => colDomain%GetMeshPointer(dim=colDomain%GetNSD())
      IF (.NOT. ASSOCIATED(colMesh)) CYCLE
      IF (colMesh%isEmpty()) CYCLE

      CALL domainConn%DEALLOCATE()
      CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
                                             domain2=colDomain)
      nodeToNode => domainConn%GetNodeToNodePointer()

      CALL rowMesh%SetSparsity(mat=mat, colMesh=colMesh, &
                               nodeToNode=nodeToNode, ivar=ivar, jvar=jvar)
    END DO
  END DO

  CALL CSRMatrix_SetSparsity(mat)
  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)
  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE part1_obj_Set_sparsity2

!----------------------------------------------------------------------------
!                                                   part2_obj_Set_sparsity2
!----------------------------------------------------------------------------

SUBROUTINE part2_obj_Set_sparsity2(domains, mat)
  CLASS(AbstractDomainPointer_), INTENT(IN) :: domains(2)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "part2_obj_Set_sparsity2()"
  INTEGER(I4B), PARAMETER :: tvar = 2, ivar = 1, jvar = 1
  INTEGER(I4B) :: nsd(tvar), ii
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  CLASS(AbstractDomain_), POINTER :: rowDomain, colDomain
  TYPE(FEDomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  DO ii = 1, tvar
    nsd(ii) = domains(ii)%ptr%GetNSD()
  END DO

  rowDomain => NULL()
  colDomain => NULL()
  rowDomain => domains(1)%ptr
  colDomain => domains(2)%ptr

#ifdef DEBUG_VER

  problem = (.NOT. ASSOCIATED(rowDomain)) .OR. (.NOT. ASSOCIATED(colDomain))

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: rowMesh not ASSOCIATED')
    RETURN
  END IF

#endif

  rowMesh => NULL()
  colMesh => NULL()
  rowMesh => rowDomain%GetMeshPointer(dim=nsd(1))
  colMesh => colDomain%GetMeshPointer(dim=nsd(2))

#ifdef DEBUG_VER

  problem = (.NOT. ASSOCIATED(rowMesh)) .OR. (.NOT. ASSOCIATED(colMesh))

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: rowMesh or colMesh not ASSOCIATED')
    RETURN
  END IF

#endif

  CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
                                         domain2=colDomain)
  nodeToNode => domainConn%GetNodeToNodePointer()

  CALL rowMesh%SetSparsity(mat=mat, colMesh=colMesh, &
                           nodeToNode=nodeToNode, ivar=ivar, jvar=jvar)

  CALL CSRMatrix_SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)

  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE part2_obj_Set_sparsity2

!----------------------------------------------------------------------------
!                                                           SetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalMaterial()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
CALL meshptr%SetTotalMaterial(n)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                           SetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMaterial()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

#ifdef DEBUG_VER
isok = ASSOCIATED(meshptr)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::obj_SetMaterial - '// &
                    '[INTERNAL ERROR] :: meshptr not associated')
  RETURN
END IF
#endif

CALL meshptr%SetMaterial(medium=medium, material=material, &
                         entityNum=entityNum)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetMaterial

!----------------------------------------------------------------------------
!                                                              SetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetNodeCoord1()"
LOGICAL(LGT) :: problem
#endif

REAL(DFP) :: scale0
LOGICAL(LGT) :: add0
INTEGER(I4B) :: ii, tnodes, nsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%nodeCoord)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: AbstractDomain_::obj%nodeCoord not allocated')
  RETURN
END IF

problem = ALL(SHAPE(nodeCoord) .NE. SHAPE(obj%nodeCoord))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: Shape of nodeCoord does not match '// &
                    'with obj_::obj%nodeCoord')
  RETURN
END IF
#endif

scale0 = Input(option=scale, default=1.0_DFP)
add0 = Input(option=addContribution, default=.FALSE.)
tnodes = SIZE(nodeCoord, 2)
nsd = obj%nsd

IF (add0) THEN
  DO CONCURRENT(ii=1:tnodes)
    obj%nodeCoord(1:nsd, ii) = nodeCoord(1:nsd, ii) * scale0 &
                               + obj%nodeCoord(1:nsd, ii)
  END DO
  RETURN
END IF

! make do concurrent loop for setting obj%nodeCoord to nodeCoord
DO CONCURRENT(ii=1:tnodes)
  obj%nodeCoord(1:nsd, ii) = nodeCoord(1:nsd, ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetNodeCoord

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
! CLASS(Mesh_), POINTER :: meshptr
! CHARACTER(*), PARAMETER :: myName = "obj_SetQuality"
! REAL(DFP), ALLOCATABLE :: max_(:, :), min_(:, :)
! INTEGER(I4B) :: tmesh, imesh, dim0
!
!
! dim0 = Input(default=obj%nsd, option=dim)
!
! IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
!   meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!   IF (meshptr%GetTotalElements() .EQ. 0) THEN
!     CALL e%RaiseWarning(modName//'::'//myName//' - '// &
!     & 'mesh if empty')
!   ELSE
!     CALL meshptr%SetQuality(&
!       & measures=measures, &
!       & max_measures=max_measures, &
!       & min_measures=min_measures, &
!       & nodeCoord=obj%nodeCoord, &
!       & local_nptrs=obj%local_nptrs &
!       & )
!   END IF
!   NULLIFY (meshptr)
!   RETURN
! END IF
!
! IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
!   tmesh = obj%GetTotalMesh(dim=dim)
!   CALL Reallocate(max_, SIZE(measures), tmesh)
!   min_ = max_
!
!   DO imesh = 1, tmesh
!     meshptr => obj%GetMeshPointer(dim=dim, entityNum=imesh)
!     IF (meshptr%GetTotalElements() .EQ. 0) THEN
!       max_(:, imesh) = -1 * MaxDFP
!       min_(:, imesh) = MaxDFP
!     ELSE
!       CALL meshptr%SetQuality(&
!         & measures=measures, &
!         & max_measures=max_(:, imesh), &
!         & min_measures=min_(:, imesh), &
!         & nodeCoord=obj%nodeCoord, &
!         & local_nptrs=obj%local_nptrs &
!         & )
!     END IF
!   END DO
!
!   max_measures = MAXVAL(max_, dim=2)
!   min_measures = MINVAL(min_, dim=2)
!   NULLIFY (meshptr)
!   DEALLOCATE (max_, min_)
!   RETURN
! END IF
!
! CALL e%RaiseError(modName//'::'//myName//' - '// &
!   & 'No case found')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!                                                           SetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalElements()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = indx .LT. SIZE(obj%tElements)
CALL AssertError1(isok, myName, &
                  "indx is out of bound, it should be between 0,1,2,3")
#endif

obj%tElements(indx) = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetTotalElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
