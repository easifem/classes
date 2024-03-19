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

SUBMODULE(BetterMesh_Class) SetMethods
! USE BaseMethod
! USE MeshUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

problem = .NOT. obj%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: BetterMesh_::obj is not initiated.")
  RETURN
END IF

! if the mesh is empty then return
problem = obj%GetTotalElements() .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Empty mesh found, returning')
  RETURN
END IF

! check
problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: In mesh NodeToNodeData is not initiated')
  RETURN
END IF

! Call from MeshUtility
! CALL SetSparsity1(obj=obj, mat=mat, localNodeNumber=localNodeNumber, &
!   & lbound=lbound, ubound=ubound)

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
LOGICAL(LGT) :: problem

! check
problem = .NOT. obj%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

! check
problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: In mesh NodeToNodeData is not initiated')
  RETURN
END IF

! Call from MeshUtility
! CALL SetSparsity2(obj=obj, mat=mat)

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity3()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

! check
problem = .NOT. obj%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

! check
problem = .NOT. colMesh%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: colMesh data is not initiated, first initiate")
  RETURN
END IF

! check
problem = SIZE(nodeToNode) .NE. obj%maxNptrs
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: SIZE( nodeToNode ) .NE. obj%maxNptrs")
  RETURN
END IF

! check
problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: In mesh NodeToNodeData is not initiated')
  RETURN
END IF

! Call from MeshUtility
! SELECT TYPE (colMesh)
! CLASS IS (Mesh_)
!   CALL SetSparsity3(obj=obj, colMesh=colMesh, nodeToNode=nodeToNode, &
!     & mat=mat, ivar=ivar, jvar=jvar)
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: No case found for the type of colMesh')
! END SELECT

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity4
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity4()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

problem = .NOT. obj%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

problem = .NOT. colMesh%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: colMesh data is not initiated, first initiate")
  RETURN
END IF

problem = SIZE(nodeToNode) .LT. obj%maxNptrs
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: SIZE( nodeToNode ) .LT. obj%maxNptrs "//  &
    & "[easifemClasses ISSUE#63]")
  RETURN
END IF

problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: In mesh NodeToNodeData is not initiated')
  RETURN
END IF

! Call from MeshUtility
! SELECT TYPE (colMesh)
! CLASS IS (Mesh_)
!   CALL SetSparsity4(obj=obj, colMesh=colMesh, nodeToNode=nodeToNode, &
!     & mat=mat, rowGlobalToLocalNodeNum=rowGlobalToLocalNodeNum, &
!     & colGlobalToLocalNodeNum=colGlobalToLocalNodeNum, &
!     & rowLBOUND=rowLBOUND, rowUBOUND=rowUBOUND, &
!     & colLBOUND=colLBOUND, colUBOUND=colUBOUND, &
!     & ivar=ivar, jvar=jvar)
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: No case found for given type of colMesh')
! END SELECT

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                           setQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
! INTEGER(I4B) :: a, b, c, tsize, telements, iel, ii, nsd
! INTEGER(I4B), ALLOCATABLE :: indx(:), nptrs(:)
! REAL(DFP), ALLOCATABLE :: xij(:, :)
!
! a = SIZE(measures)
! b = SIZE(max_measures)
! c = SIZE(min_measures)
!
! IF (a .NE. b &
!   & .OR. a .NE. c) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & 'size of measures, max_measures, min_measures are not same.')
! END IF
!
! tsize = a
! telements = obj%telements
!
! IF (ALLOCATED(obj%quality)) THEN
!   tsize = SIZE(obj%quality, 1)
!   IF (tsize .NE. a) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!       & 'Mesh_::obj%quality is allocated row size is not same as '// &
!     & CHAR_LF//" the size of measures")
!   END IF
! ELSE
!   CALL Reallocate(obj%quality, tsize, telements)
! END IF
!
! a = .NNE.obj%refelem
!
! CALL Reallocate(indx, a, nptrs, a)
!
! nsd = obj%getNSD()
! CALL Reallocate(xij, nsd, a)
!
! b = 0
!
! DO iel = obj%minElemNum, obj%maxElemNum
!   IF (.NOT. obj%isElementPresent(iel)) CYCLE
!   b = b + 1
!   nptrs = obj%getConnectivity(globalElement=iel)
!   indx = local_nptrs(nptrs)
!   xij = nodeCoord(1:nsd, indx)
!
!   DO ii = 1, tsize
!     obj%quality(ii, b) = ElementQuality(refelem=obj%refelem, &
!     & xij=xij, measure=measures(ii))
!   END DO
!
! END DO
!
! max_measures = MAXVAL(obj%quality, dim=2)
! min_measures = MINVAL(obj%quality, dim=2)
!
! IF (ALLOCATED(indx)) DEALLOCATE (indx)
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! IF (ALLOCATED(xij)) DEALLOCATE (xij)

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
