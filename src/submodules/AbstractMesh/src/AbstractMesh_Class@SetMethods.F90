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

SUBMODULE(AbstractMesh_Class) SetMethods
USE GlobalData, ONLY: INT8
USE BoundingBox_Method
USE ReallocateUtility
USE CSRMatrix_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetShowTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetShowTime
obj%showTime = VALUE
END PROCEDURE obj_SetShowTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBoundingBox1
obj%minX = (.Xmin.box)
obj%minY = (.Ymin.box)
obj%minZ = (.Zmin.box)
obj%maxX = (.Xmax.box)
obj%maxY = (.Ymax.box)
obj%maxZ = (.Zmax.box)
END PROCEDURE obj_SetBoundingBox1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBoundingBox2
TYPE(BoundingBox_) :: box
Box = obj%GetBoundingBox(nodes=nodes, local_nptrs=local_nptrs)
CALL obj%SetBoundingBox(box=box)
CALL DEALLOCATE (box)
END PROCEDURE obj_SetBoundingBox2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity1()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: i, j, k, tNodes
INTEGER(I4B), ALLOCATABLE :: n2n(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

tsize = obj%GetTotalElements()
problem = tsize .EQ. 0_I4B
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
#endif

tNodes = obj%GetTotalNodes()

DO i = 1, tNodes
  j = obj%GetGlobalNodeNumber(localNode=i)
  k = localNodeNumber(j)
  IF (k .NE. 0) THEN
    n2n = localNodeNumber( &
      & obj%GetNodeToNodes(globalNode=j, includeSelf=.TRUE.))
    CALL SetSparsity(obj=mat, row=k, col=n2n)
  END IF
END DO
IF (ALLOCATED(n2n)) DEALLOCATE (n2n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity1()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: i, j, tNodes
INTEGER(I4B), ALLOCATABLE :: n2n(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

tsize = obj%GetTotalElements()
problem = tsize .EQ. 0_I4B
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
#endif

tNodes = obj%GetTotalNodes()

DO i = 1, tNodes
  j = obj%GetGlobalNodeNumber(localNode=i)
  n2n = obj%GetNodeToNodes(globalNode=j, includeSelf=.TRUE.)
  CALL SetSparsity(obj=mat, row=j, col=n2n)
END DO
IF (ALLOCATED(n2n)) DEALLOCATE (n2n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity3()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity4
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity4()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial
INTEGER(I4B), ALLOCATABLE :: temp_material(:)
INTEGER(I4B) :: n0

IF (ALLOCATED(obj%material)) THEN
  n0 = SIZE(obj%material)
  CALL Reallocate(temp_material, n0 + n)
  temp_material(1:n0) = obj%material(1:n0)
  CALL MOVE_ALLOC(from=temp_material, to=obj%material)
  RETURN
END IF

CALL Reallocate(obj%material, n)
END PROCEDURE obj_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial1
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

! start a loop of obj%elementData with ii = 1, size(obj%elementData)

DO CONCURRENT(ii=1:obj%tElements)
  isok = obj%elementData(ii)%isActive
  IF (.NOT. isok) CYCLE

  ! if obj%elementData(ii)%meshID is equal to entityNum then
  ! set %material(medium) = material
  isok = obj%elementData(ii)%meshID .EQ. entityNum
  IF (isok) THEN
    CALL ElemDataSet(obj%elementData(ii), material=material, &
                     medium=medium)
  END IF

END DO

END PROCEDURE obj_SetMaterial1

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial2
obj%material(medium) = material
END PROCEDURE obj_SetMaterial2

!----------------------------------------------------------------------------
!                                                        setFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetElementType
INTEGER(I4B) :: localElem
localElem = obj%GetLocalElemNumber(globalElement=globalElement,  &
  & islocal=islocal)
obj%facetElementType(iface, localElem) = facetElementType
obj%elementData(localElem)%elementType = facetElementType
END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                           setQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
