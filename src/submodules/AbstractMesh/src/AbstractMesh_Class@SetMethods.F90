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
USE globalData, ONLY: INT8
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
#endif
LOGICAL(LGT) :: problem

INTEGER(I4B) :: i, j, k, tNodes, tsize
INTEGER(I4B) :: n2n(PARAM_MAX_NODE_TO_NODE)

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

#endif

! check
problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) CALL obj%InitiateNodeToNodes()

tNodes = obj%GetTotalNodes()

! TODO:
! Use openmp parallel loop
! make n2n a variable, each thread has its own copy of n2n
! each thread will call setSparsity with its own copy of n2n

DO i = 1, tNodes
  j = obj%GetglobalNodeNumber(localNode=i)
  k = localNodeNumber(j)

  IF (k .EQ. 0) CYCLE

  CALL obj%GetNodeToNodes_(globalNode=i, includeSelf=.TRUE., &
    & ans=n2n, tsize=tsize, islocal=.TRUE.)

  CALL SetSparsity(obj=mat, row=k, col=n2n(1:tsize))

END DO

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
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity2()"
INTEGER(I4B) :: tsize
#endif
LOGICAL(LGT) :: problem

INTEGER(I4B) :: i, j, tNodes, tsize
INTEGER(I4B) :: n2n(PARAM_MAX_NODE_TO_NODE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
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

#endif

problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) CALL obj%InitiateNodeToNodes()

tNodes = obj%GetTotalNodes()

DO i = 1, tNodes

  j = obj%GetglobalNodeNumber(localNode=i)

  CALL obj%GetNodeToNodes_(globalNode=i, includeSelf=.TRUE., &
    & ans=n2n, tsize=tsize, islocal=.TRUE.)

  CALL SetSparsity(obj=mat, row=j, col=n2n(1:tsize))

END DO

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
LOGICAL(LGT) :: problem, isok

INTEGER(I4B) :: n2n(PARAM_MAX_NODE_TO_NODE), tsize, ii, &
                temp(PARAM_MAX_NODE_TO_NODE), ll, jj, kk

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

IF (.NOT. colMesh%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: colMesh data is not initiated, first initiate")
  RETURN
END IF

problem = SIZE(nodeToNode) .NE. obj%maxNptrs
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: SIZE(nodeToNode) .NE. obj%maxNptrs")
  RETURN
END IF

#endif

! check
IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()

DO ii = 1, obj%tNodes

  CALL obj%GetNodeToNodes_(globalNode=ii, includeSelf=.TRUE., &
                           ans=n2n, tsize=tsize, islocal=.TRUE.)
                         !! n2n(1) will contains the global node for ii

  ll = 0
  DO jj = 1, tsize
    kk = nodeToNode(n2n(jj))
    isok = colMesh%IsNodePresent(globalNode=kk, islocal=.FALSE.)

    IF (isok) THEN
      ll = ll + 1
      temp(ll) = kk
    END IF

  END DO

  IF (ll .EQ. 0) CYCLE

  CALL SetSparsity(obj=mat, row=n2n(1), col=temp(1:ll), ivar=ivar, jvar=jvar)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

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

MODULE PROCEDURE obj_SetTotalMaterial1
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
CALL ElemData_SetTotalMaterial(obj%elementData(iel), n=n)
END PROCEDURE obj_SetTotalMaterial1

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial2
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

DO CONCURRENT(ii=1:obj%tElements)
  isok = obj%elementData(ii)%isActive
  IF (.NOT. isok) CYCLE
  CALL ElemData_SetTotalMaterial(obj%elementData(ii), n=n)
END DO
END PROCEDURE obj_SetTotalMaterial2

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
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

! start a loop of obj%elementData with ii = 1, size(obj%elementData)

DO CONCURRENT(ii=1:obj%tElements)
  isok = obj%elementData(ii)%isActive
  IF (.NOT. isok) CYCLE
  CALL ElemDataSet(obj%elementData(ii), material=material, &
                   medium=medium)
END DO
END PROCEDURE obj_SetMaterial2

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial3
INTEGER(I4B) :: iel

iel = obj%GetLocalElemNumber(globalElement=globalElement,  &
  & islocal=islocal)

CALL ElemDataSet(obj%elementData(iel), material=material, &
                 medium=medium)

END PROCEDURE obj_SetMaterial3

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
