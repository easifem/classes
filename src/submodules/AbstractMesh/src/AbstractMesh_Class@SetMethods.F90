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

USE BoundingBox_Method, ONLY: DeallocateBox => DEALLOCATE, &
                              OPERATOR(.Xmin.), &
                              OPERATOR(.Ymin.), &
                              OPERATOR(.Zmin.), &
                              OPERATOR(.Xmax.), &
                              OPERATOR(.Ymax.), &
                              OPERATOR(.Zmax.)

USE ReallocateUtility, ONLY: Reallocate

USE CSRMatrix_Method, ONLY: SetSparsity

USE FacetData_Class, ONLY: FacetData_SetParam

USE ElemData_Class, ONLY: ElemData_SetTotalMaterial, &
                          ElemData_Set

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
CALL DeallocateBox(box)
END PROCEDURE obj_SetBoundingBox2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity1()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: problem
INTEGER(I4B) :: i, j, k, tNodes, ii
INTEGER(I4B) :: n2n(PARAM_MAX_NODE_TO_NODE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
             "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

tsize = obj%GetTotalElements()
problem = tsize .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: Empty mesh found, returning')
  RETURN
END IF

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
                           ans=n2n, tsize=tsize, islocal=.TRUE.)

  DO ii = 1, tsize
    n2n(ii) = localNodeNumber(n2n(ii))
  END DO

  CALL SetSparsity(obj=mat, row=k, col=n2n(1:tsize))

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity2()"
LOGICAL(LGT) :: problem
INTEGER(I4B) :: i, j, tNodes, tsize
INTEGER(I4B) :: n2n(PARAM_MAX_NODE_TO_NODE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

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

problem = .NOT. obj%isNodeToNodesInitiated
IF (problem) CALL obj%InitiateNodeToNodes()

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
                        '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
             "[INTERNAL ERROR] :: Mesh data is not initiated, first initiate")
  RETURN
END IF

IF (.NOT. colMesh%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
          "[INTERNAL ERROR] :: colMesh data is not initiated, first initiate")
  RETURN
END IF

problem = SIZE(nodeToNode) .NE. obj%maxNptrs
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    "[INTERNAL ERROR] :: SIZE(nodeToNode) .NE. obj%maxNptrs")
  RETURN
END IF

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
LOGICAL(LGT) :: isok

INTEGER(I4B) :: n2n(PARAM_MAX_NODE_TO_NODE), tsize, ii, &
                temp(PARAM_MAX_NODE_TO_NODE), ll, jj, kk, row

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

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

isok = SIZE(nodeToNode) .GE. obj%GetMaxNodeNumber()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: SIZE( nodeToNode ) .LT. obj%maxNptrs "//  &
    & "[easifemClasses ISSUE#63]")
  RETURN
END IF

isok = obj%isNodeToNodesInitiated
IF (.NOT. isok) CALL obj%InitiateNodeToNodes()

DO ii = 1, obj%tNodes

  CALL obj%GetNodeToNodes_(globalNode=ii, includeSelf=.TRUE., &
                           ans=n2n, tsize=tsize, islocal=.TRUE.)
                         !! n2n(1) will contains the global node for ii
  row = rowGlobalToLocalNodeNum(n2n(1))

  ll = 0
  DO jj = 1, tsize
    kk = nodeToNode(n2n(jj))
    isok = colMesh%IsNodePresent(globalNode=kk, islocal=.FALSE.)

    IF (isok) THEN
      ll = ll + 1
      temp(ll) = kk
    END IF

  END DO

  DO jj = 1, ll
    temp(jj) = colGlobalToLocalNodeNum(temp(jj))
  END DO

  IF (ll .EQ. 0) CYCLE
  CALL SetSparsity(obj=Mat, row=row, col=temp(1:ll), ivar=ivar, jvar=jvar)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial1
INTEGER(I4B) :: iel
iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
CALL ElemData_SetTotalMaterial(obj%elementData(iel)%ptr, n=n)
END PROCEDURE obj_SetTotalMaterial1

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial2
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

DO ii = 1, obj%tElements
  isok = obj%elementData(ii)%ptr%isActive
  IF (.NOT. isok) CYCLE
  CALL ElemData_SetTotalMaterial(obj%elementData(ii)%ptr, n=n)
END DO
END PROCEDURE obj_SetTotalMaterial2

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial1
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

! start a loop of obj%elementData with ii = 1, size(obj%elementData)

DO ii = 1, obj%tElements
  isok = obj%elementData(ii)%ptr%isActive
  IF (.NOT. isok) CYCLE

  ! if obj%elementData(ii)%ptr%meshID is equal to entityNum then
  ! set %material(medium) = material
  isok = obj%elementData(ii)%ptr%meshID .EQ. entityNum
  IF (isok) THEN
    CALL ElemData_Set(obj%elementData(ii)%ptr, material=material, &
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

DO ii = 1, obj%tElements
  isok = obj%elementData(ii)%ptr%isActive
  IF (.NOT. isok) CYCLE
  CALL ElemData_Set(obj%elementData(ii)%ptr, material=material, &
                    medium=medium)
END DO
END PROCEDURE obj_SetMaterial2

!----------------------------------------------------------------------------
!                                                                SetMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial3
INTEGER(I4B) :: iel

iel = obj%GetLocalElemNumber(globalElement=globalElement,  &
  & islocal=islocal)

CALL ElemData_Set(obj%elementData(iel)%ptr, material=material, &
                  medium=medium)

END PROCEDURE obj_SetMaterial3

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetElementType
INTEGER(I4B) :: localElem
localElem = obj%GetLocalElemNumber(globalElement=globalElement,  &
  & islocal=islocal)
obj%facetElementType(iface, localElem) = facetElementType
obj%elementData(localElem)%ptr%elementType = facetElementType
END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                              SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_SetQuality
! CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
! CALL e%RaiseError(modName//'::'//myName//' - '// &
!   & '[WIP ERROR] :: This routine is under development')
! END PROCEDURE obj_SetQuality

! MODULE PROCEDURE obj_setQuality
! CHARACTER(*), PARAMETER :: myName = "obj_setQuality()"
! INTEGER(I4B) :: a, b, c, tsize, telements, iel, ii, nsd
! INTEGER(I4B), ALLOCATABLE :: indx(:), nptrs(:)
! REAL(DFP), ALLOCATABLE :: xij(:, :)

! a = SIZE(measures)
! b = SIZE(max_measures)
! c = SIZE(min_measures)

! IF (a .NE. b &
!   & .OR. a .NE. c) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & 'size of measures, max_measures, min_measures are not same.')
! END IF

! tsize = a
! telements = obj%getTotalElements()

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

! a = .NNE.obj%refelem

! CALL Reallocate(indx, a, nptrs, a)

! nsd = obj%getNSD()
! CALL Reallocate(xij, nsd, a)

! b = 0

! DO iel = obj%GetMinElemNumber() , obj%GetMaxElemNumber()
!   IF (.NOT. obj%isElementPresent(iel)) CYCLE
!   b = b + 1
!   nptrs = obj%getConnectivity(globalElement=iel)
!   indx = local_nptrs(nptrs)
!   xij = nodeCoord(1:nsd, indx)

!   DO ii = 1, tsize
!     obj%quality(ii, b) = ElementQuality(refelem=obj%refelem, &
!     & xij=xij, measure=measures(ii))
!   END DO

! END DO

! max_measures = MAXVAL(obj%quality, dim=2)
! min_measures = MINVAL(obj%quality, dim=2)

! IF (ALLOCATED(indx)) DEALLOCATE (indx)
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! IF (ALLOCATED(xij)) DEALLOCATE (xij)

! END PROCEDURE obj_setQuality

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
IF (PRESENT(isInitiated)) obj%isInitiated = isInitiated

IF (PRESENT(isNodeToElementsInitiated)) obj%isNodeToElementsInitiated =  &
  & isNodeToElementsInitiated

IF (PRESENT(isNodeToNodesInitiated)) obj%isNodeToNodesInitiated =  &
  & isNodeToNodesInitiated

IF (PRESENT(isExtraNodeToNodesInitiated)) obj%isExtraNodeToNodesInitiated =  &
  & isExtraNodeToNodesInitiated

IF (PRESENT(isElementToElementsInitiated)) obj%isElementToElementsInitiated =  &
  & isElementToElementsInitiated

IF (PRESENT(isBoundaryDataInitiated)) obj%isBoundaryDataInitiated = &
  & isBoundaryDataInitiated

IF (PRESENT(isFacetDataInitiated)) obj%isFacetDataInitiated =  &
  & isFacetDataInitiated

IF (PRESENT(uid)) obj%uid = uid

IF (PRESENT(tElements_topology_wise))  &
  & obj%tElements_topology_wise = tElements_topology_wise

IF (PRESENT(tElemTopologies)) obj%tElemTopologies = tElemTopologies

IF (PRESENT(elemTopologies)) obj%elemTopologies = elemTopologies

IF (PRESENT(nsd)) obj%nsd = nsd

IF (PRESENT(maxNptrs)) obj%maxNptrs = maxNptrs

IF (PRESENT(minNptrs)) obj%minNptrs = minNptrs

IF (PRESENT(maxElemNum)) obj%maxElemNum = maxElemNum

IF (PRESENT(minElemNum)) obj%minElemNum = minElemNum

IF (PRESENT(tNodes)) obj%tNodes = tNodes

IF (PRESENT(tElements)) obj%tElements = tElements

IF (PRESENT(minX)) obj%minX = minX

IF (PRESENT(minY)) obj%minY = minY

IF (PRESENT(minZ)) obj%minZ = minZ

IF (PRESENT(maxX)) obj%maxX = maxX

IF (PRESENT(maxY)) obj%maxY = maxY

IF (PRESENT(maxZ)) obj%maxZ = maxZ
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                   SetBoundaryFacetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetParam
CALL FacetData_SetParam(obj=obj%facetData(facetElement), &
                        elementType=elementType)
END PROCEDURE obj_SetFacetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
