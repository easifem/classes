! This program is a part of EASIFEM library
! Copyright (C) (Since 2020)  Vikas Sharma, Ph.D
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

SUBMODULE(AbstractMesh_Class) EdgeDataMethods
USE ReferenceElement_Method, ONLY: REFELEM_MAX_EDGES,  &
& GetEdgeConnectivity, REFELEM_MAX_POINTS, RefElemGetGeoParam
USE ReallocateUtility, ONLY: Reallocate
USE EdgeData_Class
USE EdgeDataBinaryTree_Class
USE SortUtility
USE GlobalData, ONLY: INT8
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateEdgeConnectivity
CHARACTER(*), PARAMETER :: myName = "obj_InitiateEdgeConnectivity()"
INTEGER(I4B) :: tElements, iel, elemType, tEdges,  &
  & localEdges(2, REFELEM_MAX_EDGES), edge(2), sorted_edge(2),  &
  & nptrs(REFELEM_MAX_POINTS), tNodes, tsize1, tsize2, iedge
LOGICAL(LGT) :: problem
TYPE(EdgeDataBinaryTree_) :: edgeTree
TYPE(EdgeData_) :: edgeValue
TYPE(EdgeData_), POINTER :: edgePtr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%elementData)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractMesh_::obj%elementData not allocated')
  RETURN
END IF
#endif

tElements = obj%GetTotalElements()

CALL edgeTree%Initiate()

DO iel = 1, tElements
  problem = .NOT. obj%elementData(iel)%isActive
  IF (problem) CYCLE
  elemType = obj%elementData(iel)%name
  CALL RefElemGetGeoParam(elemType=elemType,  &
    & tEdges=tEdges, tNodes=tNodes, edgeCon=localEdges,  &
    & edgeOpt=1_I4B)

  CALL Reallocate(obj%elementData(iel)%globalEdges, tEdges)
  CALL Reallocate(obj%elementData(iel)%edgeOrient, tEdges)

  nptrs(1:tNodes) = obj%elementData(iel)%globalNodes

  DO iedge = 1, tEdges

    edge = nptrs(localEdges(:, iedge))
    sorted_edge = SORT(edge)

    edgePtr => EdgeData_Pointer(sorted_edge)

    tsize1 = edgeTree%SIZE()
    CALL edgeTree%Insert(edgePtr)
    tsize2 = edgeTree%SIZE()

    IF (edge(1) .GT. edge(2)) THEN
      obj%elementData(iel)%edgeOrient(iedge) = -1_INT8
    ELSE
      obj%elementData(iel)%edgeOrient(iedge) = 1_INT8
    END IF

    IF (tsize1 .NE. tsize2) THEN
      obj%elementData(iel)%globalEdges(iedge) = tsize2
      edgePtr%id = tsize2
    ELSE
      CALL Initiate(edgeValue, sorted_edge)
      edgePtr => edgeTree%GetValuePointer(edgeValue)
      obj%elementData(iel)%globalEdges(iedge) = edgePtr%id
    END IF

  END DO

END DO

CALL edgeTree%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateEdgeConnectivity

END SUBMODULE EdgeDataMethods
