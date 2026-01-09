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
USE BaseType, ONLY: TypeRefelemOpt
USE BaseType, ONLY: math => TypeMathOpt
USE EdgeDataBinaryTree_Class, ONLY: EdgeDataBinaryTree_
USE EdgeData_Class, ONLY: EdgeDataInitiate => Initiate
USE EdgeData_Class, ONLY: EdgeData_
USE EdgeData_Class, ONLY: EdgeData_Pointer
USE GlobalData, ONLY: INT8
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: RefElemGetGeoParam
USE ReferenceLine_Method, ONLY: MaxOrder_Line
USE SortUtility, ONLY: Sort

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateEdgeConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateEdgeConnectivity()"
#endif

INTEGER(I4B) :: tElements, iel, elemType, tEdges, &
                localEdges(MaxOrder_Line + 1, TypeRefelemOpt%maxEdges), &
                edge(2), sorted_edge(2), &
                tNodes, tsize1, tsize2, iedge
LOGICAL(LGT) :: isok
TYPE(EdgeDataBinaryTree_) :: edgeTree
TYPE(EdgeData_) :: edgeValue
TYPE(EdgeData_), POINTER :: edgePtr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = obj%isEdgeConnectivityInitiated
IF (isok) RETURN

#ifdef DEBUG_VER
isok = ALLOCATED(obj%elementData)
CALL AssertError1(isok, myName, &
                  'AbstractMesh_::obj%elementData not allocated')
#endif

tElements = obj%GetTotalElements()

CALL edgeTree%Initiate()

obj%isEdgeConnectivityInitiated = math%yes

DO iel = 1, tElements
  isok = .NOT. obj%elementData(iel)%ptr%isActive
  IF (isok) CYCLE

  elemType = obj%elementData(iel)%ptr%name
  CALL RefElemGetGeoParam(elemType=elemType, tEdges=tEdges, tNodes=tNodes, &
                          edgeCon=localEdges, edgeOpt=math%one_i)

  CALL Reallocate(obj%elementData(iel)%ptr%globalEdges, tEdges)
  CALL Reallocate(obj%elementData(iel)%ptr%edgeOrient, tEdges)

  DO iedge = 1, tEdges

    edge = obj%elementData(iel)%ptr%globalNodes(localEdges(1:2, iedge))
    sorted_edge = SORT(edge)

    edgePtr => EdgeData_Pointer(sorted_edge)

    tsize1 = edgeTree%SIZE()
    CALL edgeTree%Insert(edgePtr)
    tsize2 = edgeTree%SIZE()

    obj%tEdges = tsize2

    IF (edge(1) .GT. edge(2)) THEN
      obj%elementData(iel)%ptr%edgeOrient(iedge) = -1_INT8
    ELSE
      obj%elementData(iel)%ptr%edgeOrient(iedge) = 1_INT8
    END IF

    IF (tsize1 .NE. tsize2) THEN
      obj%elementData(iel)%ptr%globalEdges(iedge) = tsize2
      edgePtr%id = tsize2
    ELSE
      CALL EdgeDataInitiate(edgeValue, sorted_edge)
      edgePtr => edgeTree%GetValuePointer(edgeValue)
      obj%elementData(iel)%ptr%globalEdges(iedge) = edgePtr%id
    END IF

  END DO

END DO

CALL edgeTree%DEALLOCATE()
NULLIFY (edgePtr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateEdgeConnectivity

!----------------------------------------------------------------------------
!                                                           Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE EdgeDataMethods
