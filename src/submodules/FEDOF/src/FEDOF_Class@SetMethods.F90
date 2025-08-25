! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(FEDOF_Class) SetMethods
USE ElemData_Class, ONLY: ElemData_, &
                          ElemData_GetTotalEntities, &
                          ElemData_GetEdge, &
                          ElemData_GetEdgeConnectivity, &
                          ElemData_GetElementToElements, &
                          ElemData_GetFace

USE IntegerUtility, ONLY: GetIntersection

USE ReferenceElement_Method, ONLY: ReferenceElementInfo
USE ReferenceElement_Method, ONLY: PARAM_REFELEM_MAX_FACES
USE AbstractMesh_Class, ONLY: PARAM_MAX_NODE_TO_ELEM

USE ReallocateUtility, ONLY: Reallocate

USE Display_Method, ONLY: ToString

USE GlobalData, ONLY: CHAR_LF

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"
#endif

INTEGER(I4B) :: tsize, ii, jj
LOGICAL(LGT) :: isok
INTEGER(INT8) :: int8_order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(order)

CALL Reallocate(obj%cellOrder, obj%tCells)

IF (tsize .EQ. 1) THEN

  int8_order = INT(order(1), kind=INT8)
  obj%cellOrder = int8_order

ELSE

  DO ii = 1, tsize
    isok = obj%mesh%IsElementPresent(globalElement=ii, islocal=islocal)

    IF (isok) THEN
      jj = obj%mesh%GetLocalElemNumber(globalElement=ii, islocal=islocal)
      int8_order = INT(order(ii), kind=INT8)
      ! IF (jj .NE. 0)
      obj%cellOrder(jj) = int8_order
    END IF

  END DO

END IF

obj%maxCellOrder = MAXVAL(obj%cellOrder)
obj%maxFaceOrder = obj%maxCellOrder
obj%maxEdgeOrder = obj%maxFaceOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                             SetFaceOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFaceOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFaceOrder()"
#endif

INTEGER(I4B) :: nrow, ncol, ii, iel, jj, kk, &
                e2e(ReferenceElementInfo%maxFaces, 3)
LOGICAL(LGT) :: isok
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT), ALLOCATABLE :: foundFaces(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main
CALL Reallocate(foundFaces, obj%tFaces)
CALL Reallocate(obj%faceOrder, obj%tFaces)

DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=.TRUE.)
  isok = ASSOCIATED(elemdata)
  IF (.NOT. isok) CYCLE

  ! faces
  CALL ElemData_GetElementToElements(obj=elemdata, ans=e2e, nrow=nrow, &
                                     ncol=ncol, includeBoundaryElement=.TRUE.)

  DO ii = 1, nrow
    kk = ElemData_GetFace(elemdata, e2e(ii, 2))
    IF (foundFaces(kk)) CYCLE

    foundFaces(kk) = .TRUE.

   jj = obj%mesh%GetLocalElemNumber(globalElement=e2e(ii, 1), islocal=.FALSE.)
    obj%faceOrder(kk) = MIN(obj%cellOrder(iel), obj%cellOrder(jj))
  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetFaceOrder

!----------------------------------------------------------------------------
!                                                               SetEdgeOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEdgeOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetEdgeOrder()"
#endif

INTEGER(I4B) :: tsize, ii, iel, ent(4), jj, &
                kk, edgeCon(2), n2e1(PARAM_MAX_NODE_TO_ELEM), &
                n2e2(PARAM_REFELEM_MAX_FACES), n2e(PARAM_MAX_NODE_TO_ELEM), &
                tsize1, tsize2
LOGICAL(LGT) :: isok
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT), ALLOCATABLE :: foundEdges(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main
CALL Reallocate(foundEdges, obj%tEdges)
CALL Reallocate(obj%edgeOrder, obj%tEdges)

DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=.TRUE.)
  isok = ASSOCIATED(elemdata)
  IF (.NOT. isok) CYCLE

  ent = ElemData_GetTotalEntities(elemdata)

  !edges
  DO ii = 1, ent(2)
    jj = ElemData_GetEdge(elemdata, ii)
    IF (foundEdges(jj)) CYCLE

    foundEdges(jj) = .TRUE.
    CALL ElemData_GetEdgeConnectivity(obj=elemdata, ii=ii, ans=edgeCon, &
                                      tsize=tsize)

    CALL obj%mesh%GetNodeToElements_(globalNode=edgeCon(1), islocal=.FALSE., &
                                     ans=n2e1, tsize=tsize1)

    CALL obj%mesh%GetNodeToElements_(globalNode=edgeCon(2), islocal=.FALSE., &
                                     ans=n2e2, tsize=tsize2)

    CALL GetIntersection(a=n2e1(1:tsize1), b=n2e2(1:tsize2), c=n2e, &
                         tsize=tsize)

    DO kk = 1, tsize
      n2e(kk) = obj%mesh%GetLocalElemNumber(globalElement=n2e(kk), &
                                            islocal=.FALSE.)
    END DO

    obj%edgeOrder(jj) = MINVAL(obj%cellOrder(n2e(1:tsize)))

  END DO

END DO

IF (ALLOCATED(foundEdges)) DEALLOCATE (foundEdges)
elemdata => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetEdgeOrder

!----------------------------------------------------------------------------
!                                                         SetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_SetQuadraturePoints1()'
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetElemTopologyIndx(globalElement=globalElement, &
                                  islocal=islocal)

CALL obj%fe(ii)%ptr%SetQuadraturePoints()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadraturePoints

!----------------------------------------------------------------------------
!                                                       SetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_SetLocalElemShapeData()'
#endif

CHARACTER(6) :: casename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

casename = obj%GetCaseName()

SELECT CASE (casename)
CASE ('H1LAGR')
  CALL obj%SetLocalElemShapeDataH1Lagrange(globalElement=globalElement, &
                                           islocal=islocal)
CASE ('H1HIER', 'H1HEIR')
  CALL obj%SetLocalElemShapeDataH1Hierarchical(globalElement=globalElement, &
                                               islocal=islocal)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for case name')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetLocalElemShapeData

!----------------------------------------------------------------------------
!                                             SetLocalElemShapeDataH1Lagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLocalElemShapeDataH1Lagrange
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_SetLocalElemShapeDataH1Lagrange()'
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, iel, cellOrder(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                  islocal=islocal)
cellOrder(1) = obj%cellOrder(iel)
ii = obj%mesh%GetElemTopologyIndx(globalElement=iel, islocal=.TRUE.)

#ifdef DEBUG_VER
isok = ii .NE. 0
CALL AssertError1(isok, myname, &
                  'Element topology index is not found')
#endif

CALL obj%fe(ii)%ptr%SetOrder(order=cellOrder(1), errCheck=.TRUE.)
CALL obj%fe(ii)%ptr%SetLocalElemShapeData()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetLocalElemShapeDataH1Lagrange

!----------------------------------------------------------------------------
!                                       SetLocalElemShapeDataH1Hierarchhical
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLocalElemShapeDataH1Hierarchical
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName='obj_SetLocalElemShapeDataH1Hierarchical()'
#endif

INTEGER(I4B) :: ii, cellOrder(3), &
                faceOrder(3, ReferenceElementInfo%maxEdges), &
                edgeOrder(ReferenceElementInfo%maxEdges), &
                faceOrient(3, ReferenceElementInfo%maxEdges), &
                edgeOrient(ReferenceElementInfo%maxEdges), &
                cellOrient(3), indx(10)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetElemTopologyIndx(globalElement=globalElement, &
                                  islocal=islocal)

CALL obj%GetOrders(globalElement=globalElement, islocal=islocal, &
                   cellOrder=cellOrder, faceOrder=faceOrder, &
                   edgeOrder=edgeOrder, cellOrient=cellOrient, &
                   faceOrient=faceOrient, edgeOrient=edgeOrient, &
                   tcellOrder=indx(1), tfaceOrder=indx(2), &
                   tedgeOrder=indx(3), tcellOrient=indx(4), &
                   tfaceOrient=indx(5:6), tedgeOrient=indx(7))

! The above modification is necessary for quad and hexa elements
! Here we are setting uniform cell order in all directions
! for such elements
IF (indx(1) .EQ. 1) cellOrder(2:3) = cellOrder(1)
CALL obj%fe(ii)%ptr%SetOrder(order=cellOrder(1), cellOrder=cellOrder, &
                             faceOrder=faceOrder, edgeOrder=edgeOrder, &
                             cellOrient=cellOrient, faceOrient=faceOrient, &
                             edgeOrient=edgeOrient, errCheck=.TRUE., &
                             tcell=indx(1), tface=indx(2), tedge=indx(3))

CALL obj%fe(ii)%ptr%SetLocalElemShapeData()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetLocalElemShapeDataH1Hierarchical

!----------------------------------------------------------------------------
!                                                         SetGeoElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetGeoElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_SetGeoElemShapeData()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetGeoElemShapeData

!----------------------------------------------------------------------------
!                                                     SetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_SetGlobalElemShapeData()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                               SetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_SetNodeCoord()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetNodeCoord

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
