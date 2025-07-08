! This program is a part of EASIFEM library
!) Expandable And Scalable Infrastructure for Finite Element Methods
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

SUBMODULE(FEDOF_Class) GetMethods
USE AbstractMesh_Class, ONLY: PARAM_MAX_CONNECTIVITY_SIZE
USE ElemData_Class, ONLY: ElemData_, &
                          ElemData_GetTotalEntities, &
                          ElemData_GetTotalGlobalVertexNodes, &
                          ElemData_GetEdge, &
                          ElemData_GetFace, &
                          ElemData_GetCell

USE ReferenceElement_Method, ONLY: ReferenceElementInfo

USE Display_Method, ONLY: ToString

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%baseContinuity//obj%baseInterpolation
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                               GetVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVertexDOF
tsize = 1
ans(1) = obj%mesh%GetLocalNodeNumber(globalNode, islocal=islocal)
END PROCEDURE obj_GetVertexDOF

!----------------------------------------------------------------------------
!                                                                 GetEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEdgeDOF1
INTEGER(I4B) :: ii
tsize = 0
DO ii = obj%edgeIA(globalEdge), obj%edgeIA(globalEdge + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO
END PROCEDURE obj_GetEdgeDOF1

!----------------------------------------------------------------------------
!                                                           GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEdgeDOF1
ans = obj%edgeIA(globalEdge + 1) - obj%edgeIA(globalEdge)
END PROCEDURE obj_GetTotalEdgeDOF1

!----------------------------------------------------------------------------
!                                                                 GetEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEdgeDOF2
INTEGER(I4B) :: globalEdge
globalEdge = obj%mesh%GetGlobalEdgeNumber(globalElement=globalElement, &
                             islocal=islocal, localEdgeNumber=localEdgeNumber)
CALL obj%GetEdgeDOF(globalEdge=globalEdge, ans=ans, tsize=tsize, &
                    islocal=islocal)
END PROCEDURE obj_GetEdgeDOF2

!----------------------------------------------------------------------------
!                                                       GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEdgeDOF2
INTEGER(I4B) :: globalEdge
globalEdge = obj%mesh%GetGlobalEdgeNumber(globalElement=globalElement, &
                             islocal=islocal, localEdgeNumber=localEdgeNumber)
ans = obj%GetTotalEdgeDOF(globalEdge=globalEdge, islocal=islocal)
END PROCEDURE obj_GetTotalEdgeDOF2

!----------------------------------------------------------------------------
!                                                                 GetFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFaceDOF1
INTEGER(I4B) :: ii
tsize = 0
DO ii = obj%faceIA(globalface), obj%faceIA(globalface + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO
END PROCEDURE obj_GetFaceDOF1

!----------------------------------------------------------------------------
!                                                            GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFaceDOF1
ans = obj%faceIA(globalface + 1) - obj%faceIA(globalface)
END PROCEDURE obj_GetTotalFaceDOF1

!----------------------------------------------------------------------------
!                                                                 GetFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFaceDOF2
INTEGER(I4B) :: globalFace
globalFace = obj%mesh%GetGlobalFaceNumber(globalElement=globalElement, &
                             islocal=islocal, localFaceNumber=localFaceNumber)
CALL obj%GetFaceDOF(globalFace=globalFace, ans=ans, tsize=tsize, &
                    islocal=islocal)
END PROCEDURE obj_GetFaceDOF2

!----------------------------------------------------------------------------
!                                                           GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFaceDOF2
INTEGER(I4B) :: globalFace
globalFace = obj%mesh%GetGlobalFaceNumber(globalElement=globalElement, &
                             islocal=islocal, localFaceNumber=localFaceNumber)
ans = obj%GetTotalFaceDOF(globalFace=globalFace, islocal=islocal)
END PROCEDURE obj_GetTotalFaceDOF2

!----------------------------------------------------------------------------
!                                                                 GetCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellDOF
INTEGER(I4B) :: ii, jj
jj = obj%mesh%GetLocalElemNumber(globalElement=globalCell, islocal=islocal)
tsize = 0
DO ii = obj%cellIA(jj), obj%cellIA(jj + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO
END PROCEDURE obj_GetCellDOF

!----------------------------------------------------------------------------
!                                                            GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalCellDOF
INTEGER(I4B) :: jj
jj = obj%mesh%GetLocalElemNumber(globalElement=globalCell, islocal=islocal)
ans = obj%cellIA(jj + 1) - obj%cellIA(jj)
END PROCEDURE obj_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                     GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexDOF
ans = obj%tNodes
END PROCEDURE obj_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                           GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF1
ans = obj%tdof
END PROCEDURE obj_GetTotalDOF1

!----------------------------------------------------------------------------
!                                                           GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetTotalDOF2()'
#endif

TYPE(ElemData_), POINTER :: elemdata
INTEGER(I4B) :: ii, jj, ent(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

elemdata => obj%mesh%GetElemDataPointer(globalElement=globalElement, &
                                        islocal=islocal)

ent = ElemData_GetTotalEntities(elemdata)

ans = ElemData_GetTotalGlobalVertexNodes(elemdata)

DO ii = 1, ent(2)
  jj = ElemData_GetEdge(elemdata, ii)
  ans = ans + obj%edgeIA(jj + 1) - obj%edgeIA(jj)
END DO

DO ii = 1, ent(3)
  jj = ElemData_GetFace(elemdata, ii)
  ans = ans + obj%faceIA(jj + 1) - obj%faceIA(jj)
END DO

jj = ElemData_GetCell(elemdata, islocal=.TRUE.)
ans = ans + obj%cellIA(jj + 1) - obj%cellIA(jj)

elemdata => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTotalDOF2

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetTotalDOF3()'
#endif

TYPE(ElemData_), POINTER :: elemdata
INTEGER(I4B) :: ii, jj, ent(4)
CHARACTER(1) :: opt0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

opt0 = opt(1:1)

elemdata => obj%mesh%GetElemDataPointer(globalElement=globalElement, &
                                        islocal=islocal)

ent = ElemData_GetTotalEntities(elemdata)

SELECT CASE (opt0)
CASE ('v', 'V')
  CALL onlyVertex
CASE ('e', 'E')
  CALL onlyEdge
CASE ('f', 'F')
  CALL onlyFace
CASE ('c', 'C')
  CALL onlyCell

CASE DEFAULT
  CALL onlyVertex
  CALL onlyEdge
  CALL onlyFace
  CALL onlyCell
END SELECT

elemdata => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

CONTAINS
SUBROUTINE onlyVertex
  ans = ElemData_GetTotalGlobalVertexNodes(elemdata)
END SUBROUTINE onlyVertex

SUBROUTINE onlyEdge
  DO ii = 1, ent(2)
    jj = ElemData_GetEdge(elemdata, ii)
    ans = ans + obj%edgeIA(jj + 1) - obj%edgeIA(jj)
  END DO
END SUBROUTINE onlyEdge

SUBROUTINE onlyFace
  DO ii = 1, ent(3)
    jj = ElemData_GetFace(elemdata, ii)
    ans = ans + obj%faceIA(jj + 1) - obj%faceIA(jj)
  END DO
END SUBROUTINE onlyFace

SUBROUTINE onlyCell
  jj = ElemData_GetCell(elemdata, islocal=.TRUE.)
  ans = ans + obj%cellIA(jj + 1) - obj%cellIA(jj)
END SUBROUTINE onlyCell

END PROCEDURE obj_GetTotalDOF3

!----------------------------------------------------------------------------
!                                                         GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetConnectivity()'
#endif

INTEGER(I4B) :: tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tdof = obj%GetTotalDOF(globalElement=globalElement, isLocal=isLocal)
ALLOCATE (ans(tdof))
CALL obj%GetConnectivity_(ans=ans, tsize=tdof, opt=opt, &
                          globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                           GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetConnectivity_()'
#endif

INTEGER(I4B) :: ent(4)
INTEGER(I4B) :: ii, jj, kk, a, b, localElement, tvertices
INTEGER(I4B) :: temp(PARAM_MAX_CONNECTIVITY_SIZE)
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

localElement = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                           islocal=islocal)

ent = obj%mesh%GetTotalEntities(globalElement=localElement, islocal=yes)

CALL obj%mesh%GetConnectivity_(globalElement=localElement, islocal=yes, &
                               opt=opt, tsize=jj, ans=temp)

! points
tvertices = obj%mesh%GetTotalVertexNodes(globalElement=localElement, &
                                         islocal=yes)

a = 1; b = ent(1)
jj = 1
DO ii = a, tvertices
  CALL obj%GetVertexDOF(globalNode=temp(ii), ans=ans(jj:), tsize=kk, &
                        islocal=.FALSE.)
  jj = jj + kk
END DO

! edges
a = b + 1; b = b + ent(2)
DO ii = a, b
  CALL obj%GetEdgeDOF(globalEdge=temp(ii), ans=ans(jj:), tsize=kk)
  jj = jj + kk
END DO

! faces
a = b + 1; b = b + ent(3)
DO ii = a, b
  CALL obj%GetFaceDOF(globalFace=temp(ii), ans=ans(jj:), tsize=kk)
  jj = jj + kk
END DO

! cell
a = b + 1; b = b + ent(4)
DO ii = a, b
  CALL obj%GetCellDOF(globalCell=temp(ii), ans=ans(jj:), tsize=kk, &
                      islocal=.FALSE.)
  jj = jj + kk
END DO

tsize = jj - 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity_

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                           GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer
ans => obj%mesh
END PROCEDURE obj_GetMeshPointer

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
ans = obj%baseInterpolation
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                               GetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellOrder
INTEGER(I4B) :: jj

jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

cellOrder(1) = obj%cellOrder(jj)
tcellOrder = 1

END PROCEDURE obj_GetCellOrder

!----------------------------------------------------------------------------
!                                                                  GetOrders
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrders
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetOrders()'
#endif

INTEGER(I4B) :: ii, jj, tNodeOrder, cellCon(1), &
                faceCon(ReferenceElementInfo%maxEdges), &
                edgeCon(ReferenceElementInfo%maxEdges), &
                nodeCon(PARAM_MAX_CONNECTIVITY_SIZE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

CALL obj%mesh%GetConnectivity_(globalElement=jj, islocal=.TRUE., &
         cellCon=cellCon, faceCon=faceCon, edgeCon=edgeCon, nodeCon=nodeCon, &
              tCellCon=tCellOrder, tFaceCon=tFaceOrder, tEdgeCon=tEdgeOrder, &
                               tNodeCon=tNodeOrder)

DO ii = 1, tCellOrder
  cellOrder(ii) = obj%cellOrder(jj)
END DO

DO jj = 1, tFaceOrder
  ii = faceCon(jj)
  faceOrder(1, jj) = obj%faceOrder(ii)
  faceOrder(2:3, jj) = faceOrder(1, jj)
END DO

DO jj = 1, tEdgeOrder
  ii = edgeCon(jj)
  edgeOrder(jj) = obj%edgeOrder(ii)
END DO

CALL obj%mesh%GetOrientation(cellOrient=cellOrient, faceOrient=faceOrient, &
    edgeOrient=edgeOrient, tCellOrient=tCellOrient, tFaceOrient=tFaceOrient, &
         tEdgeOrient=tEdgeOrient, globalElement=globalElement, islocal=.TRUE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetOrders

!----------------------------------------------------------------------------
!                                                   GetMaxTotalConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalConnectivity()"
#endif

INTEGER(I4B) :: ii, telems, tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
telems = obj%mesh%GetTotalElements()

DO ii = 1, telems
  tdof = obj%GetTotalDOF(globalElement=ii, isLocal=.TRUE.)
  ans = MAX(ans, tdof)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetMaxTotalConnectivity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetQuadraturePoints1()'
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetElemTopologyIndx(globalElement=globalElement, islocal=islocal)

CALL obj%fe(ii)%ptr%GetQuadraturePoints(quad=quad, order=order, &
         quadratureType=quadratureType, alpha=alpha, beta=beta, lambda=lambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetQuadraturePoints1

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetQuadraturePoints2()'
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetElemTopologyIndx(globalElement=globalElement, islocal=islocal)

CALL obj%fe(ii)%ptr%GetQuadraturePoints(quad=quad, p=p, q=q, r=r, &
           quadratureType1=quadratureType1, quadratureType2=quadratureType2, &
                quadratureType3=quadratureType3, alpha1=alpha1, beta1=beta1, &
               lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                                  alpha3=alpha3, beta3=beta3, lambda3=lambda3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetQuadraturePoints2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
CHARACTER(6) :: casename
CHARACTER(*), PARAMETER :: myName = 'obj_GetLocalElemShapeData()'

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

casename = obj%GetCaseName()

SELECT CASE (casename)
CASE ('H1LAGR')
  CALL obj%GetLocalElemShapeDataH1Lagrange(globalElement=globalElement, &
                                    elemsd=elemsd, quad=quad, islocal=islocal)
CASE ('H1HIER', 'H1HEIR')
  CALL obj%GetLocalElemShapeDataH1Hierarchical(globalElement=globalElement, &
                                    elemsd=elemsd, quad=quad, islocal=islocal)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for case name')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                 GetLocalElemShapeData1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeDataH1Lagrange
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetLocalElemShapeDataH1Lagrange()'
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, iel, cellOrder(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
cellOrder(1) = obj%cellOrder(iel)
ii = obj%mesh%GetElemTopologyIndx(globalElement=iel, islocal=.TRUE.)

#ifdef DEBUG_VER
isok = ii .NE. 0
CALL AssertError1(isok, myname, &
                  'Element topology index is not found')
#endif
CALL obj%fe(ii)%ptr%SetOrder(order=cellOrder(1), errCheck=.TRUE.)
CALL obj%fe(ii)%ptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeDataH1Lagrange

!----------------------------------------------------------------------------
!                                                 GetLocalElemShapeData1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeDataH1Hierarchical
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName='obj_GetLocalElemShapeDataH1Hierarchical()'
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

ii = obj%mesh%GetElemTopologyIndx(globalElement=globalElement, islocal=islocal)

CALL obj%GetOrders(globalElement=globalElement, islocal=islocal, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
        cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient, &
                 tcellOrder=indx(1), tfaceOrder=indx(2), tedgeOrder=indx(3), &
                   tcellOrient=indx(4), tfaceOrient=indx(5:6), &
                   tedgeOrient=indx(7))

! The above modification is necessary for quad and hexa elements
! Here we are setting uniform cell order in all directions
! for such elements
IF (indx(1) .EQ. 1) cellOrder(2:3) = cellOrder(1)
CALL obj%fe(ii)%ptr%SetOrder(order=cellOrder(1), cellOrder=cellOrder, &
            faceOrder=faceOrder, edgeOrder=edgeOrder, cellOrient=cellOrient, &
              faceOrient=faceOrient, edgeOrient=edgeOrient, errCheck=.TRUE., &
                             tcell=indx(1), tface=indx(2), tedge=indx(3))

CALL obj%fe(ii)%ptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeDataH1Hierarchical

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetGlobalElemShapeData()'
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetElemTopologyIndx(globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
isok = ii .GT. 0
CALL AssertError1(isok, myname, &
             'Element topology index is zero or negative ii = '//tostring(ii))

isok = ASSOCIATED(obj%fe(ii)%ptr)
CALL AssertError1(isok, myname, &
                  'obj%fe(ii)%ptr is not associated')
#endif

CALL obj%fe(ii)%ptr%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                           geoElemsd=geoElemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
