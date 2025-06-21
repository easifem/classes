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

SUBMODULE(OneDimFEDOF_Class) GetMethods
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
ans = obj%fe%GetCaseName()
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                               GetVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVertexDOF
tsize = 1
ans(1) = obj%mesh%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_GetVertexDOF

!----------------------------------------------------------------------------
!                                                                 GetCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellDOF
INTEGER(I4B) :: ii, jj
jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
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
jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
ans = obj%cellIA(jj + 1) - obj%cellIA(jj)
END PROCEDURE obj_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                           GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexDOF
ans = obj%mesh%GetTotalVertexNodes()
END PROCEDURE obj_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF1
ans = obj%tdof
END PROCEDURE obj_GetTotalDOF1

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetTotalDOF2()'
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                 islocal=islocal)
ans = 2 + obj%cellIA(ii + 1) - obj%cellIA(ii)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (opt(1:1))
CASE ('v', 'V')
  ans = 2
CASE ('c', 'C')
  ans = obj%GetTotalCellDOF(globalElement=globalElement, &
                            islocal=islocal)

CASE DEFAULT
  ans = obj%GetTotalDOF(globalElement=globalElement, &
                        islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

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

CALL obj%mesh%GetConnectivity_(globalElement=localElement, islocal=yes, &
                               opt=opt, tsize=jj, ans=temp)

! points
tvertices = 2

a = 1; b = 2
jj = 1
DO ii = 1, 2
  CALL obj%GetVertexDOF(globalNode=temp(ii), ans=ans(jj:), tsize=kk, &
                        islocal=.FALSE.)
  jj = jj + kk
END DO

! cell
CALL obj%GetCellDOF(globalCell=temp(ii), ans=ans(jj:), tsize=kk, &
                    islocal=.FALSE.)
jj = jj + kk

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
ans = obj%fe%GetBaseInterpolation()
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
   cellCon=cellCon, nodeCon=nodeCon, tCellCon=tCellOrder, tNodeCon=tNodeOrder)

DO ii = 1, tCellOrder
  cellOrder(ii) = obj%cellOrder(jj)
END DO

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

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetQuadraturePoints1()'
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%fe%GetQuadraturePoints(quad=quad, order=order, &
         quadratureType=quadratureType, alpha=alpha, beta=beta, lambda=lambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetQuadraturePoints

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

ii = obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
CALL obj%fe%SetOrder(order=obj%cellOrder(ii), errCheck=.TRUE.)
CALL obj%fe%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeData

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

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myname, 'obj%fe is not associated')
#endif

CALL obj%fe%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, geoElemsd=geoElemsd)

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
