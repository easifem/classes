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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(FEDomain_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer1()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(dim)

IF (.NOT. isok) THEN
  ans => obj%mesh
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

SELECT CASE (dim)
CASE (0)
  ans => obj%meshPoint
CASE (1)
  ans => obj%meshCurve
CASE (2)
  ans => obj%meshSurface
CASE (3)
  ans => obj%meshVolume
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetGlobalEdgeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalEdgeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalEdgeNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetGlobalEdgeNumber(globalElement=globalElement, &
                             islocal=islocal, localEdgeNumber=localEdgeNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalEdgeNumber

!----------------------------------------------------------------------------
!                                                         GetGlobalFaceNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFaceNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalFaceNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetGlobalFaceNumber(globalElement=globalElement, &
                             islocal=islocal, localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalFaceNumber

!----------------------------------------------------------------------------
!                                                          GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                  islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemNumber1

!----------------------------------------------------------------------------
!                                                          GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                  islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemNumber2

!----------------------------------------------------------------------------
!                                                                GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetElemData(globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemData

!----------------------------------------------------------------------------
!                                                                GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemDataPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemDataPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%mesh%GetElemDataPointer(globalElement=globalElement, &
                                   islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemDataPointer

!----------------------------------------------------------------------------
!                                                        GetTotalEntitiesList
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntitiesList
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntitiesList()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetTotalEntities(globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalEntitiesList

!----------------------------------------------------------------------------
!                                                          GetConnectivity1_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity1_()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(dim)

IF (.NOT. isok) THEN
  CALL obj%mesh%GetConnectivity_(globalElement=globalElement, &
                                 ans=ans, tsize=tsize, opt=opt, &
                                 islocal=islocal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

SELECT CASE (dim)
CASE (0)
  CALL obj%meshPoint%GetConnectivity_(globalElement=globalElement, &
                                      ans=ans, tsize=tsize, opt=opt, &
                                      islocal=islocal)

CASE (1)
  CALL obj%meshCurve%GetConnectivity_(globalElement=globalElement, &
                                      ans=ans, tsize=tsize, opt=opt, &
                                      islocal=islocal)
CASE (2)
  CALL obj%meshSurface%GetConnectivity_(globalElement=globalElement, &
                                        ans=ans, tsize=tsize, opt=opt, &
                                        islocal=islocal)
CASE (3)
  CALL obj%meshVolume%GetConnectivity_(globalElement=globalElement, &
                                       ans=ans, tsize=tsize, opt=opt, &
                                       islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity1_

!----------------------------------------------------------------------------
!                                                          GetConnectivity2_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity2_()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(dim)

IF (.NOT. isok) THEN
  CALL obj%mesh%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
                                 edgeCon=edgeCon, nodeCon=nodeCon, &
                                 tCellCon=tCellCon, tFaceCon=tFaceCon, &
                                 tEdgeCon=tEdgeCon, tNodeCon=tNodeCon, &
                                 globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

SELECT CASE (dim)
CASE (0)
  CALL obj%meshPoint%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
                                      edgeCon=edgeCon, nodeCon=nodeCon, &
                                      tCellCon=tCellCon, tFaceCon=tFaceCon, &
                                      tEdgeCon=tEdgeCon, tNodeCon=tNodeCon, &
                                      globalElement=globalElement, &
                                      islocal=islocal)

CASE (1)
  CALL obj%meshCurve%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
                                      edgeCon=edgeCon, nodeCon=nodeCon, &
                                      tCellCon=tCellCon, tFaceCon=tFaceCon, &
                                      tEdgeCon=tEdgeCon, tNodeCon=tNodeCon, &
                                      globalElement=globalElement, &
                                      islocal=islocal)
CASE (2)
  CALL obj%meshSurface%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
                                        edgeCon=edgeCon, nodeCon=nodeCon, &
                                       tCellCon=tCellCon, tFaceCon=tFaceCon, &
                                       tEdgeCon=tEdgeCon, tNodeCon=tNodeCon, &
                                        globalElement=globalElement, &
                                        islocal=islocal)
CASE (3)

  CALL obj%meshVolume%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
                                       edgeCon=edgeCon, nodeCon=nodeCon, &
                                       tCellCon=tCellCon, tFaceCon=tFaceCon, &
                                       tEdgeCon=tEdgeCon, tNodeCon=tNodeCon, &
                                       globalElement=globalElement, &
                                       islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity2_

!----------------------------------------------------------------------------
!                                                         GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes1()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim)

! total vertex nodes in globalElement
isok = PRESENT(globalElement)
IF (isok) THEN
  ans = meshptr%GetTotalVertexNodes(globalElement=globalElement, &
                                    islocal=islocal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  meshptr => NULL()
  RETURN
END IF

! total vertex nodes in mesh of given entityNum
isok = PRESENT(entityNum)
IF (isok) THEN
  ans = meshptr%GetTotalVertexNodes(meshid=entityNum)
  meshptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! total vertex nodes in mesh
ans = meshptr%GetTotalVertexNodes()
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexNodes1

!----------------------------------------------------------------------------
!                                                         GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes2()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim)

! total vertex nodes in globalElement
ans = meshptr%GetTotalVertexNodes(globalElement=globalElement, &
                                  islocal=islocal)

meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexNodes2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
