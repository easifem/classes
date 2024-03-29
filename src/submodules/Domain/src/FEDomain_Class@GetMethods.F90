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
USE ReallocateUtility
USE InputUtility
USE BoundingBox_Method
USE F95_BLAS, ONLY: Copy
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             IsNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNodePresent
ans = .TRUE.
IF (globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs) THEN
  ans = .FALSE.
  RETURN
END IF

SELECT CASE (obj%nsd)
CASE (0)
  ans = obj%meshPoint%IsNodePresent(globalNode)
CASE (1)
  ans = obj%meshCurve%IsNodePresent(globalNode)
CASE (2)
  ans = obj%meshSurface%IsNodePresent(globalNode)
CASE (3)
  ans = obj%meshVolume%IsNodePresent(globalNode)
END SELECT
END PROCEDURE obj_IsNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
INTEGER(I4B) :: dim0

dim0 = Input(default=obj%nsd, option=dim)
SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%IsElementPresent(globalElement=globalElement)
CASE (2)
  ans = obj%meshSurface%IsElementPresent(globalElement=globalElement)
CASE (1)
  ans = obj%meshCurve%IsElementPresent(globalElement=globalElement)
CASE (0)
  ans = obj%meshPoint%IsElementPresent(globalElement=globalElement)
END SELECT

END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                          getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
INTEGER(I4B) :: dim0

dim0 = Input(default=obj%nsd, option=dim)

SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%GetConnectivity(globalElement=globalElement)
CASE (2)
  ans = obj%meshSurface%GetConnectivity(globalElement=globalElement)
CASE (1)
  ans = obj%meshCurve%GetConnectivity(globalElement=globalElement)
CASE (0)
  ans = obj%meshPoint%GetConnectivity(globalElement=globalElement)
END SELECT

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetNodeToElements(globalNode=globalNode)
CASE (2)
  ans = obj%meshSurface%GetNodeToElements(globalNode=globalNode)
CASE (1)
  ans = obj%meshCurve%GetNodeToElements(globalNode=globalNode)
CASE (0)
  ans = obj%meshPoint%GetNodeToElements(globalNode=globalNode)
END SELECT
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetNodeToElements(globalNode=globalNode)
CASE (2)
  ans = obj%meshSurface%GetNodeToElements(globalNode=globalNode)
CASE (1)
  ans = obj%meshCurve%GetNodeToElements(globalNode=globalNode)
CASE (0)
  ans = obj%meshPoint%GetNodeToElements(globalNode=globalNode)
END SELECT
END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                             getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
IF (PRESENT(dim)) THEN
  SELECT CASE (dim)
  CASE (3)
    ans = obj%meshVolume%GetTotalNodes()
  CASE (2)
    ans = obj%meshSurface%GetTotalNodes()
  CASE (1)
    ans = obj%meshCurve%GetTotalNodes()
  CASE (0)
    ans = obj%meshPoint%GetTotalNodes()
  END SELECT

ELSE
  ans = obj%tNodes
END IF
END PROCEDURE obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes1
ans = obj%GetTotalNodes(dim=dim)
END PROCEDURE obj_tNodes1

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes2
ans = obj%GetTotalNodes()
END PROCEDURE obj_tNodes2

!----------------------------------------------------------------------------
!                                                           getTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements
IF (PRESENT(dim)) THEN
  SELECT CASE (dim)
  CASE (3)
    ans = obj%meshVolume%GetTotalElements()
  CASE (2)
    ans = obj%meshSurface%GetTotalElements()
  CASE (1)
    ans = obj%meshCurve%GetTotalElements()
  CASE (0)
    ans = obj%meshPoint%GetTotalElements()
  END SELECT

ELSE
  ans = SUM(obj%tElements)
END IF
END PROCEDURE obj_GetTotalElements

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements1
ans = obj%GetTotalElements()
END PROCEDURE obj_tElements1

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements2
ans = obj%GetTotalElements(dim=dim)
END PROCEDURE obj_tElements2

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber1()"
#endif

SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetLocalNodeNumber(globalNode=globalNode)
CASE (2)
  ans = obj%meshSurface%GetLocalNodeNumber(globalNode=globalNode)
CASE (1)
  ans = obj%meshCurve%GetLocalNodeNumber(globalNode=globalNode)
CASE (0)
  ans = obj%meshPoint%GetLocalNodeNumber(globalNode=globalNode)
CASE DEFAULT
  ans = 0
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found')
#endif
END SELECT

END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
#endif

SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetLocalNodeNumber(globalNode=globalNode)
CASE (2)
  ans = obj%meshSurface%GetLocalNodeNumber(globalNode=globalNode)
CASE (1)
  ans = obj%meshCurve%GetLocalNodeNumber(globalNode=globalNode)
CASE (0)
  ans = obj%meshPoint%GetLocalNodeNumber(globalNode=globalNode)
CASE DEFAULT
  ans = 0
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found')
#endif
END SELECT

END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalNodeNumber1()"
#endif

SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetGlobalNodeNumber(localNode=localNode)
CASE (2)
  ans = obj%meshSurface%GetGlobalNodeNumber(localNode=localNode)
CASE (1)
  ans = obj%meshCurve%GetGlobalNodeNumber(localNode=localNode)
CASE (0)
  ans = obj%meshPoint%GetGlobalNodeNumber(localNode=localNode)
CASE DEFAULT
  ans = 0
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found')
#endif
END SELECT
END PROCEDURE obj_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalNodeNumber2()"
#endif

SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetGlobalNodeNumber(localNode=localNode)
CASE (2)
  ans = obj%meshSurface%GetGlobalNodeNumber(localNode=localNode)
CASE (1)
  ans = obj%meshCurve%GetGlobalNodeNumber(localNode=localNode)
CASE (0)
  ans = obj%meshPoint%GetGlobalNodeNumber(localNode=localNode)
CASE DEFAULT
  ans = 0
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found')
#endif
END SELECT
END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                         GetTotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities
#ifdef DEBUG_VER
LOGICAL(LGT) :: problem
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntities()"

problem = dim .LT. 0 .OR. dim .GT. 3

IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: dim of the mesh should be in [0,1,2,3]")
END IF
#endif

ans = obj%tEntities(dim)
END PROCEDURE obj_GetTotalEntities

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer1
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

END PROCEDURE obj_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                               getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord()"
LOGICAL(LGT) :: problem

problem = .NOT. ALLOCATED(obj%nodeCoord)
IF (problem) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Nodecoord is not allocated.")
  RETURN
END IF
#endif

nodeCoord(1:obj%nsd, :) = obj%nodeCoord(1:obj%nsd, :)

END PROCEDURE obj_GetNodeCoord

!----------------------------------------------------------------------------
!                                                       getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord2
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: nsd
localNode = obj%GetLocalNodeNumber(globalNode=globalNode)
nsd = SIZE(nodeCoord, 1)
nodeCoord = obj%nodeCoord(1:nsd, localNode)
END PROCEDURE obj_GetNodeCoord2

!----------------------------------------------------------------------------
!                                                        getNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoordPointer
ans => obj%nodeCoord
END PROCEDURE obj_GetNodeCoordPointer

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetNptrs()
CASE (2)
  ans = obj%meshSurface%GetNptrs()
CASE (1)
  ans = obj%meshCurve%GetNptrs()
CASE (0)
  ans = obj%meshPoint%GetNptrs()
END SELECT
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
SELECT CASE (dim)
CASE (3)
  CALL obj%meshVolume%GetNptrs_(nptrs=nptrs)
CASE (2)
  CALL obj%meshSurface%GetNptrs_(nptrs=nptrs)
CASE (1)
  CALL obj%meshCurve%GetNptrs_(nptrs=nptrs)
CASE (0)
  CALL obj%meshPoint%GetNptrs_(nptrs=nptrs)
END SELECT
END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetInternalNptrs()
CASE (2)
  ans = obj%meshSurface%GetInternalNptrs()
CASE (1)
  ans = obj%meshCurve%GetInternalNptrs()
CASE (0)
  ans = obj%meshPoint%GetInternalNptrs()
END SELECT
END PROCEDURE obj_GetInternalNptrs
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
