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

SUBMODULE(FEDomain_Class) MeshDataMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%meshVolume%InitiateNodeToElements()
CALL obj%meshSurface%InitiateNodeToElements()
CALL obj%meshCurve%InitiateNodeToElements()
CALL obj%meshPoint%InitiateNodeToElements()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%meshVolume%InitiateNodeToNodes()
CALL obj%meshSurface%InitiateNodeToNodes()
CALL obj%meshCurve%InitiateNodeToNodes()
CALL obj%meshPoint%InitiateNodeToNodes()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%meshVolume%InitiateElementToElements()
CALL obj%meshSurface%InitiateElementToElements()
CALL obj%meshCurve%InitiateElementToElements()
CALL obj%meshPoint%InitiateElementToElements()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                  InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateBoundaryData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateBoundaryData()"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%meshVolume%InitiateBoundaryData()
CALL obj%meshSurface%InitiateBoundaryData()
CALL obj%meshCurve%InitiateBoundaryData()
CALL obj%meshPoint%InitiateBoundaryData()
CALL obj%SetFacetElementType()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%meshVolume%InitiateFacetElements()
CALL obj%meshSurface%InitiateFacetElements()
CALL obj%meshCurve%InitiateFacetElements()
CALL obj%meshPoint%InitiateFacetElements()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                   InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodeToNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%meshVolume%InitiateExtraNodeToNodes()
CALL obj%meshSurface%InitiateExtraNodeToNodes()
CALL obj%meshCurve%InitiateExtraNodeToNodes()
CALL obj%meshPoint%InitiateExtraNodeToNodes()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_InitiateExtraNodeToNodes

END SUBMODULE MeshDataMethods
