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
#endif

  IF (ASSOCIATED(obj%meshVolume)) THEN
    CALL obj%meshVolume%InitiateNodeToElements()
  END IF

  IF (ASSOCIATED(obj%meshSurface)) THEN
    CALL obj%meshSurface%InitiateNodeToElements()
  END IF

  IF (ASSOCIATED(obj%meshCurve)) THEN
    CALL obj%meshCurve%InitiateNodeToElements()
  END IF

  IF (ASSOCIATED(obj%meshPoint)) THEN
    CALL obj%meshPoint%InitiateNodeToElements()
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif

END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  IF (ASSOCIATED(obj%meshVolume)) THEN
    CALL obj%meshVolume%InitiateNodeToNodes()
  END IF

  IF (ASSOCIATED(obj%meshSurface)) THEN
    CALL obj%meshSurface%InitiateNodeToNodes()
  END IF

  IF (ASSOCIATED(obj%meshCurve)) THEN
    CALL obj%meshCurve%InitiateNodeToNodes()
  END IF

  IF (ASSOCIATED(obj%meshPoint)) THEN
    CALL obj%meshPoint%InitiateNodeToNodes()
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  IF (ASSOCIATED(obj%meshVolume)) THEN
    CALL obj%meshVolume%InitiateElementToElements()
  END IF

  IF (ASSOCIATED(obj%meshSurface)) THEN
    CALL obj%meshSurface%InitiateElementToElements()
  END IF

  IF (ASSOCIATED(obj%meshCurve)) THEN
    CALL obj%meshCurve%InitiateElementToElements()
  END IF

  IF (ASSOCIATED(obj%meshPoint)) THEN
    CALL obj%meshPoint%InitiateElementToElements()
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"

  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif DEBUG_VER

  IF (ASSOCIATED(obj%meshVolume)) &
    CALL obj%meshVolume%InitiateFacetElements()
  IF (ASSOCIATED(obj%meshSurface)) &
    CALL obj%meshSurface%InitiateFacetElements()
  IF (ASSOCIATED(obj%meshCurve)) &
    CALL obj%meshCurve%InitiateFacetElements()
  IF (ASSOCIATED(obj%meshPoint)) &
    CALL obj%meshPoint%InitiateFacetElements()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                   InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  IF (ASSOCIATED(obj%meshVolume)) THEN
    CALL obj%meshVolume%InitiateExtraNodeToNodes()
  END IF

  IF (ASSOCIATED(obj%meshSurface)) THEN
    CALL obj%meshSurface%InitiateExtraNodeToNodes()
  END IF

  IF (ASSOCIATED(obj%meshCurve)) THEN
    CALL obj%meshCurve%InitiateExtraNodeToNodes()
  END IF

  IF (ASSOCIATED(obj%meshPoint)) THEN
    CALL obj%meshPoint%InitiateExtraNodeToNodes()
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_InitiateExtraNodeToNodes

END SUBMODULE MeshDataMethods
