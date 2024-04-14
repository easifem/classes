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
!

SUBMODULE(AbstractDomain_Class) MeshDataMethods
USE GlobalData, ONLY: stdout
USE Display_Method
USE DomainConnectivity_Class
USE Kdtree2_Module, ONLY: Kdtree2_create
USE CPUTime_Class, ONLY: CPUTime_
USE ElemData_Class, ONLY: BOUNDARY_ELEMENT

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateKdtree
INTEGER(I4B) :: nsd
CHARACTER(*), PARAMETER :: myName = "obj_InitiateKdtree()"

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL obj%DeallocateKdtree()

#ifdef DEBUG_VER

isok = ALLOCATED(obj%nodeCoord)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractDomain_::obj%nodeCoord not allocated')
  RETURN
END IF

#endif

nsd = obj%nsd
! FUNCTION Kdtree2_create(input_data, dim, sort, rearrange) RESULT(mr)
obj%kdtree => Kdtree2_Create(input_data=obj%nodeCoord(1:nsd, :), &
                             dim=nsd, sort=.FALSE., rearrange=.TRUE.)

ALLOCATE (obj%kdresult(obj%tNodes))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL Display(modName//" : "//myName//  &
    & " : time : "//  &
    & tostring(TypeCPUTime%GetTime()), unitno=stdout)
END IF

END PROCEDURE obj_InitiateKdtree

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

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetElementType
CHARACTER(*), PARAMETER :: myName = "obj_SetFacetElementType()"
CLASS(AbstractMesh_), POINTER :: masterMesh
INTEGER(I4B) :: kk, iel, iface, telements
INTEGER(I4B), ALLOCATABLE :: faceID(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

masterMesh => obj%GetMeshPointer(dim=obj%nsd)

CALL masterMesh%GetParam(isBoundaryDataInitiated=isok)
IF (.NOT. isok) CALL masterMesh%InitiateBoundaryData()

telements = masterMesh%GetTotalElements()

DO iel = 1, telements
  isok = masterMesh%isElementPresent(globalElement=iel, &
                                     islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  isok = masterMesh%isBoundaryElement(globalElement=iel, &
                                      islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  faceID = masterMesh%GetBoundaryElementData(globalElement=iel, &
                                             islocal=.TRUE.)

  DO iface = 1, SIZE(faceID)

    kk = faceID(iface)

    CALL masterMesh%SetFacetElementType(globalElement=iel, &
      & iface=kk, facetElementType=BOUNDARY_ELEMENT, islocal=.TRUE.)

  END DO

END DO

NULLIFY (masterMesh)

IF (ALLOCATED(faceID)) DEALLOCATE (faceID)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDomainFacetElement
CHARACTER(*), PARAMETER :: myName = "obj_SetDomainFacetElement"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

END PROCEDURE obj_SetDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 SetMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshmap
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshmap"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetMeshmap

!----------------------------------------------------------------------------
!                                                       SetMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshFacetElement
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshFacetElement()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetMeshFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods
