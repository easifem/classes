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

SUBMODULE(AbstractKernel_Class) BCMethods
USE BaseMethod
USE Mesh_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            AddDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddDirichletBC
CHARACTER(*), PARAMETER :: myName = "obj_AddDirichletBC"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL AppendDirichletBC(dbc=obj%dbc, dbcNo=dbcNo, param=param,  &
  & boundary=boundary, dom=obj%dom)
! INFO: AddDirichletBC is defined in DirichletBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_AddDirichletBC

!----------------------------------------------------------------------------
!                                                      GetDirichletBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCPointer"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. ALLOCATED(obj%dbc)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[ALLOCATION ERROR] :: AbstractKernel_::obj%dbc '// &
    & 'is not allocated!')
END IF

ans => GetDirichletBCPointer(dbc=obj%dbc, dbcNo=dbcNo)
! INFO:  This method is defined in DirichletBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                               AddNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddNeumannBC
CHARACTER(*), PARAMETER :: myName = "obj_AddNeumannBC()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL AppendNeumannBC(nbc=obj%nbc, nbcNo=nbcNo, param=param, &
  & boundary=boundary, dom=obj%dom)
! INFO: This method is defined in NeumannBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_AddNeumannBC

!----------------------------------------------------------------------------
!                                                             AddPointSource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPointSource
CHARACTER(*), PARAMETER :: myName = "obj_AddPointSource()"
LOGICAL(LGT) :: isSelectionByNode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

! check boundary for isSelectionByNode

CALL boundary%GetParam(isSelectionByNodeNum=isSelectionByNode)

IF (.NOT. isSelectionByNode) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
   & '[CONFIG ERROR] :: boundary is not configured for '//  &
   & 'isSelectionByNodeNum. '//  &
   & 'In AddPointSource boundary must be configured for '//  &
   & 'isSelectionByNode.')
  RETURN
END IF

CALL AppendNeumannBC(nbc=obj%nbcPointSource, nbcNo=nbcNo,  &
  & param=param, boundary=boundary, dom=obj%dom)
! INFO: This method is defined in NeumannBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_AddPointSource

!----------------------------------------------------------------------------
!                                                       GetNeumannBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNeumannBCPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetNeumannBCPointer()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. ALLOCATED(obj%nbc)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[ALLOCATION ERROR] :: AbstractKernel_::obj%nbc '// &
    & 'is not allocated!')
END IF

ans => GetNeumannBCPointer(nbc=obj%nbc, nbcNo=nbcNo)
! INFO: This method is defined in NeumannBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_GetNeumannBCPointer

!----------------------------------------------------------------------------
!                                                       GetNeumannBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointSourcePointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointSourcePointer()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. ALLOCATED(obj%nbcPointSource)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[ALLOCATION ERROR] :: AbstractKernel_::obj%nbcPointSource '// &
    & 'is not allocated!')
END IF

ans => GetNeumannBCPointer(nbc=obj%nbc, nbcNo=nbcNo)
! INFO: This method is defined in NeumannBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_GetPointSourcePointer

!----------------------------------------------------------------------------
!                                                               AddNitscheBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddNitscheBC
CHARACTER(*), PARAMETER :: myName = "obj_AddNitscheBC()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL AppendNitscheBC(dbc=obj%wdbc, dbcNo=dbcNo, param=param, &
  & boundary=boundary, dom=obj%dom)
! INFO: This method is defined in NitscheBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_AddNitscheBC

!----------------------------------------------------------------------------
!                                                        GetNitscheBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNitscheBCPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetNitscheBCPointer()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. ALLOCATED(obj%wdbc)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[ALLOCATION ERROR] :: AbstractKernel_::obj%wdbc '// &
    & 'is not allocated!')
END IF

ans => GetNitscheBCPointer(dbc=obj%wdbc, dbcNo=dbcNo)
! INFO: This method is defined in NitscheBC_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_GetNitscheBCPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNitscheMeshData
CHARACTER(*), PARAMETER :: myName = "obj_SetNitscheMeshData"
INTEGER(I4B) :: ii, jj, tMeshID
CLASS(NitscheBC_), POINTER :: nbc
CLASS(DomainConnectivity_), POINTER :: domCon
INTEGER(I4B), ALLOCATABLE :: intvec(:), meshID(:), tFacetElements(:)
LOGICAL(LGT) :: isVar
CLASS(Mesh_), POINTER :: facetMesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. obj%isNitsche) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractElasticity_::obj%isNitsche is false')
  RETURN
END IF

IF (ALLOCATED(obj%nitscheFacetToCell)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractElasticity_::obj%nitscheFacetToCell'// &
    & 'is already initiated')
  RETURN
END IF

nbc => NULL()
domCon => NULL()

DO ii = 1, SIZE(obj%wdbc)

  nbc => obj%wdbc(ii)%ptr

  IF (.NOT. ASSOCIATED(nbc)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: AbstractElasticity_::obj%wdbc( '//  &
      & tostring(ii)//" )"//"is not associated")
    RETURN
  END IF

  CALL nbc%GetParam(isSelectionByMeshID=isVar)

  IF (.NOT. isVar) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: AbstractElasticity_::obj%wdbc( '// &
      & tostring(ii)//" )"//" has isSelectionByMeshID = .FALSE. ")
  END IF

  intvec = nbc%GetMeshID(dim=obj%nsd - 1)
  CALL APPEND(meshID, intvec)

END DO

CALL RemoveDuplicates(meshID)

tMeshID = SIZE(meshID)

ALLOCATE (obj%nitscheFacetToCell(tMeshID))

ii = obj%dom%GetTotalMesh(dim=obj%nsd - 1)
CALL Reallocate(obj%nitscheLocalID, ii)
CALL Reallocate(tFacetElements, ii)

DO jj = 1, ii
  tFacetElements(jj) = obj%dom%GetTotalElements(dim=obj%nsd - 1, entityNum=jj)
END DO

jj = 0

DO ii = 1, tMeshID
  jj = jj + 1
  obj%nitscheLocalID(meshID(ii)) = jj
END DO

DO ii = 1, tMeshID
  ALLOCATE (DomainConnectivity_ :: obj%nitscheFacetToCell(ii)%ptr)
  domCon => obj%nitscheFacetToCell(ii)%ptr

  facetMesh => obj%dom%GetMeshPointer( &
    & dim=obj%nsd - 1, &
    & entityNum=meshID(ii))

  CALL domCon%InitiateFacetToCellData( &
    & facetMesh=facetMesh, &
    & cellDomain=obj%dom)
END DO

DO ii = 1, SIZE(obj%wdbc)
  nbc => obj%wdbc(ii)%ptr
  intvec = nbc%GetMeshID(dim=obj%nsd - 1)
  CALL nbc%SetCellData( &
    & meshID=intvec, &
    & localID=obj%nitscheLocalID, &
    & tFacetElements=tFacetElements(intvec), &
    & domConList=obj%nitscheFacetToCell)
END DO

NULLIFY (facetMesh, nbc, domCon)
IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
IF (ALLOCATED(intvec)) DEALLOCATE (intvec)
IF (ALLOCATED(tFacetElements)) DEALLOCATE (tFacetElements)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetNitscheMeshData

END SUBMODULE BCMethods
