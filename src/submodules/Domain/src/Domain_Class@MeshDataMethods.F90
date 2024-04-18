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

SUBMODULE(Domain_Class) MeshDataMethods
USE ElemData_Class, ONLY: BOUNDARY_ELEMENT, DOMAIN_BOUNDARY_ELEMENT
USE AbstractMesh_Class, ONLY: AbstractMeshGetFacetConnectivity
USE CSRSparsity_Method, ONLY: CSR_SetSparsity => SetSparsity, &
                              CSR_Initiate => Initiate
USE ReallocateUtility, ONLY: Reallocate
USE IntegerUtility, ONLY: OPERATOR(.IN.)
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#if defined (__METHOD_NAME__)
#undef __METHOD_NAME__
#endif
#define __METHOD_NAME__ InitiateNodeToElements
#include "./include/macro.F90"
END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodes
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToNodes()"
#if defined (__METHOD_NAME__)
#undef __METHOD_NAME__
#endif
#define __METHOD_NAME__ InitiateNodeToNodes
#include "./include/macro.F90"
END PROCEDURE obj_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#if defined (__METHOD_NAME__)
#undef __METHOD_NAME__
#endif
#define __METHOD_NAME__ InitiateElementToElements
#include "./include/macro.F90"
END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                     InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateBoundaryData
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#if defined (__METHOD_NAME__)
#undef __METHOD_NAME__
#endif
#define __METHOD_NAME__ InitiateBoundaryData
#include "./include/macro.F90"
END PROCEDURE obj_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#if defined (__METHOD_NAME__)
#undef __METHOD_NAME__
#endif
#define __METHOD_NAME__ InitiateFacetElements
#include "./include/macro.F90"
END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                     InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodeToNodes
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#if defined (__METHOD_NAME__)
#undef __METHOD_NAME__
#endif
#define __METHOD_NAME__ InitiateExtraNodeToNodes
#include "./include/macro.F90"
END PROCEDURE obj_InitiateExtraNodeToNodes

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetElementType
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, kk, iel, iface, telems
INTEGER(I4B), ALLOCATABLE :: faceID(:), faceNptrs(:)
CHARACTER(*), PARAMETER :: myName = "obj_SetFacetElementType"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

tsize = obj%GetTotalEntities(dim=obj%nsd)

meshloop: DO ii = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)

  CALL masterMesh%GetParam(isBoundaryDataInitiated=isok)
  IF (.NOT. isok) CALL masterMesh%InitiateBoundaryData()

  telems = masterMesh%GetTotalElements()

  elemloop: DO iel = 1, telems

    isok = masterMesh%isBoundaryElement(globalElement=iel, islocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    faceID = masterMesh%GetBoundaryElementData(globalElement=iel, &
                                               islocal=.TRUE.)

    faceloop: DO iface = 1, SIZE(faceID)

      kk = faceID(iface)
      faceNptrs = masterMesh%GetFacetConnectivity(globalElement=iel, &
                                                  iface=kk, islocal=.TRUE.)

      slave_meshloop: DO jj = 1, tsize

        IF (jj .NE. ii) THEN
          slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)

          isok = slaveMesh%IsAllNodePresent(faceNptrs)
          IF (isok) THEN
            CALL masterMesh%SetFacetElementType(globalElement=iel, &
                  iface=kk, facetElementType=BOUNDARY_ELEMENT, islocal=.TRUE.)
            EXIT
          END IF

        END IF

      END DO slave_meshloop

    END DO faceloop

  END DO elemloop

END DO meshloop

NULLIFY (masterMesh, slaveMesh)

IF (ALLOCATED(faceID)) DEALLOCATE (faceID)
IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDomainFacetElement
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet, elemtype
INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
LOGICAL(LGT) :: faceFound, isok
CHARACTER(*), PARAMETER :: myName = "obj_SetDomainFacetElement()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

tsize = obj%GetTotalEntities(dim=obj%nsd)

master_meshloop: DO ii = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)

  CALL masterMesh%GetParam(isFacetDataInitiated=isok)

  IF (.NOT. isok) CALL masterMesh%InitiateFacetElements()

  tDomFacet = masterMesh%GetTotalFacetElements()
  tMeshFacet = 0

  facetloop: DO iel = 1, tDomFacet

    CALL masterMesh%GetFacetParam(facetElement=iel, elementType=elemtype)
    isok = elemtype .EQ. DOMAIN_BOUNDARY_ELEMENT
    IF (.NOT. isok) CYCLE

    faceNptrs = AbstractMeshGetFacetConnectivity( &
      & obj=masterMesh, &
      & facetElement=iel, &
      & elementType=DOMAIN_BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)

    faceFound = .FALSE.

    ! INFO:
    ! The code below checks if any other mesh contains the
    ! faceNptrs; if there exists such as mesh, then
    ! the facet element (iel) is actually a mesh facet (not domainFacet).

    slave_meshloop: DO jj = 1, tsize
      IF (jj .NE. ii) THEN

        slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)

        IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN

          faceFound = .TRUE.
          tMeshFacet = tMeshFacet + 1
          EXIT slave_meshloop

        END IF
      END IF
    END DO slave_meshloop

    IF (faceFound) THEN
      CALL masterMesh%SetFacetParam(facetElement=iel, &
                                    elementType=BOUNDARY_ELEMENT)
    END IF

  END DO facetloop

END DO master_meshloop

NULLIFY (masterMesh, slaveMesh)
IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 SetMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshmap
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshmap()"
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet, elemtype
INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshmap(:, :)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (ALLOCATED(obj%meshFacetData)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: obj_::obj%meshFacetData is already ALLOCATED')
  RETURN
END IF

tsize = obj%GetTotalEntities(dim=obj%nsd)
CALL Reallocate(meshmap, tsize, tsize)

master_meshloop: DO ii = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)
  tDomFacet = masterMesh%GetTotalFacetElements()

  CALL masterMesh%GetParam(isFacetDataInitiated=isok)
  IF (.NOT. isok) CALL masterMesh%InitiateFacetElements()

  slave_meshloop: DO jj = ii + 1, tsize

    slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)

    master_elemloop: DO iel = 1, tDomFacet

      CALL masterMesh%GetFacetParam(elementType=elemtype, &
                                    facetElement=iel)

      isok = elemtype .EQ. BOUNDARY_ELEMENT
      IF (.NOT. isok) CYCLE

      nptrs = AbstractMeshGetFacetConnectivity( &
        & obj=masterMesh, &
        & facetElement=iel, &
        & elementType=BOUNDARY_ELEMENT, &
        & isMaster=.TRUE.)

      IF (slaveMesh%IsAllNodePresent(nptrs)) THEN

        meshmap(ii, jj) = 1
        EXIT master_elemloop

      END IF

    END DO master_elemloop

  END DO slave_meshloop

END DO master_meshloop

tMeshFacet = COUNT(meshmap .EQ. 1)

! ALLOCATE meshFacetData
ALLOCATE (obj%meshFacetData(tMeshFacet))
CALL CSR_Initiate(obj%meshMap, ncol=tsize, nrow=tsize)
CALL CSR_SetSparsity(obj%meshMap, graph=meshmap)
CALL CSR_SetSparsity(obj%meshMap)

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(meshmap)) DEALLOCATE (meshmap)
NULLIFY (masterMesh, slaveMesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetMeshmap

!----------------------------------------------------------------------------
!                                                       SetMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshFacetElement
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshFacetElement()"
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tSize, ii, imeshfacet, tFacets_master, &
  & iface_slave, iface_master, tmeshfacet, tFacets_slave, elemtype
INTEGER(I4B), ALLOCATABLE :: faceNptrs_master(:), faceNptrs_slave(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

isok = obj%meshmap%isInitiated
IF (.NOT. isok) CALL obj%SetMeshMap()

tsize = obj%GetTotalEntities(dim=obj%nsd)

!INFO: Set masterMesh and slaveMesh of meshFacetData
DO ii = 1, tSize
  DO imeshfacet = obj%meshmap%IA(ii), obj%meshmap%IA(ii + 1) - 1
    obj%meshFacetData(imeshfacet)%masterMesh = ii
    obj%meshFacetData(imeshfacet)%slaveMesh = obj%meshmap%JA(imeshfacet)
  END DO
END DO

!INFO: Count number of facet element in each meshFacetData
tsize = SIZE(obj%meshFacetData)

DO imeshfacet = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%masterMesh)

  slaveMesh => obj%GetMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%slaveMesh)

  tFacets_master = masterMesh%GetTotalFacetElements()
  tFacets_slave = slaveMesh%GetTotalFacetElements()

  ! count the number of facet elements in imeshfacet
  tmeshfacet = 0
  DO iface_master = 1, tFacets_master

    CALL masterMesh%GetFacetParam(elementType=elemtype, &
                                  facetElement=iface_master)

    isok = elemtype .EQ. BOUNDARY_ELEMENT
    IF (.NOT. isok) CYCLE

    faceNptrs_master = AbstractMeshGetFacetConnectivity( &
      & obj=masterMesh, &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)

    IF (slaveMesh%IsAllNodePresent(faceNptrs_master)) &
      tmeshfacet = tmeshfacet + 1

  END DO

  ! Prepare data for imeshfacet
  CALL obj%meshFacetData(imeshfacet)%Initiate(tmeshfacet)

  ii = 0

  DO iface_master = 1, tFacets_master

    CALL masterMesh%GetFacetParam(elementType=elemtype, &
                                  facetElement=iface_master)

    isok = elemtype .EQ. BOUNDARY_ELEMENT
    IF (.NOT. isok) CYCLE

    faceNptrs_master = AbstractMeshGetFacetConnectivity( &
                       obj=masterMesh, &
                       facetElement=iface_master, &
                       elementType=BOUNDARY_ELEMENT, &
                       isMaster=.TRUE.)

    IF (slaveMesh%IsAllNodePresent(faceNptrs_master)) THEN

      DO iface_slave = 1, tFacets_slave

        CALL slaveMesh%GetFacetParam(elementType=elemtype, &
                                     facetElement=iface_slave)

        isok = elemtype .EQ. BOUNDARY_ELEMENT
        IF (.NOT. isok) CYCLE

        faceNptrs_slave = AbstractMeshGetFacetConnectivity( &
                          obj=slaveMesh, &
                          facetElement=iface_slave, &
                          elementType=BOUNDARY_ELEMENT, &
                          isMaster=.TRUE.)

        IF (faceNptrs_master.IN.faceNptrs_slave) THEN

          ii = ii + 1

          ! masterCellNumber
          obj%meshFacetData(imeshfacet)%masterCellNumber(ii) = &
            masterMesh%GetMasterCellNumber( &
            facetElement=iface_master, &
            elementType=BOUNDARY_ELEMENT)

          ! masterLocalFacetID
          obj%meshFacetData(imeshfacet)%masterLocalFacetID(ii) = &
            masterMesh%GetLocalFacetID( &
            facetElement=iface_master, &
            isMaster=.TRUE., &
            elementType=BOUNDARY_ELEMENT)

          ! slaveCellNumber
          obj%meshFacetData(imeshfacet)%slaveCellNumber(ii) = &
            slaveMesh%GetMasterCellNumber( &
            facetElement=iface_slave, &
            elementType=BOUNDARY_ELEMENT)

          ! slaveLocalFacetID
          obj%meshFacetData(imeshfacet)%slaveLocalFacetID(ii) = &
            slaveMesh%GetLocalFacetID( &
            facetElement=iface_slave, &
            isMaster=.TRUE., &
            elementType=BOUNDARY_ELEMENT)

          EXIT

        END IF

      END DO

    END IF

  END DO

END DO

IF (ALLOCATED(faceNptrs_master)) DEALLOCATE (faceNptrs_master)
IF (ALLOCATED(faceNptrs_slave)) DEALLOCATE (faceNptrs_slave)
NULLIFY (masterMesh, slaveMesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetMeshFacetElement

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods
