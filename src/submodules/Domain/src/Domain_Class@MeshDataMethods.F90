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
USE BaseMethod
USE DomainConnectivity_Class
USE AbstractMesh_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateNodeToElements
CHARACTER(*), PARAMETER :: myName = "Domain_InitiateNodeToElements()"
INTEGER(I4B) :: ii, dim
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO dim = 1, obj%GetNSD()
  DO ii = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateNodeToElements()
  END DO
END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE Domain_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateNodeToNodes
INTEGER(I4B) :: ii, dim
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(*), PARAMETER :: myName = "Domain_InitiateExtraNodeToNodes()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO dim = 1, obj%GetNSD()
  DO ii = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateNodeToNodes()
  END DO
END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE Domain_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateElementToElements
CHARACTER(*), PARAMETER :: myName = "Domain_InitiateElementToElements()"
INTEGER(I4B) :: ii, dim
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO dim = 1, obj%GetNSD()
  DO ii = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateElementToElements()
  END DO
END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Domain_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                  InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateBoundaryData
CHARACTER(*), PARAMETER :: myName = "Domain_InitiateBoundaryData()"
INTEGER(I4B) :: ii, dim
CLASS(AbstractMesh_), POINTER :: meshptr
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO dim = 1, obj%GetNSD()
  DO ii = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateBoundaryData()
  END DO
END DO

CALL obj%SetFacetElementType()

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE Domain_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateFacetElements
CHARACTER(*), PARAMETER :: myName = "Domain_InitiateFacetElements()"
INTEGER(I4B) :: ii, dim, nsd, tmesh
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

nsd = obj%GetNSD()
DO dim = 1, nsd
  tmesh = obj%GetTotalMesh(dim=dim)
  DO ii = 1, tmesh
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateFacetElements()
  END DO
END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE Domain_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                   InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateExtraNodeToNodes
INTEGER(I4B) :: ii, dim
CHARACTER(*), PARAMETER :: myName = "Domain_InitiateExtraNodeToNodes()"
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO dim = 1, obj%GetNSD()
  DO ii = 1, obj%GetTotalMesh(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateExtraNodeToNodes()
  END DO
END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Domain_InitiateExtraNodeToNodes

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetFacetElementType
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, kk, iel, iface
INTEGER(I4B), ALLOCATABLE :: faceID(:), faceNptrs(:)
CHARACTER(*), PARAMETER :: myName = "Domain_SetFacetElementType"
LOGICAL(LGT) :: isVar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

tsize = obj%GetTotalMesh(dim=obj%nsd)

DO ii = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)

  CALL masterMesh%GetParam(isBoundaryDataInitiated=isVar)

  IF (.NOT. isVar) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
      & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
      & ', entityNum = '//tostring(ii)// &
      & ' Boundary data is not initiated, calling '// &
      & ' InitiateBoundaryData')
    CALL masterMesh%InitiateBoundaryData()
  END IF

  DO iel = masterMesh%GetMinElemNumber(), masterMesh%GetMaxElemNumber()

    IF (.NOT. masterMesh%isElementPresent(iel)) CYCLE
    IF (.NOT. masterMesh%isBoundaryElement(iel)) CYCLE

    faceID = masterMesh%GetBoundaryElementData(globalElement=iel)

    DO iface = 1, SIZE(faceID)

      kk = faceID(iface)
      faceNptrs = masterMesh%GetFacetConnectivity(globalElement=iel, &
        & iface=kk)

      DO jj = 1, tsize
        IF (jj .NE. ii) THEN
          slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)
          IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN
            CALL masterMesh%SetFacetElementType(globalElement=iel, &
              & iface=kk, facetElementType=BOUNDARY_ELEMENT)
            EXIT
          END IF
        END IF
      END DO

    END DO

  END DO

END DO

NULLIFY (masterMesh, slaveMesh)

IF (ALLOCATED(faceID)) DEALLOCATE (faceID)
IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Domain_SetFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetDomainFacetElement
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet, elemtype
INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
LOGICAL(LGT) :: faceFound, isok
CHARACTER(*), PARAMETER :: myName = "Domain_SetDomainFacetElement()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

tsize = obj%GetTotalMesh(dim=obj%nsd)

DO ii = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)

  CALL masterMesh%GetParam(isFacetDataInitiated=isok)

  IF (.NOT. isok) CALL masterMesh%InitiateFacetElements()

  !INFO:
  ! Get total number of facet elements
  tDomFacet = masterMesh%GetTotalFacetElements()
  tMeshFacet = 0

  !INFO:
  ! Start a loop over all facet elements
  DO iel = 1, tDomFacet

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
    ! facetNptrs; if there exists such as mesh, then
    ! the face-element is actually meshFacet (not domainFacet).

    DO jj = 1, tsize
      IF (jj .NE. ii) THEN

        slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)

        IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN

          faceFound = .TRUE.
          tMeshFacet = tMeshFacet + 1
          EXIT

        END IF
      END IF
    END DO

    IF (faceFound) THEN
      CALL masterMesh%SetFacetParam(facetElement=iel, &
                                    elementType=BOUNDARY_ELEMENT)
    END IF

  END DO

END DO

NULLIFY (masterMesh, slaveMesh)
IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Domain_SetDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 SetMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetMeshmap
CHARACTER(*), PARAMETER :: myName = "Domain_SetMeshmap()"
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
    & '[INTERNAL ERROR] :: Domain_::obj%meshFacetData is already ALLOCATED')
  RETURN
END IF

tsize = obj%GetTotalMesh(dim=obj%nsd)
CALL Reallocate(meshmap, tsize, tsize)

DO ii = 1, tsize

  masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)
  tDomFacet = masterMesh%GetTotalBoundaryFacetElements()

  CALL masterMesh%GetParam(isFacetDataInitiated=isok)

  IF (.NOT. isok) CALL masterMesh%InitiateFacetElements()

  DO jj = ii + 1, tsize

    slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)

    DO iel = 1, tDomFacet

      CALL masterMesh%GetFacetParam(elementType=elemtype, &
                                    facetElement=iel)

      IF (elemtype .EQ. BOUNDARY_ELEMENT) THEN

        nptrs = AbstractMeshGetFacetConnectivity( &
          & obj=masterMesh, &
          & facetElement=iel, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE.)

        IF (slaveMesh%isAllNodePresent(nptrs)) THEN

          meshmap(ii, jj) = 1
          EXIT

        END IF

      END IF

    END DO

  END DO

END DO

tMeshFacet = COUNT(meshmap .EQ. 1)

! ALLOCATE meshFacetData
ALLOCATE (obj%meshFacetData(tMeshFacet))
CALL Initiate(obj%meshMap, ncol=tsize, nrow=tsize)
CALL SetSparsity(obj%meshMap, graph=meshmap)
CALL SetSparsity(obj%meshMap)

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(meshmap)) DEALLOCATE (meshmap)
NULLIFY (masterMesh, slaveMesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Domain_SetMeshmap

!----------------------------------------------------------------------------
!                                                       SetMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetMeshFacetElement
CHARACTER(*), PARAMETER :: myName = "Domain_SetMeshFacetElement()"
CLASS(AbstractMesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tSize, ii, imeshfacet, tBndyFacet_master, &
  & iface_slave, iface_master, tmeshfacet, tBndyFacet_slave, elemtype
INTEGER(I4B), ALLOCATABLE :: faceNptrs_master(:), faceNptrs_slave(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

! main
IF (.NOT. obj%meshmap%isInitiated) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Domain_::obj%meshMap is not initiated, calling obj%SetMeshMap()')
  CALL obj%SetMeshMap()
END IF

tsize = obj%GetTotalMesh(dim=obj%nsd)

! Set masterMesh and slaveMesh of meshFacetData
DO ii = 1, tSize
  DO imeshfacet = obj%meshmap%IA(ii), obj%meshmap%IA(ii + 1) - 1
    obj%meshFacetData(imeshfacet)%masterMesh = ii
    obj%meshFacetData(imeshfacet)%slaveMesh = obj%meshmap%JA(imeshfacet)
  END DO
END DO

! Count number of facet element in each meshFacetData
DO imeshfacet = 1, SIZE(obj%meshFacetData)
  masterMesh => obj%GetMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%masterMesh)

  slaveMesh => obj%GetMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%slaveMesh)

  ! FIXME:
  tBndyFacet_master = masterMesh%GetTotalFacetElements()
  ! FIXME:
  tBndyFacet_slave = slaveMesh%GetTotalFacetElements()

  ! count the number of facet elements in imeshfacet

  tmeshfacet = 0

  ! FIXME:
  DO iface_master = 1, tBndyFacet_master

    CALL masterMesh%GetFacetParam(elementType=elemtype, &
                                  facetElement=iface_master)

    isok = elemtype .EQ. BOUNDARY_ELEMENT
    IF (.NOT. isok) CYCLE

    faceNptrs_master = AbstractMeshGetFacetConnectivity( &
      & obj=masterMesh, &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)

    IF (slaveMesh%isAllNodePresent(faceNptrs_master)) &
      & tmeshfacet = tmeshfacet + 1

  END DO

  ! Prepare data for imeshfacet
  CALL obj%meshFacetData(imeshfacet)%Initiate(tmeshfacet)

  ii = 0

  DO iface_master = 1, tBndyFacet_master

    CALL masterMesh%GetFacetParam(elementType=elemtype, &
                                  facetElement=iface_master)

    isok = elemtype .EQ. BOUNDARY_ELEMENT
    IF (.NOT. isok) CYCLE

    faceNptrs_master = AbstractMeshGetFacetConnectivity( &
      & obj=masterMesh, &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)

    IF (slaveMesh%isAllNodePresent(faceNptrs_master)) THEN

      DO iface_slave = 1, tBndyFacet_slave

        CALL slaveMesh%GetFacetParam(elementType=elemtype, &
                                     facetElement=iface_slave)

        isok = elemtype .EQ. BOUNDARY_ELEMENT
        IF (.NOT. isok) CYCLE

        faceNptrs_slave = AbstractMeshGetFacetConnectivity( &
          & obj=slaveMesh, &
          & facetElement=iface_slave, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE.)

        IF (faceNptrs_master.IN.faceNptrs_slave) THEN

          ii = ii + 1

          ! masterCellNumber
          obj%meshFacetData(imeshfacet)%masterCellNumber(ii) = &
            & masterMesh%GetMasterCellNumber( &
              & facetElement=iface_master, &
              & elementType=BOUNDARY_ELEMENT)

          ! masterLocalFacetID
          obj%meshFacetData(imeshfacet)%masterLocalFacetID(ii) = &
            & masterMesh%GetLocalFacetID( &
              & facetElement=iface_master, &
              & isMaster=.TRUE., &
              & elementType=BOUNDARY_ELEMENT)

          ! slaveCellNumber
          obj%meshFacetData(imeshfacet)%slaveCellNumber(ii) = &
            & slaveMesh%GetMasterCellNumber( &
            & facetElement=iface_slave, &
            & elementType=BOUNDARY_ELEMENT)

          ! slaveLocalFacetID
          obj%meshFacetData(imeshfacet)%slaveLocalFacetID(ii) = &
            & slaveMesh%GetLocalFacetID( &
              & facetElement=iface_slave, &
              & isMaster=.TRUE., &
              & elementType=BOUNDARY_ELEMENT)

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

END PROCEDURE Domain_SetMeshFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods
