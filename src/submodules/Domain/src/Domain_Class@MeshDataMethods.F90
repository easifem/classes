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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateNodeToElements
INTEGER(I4B) :: ii, dim
CLASS(Mesh_), POINTER :: meshptr
!
DO dim = 1, obj%getNSD()
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateNodeToElements()
  END DO
END DO
!
NULLIFY (meshptr)
!
END PROCEDURE Domain_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateNodeToNodes
INTEGER(I4B) :: ii, dim
CLASS(Mesh_), POINTER :: meshptr
!
DO dim = 1, obj%getNSD()
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateNodeToNodes()
  END DO
END DO
!
NULLIFY (meshptr)
!
END PROCEDURE Domain_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateElementToElements
INTEGER(I4B) :: ii, dim
CLASS(Mesh_), POINTER :: meshptr
!
DO dim = 1, obj%getNSD()
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateElementToElements()
  END DO
END DO
!
NULLIFY (meshptr)
!
END PROCEDURE Domain_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                  InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateBoundaryData
INTEGER(I4B) :: ii, dim
CLASS(Mesh_), POINTER :: meshptr
!
DO dim = 1, obj%getNSD()
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateBoundaryData()
  END DO
END DO
!
CALL obj%SetFacetElementType()
!
NULLIFY (meshptr)
!
END PROCEDURE Domain_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateFacetElements
INTEGER(I4B) :: ii, dim
CLASS(Mesh_), POINTER :: meshptr
!
DO dim = 1, obj%getNSD()
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateFacetElements()
  END DO
END DO
!
NULLIFY (meshptr)
!
END PROCEDURE Domain_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                   InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateExtraNodeToNodes
INTEGER(I4B) :: ii, dim
CLASS(Mesh_), POINTER :: meshptr
!
DO dim = 1, obj%getNSD()
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
    CALL meshptr%InitiateExtraNodeToNodes()
  END DO
END DO
!
NULLIFY (meshptr)
!
END PROCEDURE Domain_InitiateExtraNodeToNodes

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetFacetElementType
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, kk, iel, iface
INTEGER(I4B), ALLOCATABLE :: faceID(:), faceNptrs(:)
CHARACTER(*), PARAMETER :: myName = "Domain_SetFacetElementType"
LOGICAL(LGT) :: isVar

tsize = obj%getTotalMesh(dim=obj%nsd)

DO ii = 1, tsize

  masterMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=ii)

  CALL masterMesh%GetQuery(isBoundaryDataInitiated=isVar)

  IF (.NOT. isVar) THEN
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
    & ', entityNum = '//tostring(ii)// &
    & ' Boundary data is not initiated, calling '// &
    & ' InitiateBoundaryData')
    CALL masterMesh%InitiateBoundaryData()
  END IF

  DO iel = masterMesh%minElemNum, masterMesh%maxElemNum

    IF (.NOT. masterMesh%isElementPresent(iel)) CYCLE
    IF (.NOT. masterMesh%isBoundaryElement(iel)) CYCLE

    faceID = masterMesh%getBoundaryElementData(globalElement=iel)

    DO iface = 1, SIZE(faceID)

      kk = faceID(iface)
      faceNptrs = masterMesh%getFacetConnectivity(globalElement=iel, &
        & iface=kk)

      DO jj = 1, tsize
        IF (jj .NE. ii) THEN
          slaveMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=jj)
          IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN
            CALL masterMesh%setFacetElementType(globalElement=iel, &
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

END PROCEDURE Domain_SetFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setDomainFacetElement
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet
INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
LOGICAL(LGT) :: faceFound, isVar
CHARACTER(*), PARAMETER :: myName = "Domain_setDomainFacetElement"

tsize = obj%getTotalMesh(dim=obj%nsd)

DO ii = 1, tsize

  masterMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=ii)

  CALL masterMesh%GetQuery(isFacetDataInitiated=isVar)

  IF (.NOT. isVar) THEN
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
    & ', entityNum = '//tostring(ii)// &
    & ' Facet data is not initiated, calling '// &
    & ' InitiateFacetElements')
    CALL masterMesh%InitiateFacetElements()
  END IF

  tDomFacet = masterMesh%getTotalBoundaryFacetElements()
  tMeshFacet = 0

  DO iel = 1, tDomFacet

    faceNptrs = masterMesh%getFacetConnectivity( &
      & facetElement=iel, &
      & elementType=DOMAIN_BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)

    faceFound = .FALSE.

    ! The code below checks if any other mesh contains the
    ! facetNptrs; if there exists such as mesh, then
    ! the face-element is actually meshFacet (not domainFacet).

    DO jj = 1, tsize
      IF (jj .NE. ii) THEN

        slaveMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=jj)

        IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN

          faceFound = .TRUE.
          tMeshFacet = tMeshFacet + 1
          EXIT

        END IF
      END IF
    END DO

    IF (faceFound) THEN
      masterMesh%boundaryFacetData(iel)%elementType = &
        & BOUNDARY_ELEMENT
    END IF

  END DO

END DO

NULLIFY (masterMesh, slaveMesh)
IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)

END PROCEDURE Domain_setDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 setMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMeshmap
CHARACTER(*), PARAMETER :: myName = "Domain_setMeshmap"
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet
INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshmap(:, :)
LOGICAL(LGT) :: isVar
!
! main
!

IF (ALLOCATED(obj%meshFacetData)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'meshFacetData is already allocated... dellocate it first')
END IF

tsize = obj%getTotalMesh(dim=obj%nsd)
CALL Reallocate(meshmap, tsize, tsize)

DO ii = 1, tsize

  masterMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=ii)
  tDomFacet = masterMesh%getTotalBoundaryFacetElements()

  CALL masterMesh%GetQuery(isFacetDataInitiated=isVar)

  IF (.NOT. isVar) THEN
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
    & ', entityNum = '//tostring(ii)// &
    & ' Facet data is not initiated, calling '// &
    & ' InitiateFacetElements')
    CALL masterMesh%InitiateFacetElements()
  END IF

  DO jj = ii + 1, tsize

    slaveMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=jj)

    DO iel = 1, tDomFacet

      IF (masterMesh%boundaryFacetData(iel)%elementType &
        & .EQ. BOUNDARY_ELEMENT) THEN

        nptrs = masterMesh%getFacetConnectivity( &
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
!
! ALLOCATE meshFacetData
!
ALLOCATE (obj%meshFacetData(tMeshFacet))
CALL Initiate(obj%meshMap, ncol=tsize, nrow=tsize)
CALL SetSparsity(obj%meshMap, graph=meshmap)
CALL SetSparsity(obj%meshMap)

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(meshmap)) DEALLOCATE (meshmap)
NULLIFY (masterMesh, slaveMesh)

END PROCEDURE Domain_setMeshmap

!----------------------------------------------------------------------------
!                                                       setMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMeshFacetElement
CHARACTER(*), PARAMETER :: myName = "Domain_setMeshFacetElement"
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tSize, ii, imeshfacet, tBndyFacet_master, &
  & iface_slave, iface_master, tmeshfacet, tBndyFacet_slave
INTEGER(I4B), ALLOCATABLE :: faceNptrs_master(:), faceNptrs_slave(:)
!
! main
!
IF (.NOT. obj%meshmap%isInitiated) THEN
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'Domain_::obj%meshMap is not initiated, calling obj%setMeshMap()')
  CALL obj%setMeshMap()
END IF

tsize = obj%getTotalMesh(dim=obj%nsd)
!
! set masterMesh and slaveMesh of meshFacetData
!
DO ii = 1, tSize
  DO imeshfacet = obj%meshmap%IA(ii), obj%meshmap%IA(ii + 1) - 1
    obj%meshFacetData(imeshfacet)%masterMesh = ii
    obj%meshFacetData(imeshfacet)%slaveMesh = obj%meshmap%JA(imeshfacet)
  END DO
END DO
!
! Count number of facet element in each meshFacetData
!
DO imeshfacet = 1, SIZE(obj%meshFacetData)
  masterMesh => obj%getMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%masterMesh)

  slaveMesh => obj%getMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%slaveMesh)

  tBndyFacet_master = masterMesh%getTotalBoundaryFacetElements()
  tBndyFacet_slave = slaveMesh%getTotalBoundaryFacetElements()

  ! count the number of facet elements in imeshfacet

  tmeshfacet = 0
  !
  DO iface_master = 1, tBndyFacet_master
    !
    IF (masterMesh%boundaryFacetData(iface_master)%elementType .EQ. &
      & DOMAIN_BOUNDARY_ELEMENT) CYCLE
    !
    faceNptrs_master = masterMesh%getFacetConnectivity( &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)
    !
    IF (slaveMesh%isAllNodePresent(faceNptrs_master)) &
      & tmeshfacet = tmeshfacet + 1
    !
  END DO
  !
  ! Prepare data for imeshfacet
  !
  CALL obj%meshFacetData(imeshfacet)%Initiate(tmeshfacet)
  !
  ii = 0
  !
  DO iface_master = 1, tBndyFacet_master
    !
    IF (masterMesh%boundaryFacetData(iface_master)%elementType .EQ. &
      & DOMAIN_BOUNDARY_ELEMENT) CYCLE
    !
    faceNptrs_master = masterMesh%getFacetConnectivity( &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)
    !
    IF (slaveMesh%isAllNodePresent(faceNptrs_master)) THEN
      !
      DO iface_slave = 1, tBndyFacet_slave
        !
        IF (slaveMesh%boundaryFacetData(iface_slave)%elementType .EQ. &
          & DOMAIN_BOUNDARY_ELEMENT) CYCLE
        !
        faceNptrs_slave = slaveMesh%getFacetConnectivity( &
          & facetElement=iface_slave, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE.)
        !
        IF (faceNptrs_master.IN.faceNptrs_slave) THEN
          !
          ii = ii + 1
          !
          ! masterCellNumber
          !
          obj%meshFacetData(imeshfacet)%masterCellNumber(ii) = &
            & masterMesh%getMasterCellNumber( &
              & facetElement=iface_master, &
              & elementType=BOUNDARY_ELEMENT)
          !
          ! masterLocalFacetID
          !
          obj%meshFacetData(imeshfacet)%masterLocalFacetID(ii) = &
            & masterMesh%getLocalFacetID( &
              & facetElement=iface_master, &
              & isMaster=.TRUE., &
              & elementType=BOUNDARY_ELEMENT)
          !
          ! slaveCellNumber
          !
          obj%meshFacetData(imeshfacet)%slaveCellNumber(ii) = &
            & slaveMesh%getMasterCellNumber( &
            & facetElement=iface_slave, &
            & elementType=BOUNDARY_ELEMENT)
          !
          ! slaveLocalFacetID
          !
          obj%meshFacetData(imeshfacet)%slaveLocalFacetID(ii) = &
            & slaveMesh%getLocalFacetID( &
              & facetElement=iface_slave, &
              & isMaster=.TRUE., &
              & elementType=BOUNDARY_ELEMENT)
          !
          EXIT
          !
        END IF
        !
      END DO
      !
    END IF
    !
  END DO
  !
END DO
!
IF (ALLOCATED(faceNptrs_master)) DEALLOCATE (faceNptrs_master)
IF (ALLOCATED(faceNptrs_slave)) DEALLOCATE (faceNptrs_slave)
NULLIFY (masterMesh, slaveMesh)
!
END PROCEDURE Domain_setMeshFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods
