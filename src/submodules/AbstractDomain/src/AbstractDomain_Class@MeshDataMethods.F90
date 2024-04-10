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
USE BaseMethod
USE DomainConnectivity_Class
USE Kdtree2_Module, ONLY: Kdtree2_create
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateKdtree
INTEGER(I4B) :: nsd
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateKdtree()"
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
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
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
CHARACTER(*), PARAMETER :: myName = "obj_SetFacetElementType"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

! CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
! INTEGER(I4B) :: tsize, ii, jj, kk, iel, iface
! INTEGER(I4B), ALLOCATABLE :: faceID(:), faceNptrs(:)
! LOGICAL(LGT) :: isVar
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[START] ')
! #endif DEBUG_VER
!
! tsize = obj%GetTotalMesh(dim=obj%nsd)
!
! DO ii = 1, tsize
!
!   masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)
!
!   CALL masterMesh%GetParam(isBoundaryDataInitiated=isVar)
!
!   IF (.NOT. isVar) THEN
!     CALL e%raiseInformation(modName//'::'//myName//' - '// &
!       & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
!       & ', entityNum = '//tostring(ii)// &
!       & ' Boundary data is not initiated, calling '// &
!       & ' InitiateBoundaryData')
!     CALL masterMesh%InitiateBoundaryData()
!   END IF
!
!   DO iel = masterMesh%minElemNum, masterMesh%maxElemNum
!
!     IF (.NOT. masterMesh%isElementPresent(iel)) CYCLE
!     IF (.NOT. masterMesh%isBoundaryElement(iel)) CYCLE
!
!     faceID = masterMesh%GetBoundaryElementData(globalElement=iel)
!
!     DO iface = 1, SIZE(faceID)
!
!       kk = faceID(iface)
!       faceNptrs = masterMesh%GetFacetConnectivity(globalElement=iel, &
!         & iface=kk)
!
!       DO jj = 1, tsize
!         IF (jj .NE. ii) THEN
!           slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)
!           IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN
!             CALL masterMesh%SetFacetElementType(globalElement=iel, &
!               & iface=kk, facetElementType=BOUNDARY_ELEMENT)
!             EXIT
!           END IF
!         END IF
!       END DO
!
!     END DO
!
!   END DO
!
! END DO
!
! NULLIFY (masterMesh, slaveMesh)
!
! IF (ALLOCATED(faceID)) DEALLOCATE (faceID)
! IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[END] ')
! #endif DEBUG_VER
!
END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDomainFacetElement
CHARACTER(*), PARAMETER :: myName = "obj_SetDomainFacetElement"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

! CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
! INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet
! INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
! LOGICAL(LGT) :: faceFound, isVar
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[START] ')
! #endif DEBUG_VER
!
! tsize = obj%GetTotalMesh(dim=obj%nsd)
!
! DO ii = 1, tsize
!
!   masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)
!
!   CALL masterMesh%GetParam(isFacetDataInitiated=isVar)
!
!   IF (.NOT. isVar) THEN
!     CALL e%raiseInformation(modName//'::'//myName//' - '// &
!       & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
!     & ', entityNum = '//tostring(ii)// &
!     & ' Facet data is not initiated, calling '// &
!     & ' InitiateFacetElements')
!     CALL masterMesh%InitiateFacetElements()
!   END IF
!
!   tDomFacet = masterMesh%GetTotalBoundaryFacetElements()
!   tMeshFacet = 0
!
!   DO iel = 1, tDomFacet
!
!     faceNptrs = masterMesh%GetFacetConnectivity( &
!       & facetElement=iel, &
!       & elementType=DOMAIN_BOUNDARY_ELEMENT, &
!       & isMaster=.TRUE.)
!
!     faceFound = .FALSE.
!
!     ! The code below checks if any other mesh contains the
!     ! facetNptrs; if there exists such as mesh, then
!     ! the face-element is actually meshFacet (not domainFacet).
!
!     DO jj = 1, tsize
!       IF (jj .NE. ii) THEN
!
!         slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)
!
!         IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN
!
!           faceFound = .TRUE.
!           tMeshFacet = tMeshFacet + 1
!           EXIT
!
!         END IF
!       END IF
!     END DO
!
!     IF (faceFound) THEN
!       masterMesh%boundaryFacetData(iel)%elementType = &
!         & BOUNDARY_ELEMENT
!     END IF
!
!   END DO
!
! END DO
!
! NULLIFY (masterMesh, slaveMesh)
! IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[END] ')
! #endif DEBUG_VER

END PROCEDURE obj_SetDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 SetMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshmap
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshmap"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

! CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
! INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet
! INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshmap(:, :)
! LOGICAL(LGT) :: isVar
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[START] ')
! #endif DEBUG_VER
!
! IF (ALLOCATED(obj%meshFacetData)) THEN
!   CALL e%raiseError(modName//'::'//myName//' - '// &
!     & 'meshFacetData is already allocated... dellocate it first')
! END IF
!
! tsize = obj%GetTotalMesh(dim=obj%nsd)
! CALL Reallocate(meshmap, tsize, tsize)
!
! DO ii = 1, tsize
!
!   masterMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=ii)
!   tDomFacet = masterMesh%GetTotalBoundaryFacetElements()
!
!   CALL masterMesh%GetParam(isFacetDataInitiated=isVar)
!
!   IF (.NOT. isVar) THEN
!     CALL e%raiseInformation(modName//'::'//myName//' - '// &
!       & 'In masterMesh (nsd = '//tostring(obj%nsd)// &
!     & ', entityNum = '//tostring(ii)// &
!     & ' Facet data is not initiated, calling '// &
!     & ' InitiateFacetElements')
!     CALL masterMesh%InitiateFacetElements()
!   END IF
!
!   DO jj = ii + 1, tsize
!
!     slaveMesh => obj%GetMeshPointer(dim=obj%nsd, entityNum=jj)
!
!     DO iel = 1, tDomFacet
!
!       IF (masterMesh%boundaryFacetData(iel)%elementType &
!         & .EQ. BOUNDARY_ELEMENT) THEN
!
!         nptrs = masterMesh%GetFacetConnectivity( &
!           & facetElement=iel, &
!           & elementType=BOUNDARY_ELEMENT, &
!           & isMaster=.TRUE.)
!
!         IF (slaveMesh%isAllNodePresent(nptrs)) THEN
!
!           meshmap(ii, jj) = 1
!           EXIT
!
!         END IF
!
!       END IF
!
!     END DO
!
!   END DO
!
! END DO
!
! tMeshFacet = COUNT(meshmap .EQ. 1)
! !
! ! ALLOCATE meshFacetData
! !
! ALLOCATE (obj%meshFacetData(tMeshFacet))
! CALL Initiate(obj%meshMap, ncol=tsize, nrow=tsize)
! CALL SetSparsity(obj%meshMap, graph=meshmap)
! CALL SetSparsity(obj%meshMap)
!
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! IF (ALLOCATED(meshmap)) DEALLOCATE (meshmap)
! NULLIFY (masterMesh, slaveMesh)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[END] ')
! #endif DEBUG_VER
!
END PROCEDURE obj_SetMeshmap

!----------------------------------------------------------------------------
!                                                       SetMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshFacetElement
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshFacetElement()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

! CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
! INTEGER(I4B) :: tSize, ii, imeshfacet, tBndyFacet_master, &
!   & iface_slave, iface_master, tmeshfacet, tBndyFacet_slave
! INTEGER(I4B), ALLOCATABLE :: faceNptrs_master(:), faceNptrs_slave(:)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[END] ')
! #endif DEBUG_VER
!
! ! main
! IF (.NOT. obj%meshmap%isInitiated) THEN
!   CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & 'obj_::obj%meshMap is not initiated, calling obj%SetMeshMap()')
!   CALL obj%SetMeshMap()
! END IF
!
! tsize = obj%GetTotalMesh(dim=obj%nsd)
!
! ! Set masterMesh and slaveMesh of meshFacetData
! DO ii = 1, tSize
!   DO imeshfacet = obj%meshmap%IA(ii), obj%meshmap%IA(ii + 1) - 1
!     obj%meshFacetData(imeshfacet)%masterMesh = ii
!     obj%meshFacetData(imeshfacet)%slaveMesh = obj%meshmap%JA(imeshfacet)
!   END DO
! END DO
!
! ! Count number of facet element in each meshFacetData
! DO imeshfacet = 1, SIZE(obj%meshFacetData)
!   masterMesh => obj%GetMeshPointer(dim=obj%nsd, &
!     & entityNum=obj%meshFacetData(imeshfacet)%masterMesh)
!
!   slaveMesh => obj%GetMeshPointer(dim=obj%nsd, &
!     & entityNum=obj%meshFacetData(imeshfacet)%slaveMesh)
!
!   tBndyFacet_master = masterMesh%GetTotalBoundaryFacetElements()
!   tBndyFacet_slave = slaveMesh%GetTotalBoundaryFacetElements()
!
!   ! count the number of facet elements in imeshfacet
!
!   tmeshfacet = 0
!
!   DO iface_master = 1, tBndyFacet_master
!
!     IF (masterMesh%boundaryFacetData(iface_master)%elementType .EQ. &
!       & DOMAIN_BOUNDARY_ELEMENT) CYCLE
!
!     faceNptrs_master = masterMesh%GetFacetConnectivity( &
!       & facetElement=iface_master, &
!       & elementType=BOUNDARY_ELEMENT, &
!       & isMaster=.TRUE.)
!
!     IF (slaveMesh%isAllNodePresent(faceNptrs_master)) &
!       & tmeshfacet = tmeshfacet + 1
!
!   END DO
!
!   ! Prepare data for imeshfacet
!   CALL obj%meshFacetData(imeshfacet)%Initiate(tmeshfacet)
!
!   ii = 0
!
!   DO iface_master = 1, tBndyFacet_master
!
!     IF (masterMesh%boundaryFacetData(iface_master)%elementType .EQ. &
!       & DOMAIN_BOUNDARY_ELEMENT) CYCLE
!
!     faceNptrs_master = masterMesh%GetFacetConnectivity( &
!       & facetElement=iface_master, &
!       & elementType=BOUNDARY_ELEMENT, &
!       & isMaster=.TRUE.)
!
!     IF (slaveMesh%isAllNodePresent(faceNptrs_master)) THEN
!
!       DO iface_slave = 1, tBndyFacet_slave
!
!         IF (slaveMesh%boundaryFacetData(iface_slave)%elementType .EQ. &
!           & DOMAIN_BOUNDARY_ELEMENT) CYCLE
!
!         faceNptrs_slave = slaveMesh%GetFacetConnectivity( &
!           & facetElement=iface_slave, &
!           & elementType=BOUNDARY_ELEMENT, &
!           & isMaster=.TRUE.)
!
!         IF (faceNptrs_master.IN.faceNptrs_slave) THEN
!
!           ii = ii + 1
!
!           ! masterCellNumber
!           obj%meshFacetData(imeshfacet)%masterCellNumber(ii) = &
!             & masterMesh%GetMasterCellNumber( &
!               & facetElement=iface_master, &
!               & elementType=BOUNDARY_ELEMENT)
!
!           ! masterLocalFacetID
!           obj%meshFacetData(imeshfacet)%masterLocalFacetID(ii) = &
!             & masterMesh%GetLocalFacetID( &
!               & facetElement=iface_master, &
!               & isMaster=.TRUE., &
!               & elementType=BOUNDARY_ELEMENT)
!
!           ! slaveCellNumber
!           obj%meshFacetData(imeshfacet)%slaveCellNumber(ii) = &
!             & slaveMesh%GetMasterCellNumber( &
!             & facetElement=iface_slave, &
!             & elementType=BOUNDARY_ELEMENT)
!
!           ! slaveLocalFacetID
!           obj%meshFacetData(imeshfacet)%slaveLocalFacetID(ii) = &
!             & slaveMesh%GetLocalFacetID( &
!               & facetElement=iface_slave, &
!               & isMaster=.TRUE., &
!               & elementType=BOUNDARY_ELEMENT)
!
!           EXIT
!
!         END IF
!
!       END DO
!
!     END IF
!
!   END DO
!
! END DO
!
! IF (ALLOCATED(faceNptrs_master)) DEALLOCATE (faceNptrs_master)
! IF (ALLOCATED(faceNptrs_slave)) DEALLOCATE (faceNptrs_slave)
! NULLIFY (masterMesh, slaveMesh)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!   & '[END] ')
! #endif DEBUG_VER

END PROCEDURE obj_SetMeshFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods
