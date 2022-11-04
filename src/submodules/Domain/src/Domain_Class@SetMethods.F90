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

SUBMODULE(Domain_Class) SetMethods
USE BaseMethod
USE DomainConnectivity_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity1
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_setSparsity1"
INTEGER(I4B) :: imesh, dim, tmesh, lb, ub
CLASS(Mesh_), POINTER :: meshobj
!!
!! main
!!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain is not initiated, first initiate")
END IF
!!
meshobj => NULL()
lb = LBOUND(obj%local_nptrs, 1)
ub = UBOUND(obj%local_nptrs, 1)
!!
DO dim = 1, 3
  tmesh = obj%getTotalMesh(dim=dim)
  DO imesh = 1, tmesh
    meshobj => obj%getMeshPointer(dim=dim, entityNum=imesh)
    IF (ASSOCIATED(meshobj)) &
    & CALL meshobj%setSparsity(mat=mat, &
    & localNodeNumber=obj%local_nptrs, lbound=lb, ubound=ub)
  END DO
END DO
!!
CALL setSparsity(mat)
!!
NULLIFY (meshobj)
END PROCEDURE Domain_setSparsity1

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity2
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_setSparsity2"
INTEGER(I4B) :: rowMeshID, colMeshID, rowMeshSize, colMeshSize,  &
  & ivar, jvar, nsd(SIZE(domains))
CLASS(Mesh_), POINTER :: rowMesh, colMesh
CLASS(Domain_), POINTER :: rowDomain, colDomain
TYPE(DomainConnectivity_) :: domainConn
INTEGER(I4B), POINTER :: nodeToNode(:)
! main
!!
!! check
!!
DO ivar = 1, SIZE(domains)
  IF (.NOT. ASSOCIATED(domains(ivar)%ptr)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & 'DOMAINS( '//TOSTRING(ivar)//' ) NOT ASSOCIATED')
  ELSE
    IF (.NOT. domains(ivar)%ptr%isInitiated)  &
      & CALL e%raiseError(modName//"::"//myName//" - "// &
      & 'DOMAINS( '//TOSTRING(ivar)//' )%ptr NOT INITIATED')
  END IF
  nsd(ivar) = domains(ivar)%ptr%getNSD()
END DO
!!
!! check
!!
IF (ANY(nsd .NE. nsd(1))) &
  & CALL e%raiseError(modName//"::"//myName//" - "// &
  & 'It seems that NSD (number of spatial dimensions) of domains are &
  & not identical')
!!
!! nullify first for safety
!!
rowMesh => NULL(); colMesh => NULL(); rowDomain => NULL(); colDomain => NULL()
DO ivar = 1, SIZE(domains)
  rowDomain => domains(ivar)%ptr
  rowMeshSize = rowDomain%getTotalMesh(dim=nsd(ivar))
  DO jvar = 1, SIZE(domains)
    colDomain => domains(jvar)%ptr
    CALL domainConn%DEALLOCATE()
    CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
      & domain2=colDomain)
    nodeToNode => domainConn%getNodeToNodePointer()
    colMeshSize = colDomain%getTotalMesh(dim=nsd(jvar))
    !>
    DO rowMeshID = 1, rowMeshSize
      rowMesh => rowDomain%getMeshPointer(dim=nsd(ivar), &
        & entityNum=rowMeshID)
      IF (.NOT. ASSOCIATED(rowMesh)) CYCLE
      DO colMeshID = 1, colMeshSize
        colMesh => colDomain%getMeshPointer(dim=nsd(jvar), &
          & entityNum=colMeshID)
        IF (ASSOCIATED(colMesh)) &
          & CALL rowMesh%SetSparsity(mat=mat, &
          & colMesh=colMesh, &
          & nodeToNode=nodeToNode, &
          & rowGlobalToLocalNodeNum=rowDomain%local_nptrs, &
          & rowLBOUND=LBOUND(rowDomain%local_nptrs, 1), &
          & rowUBOUND=UBOUND(rowDomain%local_nptrs, 1), &
          & colGlobalToLocalNodeNum=colDomain%local_nptrs, &
          & colLBOUND=LBOUND(colDomain%local_nptrs, 1), &
          & colUBOUND=UBOUND(colDomain%local_nptrs, 1), &
          & ivar=ivar, jvar=jvar)
      END DO
    END DO
  END DO
END DO
CALL setSparsity(mat)
NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)
CALL domainConn%DEALLOCATE()
END PROCEDURE Domain_setSparsity2

!----------------------------------------------------------------------------
!                                                          setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setTotalMaterial
INTEGER(I4B) :: ii
CLASS(mesh_), POINTER :: meshptr
  !!
DO ii = 1, obj%getTotalMesh(dim=dim)
  meshptr => obj%getMeshPointer(dim=dim, entityNum=ii)
  CALL meshptr%setTotalMaterial(n)
END DO
meshptr => null()
END PROCEDURE Domain_setTotalMaterial

!----------------------------------------------------------------------------
!                                                          setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMaterial
INTEGER(I4B) :: ii
CLASS(mesh_), POINTER :: meshptr
  !!
meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
CALL meshptr%setMaterial(medium=medium, material=material)
meshptr => null()
END PROCEDURE Domain_setMaterial

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setFacetElementType
  !!
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, kk, iel, iface
INTEGER(I4B), ALLOCATABLE :: faceID(:), faceNptrs(:)
  !!
tsize = obj%getTotalMesh(dim=obj%nsd)
  !!
DO ii = 1, tsize
    !!
  masterMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=ii)
    !!
  DO iel = masterMesh%minElemNum, masterMesh%maxElemNum
      !!
    IF (.NOT. masterMesh%isElementPresent(iel)) CYCLE
    IF (.NOT. masterMesh%isBoundaryElement(iel)) CYCLE
      !!
    faceID = masterMesh%getBoundaryElementData(globalElement=iel)
      !!
    DO iface = 1, SIZE(faceID)
        !!
      kk = faceID(iface)
      faceNptrs = masterMesh%getFacetConnectivity(globalElement=iel, &
        & iface=kk)
        !!
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
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
NULLIFY (masterMesh, slaveMesh)
  !!
IF (ALLOCATED(faceID)) DEALLOCATE (faceID)
IF (ALLOCATED(faceNptrs)) DEALLOCATE (faceNptrs)
  !!
END PROCEDURE Domain_setFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setDomainFacetElement
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet
INTEGER(I4B), ALLOCATABLE :: faceNptrs(:)
LOGICAL(LGT) :: faceFound
  !!
  !! main
  !!
tsize = obj%getTotalMesh(dim=obj%nsd)
  !!
DO ii = 1, tsize
    !!
  masterMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=ii)
  tDomFacet = masterMesh%getTotalBoundaryFacetElements()
  tMeshFacet = 0
    !!
    !!
  DO iel = 1, tDomFacet
      !!
    faceNptrs = masterMesh%getFacetConnectivity( &
      & facetElement=iel, &
      & elementType=DOMAIN_BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)
      !!
    faceFound = .FALSE.
      !!
      !! The code below checks if any other mesh contains the
      !! facetNptrs; if there exists such as mesh, then
      !! the face element is actually meshFacet.
      !!
    DO jj = 1, tsize
      IF (jj .NE. ii) THEN
          !!
        slaveMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=jj)
          !!
        IF (slaveMesh%isAllNodePresent(faceNptrs)) THEN
            !!
          faceFound = .TRUE.
          tMeshFacet = tMeshFacet + 1
          EXIT
            !!
        END IF
      END IF
    END DO
      !!
    IF (faceFound) THEN
      masterMesh%boundaryFacetData(iel)%elementType = &
        & BOUNDARY_ELEMENT
    END IF
      !!
  END DO
    !!
END DO
  !!
END PROCEDURE Domain_setDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 setMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMeshmap
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_setMeshmap"
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tsize, ii, jj, iel, tDomFacet, tMeshFacet
INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshmap(:, :)
  !!
  !! main
  !!
tsize = obj%getTotalMesh(dim=obj%nsd)
CALL Reallocate(meshmap, tsize, tsize)
  !!
DO ii = 1, tsize
    !!
  masterMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=ii)
  tDomFacet = masterMesh%getTotalBoundaryFacetElements()
    !!
  DO jj = ii + 1, tsize
      !!
    slaveMesh => obj%getMeshPointer(dim=obj%nsd, entityNum=jj)
      !!
    DO iel = 1, tDomFacet
        !!
      IF (masterMesh%boundaryFacetData(iel)%elementType &
        & .EQ. BOUNDARY_ELEMENT) THEN
          !!
        nptrs = masterMesh%getFacetConnectivity( &
          & facetElement=iel, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE.)
          !!
        IF (slaveMesh%isAllNodePresent(nptrs)) THEN
            !!
          meshmap(ii, jj) = 1
          EXIT
            !!
        END IF
          !!
      END IF
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
tMeshFacet = COUNT(meshmap .EQ. 1)
  !!
  !! ALLOCATE meshFacetData
  !!
IF (ALLOCATED(obj%meshFacetData)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'meshFacetData is already allocated... dellocate it first')
ELSE
  ALLOCATE (obj%meshFacetData(tMeshFacet))
END IF
  !!
  !!
  !!
CALL Initiate(obj%meshMap, ncol=tsize, nrow=tsize)
CALL SetSparsity(obj%meshMap, graph=meshmap)
CALL SetSparsity(obj%meshMap)
  !!
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(meshmap)) DEALLOCATE (meshmap)
  !!
  !!
  !!
END PROCEDURE Domain_setMeshmap

!----------------------------------------------------------------------------
!                                                       setMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMeshFacetElement
  !!
CHARACTER(LEN=*), PARAMETER :: myName = "Domain_setMeshFacetElement"
CLASS(Mesh_), POINTER :: masterMesh, slaveMesh
INTEGER(I4B) :: tSize, ii, jj, imeshfacet, tBndyFacet_master, &
  & iface_slave, iface_master, tmeshfacet, tBndyFacet_slave
INTEGER(I4B), ALLOCATABLE :: faceNptrs_master(:), faceNptrs_slave(:)
  !!
  !! main
  !!
tsize = obj%getTotalMesh(dim=obj%nsd)
  !!
  !! set masterMesh and slaveMesh of meshFacetData
  !!
DO ii = 1, tSize
    !!
  DO imeshfacet = obj%meshmap%IA(ii), obj%meshmap%IA(ii + 1) - 1
    obj%meshFacetData(imeshfacet)%masterMesh = ii
    obj%meshFacetData(imeshfacet)%slaveMesh = obj%meshmap%JA(imeshfacet)
  END DO
    !!
END DO
  !!
  !! Count number of facet element in each meshFacetData
  !!
DO imeshfacet = 1, SIZE(obj%meshFacetData)
    !!
  masterMesh => obj%getMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%masterMesh)
    !!
  slaveMesh => obj%getMeshPointer(dim=obj%nsd, &
    & entityNum=obj%meshFacetData(imeshfacet)%slaveMesh)
    !!
  tBndyFacet_master = masterMesh%getTotalBoundaryFacetElements()
  tBndyFacet_slave = slaveMesh%getTotalBoundaryFacetElements()
    !!
    !! count the number of facet elements in imeshfacet
    !!
  tmeshfacet = 0
    !!
  DO iface_master = 1, tBndyFacet_master
      !!
    IF (masterMesh%boundaryFacetData(iface_master)%elementType .EQ. &
      & DOMAIN_BOUNDARY_ELEMENT) CYCLE
      !!
    faceNptrs_master = masterMesh%getFacetConnectivity( &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)
      !!
    IF (slaveMesh%isAllNodePresent(faceNptrs_master)) &
      & tmeshfacet = tmeshfacet + 1
      !!
  END DO
    !!
    !! Prepare data for imeshfacet
    !!
  CALL obj%meshFacetData(imeshfacet)%Initiate(tmeshfacet)
    !!
  ii = 0
    !!
  DO iface_master = 1, tBndyFacet_master
      !!
    IF (masterMesh%boundaryFacetData(iface_master)%elementType .EQ. &
      & DOMAIN_BOUNDARY_ELEMENT) CYCLE
      !!
    faceNptrs_master = masterMesh%getFacetConnectivity( &
      & facetElement=iface_master, &
      & elementType=BOUNDARY_ELEMENT, &
      & isMaster=.TRUE.)
      !!
    IF (slaveMesh%isAllNodePresent(faceNptrs_master)) THEN
        !!
      DO iface_slave = 1, tBndyFacet_slave
          !!
        IF (slaveMesh%boundaryFacetData(iface_slave)%elementType .EQ. &
          & DOMAIN_BOUNDARY_ELEMENT) CYCLE
          !!
        faceNptrs_slave = slaveMesh%getFacetConnectivity( &
          & facetElement=iface_slave, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE.)
          !!
        IF (faceNptrs_master.IN.faceNptrs_slave) THEN
            !!
          ii = ii + 1
            !!
            !! masterCellNumber
            !!
          obj%meshFacetData(imeshfacet)%masterCellNumber(ii) = &
            & masterMesh%getMasterCellNumber( &
              & facetElement=iface_master, &
              & elementType=BOUNDARY_ELEMENT)
            !!
            !! masterLocalFacetID
            !!
          obj%meshFacetData(imeshfacet)%masterLocalFacetID(ii) = &
            & masterMesh%getLocalFacetID( &
              & facetElement=iface_master, &
              & isMaster=.TRUE., &
              & elementType=BOUNDARY_ELEMENT)
            !!
            !! slaveCellNumber
            !!
          obj%meshFacetData(imeshfacet)%slaveCellNumber(ii) = &
            & slaveMesh%getMasterCellNumber( &
            & facetElement=iface_slave, &
            & elementType=BOUNDARY_ELEMENT)
            !!
            !! slaveLocalFacetID
            !!
          obj%meshFacetData(imeshfacet)%slaveLocalFacetID(ii) = &
            & slaveMesh%getLocalFacetID( &
              & facetElement=iface_slave, &
              & isMaster=.TRUE., &
              & elementType=BOUNDARY_ELEMENT)
            !!
          EXIT
            !!
        END IF
          !!
      END DO
        !!
    END IF
      !!
  END DO
    !!
END DO
  !!
IF (ALLOCATED(faceNptrs_master)) DEALLOCATE (faceNptrs_master)
IF (ALLOCATED(faceNptrs_slave)) DEALLOCATE (faceNptrs_slave)
NULLIFY (masterMesh, slaveMesh)
  !!
END PROCEDURE Domain_setMeshFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
