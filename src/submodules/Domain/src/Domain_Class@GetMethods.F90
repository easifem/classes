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

! SUBMODULE(Domain_Class) GetMethods
! USE BaseType, ONLY: IntVector_
! USE IntVector_Method
!
! IMPLICIT NONE
! CONTAINS
!
! !----------------------------------------------------------------------------
! !                                                          isElementPresent
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_IsElementPresent
! CLASS(Mesh_), POINTER :: meshptr
! INTEGER(I4B) :: dim0, entityNum, tsize, nsd
!
! ans = .FALSE.
!
! IF (PRESENT(dim)) THEN
!
!   tsize = obj%GetTotalEntities(dim=dim)
!
!   DO entityNum = 1, tsize
!     meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!     ans = meshptr%IsElementPresent(globalElement=globalElement, &
!                                    islocal=islocal)
!     IF (ans) EXIT
!   END DO
!
!   NULLIFY (meshptr)
!   RETURN
!
! END IF
!
! nsd = obj%GetNSD()
!
! dimloop: DO dim0 = 0, nsd
!
!   tsize = obj%GetTotalEntities(dim=dim0)
!
!   DO entityNum = 1, tsize
!
!     meshptr => obj%GetMeshPointer(dim=dim0, entityNum=entityNum)
!     ans = meshptr%IsElementPresent(globalElement=globalElement, &
!                                    islocal=islocal)
!
!     IF (ans) EXIT dimloop
!
!   END DO
!
! END DO dimloop
!
! NULLIFY (meshptr)
!
! END PROCEDURE obj_IsElementPresent
!
! !----------------------------------------------------------------------------
! !                                                          getConnectivity
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetConnectivity
! CLASS(Mesh_), POINTER :: meshptr
!
! ! main
! meshptr => obj%GetMeshPointer(globalElement=globalElement, islocal=islocal)
! ans = meshptr%GetConnectivity(globalElement=globalElement, &
!                               islocal=islocal)
! meshptr => NULL()
! END PROCEDURE obj_GetConnectivity
!
! !----------------------------------------------------------------------------
! !                                                         getNodeToElements
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNodeToElements1
! CLASS(Mesh_), POINTER :: meshptr
! INTEGER(I4B) :: dim, entityNum
! INTEGER(I4B), ALLOCATABLE :: ivec(:)
! LOGICAL(LGT) :: isok
!
! meshptr => NULL()
! isok = obj%isNodePresent(globalNode=globalNode)
!
! IF (isok) THEN
!   dimloop: DO dim = 0, obj%nsd
!     DO entityNum = 1, obj%GetTotalEntities(dim=dim)
!       meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!       ivec = meshptr%GetNodeToElements(globalNode=globalNode, &
!                                        islocal=islocal)
!       CALL Append(ans, ivec)
!     END DO
!   END DO dimloop
!   meshptr => NULL()
!   IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
! ELSE
!   ALLOCATE (ans(0))
! END IF
!
! END PROCEDURE obj_GetNodeToElements1
!
! !----------------------------------------------------------------------------
! !                                                         getNodeToElements
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNodeToElements2
! TYPE(IntVector_) :: intvec
! INTEGER(I4B), ALLOCATABLE :: ivec(:)
! INTEGER(I4B) :: ii
!
! DO ii = 1, SIZE(globalNode)
!   ivec = obj%GetNodeToElements(globalNode=GlobalNode(ii), islocal=islocal)
!   CALL Append(intvec, ivec)
! END DO
! ans = intvec
! CALL DEALLOCATE (intvec)
! IF (ALLOCATED(ivec)) DEALLOCATE (ivec)
! END PROCEDURE obj_GetNodeToElements2
!
! !----------------------------------------------------------------------------
! !                                                             getTotalNodes
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetTotalNodes
! CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodes()"
! CLASS(Mesh_), POINTER :: meshPtr
! INTEGER(I4B) :: ii
! LOGICAL(LGT) :: case1, isEntityNum, isDim, problem, case2
!
! isEntityNum = PRESENT(entityNum)
! isDim = PRESENT(dim)
!
! case1 = isEntityNum .AND. isDim
!
! IF (case1) THEN
!
! #ifdef DEBUG_VER
!   problem = entityNum .GT. obj%GetTotalEntities(dim)
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//'-'// &
!     & '[INTERNAL ERROR] :: The the enitityNum='//tostring(entityNum) &
!     & //" for dimension="//tostring(dim)// &
!     & " is out of bound.")
!     RETURN
!   END IF
! #endif
!
!   meshPtr => NULL()
!   meshPtr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!
! #ifdef DEBUG_VER
!   problem = .NOT. ASSOCIATED(meshPtr)
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//'-'// &
!       & '[INTERNAL ERROR] :: There is some issue in getting pointer to mesh')
!   END IF
! #endif
!
!   ans = meshPtr%GetTotalNodes()
!   NULLIFY (meshPtr)
!
!   RETURN
!
! END IF
!
! #ifdef DEBUG_VER
!
! case2 = .NOT. isEntityNum .AND. isDim
! IF (case2) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[INTERNAL ERROR] :: both entityNum and dim  should be PRESENT.')
!   RETURN
! END IF
!
! #endif
!
! ans = obj%tNodes
! END PROCEDURE obj_GetTotalNodes
!
! !----------------------------------------------------------------------------
! !                                                                   tNodes
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_tNodes1
! ans = obj%GetTotalNodes(dim=opt(1), entityNum=opt(2))
! END PROCEDURE obj_tNodes1
!
! !----------------------------------------------------------------------------
! !                                                                   tNodes
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_tNodes2
! ans = obj%GetTotalNodes()
! END PROCEDURE obj_tNodes2
!
! !----------------------------------------------------------------------------
! !                                                                  tElements
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_tElements3
! ans = obj%GetTotalElements(dim=opt(1), entityNum=opt(2))
! END PROCEDURE obj_tElements3
!
! !----------------------------------------------------------------------------
! !                                                         getLocalNodeNumber
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetLocalNodeNumber1
! LOGICAL(LGT) :: isok
! ans = 0
! isok = obj%IsNodePresent(globalNode=globalNode, islocal=islocal)
! IF (isok) THEN
!   ans = obj%local_nptrs(globalNode)
! END IF
! END PROCEDURE obj_GetLocalNodeNumber1
!
! !----------------------------------------------------------------------------
! !                                                         getLocalNodeNumber
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetLocalNodeNumber2
! INTEGER(I4B) :: ii
! LOGICAL(LGT) :: isok
!
! DO ii = 1, SIZE(globalNode)
!   isok = obj%IsNodePresent(globalNode(ii), islocal=islocal)
!   ans(ii) = 0
!   IF (isok) THEN
!     ans(ii) = obj%local_nptrs(globalNode(ii))
!   END IF
! END DO
! END PROCEDURE obj_GetLocalNodeNumber2
!
! !----------------------------------------------------------------------------
! !                                                       getGlobalNodeNumber
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetGlobalNodeNumber1
! LOGICAL(LGT) :: isok
! ans = 0
! isok = localNode .LE. obj%GetTotalNodes()
! IF (isok) THEN
!   ans = obj%global_nptrs(localNode)
! END IF
! END PROCEDURE obj_GetGlobalNodeNumber1
!
! !----------------------------------------------------------------------------
! !                                                         getGlobalNodeNumber
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetGlobalNodeNumber2
! INTEGER(I4B) :: ii
! LOGICAL(LGT) :: isok
! DO ii = 1, SIZE(localNode)
!   ans(ii) = 0
!   isok = localNode(ii) .LE. obj%GetTotalNodes()
!   IF (isok) THEN
!     ans(ii) = obj%global_nptrs(localNode(ii))
!   END IF
! END DO
! END PROCEDURE obj_GetGlobalNodeNumber2
!
! !----------------------------------------------------------------------------
! !                                                              getTotalMesh
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetTotalMesh
! ans = obj%GetTotalEntities(dim=dim)
! END PROCEDURE obj_GetTotalMesh
!
! !----------------------------------------------------------------------------
! !                                                             getMeshPointer
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetMeshPointer1
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer1()"
! LOGICAL(LGT) :: problem
!
! problem = entityNum .GT. obj%tEntities(dim)
! IF (problem) THEN
!   CALL e%RaiseError(modName//"::"//myName//" - "// &
!     & "[INTERNAL ERROR] :: entityNum are out of bound")
!   RETURN
! END IF
! #endif
!
! SELECT CASE (dim)
! CASE (0)
!   ans => obj%meshPoint(entityNum)%ptr
! CASE (1)
!   ans => obj%meshCurve(entityNum)%ptr
! CASE (2)
!   ans => obj%meshSurface(entityNum)%ptr
! CASE (3)
!   ans => obj%meshVolume(entityNum)%ptr
! END SELECT
!
! END PROCEDURE obj_GetMeshPointer1
!
! !----------------------------------------------------------------------------
! !                                                             getMeshPointer
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetMeshPointer2
! ! CHARACTER(*), PARAMETER :: myname = "obj_GetMeshPointer2()"
! INTEGER(i4b) :: dim, entityNum
! LOGICAL(LGT) :: abool
!
! ans => NULL()
!
! dimloop: DO dim = 0, obj%nsd
!
!   DO entityNum = 1, obj%GetTotalMesh(dim=dim)
!
!     ans => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!     abool = ans%isElementPresent(globalElement=globalElement)
!
!     IF (abool) THEN
!       EXIT dimloop
!     END IF
!
!   END DO
!
! END DO dimloop
!
! END PROCEDURE obj_GetMeshPointer2
!
! !----------------------------------------------------------------------------
! !                                                           getDimEntityNum
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetDimEntityNum
! INTEGER(i4b) :: dim, entityNum
! CLASS(Mesh_), POINTER :: meshptr
! ! main
! ans = 0
! dimloop: DO dim = 0, obj%nsd
!   DO entityNum = 1, obj%GetTotalEntities(dim=dim)
!     meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!     IF (meshptr%IsElementPresent(globalElement=globalElement, &
!                                  islocal=islocal)) THEN
!       ans = [dim, entityNum]
!       EXIT dimloop
!     END IF
!   END DO
! END DO dimloop
! END PROCEDURE obj_GetDimEntityNum
!
! !----------------------------------------------------------------------------
! !                                                               getNodeCoord
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNodeCoord
! CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord()"
! CLASS(Mesh_), POINTER :: meshPtr
! INTEGER(I4B) :: np, ii, jj
!
! #ifdef DEBUG_VER
!
! LOGICAL(LGT) :: problem
! problem = .NOT. ALLOCATED(obj%nodeCoord)
! IF (problem) THEN
!   CALL e%RaiseError(modName//"::"//myName//" - "// &
!     & "[INTERNAL ERROR] :: Nodecoord is not allocated.")
!   RETURN
! END IF
!
! #endif
!
! IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
!   meshPtr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!   np = meshPtr%GetTotalNodes()
!   CALL Reallocate(nodeCoord, 3_I4B, np)
!   jj = SIZE(nodeCoord, 1)
!   DO ii = 1, np
!     nodeCoord(1:jj, ii) = obj%nodeCoord(1:jj, &
!       & obj%GetLocalNodeNumber(globalNode=ii, islocal=.TRUE.))
!   END DO
!   NULLIFY (meshPtr)
! ELSE
!   nodeCoord = obj%nodeCoord
! END IF
!
! END PROCEDURE obj_GetNodeCoord
!
! !----------------------------------------------------------------------------
! !                                                       getNodeCoord
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNodeCoord2
! INTEGER(I4B) :: localNode(SIZE(globalNode))
! INTEGER(I4B) :: nsd
! localNode = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)
! nsd = SIZE(nodeCoord, 1)
! nodeCoord = obj%nodeCoord(1:nsd, localNode)
! END PROCEDURE obj_GetNodeCoord2
!
! !----------------------------------------------------------------------------
! !                                                        getNodeCoordPointer
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNodeCoordPointer
! ans => obj%nodeCoord
! END PROCEDURE obj_GetNodeCoordPointer
!
! !----------------------------------------------------------------------------
! !                                            getGlobalToLocalNodeNumPointer
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetGlobalToLocalNodeNumPointer
! ans => obj%local_nptrs
! END PROCEDURE obj_GetGlobalToLocalNodeNumPointer
!
! !----------------------------------------------------------------------------
! !                                                                   getNptrs
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNptrs
! CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs()"
! INTEGER(I4B) :: ii, tentity, tnodes
! CLASS(Mesh_), POINTER :: meshptr
! TYPE(IntVector_) :: intvec
! INTEGER(I4B), ALLOCATABLE :: nptrs(:)
! LOGICAL(LGT) :: problem
!
! meshptr => NULL()
! tentity = SIZE(entityNum)
! DO ii = 1, tentity
!
!   meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))
!
!   problem = .NOT. ASSOCIATED(meshptr)
!   IF (problem) CYCLE
!
!   problem = meshptr%isEmpty()
!   IF (problem) CYCLE
!
!   nptrs = meshptr%GetNptrs()
!
!   CALL APPEND(intvec, nptrs)
!
! END DO
!
! CALL RemoveDuplicates(intvec)
!
! IF (isAllocated(intvec)) THEN
!   ans = intvec
! ELSE
!   CALL reallocate(ans, 0)
! END IF
!
! CALL DEALLOCATE (intvec)
!
! NULLIFY (meshptr)
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
!
! END PROCEDURE obj_GetNptrs
!
! !----------------------------------------------------------------------------
! !                                                                   getNptrs
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetInternalNptrs
! INTEGER(I4B) :: ii
! CLASS(Mesh_), POINTER :: meshptr
! TYPE(IntVector_) :: intvec
!
! meshptr => NULL()
! DO ii = 1, SIZE(entityNum)
!   meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum(ii))
!   IF (ASSOCIATED(meshptr)) THEN
!     CALL APPEND(intvec, meshptr%GetInternalNptrs())
!   END IF
! END DO
! CALL RemoveDuplicates(intvec)
! ans = intvec
! CALL DEALLOCATE (intvec)
! NULLIFY (meshptr)
! END PROCEDURE obj_GetInternalNptrs
!
! !----------------------------------------------------------------------------
! !                                                                     getNSD
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetNSD
! ans = obj%NSD
! END PROCEDURE obj_GetNSD
!
! !----------------------------------------------------------------------------
! !                                                                    getNSD
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetOrder
! INTEGER(I4B) :: ii
! CLASS(Mesh_), POINTER :: meshptr
!
! CALL Reallocate(ans, obj%GetTotalMesh(dim=dim))
!
! DO ii = 1, SIZE(ans)
!   meshptr => obj%GetMeshPointer(dim=dim, entityNum=ii)
!   IF (meshptr%GetTotalElements() .EQ. 0_I4B) THEN
!     ans(ii) = 0
!   ELSE
!     ans(ii) = meshptr%GetOrder()
!   END IF
!   meshptr => NULL()
! END DO
!
! END PROCEDURE obj_GetOrder
!
! !----------------------------------------------------------------------------
! !                                                             getBoundingBox
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetBoundingBox
! REAL(DFP) :: lim(6)
! INTEGER(I4B) :: nsd
! !> main
! lim = 0.0_DFP
! nsd = SIZE(obj%nodeCoord, 1)
! lim(1:nsd * 2:2) = MINVAL(obj%nodeCoord(1:nsd, :), dim=2)
! lim(2:nsd * 2:2) = MAXVAL(obj%nodeCoord(1:nsd, :), dim=2)
! CALL Initiate(obj=ans, nsd=3_I4B, lim=lim)
! END PROCEDURE obj_GetBoundingBox
!
! !----------------------------------------------------------------------------
! !                                                     getTotalMeshFacetData
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetTotalMeshFacetData
! IF (PRESENT(imeshFacetData)) THEN
!   IF (ALLOCATED(obj%meshFacetData)) THEN
!     IF (obj%meshFacetData(imeshFacetData)%isInitiated()) THEN
!       ans = obj%meshFacetData(imeshFacetData)%SIZE()
!     ELSE
!       ans = 0
!     END IF
!   ELSE
!     ans = 0
!   END IF
! ELSE
!   IF (ALLOCATED(obj%meshFacetData)) THEN
!     ans = SIZE(obj%meshFacetData)
!   ELSE
!     ans = 0
!   END IF
! END IF
! END PROCEDURE obj_GetTotalMeshFacetData
!
! !----------------------------------------------------------------------------
! !                                                         GetTotalMaterial
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetTotalMaterial1
! CLASS(mesh_), POINTER :: meshptr
!
! meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
! ans = meshptr%GetTotalMaterial(globalElement=globalElement, &
!                                islocal=islocal)
! meshptr => NULL()
! END PROCEDURE obj_GetTotalMaterial1
!
! !----------------------------------------------------------------------------
! !                                                       obj_GetElemType
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetElemType
! CHARACTER(*), PARAMETER :: myName = "obj_GetElemType()"
! CLASS(mesh_), POINTER :: meshptr
! INTEGER(I4B) :: ii, tMesh, idim, nsd, jj
!
! #ifdef DEBUG_VER
!
! IF (dim .GT. 3) THEN
!   CALL e%RaiseError(modName//"::"//myName//" - "// &
!     & "[INTERNAL ERROR] :: Dim of the mesh should be in [0,1,2,3]"//  &
!     & " given dim is equal to "//tostring(dim))
!   RETURN
! END IF
!
! #endif
!
! IF (dim .LT. 0) THEN
!   tMesh = 0
!   nsd = obj%GetNSD()
!   jj = 0
!
!   DO idim = 1, nsd
!     tMesh = tMesh + obj%GetTotalMesh(dim=idim)
!   END DO
!
!   CALL Reallocate(ans, tMesh)
!
!   DO idim = 1, nsd
!     DO ii = 1, obj%GetTotalMesh(dim=idim)
!       meshptr => obj%GetMeshPointer( &
!         & dim=idim, &
!         & entityNum=ii)
!       jj = jj + 1
!       CALL meshptr%GetParam(elemType=ans(jj))
!     END DO
!   END DO
!
!   meshptr => NULL()
!   RETURN
! END IF
!
! tMesh = obj%GetTotalMesh(dim=dim)
! CALL Reallocate(ans, tMesh)
!
! DO ii = 1, tMesh
!   meshptr => obj%GetMeshPointer( &
!     & dim=dim, &
!     & entityNum=ii)
!   CALL meshptr%GetParam(elemType=ans(ii))
! END DO
!
! meshptr => NULL()
!
! END PROCEDURE obj_GetElemType
!
! !----------------------------------------------------------------------------
! !                                                         GetUniqueElemType
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetUniqueElemType
! ans = obj%GetElemType(dim=dim)
! CALL RemoveDuplicates(ans)
! END PROCEDURE obj_GetUniqueElemType
!
! !----------------------------------------------------------------------------
! !                                                         IsInit
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_IsInit
! ans = obj%isInitiated
! END PROCEDURE obj_IsInit
!
! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------
! END SUBMODULE GetMethods
