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

SUBMODULE(AbstractDomain_Class) GetMethods
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE BoundingBox_Method, ONLY: Center
USE BoundingBox_Method, ONLY: GetRadiusSqr
USE BoundingBox_Method, ONLY: isInside
USE BoundingBox_Method, ONLY: BoundingBox_Initiate => Initiate
USE F95_BLAS, ONLY: Copy
USE Kdtree2_Module, ONLY: Kdtree2_r_nearest, Kdtree2_n_nearest
USE Display_Method, ONLY: Display, ToString
USE IntegerUtility, ONLY: RemoveDuplicates
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractDomain_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsInitiated()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%isInit

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                             IsNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNodePresent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsNodePresent()"
#endif
LOGICAL(LGT) :: islocal0
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

islocal0 = Input(default=math%no, option=islocal)

IF (islocal0) THEN
  ans = (globalNode .GT. math%zero_i) .AND. (globalNode .LE. obj%tNodes)
  RETURN
END IF

ans = (globalNode .GE. obj%minNptrs) .AND. (globalNode .LE. obj%maxNptrs)

IF (ans) THEN
  aint = obj%GetLocalNodeNumber(globalNode)
  ans = aint .NE. math%zero_i
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsElementPresent()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
ans = meshptr%IsElementPresent(globalElement=globalElement, islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                          GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum, &
                              globalElement=globalElement, islocal=islocal)
ans = meshptr%GetConnectivity(globalElement=globalElement, islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                                    GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNNE()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum, &
                              globalElement=globalElement, islocal=islocal)
ans = meshptr%GetNNE(globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                         GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer()
ans = meshptr%GetNodeToElements(globalNode=globalNode, islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements2()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer()
ans = meshptr%GetNodeToElements(globalNode=globalNode, islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                         GetNodeToElements_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1_()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer()
CALL meshptr%GetNodeToElements_(globalNode=globalNode, &
                                islocal=islocal, ans=ans, tsize=tsize)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                         GetNodeToElements_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements2_()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer()
CALL meshptr%GetNodeToElements_(globalNode=globalNode, &
                                islocal=islocal, ans=ans, tsize=tsize)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeToElements2_

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodes()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: case1, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
case1 = (.NOT. PRESENT(dim)) .AND. (.NOT. PRESENT(entityNum))
IF (case1) THEN
  ans = obj%tNodes
  RETURN
END IF

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

problem = .NOT. ASSOCIATED(meshptr)
IF (problem) RETURN

ans = meshptr%GetTotalNodes()
NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_tNodes1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalNodes(dim=dim)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_tNodes1

!----------------------------------------------------------------------------
!                                                                   tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_tNodes2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalNodes()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_tNodes2

!----------------------------------------------------------------------------
!                                                           GetMaxNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_tNodes3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalNodes(dim=opt(1), entityNum=opt(2))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_tNodes3

!----------------------------------------------------------------------------
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElements()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
isok = PRESENT(entityNum)

IF (isok) THEN
  ans = meshptr%GetTotalElements(meshid=entityNum)
ELSE
  ans = meshptr%GetTotalElements()
END IF

meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalElements

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_tElements1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalElements()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_tElements1

!----------------------------------------------------------------------------
!                                                                  tElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_tElements2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalElements(dim=dim)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_tElements2

!----------------------------------------------------------------------------
!                                                           GetMaxNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tElements3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_tElements3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalElements(dim=opt(1), entityNum=opt(2))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_tElements3

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = globalNode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = globalNode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       GetGlobalNodeNumber
!----------------------------------------------------------------------------

! this is a pure method
MODULE PROCEDURE obj_GetGlobalNodeNumber1
ans = localNode
END PROCEDURE obj_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetGlobalNodeNumber
!----------------------------------------------------------------------------

! this is a pure method
MODULE PROCEDURE obj_GetGlobalNodeNumber2
ans = localNode
END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                         GetTotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntities()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tEntities(dim)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalEntities

!----------------------------------------------------------------------------
!                                                        GetTotalEntitiesList
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntitiesList
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntitiesList()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this method should be implemented by child class.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalEntitiesList

!----------------------------------------------------------------------------
!                                                           GetDimEntityNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDimEntityNum
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDimEntityNum()"
#endif

INTEGER(I4B) :: dim, entityNum, tsize
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
dimloop: DO dim = 0, obj%nsd

  tsize = obj%GetTotalEntities(dim=dim)
  DO entityNum = 1, tsize

    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

    IF (meshptr%IsElementPresent(globalElement=globalElement, &
                                 islocal=islocal)) THEN
      ans = [dim, entityNum]
      EXIT dimloop
    END IF

  END DO

END DO dimloop

meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDimEntityNum

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord1()"
#endif

INTEGER(I4B) :: ii, tsize, nsd, jj
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%nodeCoord)
CALL AssertError1(isok, myName, &
                  "obj%nodeCoord is not allocated.")
#endif

isok = (.NOT. PRESENT(dim)) .AND. (.NOT. PRESENT(entityNum))
IF (isok) THEN

  tsize = SIZE(obj%nodeCoord, 2)
  nsd = SIZE(obj%nodeCoord, 1)
  CALL Reallocate(nodeCoord, nsd, tsize)

  DO CONCURRENT(ii=1:tsize)
    nodeCoord(1:nsd, ii) = obj%nodeCoord(1:nsd, ii)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
tsize = meshptr%GetTotalNodes()
CALL Reallocate(nodeCoord, 3_I4B, tsize)
nsd = SIZE(obj%nodeCoord, 1)
DO ii = 1, tsize
  jj = meshptr%GetGlobalNodeNumber(localNode=ii)
  jj = obj%GetLocalNodeNumber(globalNode=jj)
  nodeCoord(1:nsd, ii) = obj%nodeCoord(1:nsd, jj)
END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord1

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord2()"
#endif
INTEGER(I4B) :: ii, localnode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncol = SIZE(globalNode)
nrow = obj%nsd

DO ii = 1, ncol
  localnode = obj%GetLocalNodeNumber(globalNode=globalNode(ii), &
                                     islocal=islocal)
  nodeCoord(1:nrow, ii) = obj%nodeCoord(1:nrow, localNode)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord2

!----------------------------------------------------------------------------
!                                                         GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord3()"
#endif
INTEGER(I4B) :: localNode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%nsd
localNode = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)
nodeCoord(1:tsize) = obj%nodeCoord(1:tsize, localNode)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord3

!----------------------------------------------------------------------------
!                                                        GetNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoordPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoordPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%nodeCoord

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeCoordPointer

!----------------------------------------------------------------------------
!                                                           GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode1()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'obj%kdtree is not initiated, initiating it.')
#endif

  CALL obj%InitiateKdtree()
END IF

CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=1, &
                       results=obj%kdresult)

globalNode = obj%kdresult(1)%idx
x(1:obj%nsd) = obj%nodeCoord(1:obj%nsd, globalNode)
globalNode = obj%GetGlobalNodeNumber(localnode=globalNode)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNearestNode1

!----------------------------------------------------------------------------
!                                                           GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode2()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
IF (.NOT. isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'obj%kdtree is not initiated, initiating it.')
#endif

  CALL obj%InitiateKdtree()
END IF

CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=nn, &
                       results=obj%kdresult)

DO ii = 1, nn
  globalNode(ii) = obj%kdresult(ii)%idx
  x(1:obj%nsd, ii) = obj%nodeCoord(1:obj%nsd, globalNode(ii))
  globalNode(ii) = obj%GetGlobalNodeNumber(localnode=globalNode(ii))
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNearestNode2

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim)
ans = meshptr%GetNptrs()
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs_()"
#endif
INTEGER(I4B) :: jj
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim)
CALL meshptr%GetNptrs_(ans=nptrs, tsize=jj)
IF (PRESENT(tsize)) tsize = jj
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInternalNptrs()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim)
ans = meshptr%GetInternalNptrs()
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                             GetNptrsInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox()"
#endif
INTEGER(I4B) :: tnodes, ii
INTEGER(I4B), ALLOCATABLE :: nptrs0(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tnodes = obj%GetTotalNodes()
ALLOCATE (nptrs0(tnodes))
CALL obj%GetNptrsInBox_(box=box, nptrs=nptrs0, tnodes=tnodes, &
                        isStrict=isStrict)

ALLOCATE (nptrs(tnodes))
DO CONCURRENT(ii=1:tnodes)
  nptrs(ii) = nptrs0(ii)
END DO
DEALLOCATE (nptrs0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNptrsInBox

!----------------------------------------------------------------------------
!                                                             GetNptrsInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox_()"
#endif

! nptrs = box.Nptrs.obj%nodeCoord
REAL(DFP) :: qv(3), r2
INTEGER(I4B) :: ii, jj, kk, nsd
LOGICAL(LGT) :: isok, abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = (.NOT. ASSOCIATED(obj%kdtree)) .OR. (.NOT. ALLOCATED(obj%kdresult))
IF (isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'obj%kdtree not initiated, initiating it...')
#endif

  CALL obj%InitiateKdtree()
END IF

qv = Center(box)
r2 = GetRadiusSqr(box)
nsd = obj%nsd

CALL Kdtree2_r_nearest(tp=obj%kdtree, qv=qv(1:nsd), r2=r2, &
                       nfound=tnodes, nalloc=SIZE(obj%kdresult), &
                       results=obj%kdresult)

#ifdef DEBUG_VER
ii = SIZE(nptrs)
CALL AssertError3(tnodes, ii, myName, &
                  "size of nptrs is not enough, a=tnodes, b=size(nptrs)")
#endif

isok = Input(default=math%yes, option=isStrict)

IF (.NOT. isok) THEN
  DO CONCURRENT(ii=1:tnodes)
    nptrs(ii) = obj%kdresult(ii)%idx
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

jj = 0
DO ii = 1, tnodes

  kk = obj%kdresult(ii)%idx
  abool = isInside(box, obj%nodeCoord(1:nsd, kk))
  IF (abool) THEN
    jj = jj + 1
    nptrs(jj) = kk
  END IF

END DO

tnodes = jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNptrsInBox_

!----------------------------------------------------------------------------
!                                                             GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBoundingBox()"
#endif
LOGICAL(LGT) :: acase

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

acase = (.NOT. PRESENT(entityNum)) .AND. (.NOT. PRESENT(dim))

IF (acase) THEN
  CALL case1
ELSE
  CALL case2
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

CONTAINS

SUBROUTINE case1
  REAL(DFP) :: lim(6)
  INTEGER(I4B) :: nsd
  !> main
  lim = 0.0_DFP
  nsd = SIZE(obj%nodeCoord, 1)
  lim(1:nsd * 2:2) = MINVAL(obj%nodeCoord(1:nsd, :), dim=2)
  lim(2:nsd * 2:2) = MAXVAL(obj%nodeCoord(1:nsd, :), dim=2)
  CALL BoundingBox_Initiate(obj=ans, nsd=3_I4B, lim=lim)
END SUBROUTINE case1

SUBROUTINE case2
  CLASS(AbstractMesh_), POINTER :: meshptr
  LOGICAL(LGT) :: isok

  meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

#ifdef DEBUG_VER
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myName, &
                    "meshptr is not initiated.")
#endif

  ans = meshptr%GetBoundingBox(nodes=obj%nodeCoord)
  meshptr => NULL()
END SUBROUTINE case2

END PROCEDURE obj_GetBoundingBox

!----------------------------------------------------------------------------
!                                                                     GetNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNSD
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNSD()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%nsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNSD

!----------------------------------------------------------------------------
!                                                             GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should ne implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                     GetTotalMeshFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMeshFacetData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalMeshFacetData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This method is deprecated, working on alternative.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalMeshFacetData

!----------------------------------------------------------------------------
!                                                           GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalMaterial()"
#endif
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

#ifdef DEBUG_VER
isok = ASSOCIATED(meshptr)
CALL AssertError1(isok, myName, &
                  "meshptr is not initiated...")
#endif

ans = meshptr%GetTotalMaterial(globalElement=globalElement, islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalMaterial

!----------------------------------------------------------------------------
!                                                               GetElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this method is under developmenet.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemType

!----------------------------------------------------------------------------
!                                                         GetUniqueElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetUniqueElemType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetUniqueElemType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetElemType(dim=dim)
CALL RemoveDuplicates(ans)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetUniqueElemType

!----------------------------------------------------------------------------
!                                                                  GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isInitiated)) isInitiated = obj%isInit
IF (PRESENT(engine)) engine = obj%engine%chars()
IF (PRESENT(majorVersion)) majorVersion = obj%majorVersion
IF (PRESENT(minorVersion)) minorVersion = obj%minorVersion
IF (PRESENT(version)) version = obj%version
IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(maxNptrs)) maxNptrs = obj%maxNptrs
IF (PRESENT(minNptrs)) minNptrs = obj%minNptrs
IF (PRESENT(tNodes)) tNodes = obj%tNodes
IF (PRESENT(isNodeNumberSparse)) isNodeNumberSparse = obj%isNodeNumberSparse
IF (PRESENT(maxElemNum)) maxElemNum = obj%maxElemNum
IF (PRESENT(minElemNum)) minElemNum = obj%minElemNum
IF (PRESENT(isElemNumberSparse)) isElemNumberSparse = obj%isElemNumberSparse
IF (PRESENT(tEntitiesForElements)) tEntitiesForElements = &
  obj%tEntitiesForElements
IF (PRESENT(tEntitiesForNodes)) tEntitiesForNodes = obj%tEntitiesForNodes
IF (PRESENT(tElements)) tElements = obj%tElements
IF (PRESENT(tEntities)) tEntities = obj%tEntities
IF (PRESENT(nodeCoord)) nodeCoord = obj%nodeCoord

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                           GetMinElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinElemNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMinElemNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%minElemNum

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMinElemNumber

!----------------------------------------------------------------------------
!                                                           GetMaxElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxElemNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxElemNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%maxElemNum

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxElemNumber

!----------------------------------------------------------------------------
!                                                           GetMinNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinNodeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMinNodeNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%minNptrs

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMinNodeNumber

!----------------------------------------------------------------------------
!                                                           GetMaxNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxNodeNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%maxNptrs

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxNodeNumber

!----------------------------------------------------------------------------
!                                                        GetGlobalEdgeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalEdgeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalEdgeNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalEdgeNumber

!----------------------------------------------------------------------------
!                                                        GetGlobalFaceNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFaceNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalFaceNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalFaceNumber

!----------------------------------------------------------------------------
!                                                        GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemNumber1

!----------------------------------------------------------------------------
!                                                        GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemNumber2

!----------------------------------------------------------------------------
!                                                                GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemData

!----------------------------------------------------------------------------
!                                                                GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemDataPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemDataPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemDataPointer

!----------------------------------------------------------------------------
!                                                          GetConnectivity_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity1_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetConnectivity1_

!----------------------------------------------------------------------------
!                                                          GetConnectivity_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Getconnectivity2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Getconnectivity2_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Getconnectivity2_

!----------------------------------------------------------------------------
!                                                         GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexNodes1

!----------------------------------------------------------------------------
!                                                         GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "this routine should be implemeneted by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexNodes2

!----------------------------------------------------------------------------
!                                                             GetOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetOrientation()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
CALL meshptr%GetOrientation(cellOrient=cellOrient, &
                            faceOrient=faceOrient, &
                            edgeOrient=edgeOrient, &
                            tCellOrient=tCellOrient, &
                            tFaceOrient=tFaceOrient, &
                            tEdgeOrient=tEdgeOrient, &
                            globalElement=globalElement, &
                            islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetOrientation

!----------------------------------------------------------------------------
!                                                       GetElemTopologyIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopologyIndx
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemTopologyIndx()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

ans = meshptr%GetElemTopologyIndx(globalElement=globalElement, &
                                  islocal=islocal)

meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemTopologyIndx

!----------------------------------------------------------------------------
!                                                           IsElementActive
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementActive
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsElementActive()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
ans = meshptr%IsElementActive(globalElement=globalElement, &
                              islocal=islocal)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsElementActive

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
