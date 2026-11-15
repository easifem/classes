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

SUBMODULE(AbstractMesh_Class) GetMethods
USE HashTables, ONLY: HashTable_
USE HashTables, ONLY: Hashkey
USE GlobalData, ONLY: MaxDFP, MinDFP
USE ReallocateUtility, ONLY: Reallocate
USE IntegerUtility, ONLY: RemoveDuplicates
USE IntegerUtility, ONLY: RemoveDuplicates_
USE AppendUtility, ONLY: Append
USE BoundingBox_Method, ONLY: BoundingBoxCenter => Center
USE BoundingBox_Method, ONLY: BoundingBoxGetRadiusSqr => GetRadiusSqr
USE BoundingBox_Method, ONLY: BoundingBoxIsInside => IsInside
USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: Display, ToString
USE ReferenceElement_Method, ONLY: GetEdgeConnectivity
USE ReferenceElement_Method, ONLY: GetFaceConnectivity
USE ReferenceElement_Method, ONLY: ElementOrder
USE ReferenceElement_Method, ONLY: TotalEntities
USE ReferenceElement_Method, ONLY: RefElemGetGeoParam
USE BaseType, ONLY: TypeRefelemOpt
USE FacetData_Class, ONLY: FacetData_Iselement
USE FacetData_Class, ONLY: FacetData_GetParam
USE Elemdata_Class, ONLY: INTERNAL_ELEMENT
USE Elemdata_Class, ONLY: BOUNDARY_ELEMENT
USE Elemdata_Class, ONLY: DOMAIN_BOUNDARY_ELEMENT
USE Elemdata_Class, ONLY: Elemdata_GetTotalEntities
USE Elemdata_Class, ONLY: Elemdata_GetConnectivity
USE Elemdata_Class, ONLY: Elemdata_GetConnectivity2
USE Elemdata_Class, ONLY: Elemdata_GetElementToElements
USE Elemdata_Class, ONLY: Elemdata_GetGlobalNodesPointer
USE Elemdata_Class, ONLY: Elemdata_GetTotalGlobalElements
USE Elemdata_Class, ONLY: Elemdata_name
USE Elemdata_Class, ONLY: Elemdata_topoName
USE Elemdata_Class, ONLY: Elemdata_topoIndx
USE Elemdata_Class, ONLY: Elemdata_GetOrientation
USE Elemdata_Class, ONLY: Elemdata_GetCellOrient
USE Elemdata_Class, ONLY: Elemdata_GetFaceOrient
USE Elemdata_Class, ONLY: Elemdata_GetEdgeOrient
USE Elemdata_Class, ONLY: Elemdata_Meshid
USE Elemdata_Class, ONLY: Elemdata_localElemNum
USE Elemdata_Class, ONLY: Elemdata_globalElemNum
USE Elemdata_Class, ONLY: Elemdata_GetTotalGlobalNodes
USE Elemdata_Class, ONLY: Elemdata_GetTotalGlobalVertexNodes
USE Elemdata_Class, ONLY: Elemdata_IsBoundaryElement
USE Elemdata_Class, ONLY: Elemdata_FindFace
USE Elemdata_Class, ONLY: Elemdata_FindEdge
USE Elemdata_Class, ONLY: Elemdata_GetGlobalFaceNumber
USE Elemdata_Class, ONLY: Elemdata_GetGlobalEdgeNumber
USE Elemdata_Class, ONLY: Elemdata_Order
USE NodeData_Class, ONLY: INTERNAL_NODE
USE NodeData_Class, ONLY: NodeData_GetNodeType
USE NodeData_Class, ONLY: NodeData_GetGlobalNodeNum
USE NodeData_Class, ONLY: NodeData_GetTotalGlobalElements
USE NodeData_Class, ONLY: NodeData_GetGlobalElements
USE NodeData_Class, ONLY: NodeData_GetTotalGlobalNodes
USE NodeData_Class, ONLY: NodeData_GetGlobalNodes
USE NodeData_Class, ONLY: NodeData_GetGlobalNodes2
USE NodeData_Class, ONLY: NodeData_GetExtraGlobalNodes
USE NodeData_Class, ONLY: NodeData_GetTotalExtraGlobalNodes
USE NodeData_Class, ONLY: NodeData_GetNodeCoord
USE Kdtree2_Module, ONLY: Kdtree2_r_nearest
USE Kdtree2_Module, ONLY: Kdtree2_n_nearest
USE BaseType, ONLY: TypeMeshOpt
USE ReferenceElement_Method, ONLY: GetVTKElementType_
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: MaxNodesInElement = TypeMeshOpt%maxNNE
INTEGER(I4B), PARAMETER :: MAX_VTK_NODES = 1331

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Classs@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                             GetTotalElementsTopologyWise
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElementsTopologyWise
  ans = obj%tElements_topology_wise
END PROCEDURE obj_GetTotalElementsTopologyWise

!----------------------------------------------------------------------------
!                                                           GetTotalTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalTopology
  ans = obj%tElemTopologies
END PROCEDURE obj_GetTotalTopology

!----------------------------------------------------------------------------
!                                                       GetElemTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopology1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemTopology1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%elemTopologies

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemTopology1

!----------------------------------------------------------------------------
!                                                            GetElemTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopology2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemTopology2()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
  ans = Elemdata_topoName(obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemTopology2

!----------------------------------------------------------------------------
!                                                        GetElemTopologyIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemTopologyIndx
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemTopologyIndx()"
#endif
  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
  ans = Elemdata_topoIndx(obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetElemTopologyIndx

!----------------------------------------------------------------------------
!                                                                GetElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemType
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemType()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalelement, islocal=islocal)
  ans = Elemdata_name(obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemType

!----------------------------------------------------------------------------
!                                                              GetElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemData()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  elemdata = obj%elementData(iel)%ptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemData

!----------------------------------------------------------------------------
!                                                       GetElemDataPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemDataPointer
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemDataPointer()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans => obj%elementData(iel)%ptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemDataPointer

!----------------------------------------------------------------------------
!                                                                  GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNNE()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = SIZE(obj%elementData(iel)%ptr%globalNodes)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                                  GetMaxNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNNE
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMaxNNE()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%maxNNE

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaxNNE

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Size()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%tElements

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElements2()"
#endif
  INTEGER(I4B) :: ii, found
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  elemloop: DO ii = 1, obj%tElements
    isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
    IF (.NOT. isok) CYCLE elemloop
    found = Elemdata_MeshID(obj=obj%elementData(ii)%ptr)
    IF (found .EQ. meshid) ans = ans + 1
  END DO elemloop

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalElements2

!----------------------------------------------------------------------------
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElements3()"
#endif
  INTEGER(I4B) :: ii, found
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  elemloop: DO ii = 1, obj%tElements
    isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
    IF (.NOT. isok) CYCLE elemloop
    found = Elemdata_MeshID(obj=obj%elementData(ii)%ptr)
    isok = ANY(found .EQ. meshid)
    IF (isok) ans = ans + 1
  END DO elemloop

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalElements3

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum1()"
#endif

  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%GetTotalElements()
  ALLOCATE(ans(tsize))
  islocal0 = math%no
  IF (PRESENT(islocal)) islocal0 = islocal

  CALL obj%GetElemNum_(ans=ans, tsize=tsize, islocal=islocal0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNum1

!----------------------------------------------------------------------------
!                                                           GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum2()"
#endif

  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%GetTotalElements(meshid=meshid)
  ALLOCATE(ans(tsize))
  islocal0 = math%no
  IF (PRESENT(islocal)) islocal0 = islocal

  CALL obj%GetElemNum_(meshid=meshid, ans=ans, tsize=tsize, islocal=islocal0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNum2

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum1_()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%GetTotalElements()
  IF (islocal) THEN
    DO ii = 1, tsize
      ans(ii) = Elemdata_localElemNum(obj%elementData(ii)%ptr)
    END DO

  ELSE
    DO ii = 1, tsize
      ans(ii) = Elemdata_globalElemNum(obj%elementData(ii)%ptr)
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNum1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum2_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum2_()"
#endif

  INTEGER(I4B) :: ii, found, fake_tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fake_tsize = obj%GetTotalElements()
  tsize = 0

  IF (islocal) THEN

    loop1: DO ii = 1, fake_tsize
      isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
      IF (.NOT. isok) CYCLE loop1
      found = Elemdata_Meshid(obj=obj%elementData(ii)%ptr)
      IF (found .EQ. meshid) THEN
        tsize = tsize + 1
        ans(tsize) = Elemdata_localElemNum(obj%elementData(ii)%ptr)
      END IF
    END DO loop1

  ELSE

    loop2: DO ii = 1, fake_tsize
      isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
      IF (.NOT. isok) CYCLE loop2
      found = Elemdata_Meshid(obj=obj%elementData(ii)%ptr)
      IF (found .EQ. meshid) THEN
        tsize = tsize + 1
        ans(tsize) = Elemdata_globalElemNum(obj%elementData(ii)%ptr)
      END IF
    END DO loop2

  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNum2_

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum3_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum3_()"
#endif

  INTEGER(I4B) :: tmeshid, mysize, ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = 0
  tmeshid = SIZE(meshid)

  DO ii = 1, tmeshid
    CALL obj%GetElemNum_(meshid=meshid(ii), islocal=islocal, &
                         ans=ans(tsize + 1:), tsize=mysize)
    tsize = tsize + mysize
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNum3_

!----------------------------------------------------------------------------
!                                                         GetBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingEntity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBoundingEntity()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (ALLOCATED(obj%boundingEntity)) THEN
    ans = obj%boundingEntity
  ELSE
    ALLOCATE(ans(0))
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetBoundingEntity

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs1()"
#endif

  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%GetNptrs_(ans=ans, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrs1

!----------------------------------------------------------------------------
!                                                                GetNptrs2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs2()"
#endif

  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%GetTotalNodes(meshid=meshid)
  ALLOCATE(ans(tsize))
  CALL obj%GetNptrs_(ans=ans, tsize=tsize, meshid=meshid)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrs2

!----------------------------------------------------------------------------
!                                                                  GetNptrs_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs1_()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = SIZE(obj%nodeData)
  DO CONCURRENT(ii=1:tsize)
    ans(ii) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrs1_

!----------------------------------------------------------------------------
!                                                                 GetNptrs2_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs2_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs2_()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, fake_tsize, found_meshid
  LOGICAL(LGT) :: isok
  LOGICAL(LGT), ALLOCATABLE :: foundNodes(:)
  INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fake_tsize = obj%GetTotalElements()
  tsize = 0

  ii = obj%GetMaxNodeNumber()
  ALLOCATE(foundNodes(ii))
  foundNodes = math%no

  loop1: DO ii = 1, fake_tsize
    isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    found_meshid = Elemdata_meshid(obj%elementData(ii)%ptr)

    isok = found_meshid .EQ. meshid
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(ii)%ptr)
    jj = Elemdata_GetTotalGlobalNodes(obj%elementData(ii)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)

      isok = foundNodes(ll)
      IF (isok) CYCLE loop2

      foundNodes(ll) = math%yes
      tsize = tsize + 1
      ans(tsize) = ll

    END DO loop2

  END DO loop1

  DEALLOCATE(foundNodes)
  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrs2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs3_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs3_()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, ierr, telem, iel
  TYPE(HashTable_) :: tbl
  INTEGER(I4B), POINTER :: intptr(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = 0
  telem = SIZE(globalElement)

  loop1: DO ii = 1, telem
    iel = obj%GetLocalElemNumber(globalElement=globalElement(ii), &
                                 islocal=islocal)

    isok = obj%IsElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)
    jj = Elemdata_GetTotalGlobalNodes(obj%elementData(iel)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)
      !! check if the key is present
      CALL tbl%check_key(key=Hashkey(ll), STAT=ierr)
      isok = ierr .EQ. 0_I4B
      IF (isok) CYCLE loop2
      !! if not present then set the key
      CALL tbl%Set(key=Hashkey(ll), VALUE=math%yes)
      tsize = tsize + 1
      ans(tsize) = ll
    END DO loop2

  END DO loop1

  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrs3_

!----------------------------------------------------------------------------
!                                                               GetNptrsInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox()"
#endif

  INTEGER(I4B) :: tnodes, ii
  INTEGER(I4B), ALLOCATABLE :: nptrs0(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tnodes = obj%GetTotalNodes()
  ALLOCATE(nptrs0(tnodes))
  CALL obj%GetNptrsInBox_(box=box, nptrs=nptrs0, tnodes=tnodes, &
                          isStrict=isStrict)

  IF (ALLOCATED(nptrs)) DEALLOCATE(nptrs)
  ALLOCATE(nptrs(tnodes))
  DO CONCURRENT(ii=1:tnodes)
    nptrs(ii) = nptrs0(ii)
  END DO
  DEALLOCATE(nptrs0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrsInBox

!----------------------------------------------------------------------------
!                                                           GetNptrsInBox_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrsInBox_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNptrsInBox_()"
#endif
  REAL(DFP) :: qv(3), r2
  INTEGER(I4B) :: ii, jj, kk, nsd, tsize
  LOGICAL(LGT) :: isok, abool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
  IF (.NOT. isok) THEN
    CALL obj%InitiateKdtree()
  END IF

  qv = BoundingBoxCenter(box)
  r2 = BoundingBoxGetRadiusSqr(box)
  nsd = obj%GetNSD()

  CALL Kdtree2_r_nearest(tp=obj%kdtree, qv=qv(1:nsd), r2=r2, &
                         nfound=tnodes, nalloc=SIZE(obj%kdresult), &
                         results=obj%kdresult)

  isok = Input(DEFAULT=math%yes, option=isStrict)

  IF (.NOT. isok) THEN
    DO ii = 1, tnodes
      nptrs(ii) = obj%GetGlobalNodeNumber(obj%kdresult(ii)%idx)
    END DO

  ELSE

    jj = 0
    qv = 0.0_DFP
    DO ii = 1, tnodes
      kk = obj%kdresult(ii)%idx
      CALL NodeData_GetNodeCoord(obj=obj%nodeData(kk)%ptr, ans=qv, &
                                 tsize=tsize)
      abool = BoundingBoxIsInside(box, qv(1:nsd))
      IF (abool) THEN
        jj = jj + 1
        nptrs(jj) = obj%GetGlobalNodeNumber(kk)
      END IF
    END DO
    tnodes = jj
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNptrsInBox_

!----------------------------------------------------------------------------
!                                                         GetElemNumberInBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNumberInBox
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNumberInBox()"
#endif
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%GetNptrsInBox(box=box, nptrs=nptrs, isStrict=math%yes)
  elemNumber = obj%GetNodeToElements(globalNode=nptrs, islocal=math%no)
  IF (ALLOCATED(nptrs)) DEALLOCATE(nptrs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNumberInBox

!----------------------------------------------------------------------------
!                                                         GetElemNumberInBox_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNumberInBox_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElemNumberInBox_()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%GetNptrsInBox_(box=box, nptrs=nptrs, tnodes=tnodes, &
                          isStrict=math%yes)
  CALL obj%GetNodeToElements2_(ans=elemNumber, tsize=tElements, &
                               globalNode=nptrs(1:tnodes), islocal=math%no)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElemNumberInBox_

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetInternalNptrs()"
#endif
  INTEGER(I4B) :: ii, dummy, nodeType

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  dummy = obj%GetTotalInternalNodes()
  ALLOCATE(ans(dummy))
  dummy = 0
  DO ii = 1, obj%tNodes
    nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)
    IF (nodeType .EQ. INTERNAL_NODE) THEN
      dummy = dummy + 1
      ans(dummy) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                          GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetInternalNptrs_()"
  LOGICAL(LGT) :: problem
#endif
  INTEGER(I4B) :: ii, dummy, nodeType

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  dummy = obj%GetTotalInternalNodes()

#ifdef DEBUG_VER
  problem = dummy .GT. SIZE(nptrs)
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: size of nptrs is not enough ' // &
                      'it should be ateast ' // ToString(dummy))
    RETURN
  END IF
#endif

  dummy = 0
  DO ii = 1, obj%tNodes
    nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)
    IF (nodeType .EQ. INTERNAL_NODE) THEN
      dummy = dummy + 1
      nptrs(dummy) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetInternalNptrs_

!----------------------------------------------------------------------------
!                                                          GetBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryNptrs
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBoundaryNptrs()"
#endif

  INTEGER(I4B) :: ii, dummy, nodeType

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  dummy = obj%GetTotalBoundaryNodes()
  CALL Reallocate(ans, dummy)
  dummy = 0
  DO ii = 1, obj%tNodes

    nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)

    IF (nodeType .NE. INTERNAL_NODE) THEN
      dummy = dummy + 1
      ans(dummy) = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
      ! ans(dummy) = obj%nodeData(ii)%ptr%globalNodeNum

    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetBoundaryNptrs

!----------------------------------------------------------------------------
!                                                            isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryNode
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isBoundaryNode()"
#endif

  INTEGER(I4B) :: localnode, nodeType

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  localnode = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
  nodeType = NodeData_GetNodeType(obj%nodeData(localnode)%ptr)
  ans = nodeType .NE. INTERNAL_NODE

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isBoundaryNode

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodePresent1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isNodePresent1()"
#endif

  LOGICAL(LGT) :: abool, islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  islocal0 = Input(DEFAULT=math%no, option=islocal)

  IF (islocal0) THEN
    ans = (globalNode .GT. 0_I4B) .AND. (globalNode .LE. obj%tNodes)
  ELSE
    abool = globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs
    ans = .NOT. abool
    IF (ans) THEN
      ans = obj%local_nptrs(globalNode) .GT. 0
    END IF
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isNodePresent1

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodePresent2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isNodePresent2()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  DO ii = 1, SIZE(globalNode)
    ans(ii) = obj%isNodePresent(globalNode(ii), islocal=islocal)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isNodePresent2

!----------------------------------------------------------------------------
!                                                         GetNodeMask
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeMask
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeMask()"
#endif

  INTEGER(I4B) :: ii, jj, kk, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  isok = .NOT. PRESENT(local_nptrs)
  mask = math%no

  IF (isok) THEN

    tsize = SIZE(obj%nodeData)
    DO CONCURRENT(ii=1:tsize)
      ! jj = obj%nodeData(ii)%ptr%globalNodeNum
      jj = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
      mask(jj) = math%yes
    END DO

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN

  END IF

  tsize = SIZE(obj%nodeData)
  DO CONCURRENT(ii=1:tsize)
    ! jj = obj%nodeData(ii)%ptr%globalNodeNum
    jj = NodeData_GetGlobalNodeNum(obj%nodeData(ii)%ptr)
    kk = local_nptrs(jj)
    mask(kk) = math%yes
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeMask

!----------------------------------------------------------------------------
!                                                           isAnyNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAnyNodePresent
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isAnyNodePresent()"
#endif

  LOGICAL(LGT) :: cond(SIZE(globalNode))
  INTEGER(I4B) :: ii, n

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  n = SIZE(globalNode)

  DO ii = 1, n
    cond(ii) = obj%isNodePresent(globalNode=globalNode(ii), islocal=islocal)
  END DO

  ans = ANY(cond)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isAnyNodePresent

!----------------------------------------------------------------------------
!                                                           isAllNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAllNodePresent
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isAllNodePresent()"
#endif

  LOGICAL(LGT) :: cond(SIZE(globalNode))
  INTEGER(I4B) :: ii, n

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  n = SIZE(globalNode)

  DO ii = 1, n
    cond(ii) = obj%isNodePresent(globalNode=globalNode(ii), islocal=islocal)
  END DO

  ans = ALL(cond)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isAllNodePresent

!----------------------------------------------------------------------------
!                                                           isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementPresent
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isElementPresent()"
#endif

  LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  islocal0 = Input(DEFAULT=math%no, option=islocal)

  IF (islocal0) THEN
    ans = (globalElement .GT. 0_I4B) .AND. (globalElement .LE. obj%tElements)
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  ans = (globalElement .LE. obj%maxElemNum) &
    .AND. (globalElement .GE. obj%minElemNum)

  IF (ans) ans = obj%local_elemNumber(globalElement) .NE. 0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isElementPresent

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryElement
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isBoundaryElement()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = Elemdata_IsBoundaryElement(obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isBoundaryElement

!----------------------------------------------------------------------------
!                                                   isDomainBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainBoundaryElement
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isDomainBoundaryElement()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = obj%elementData(iel)%ptr%elementType .EQ. DOMAIN_BOUNDARY_ELEMENT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isDomainBoundaryElement

!----------------------------------------------------------------------------
!                                                        isDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isDomainFacetElement
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isDomainFacetElement()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = FacetData_Iselement(obj=obj%facetData(facetElement), &
                            filter=DOMAIN_BOUNDARY_ELEMENT)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isDomainFacetElement

!----------------------------------------------------------------------------
!                                                     GetTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalInternalNodes()"
#endif

  INTEGER(I4B) :: ii, nodeType

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  DO ii = 1, obj%tNodes
    nodeType = NodeData_GetNodeType(obj%nodeData(ii)%ptr)
    IF (nodeType .EQ. INTERNAL_NODE) THEN
      ans = ans + 1
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalInternalNodes

!----------------------------------------------------------------------------
!                                                              GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodes1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%tNodes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalNodes1

!----------------------------------------------------------------------------
!                                                           GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodes2()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, fake_tsize, found_meshid
  LOGICAL(LGT) :: isok
  LOGICAL(LGT), ALLOCATABLE :: foundNodes(:)
  INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fake_tsize = obj%GetTotalElements()
  ans = 0

  ii = obj%GetMaxNodeNumber()
  ALLOCATE(foundNodes(ii))
  foundNodes = math%no

  loop1: DO ii = 1, fake_tsize
    isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    found_meshid = Elemdata_meshid(obj%elementData(ii)%ptr)

    isok = found_meshid .EQ. meshid
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(ii)%ptr)
    jj = Elemdata_GetTotalGlobalNodes(obj%elementData(ii)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)
      isok = foundNodes(ll)
      IF (isok) CYCLE loop2
      foundNodes(ll) = math%yes
      ans = ans + 1
    END DO loop2

  END DO loop1

  DEALLOCATE(foundNodes)
  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetTotalNodes2

!----------------------------------------------------------------------------
!                                                       GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodes3()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, ierr, tsize, iel
  TYPE(HashTable_) :: tbl
  INTEGER(I4B), POINTER :: intptr(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  tsize = SIZE(globalElement)

  loop1: DO ii = 1, tsize
    iel = obj%GetLocalElemNumber(globalElement(ii), islocal=islocal)
    isok = obj%IsElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)
    jj = Elemdata_GetTotalGlobalNodes(obj%elementData(iel)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)
      !! check if the key is present
      CALL tbl%check_key(key=Hashkey(ll), STAT=ierr)
      isok = ierr .EQ. 0_I4B
      IF (isok) CYCLE loop2
      !! if not present then set the key
      CALL tbl%Set(key=Hashkey(ll), VALUE=math%yes)
      ans = ans + 1
    END DO loop2
  END DO loop1

  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetTotalNodes3

!----------------------------------------------------------------------------
!                                                       GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes1()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, fake_tsize
  LOGICAL(LGT) :: isok
  LOGICAL(LGT), ALLOCATABLE :: foundNodes(:)
  INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fake_tsize = obj%GetTotalElements()
  ans = 0

  ii = obj%GetMaxNodeNumber()
  ALLOCATE(foundNodes(ii))
  foundNodes = math%no

  loop1: DO ii = 1, fake_tsize
    isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(ii)%ptr)
    jj = Elemdata_GetTotalGlobalVertexNodes(obj%elementData(ii)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)
      isok = foundNodes(ll)
      IF (isok) CYCLE loop2
      foundNodes(ll) = math%yes
      ans = ans + 1
    END DO loop2
  END DO loop1

  DEALLOCATE(foundNodes)
  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetTotalVertexNodes1

!----------------------------------------------------------------------------
!                                                       GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes2()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, fake_tsize, found_meshid
  LOGICAL(LGT) :: isok
  LOGICAL(LGT), ALLOCATABLE :: foundNodes(:)
  INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fake_tsize = obj%GetTotalElements()
  ans = 0

  ii = obj%GetMaxNodeNumber()
  ALLOCATE(foundNodes(ii))
  foundNodes = math%no

  loop1: DO ii = 1, fake_tsize
    isok = obj%IsElementActive(globalElement=ii, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    found_meshid = Elemdata_meshid(obj%elementData(ii)%ptr)

    isok = found_meshid .EQ. meshid
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(ii)%ptr)
    jj = Elemdata_GetTotalGlobalVertexNodes(obj%elementData(ii)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)
      isok = foundNodes(ll)
      IF (isok) CYCLE loop2
      foundNodes(ll) = math%yes
      ans = ans + 1
    END DO loop2
  END DO loop1

  DEALLOCATE(foundNodes)
  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetTotalVertexNodes2

!----------------------------------------------------------------------------
!                                                       GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes3()"
#endif

  INTEGER(I4B) :: ii, jj, kk, ll, ierr, tsize, iel
  TYPE(HashTable_) :: tbl
  INTEGER(I4B), POINTER :: intptr(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  tsize = SIZE(globalElement)

  loop1: DO ii = 1, tsize
    iel = obj%GetLocalElemNumber(globalElement(ii), islocal=islocal)

    isok = obj%IsElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    intptr => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)
    jj = Elemdata_GetTotalGlobalVertexNodes(obj%elementData(iel)%ptr)

    loop2: DO kk = 1, jj
      ll = intptr(kk)
      !! check if the key is present
      CALL tbl%check_key(key=Hashkey(ll), STAT=ierr)
      isok = ierr .EQ. 0_I4B
      IF (isok) CYCLE loop2
      !! if not present then set the key
      CALL tbl%Set(key=Hashkey(ll), VALUE=math%yes)
      ans = ans + 1
    END DO loop2

  END DO loop1

  intptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetTotalVertexNodes3

!----------------------------------------------------------------------------
!                                                        GetTotalVertexNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes4()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = Elemdata_GetTotalGlobalVertexNodes(obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetTotalVertexNodes4

!----------------------------------------------------------------------------
!                                                             GetTotalFaces
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFaces
  ans = obj%tFaces
END PROCEDURE obj_GetTotalFaces

!----------------------------------------------------------------------------
!                                                             GetTotalEdges
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEdges
  ans = obj%tEdges
END PROCEDURE obj_GetTotalEdges

!----------------------------------------------------------------------------
!                                                   GetTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalBoundaryNodes()"
#endif

  INTEGER(I4B) :: tIntNodes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tIntNodes = obj%GetTotalInternalNodes()
  ans = obj%tNodes - tIntNodes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalBoundaryNodes

!----------------------------------------------------------------------------
!                                                 GetTotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalBoundaryElements()"
#endif

  INTEGER(I4B) :: ii, tcells

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  tcells = obj%GetTotalElements()
  DO ii = 1, tcells
    IF (Elemdata_IsBoundaryElement(obj%elementData(ii)%ptr)) THEN
      ans = ans + 1
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBoundingBox1()"
#endif
  REAL(DFP) :: lim(6), x(3)
  INTEGER(I4B) :: nsd, tnodes, ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  nsd = obj%GetNSD()
  tnodes = obj%GetTotalNodes()

! minimum values start from the largest number and maximum values
! from the smallest number, so that MIN and MAX pick the node values
  lim(1:6:2) = MaxDFP
  lim(2:6:2) = MinDFP

  DO ii = 1, tnodes
    CALL NodeData_GetNodeCoord(obj%nodeData(ii)%ptr, ans=x, tsize=tsize)
    lim(1) = MIN(lim(1), x(1))
    lim(2) = MAX(lim(2), x(1))

    lim(3) = MIN(lim(3), x(2))
    lim(4) = MAX(lim(4), x(2))

    lim(5) = MIN(lim(5), x(3))
    lim(6) = MAX(lim(6), x(3))
  END DO

  CALL BoundingBoxInitiate(obj=ans, nsd=3_I4B, lim=lim)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetBoundingBox1

!----------------------------------------------------------------------------
!                                                             GetBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBoundingBox2()"
#endif
  INTEGER(I4B) :: nsd, tsize, ii
  REAL(DFP) :: lim(6)
  LOGICAL(LGT) :: mask(SIZE(nodes, 1), SIZE(nodes, 2))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  lim = 0.0_DFP
  nsd = SIZE(nodes, 1)
  tsize = SIZE(mask, 2)

  CALL obj%GetNodeMask(mask=mask(1, :), local_nptrs=local_nptrs)
  DO ii = 2, nsd
    mask(ii, :) = mask(1, :)
  END DO

  lim(1:nsd * 2:2) = MINVAL(nodes(1:nsd, :), DIM=2, mask=mask)
  lim(2:nsd * 2:2) = MAXVAL(nodes(1:nsd, :), DIM=2, mask=mask)

  CALL BoundingBoxInitiate(obj=ans, nsd=nsd, lim=lim)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetBoundingBox2

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
#endif
  INTEGER(I4B) :: tsize
  INTEGER(I4B) :: temp(TypeMeshOpt%maxCon)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%GetConnectivity_(globalElement=globalElement, &
                            ans=temp, tsize=tsize, opt=opt, islocal=islocal)
  ALLOCATE(ans(tsize))
  ans(1:tsize) = temp(1:tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity1_()"
  LOGICAL(LGT) :: problem
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = .NOT. obj%IsElementPresent(globalElement, islocal=islocal)
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: problem in getting ' // &
                      'localElement number' // ' from globalElement = ' // &
                      ToString(globalElement))
    RETURN
  END IF
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

  CALL Elemdata_GetConnectivity(obj=obj%elementData(iel)%ptr, con=ans, &
                                tsize=tsize, opt=opt)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetConnectivity1_

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity2_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity2_()"
  LOGICAL(LGT) :: problem
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = &
    .NOT. obj%isElementPresent(globalElement=globalElement, islocal=islocal)

  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: problem in getting ' // &
                      'localElement number' // ' from globalElement = ' // &
                      ToString(globalElement))
    RETURN
  END IF
#endif

  iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

  CALL Elemdata_GetConnectivity2(obj=obj%elementData(iel)%ptr, &
                                 cellCon=cellCon, faceCon=faceCon, &
                                 edgeCon=edgeCon, nodeCon=nodeCon, &
                                 tCellCon=tCellCon, tFaceCon=tFaceCon, &
                                 tEdgeCon=tEdgeCon, tNodeCon=tNodeCon)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetConnectivity2_

!----------------------------------------------------------------------------
!                                                            GetOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrientation
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetOrientation()"
  LOGICAL(LGT) :: problem
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = .NOT. obj%isElementPresent(globalElement=globalElement, &
                                       islocal=islocal)

  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: problem in getting ' // &
                      'localElement number' // ' from globalElement = ' // &
                      ToString(globalElement))
    RETURN
  END IF
#endif

  iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

  CALL Elemdata_GetOrientation(obj=obj%elementData(iel)%ptr, &
                               cellOrient=cellOrient, &
                               faceOrient=faceOrient, &
                               edgeOrient=edgeOrient, &
                               tCellOrient=tCellOrient, &
                               tFaceOrient=tFaceOrient, &
                               tEdgeOrient=tEdgeOrient)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetOrientation

!----------------------------------------------------------------------------
!                                                         GetCellOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellOrientation
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetCellOrientation()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%isElementPresent(globalElement=globalElement, &
                              islocal=islocal)

  CALL AssertError1(isok, myName, &
                    'problem in getting localElement number from ' // &
                    'globalElement = ' // ToString(globalElement))
#endif

  iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

  CALL Elemdata_GetCellOrient(obj=obj%elementData(iel)%ptr, &
                              ans=ans, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetCellOrientation

!----------------------------------------------------------------------------
!                                                         GetFaceOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFaceOrientation
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFaceOrientation()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%isElementPresent(globalElement=globalElement, &
                              islocal=islocal)

  CALL AssertError1(isok, myName, &
                    'problem in getting localElement number from ' // &
                    'globalElement = ' // ToString(globalElement))
#endif

  iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

  CALL Elemdata_GetFaceOrient(obj=obj%elementData(iel)%ptr, &
                              ans=ans, nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFaceOrientation

!----------------------------------------------------------------------------
!                                                         GetEdgeOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEdgeOrientation
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetEdgeOrientation()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%isElementPresent(globalElement=globalElement, &
                              islocal=islocal)

  CALL AssertError1(isok, myName, &
                    'problem in getting localElement number from ' // &
                    'globalElement = ' // ToString(globalElement))
#endif

  iel = obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

  CALL Elemdata_GetEdgeOrient(obj=obj%elementData(iel)%ptr, &
                              ans=ans, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetEdgeOrientation

!----------------------------------------------------------------------------
!                                                            GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeConnectivity()"
  LOGICAL(LGT) :: problem
#endif

  INTEGER(I4B) :: iel, telem, ii, nn

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  telem = obj%GetTotalElements()

#ifdef DEBUG_VER
  problem = (SIZE(VALUE, 2) .LT. telem) .OR. (SIZE(VALUE, 1) .LT. obj%maxNNE)
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: The size of value is not correct.')
  END IF
#endif

  DO iel = 1, telem
    nn = SIZE(obj%elementData(iel)%ptr%globalNodes)
    DO ii = 1, nn
      VALUE(ii, iel) = obj%elementData(iel)%ptr%globalNodes(ii)
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeConnectivity

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%GetLocalNodeNumber_(globalNode=globalNode, ans=ans, &
                               islocal=islocal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                       GetLocalNodeNumber_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber1_()"
#endif

  INTEGER(I4B) :: ii, tsize
  LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  islocal0 = Input(option=islocal, DEFAULT=math%no)
  tsize = SIZE(globalNode)

  IF (islocal0) THEN
    DO CONCURRENT(ii=1:tsize)
      ans(ii) = globalNode(ii)
    END DO
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  DO CONCURRENT(ii=1:tsize)
    ans(ii) = obj%local_nptrs(globalNode(ii))
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber1_

!----------------------------------------------------------------------------
!                                                        GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalNodeNumber2()"
#endif

  LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  islocal0 = Input(option=islocal, DEFAULT=math%no)

  IF (islocal0) THEN
    ans = globalNode
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  ans = obj%local_nptrs(globalNode)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetglobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalNodeNumber1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetglobalNodeNumber1()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  DO ii = 1, SIZE(localNode)
    ans(ii) = obj%GetglobalNodeNumber(localNode(ii))
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetglobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         GetglobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalNodeNumber2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetglobalNodeNumber2()"
  LOGICAL(LGT) :: problem
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = (localNode .EQ. 0) .OR. (localNode .GT. obj%tNodes)

  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: localNode is out of bound.')
  END IF
#endif

! ans = obj%nodeData(localNode)%ptr%globalNodeNum
  ans = NodeData_GetGlobalNodeNum(obj%nodeData(localNode)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetglobalNodeNumber2

!----------------------------------------------------------------------------
!                                                        GetglobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalElemNumber1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetglobalElemNumber1()"
#endif
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  DO ii = 1, SIZE(localElement)
    ans(ii) = obj%GetglobalElemNumber(localElement(ii))
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetglobalElemNumber1

!----------------------------------------------------------------------------
!                                                        GetglobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetglobalElemNumber2

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetglobalNodeNumber2()"
  LOGICAL(LGT) :: problem
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = (localElement .EQ. 0) .OR. (localElement .GT. obj%tElements)
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: localElement is out of bound.')
  END IF
#endif

  ans = obj%elementData(localElement)%ptr%globalElemNum

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetglobalElemNumber2

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber1()"
#endif
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = SIZE(globalElement)

  DO ii = 1, tsize
    ans(ii) = obj%GetLocalElemNumber(globalElement(ii), islocal=islocal)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetLocalElemNumber1

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber2()"
  LOGICAL(LGT) :: problem
#endif

  LOGICAL(LGT) :: islocal0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  islocal0 = Input(DEFAULT=math%no, option=islocal)

  IF (islocal0) THEN
    ans = globalElement

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  problem = (globalElement .LT. obj%minElemNum) &
    .OR. (globalElement .GT. obj%maxElemNum)

  IF (problem) THEN
    ans = 0
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: globalElement ' // &
                      ToString(globalElement) // ' not present.')
    RETURN
  END IF
#endif

  ans = obj%local_elemNumber(globalElement)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetLocalElemNumber2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1()"
  LOGICAL(LGT) :: problem
#endif
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = .NOT. obj%isNodePresent(globalNode, islocal=islocal)
  IF (problem) THEN
    ALLOCATE(ans(0))
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: globalNode is not present')
  END IF
#endif

  IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

  ii = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
  ! ans = obj%nodeData(ii)%ptr%globalElements

  tsize = NodeData_GetTotalGlobalElements(obj%nodeData(ii)%ptr)
  ALLOCATE(ans(tsize))
  CALL NodeData_GetGlobalElements(obj=obj%nodeData(ii)%ptr, ans=ans, &
                                  tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements2()"
#endif

  INTEGER(I4B) :: ii, jj, n, lnode(SIZE(globalNode)), &
    nn(SIZE(globalNode) + 1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

  nn(1) = 1
  n = SIZE(globalNode)

  DO ii = 1, n
    lnode(ii) = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
    ! nn(ii + 1) = nn(ii) + SIZE(obj%nodeData(lnode(ii))%ptr%globalElements)
    nn(ii + 1) = nn(ii) + &
      NodeData_GetTotalGlobalElements(obj%nodeData(lnode(ii))%ptr)
  END DO

  CALL Reallocate(ans, nn(n + 1) - 1)

  DO ii = 1, n
    CALL NodeData_GetGlobalElements(obj=obj%nodeData(lnode(ii))%ptr, &
                                    ans=ans(nn(ii):), tsize=jj)
  END DO

  CALL RemoveDuplicates(ans)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements1_()"
#endif

  INTEGER(I4B) :: ii
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = 0
  problem = .NOT. obj%isNodePresent(globalNode, islocal=islocal)
  IF (problem) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

  ii = obj%GetLocalNodeNumber(globalNode, islocal=islocal)

  CALL NodeData_GetGlobalElements(obj=obj%nodeData(ii)%ptr, &
                                  ans=ans, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                          GetNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToElements2_()"
#endif

  INTEGER(I4B) :: ii, n, lnode, a, b

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. obj%isNodeToElementsInitiated) CALL obj%InitiateNodeToElements()

  a = 1
  n = SIZE(globalNode)

  DO ii = 1, n
    lnode = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)

    CALL NodeData_GetGlobalElements(obj=obj%nodeData(lnode)%ptr, &
                                    ans=ans(a:), tsize=b)

    a = a + b

  END DO

  tsize = a - 1

  IF (tsize .LE. 1) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  CALL RemoveDuplicates_(obj=ans(1:tsize), tsize=tsize, isSorted=math%no)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeToElements2_

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: localNode, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsNodePresent(globalNode=globalNode, islocal=islocal)
  CALL AssertError1(isok, myName, "globalNode is out of bound.")
#endif

  IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()
  localNode = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

  tsize = NodeData_GetTotalGlobalNodes(obj%nodeData(localNode)%ptr)

  IF (includeSelf) THEN
    tsize = tsize + 1
  END IF

  IF (obj%isExtraNodeToNodesInitiated) THEN
    tsize = &
      tsize + NodeData_GetTotalExtraGlobalNodes(obj%nodeData(localNode)%ptr)
  END IF

  CALL Reallocate(ans, tsize)

  CALL obj%GetNodeToNodes_(globalNode=localNode, includeSelf=includeSelf, &
                           ans=ans, tsize=tsize, islocal=math%yes)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeToNodes1

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes2()"
#endif
  INTEGER(I4B) :: ii, n, tsize, localNode
  INTEGER(I4B), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()

  n = SIZE(globalNode)

  tsize = 0
  DO ii = 1, n
    localNode = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
    tsize = tsize + NodeData_GetTotalGlobalNodes(obj%nodeData(localNode)%ptr)

    ! GetNodeToNodes_ also writes the extra nodes
    IF (obj%isExtraNodeToNodesInitiated) THEN
      tsize = tsize + &
        NodeData_GetTotalExtraGlobalNodes(obj%nodeData(localNode)%ptr)
    END IF
  END DO

  IF (includeSelf) THEN
    CALL Reallocate(temp, tsize + n)
  ELSE
    CALL Reallocate(temp, tsize)
  END IF

  CALL obj%GetNodeToNodes_(globalNode=globalNode, includeSelf=includeSelf, &
                           ans=temp, tsize=tsize, islocal=islocal)

  CALL Reallocate(ans, tsize)
  IF (tsize .GT. math%zero_i) ans(1:tsize) = temp(1:tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeToNodes2

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes1_()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: localNode, a, aint

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = 0

#ifdef DEBUG_VER
  isok = obj%IsNodePresent(globalNode=globalNode, islocal=islocal)
  CALL AssertError1(isok, myName, "globalNode is out of bound.")
#endif

  IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()
  localNode = obj%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

  a = 0
  IF (IncludeSelf) THEN
    a = 1
    tsize = 1
#ifdef DEBUG_VER
    isok = SIZE(ans) .GE. 1
    CALL AssertError1(isok, myName, "size of ans is not enough (1).")
#endif
    ans(1) = obj%GetglobalNodeNumber(localNode)
  END IF

  tsize = a + NodeData_GetTotalGlobalNodes(obj%nodeData(localNode)%ptr)

#ifdef DEBUG_VER
  isok = SIZE(ans) .GE. tsize
  CALL AssertError1(isok, myName, "size of ans not enough (2).")
#endif

  CALL NodeData_GetGlobalNodes(obj%nodeData(localNode)%ptr, &
                               ans(a + 1:), aint)

  ! handle extraNodes
  IF (obj%isExtraNodeToNodesInitiated) THEN
    a = tsize
    tsize = tsize &
      + NodeData_GetTotalExtraGlobalNodes(obj%nodeData(localNode)%ptr)

#ifdef DEBUG_VER
    isok = SIZE(ans) .GE. tsize
    CALL AssertError1(isok, myName, "size of ans not enough (3).")
#endif

    CALL NodeData_GetExtraGlobalNodes(obj%nodeData(localNode)%ptr, &
                                      ans(a + 1:), aint)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeToNodes1_

!----------------------------------------------------------------------------
!                                                            GetNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToNodes2_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeToNodes2_()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: localNode, jj, b, n

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = 0

  IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()

  n = SIZE(globalNode)

  DO jj = 1, n

#ifdef DEBUG_VER
    isok = obj%isNodePresent(globalNode=globalNode(jj), islocal=islocal)
    CALL AssertError1(isok, myName, "global node present.")
#endif

    localNode = &
      obj%GetLocalNodeNumber(globalNode=globalNode(jj), islocal=islocal)

    IF (includeSelf) THEN
      ans(tsize + 1) = obj%GetglobalNodeNumber(localNode)
      tsize = tsize + 1
    END IF

    CALL NodeData_GetGlobalNodes(obj%nodeData(localNode)%ptr, &
                                 ans(tsize + 1:), b)
    tsize = tsize + b

    IF (obj%isExtraNodeToNodesInitiated) THEN
      CALL NodeData_GetExtraGlobalNodes(obj%nodeData(localNode)%ptr, &
                                        ans(tsize + 1:), b)
      tsize = tsize + b
    END IF
  END DO

  n = tsize

  CALL RemoveDuplicates_(obj=ans(1:n), tsize=tsize, isSorted=math%no)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeToNodes2_

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElementToElements()"
#endif

  LOGICAL(LGT) :: onlyElem
  INTEGER(I4B) :: nrow, temp(TypeRefelemOpt%maxFaces, 3), ii, ncol, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  onlyElem = Input(DEFAULT=math%no, option=onlyElements)

  IF (onlyElem) THEN
    CALL obj%GetElementToElements_(globalElement=globalElement, &
                                   islocal=islocal, ans=temp(:, 1), &
                                   tsize=nrow)
    ALLOCATE(ans(nrow, 1))
    DO ii = 1, nrow
      ans(ii, 1) = temp(ii, 1)
    END DO
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN

  END IF

  CALL obj%GetElementToElements_(globalElement=globalelement, &
                                 islocal=islocal, ans=temp, nrow=nrow, &
                                 ncol=ncol)
  ALLOCATE(ans(nrow, ncol))

  DO jj = 1, ncol
    DO ii = 1, nrow
      ans(ii, jj) = temp(ii, jj)
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElementToElements

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements1_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElementToElements1_()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  CALL Elemdata_GetElementToElements(obj=obj%elementData(iel)%ptr, &
                                     ans=ans, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElementToElements1_

!----------------------------------------------------------------------------
!                                                       GetElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElementToElements2_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetElementToElements2_()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  CALL Elemdata_GetElementToElements(obj=obj%elementData(iel)%ptr, &
                                     ans=ans, nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetElementToElements2_

!----------------------------------------------------------------------------
!                                                     GetBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundaryElementData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetBoundaryElementData()"
#endif

  INTEGER(I4B) :: iel, tsize, ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  tsize = SIZE(obj%elementData(iel)%ptr%boundaryData)
  CALL Reallocate(ans, tsize)
  DO ii = 1, tsize
    ans(ii) = obj%elementData(iel)%ptr%boundaryData(ii)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetBoundaryElementData

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetOrder1()"
#endif

  INTEGER(I4B) :: iel
  LOGICAL(LGT), PARAMETER :: yes = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (obj%isMaxElemOrderSet) THEN
    ans = obj%maxElemOrder
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  ans = 0
  DO iel = 1, obj%GetTotalElements()
    ans = MAX(ans, obj%GetOrder(globalElement=iel, islocal=yes))
  END DO

  obj%maxElemOrder = ans
  obj%isMaxElemOrderSet = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetOrder1

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetOrder2()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = &
    obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
  ans = Elemdata_Order(obj=obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetOrder2

!----------------------------------------------------------------------------
!                                                                   GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetOrder_()"
#endif

  INTEGER(I4B) :: ii
  LOGICAL(LGT), PARAMETER :: yes = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%GetTotalElements()

  DO ii = 1, tsize
    order(ii) = obj%GetOrder(globalElement=ii, islocal=yes)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetOrder_

!----------------------------------------------------------------------------
!                                                                     GetNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNSD
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNSD()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%NSD

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNSD

!----------------------------------------------------------------------------
!                                                            GetXidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetXidimension
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetXidimension()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%xidim

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetXidimension

!----------------------------------------------------------------------------
!                                                                GetMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaterial1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMaterial1()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = obj%elementData(iel)%ptr%material(medium)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaterial1

!----------------------------------------------------------------------------
!                                                         GetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalMaterial1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalMaterial1()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = 0
  ! default value
  IF (ALLOCATED(obj%elementData(iel)%ptr%material)) THEN
    ans = SIZE(obj%elementData(iel)%ptr%material)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalMaterial1

!----------------------------------------------------------------------------
!                                                      GetTotalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFacetElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalFacetElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  IF (ALLOCATED(obj%facetData)) ans = SIZE(obj%facetData)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalInternalFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInternalFacetElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = &
    "obj_GetTotalInternalFacetElements()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  isok = ALLOCATED(obj%facetData)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  tsize = SIZE(obj%facetData)
  DO ii = 1, tsize
    isok = &
      FacetData_Iselement(obj=obj%facetData(ii), filter=INTERNAL_ELEMENT)
    IF (isok) ans = ans + 1
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalInternalFacetElements

!----------------------------------------------------------------------------
!                                              GetTotalBoundaryFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalBoundaryFacetElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = &
    "obj_GetTotalBoundaryFacetElements()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  isok = ALLOCATED(obj%facetData)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF
  tsize = SIZE(obj%facetData)

  DO ii = 1, tsize
    isok = &
      FacetData_Iselement(obj=obj%facetData(ii), filter=BOUNDARY_ELEMENT)
    IF (isok) ans = ans + 1
  END DO

  DO ii = 1, tsize
    isok = FacetData_Iselement(obj=obj%facetData(ii), &
                               filter=DOMAIN_BOUNDARY_ELEMENT)
    IF (isok) ans = ans + 1
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalBoundaryFacetElements

!----------------------------------------------------------------------------
!                                                       GetMasterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterCellNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMasterCellNumber()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                          masterCellNumber=ans)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMasterCellNumber

!----------------------------------------------------------------------------
!                                                         GetSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveCellNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetSlaveCellNumber()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                          slaveCellNumber=ans)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetSlaveCellNumber

!----------------------------------------------------------------------------
!                                                              GetCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetCellNumber()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                          masterCellNumber=ans(1), slaveCellNumber=ans(2))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetCellNumber

!----------------------------------------------------------------------------
!                                                                  FindFace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_FindFace
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_FindFace()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = &
    obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

  IF (onlyBoundaryElement) THEN
    CALL obj%InitiateBoundaryData()
  END IF

  CALL Elemdata_FindFace(obj=obj%elementData(iel)%ptr, faceCon=faceCon, &
                         isFace=isFace, localFaceNumber=localFaceNumber, &
                         onlyBoundaryElement=onlyBoundaryElement)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_FindFace

!----------------------------------------------------------------------------
!                                                                 FindEdge
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_FindEdge
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_FindEdge()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

  IF (onlyBoundaryElement) THEN
    CALL obj%InitiateBoundaryData()
  END IF

  CALL Elemdata_FindEdge(obj=obj%elementData(iel)%ptr, edgeCon=edgeCon, &
                         isEdge=isEdge, localEdgeNumber=localEdgeNumber, &
                         onlyBoundaryElement=onlyBoundaryElement)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_FindEdge

!----------------------------------------------------------------------------
!                                                           GetLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetID
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalFacetID()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (isMaster) THEN
    CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                            masterLocalFacetID=ans)
  ELSE
    CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                            slaveLocalFacetID=ans)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetLocalFacetID

!----------------------------------------------------------------------------
!                                                       GetGlobalFaceNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFaceNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalFaceNumber()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = &
    obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
  ans = Elemdata_GetGlobalFaceNumber(obj=obj%elementData(iel)%ptr, &
                                     localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetGlobalFaceNumber

!----------------------------------------------------------------------------
!                                                       GetGlobalEdgeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalEdgeNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalEdgeNumber()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = &
    obj%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
  ans = Elemdata_GetGlobalEdgeNumber(obj=obj%elementData(iel)%ptr, &
                                     localEdgeNumber=localEdgeNumber)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetGlobalEdgeNumber

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMeshGetFacetConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "AbstractMeshGetFacetConnectivity()"
#endif

  INTEGER(I4B) :: localFaceID, cellNum

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (isMaster) THEN
    CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                            masterCellNumber=cellNum, &
                            masterLocalFacetID=localFaceID)
  ELSE
    CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                            slaveCellNumber=cellNum, &
                            slaveLocalFacetID=localFaceID)
  END IF

  IF (cellNum .EQ. 0) THEN
    ALLOCATE(ans(0))
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

  ans = obj%GetFacetConnectivity(iface=localFaceID, globalElement=cellNum)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE AbstractMeshGetFacetConnectivity

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFacetConnectivity()"
#endif

  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%GetTotalFacetConnectivity(globalElement=globalElement, &
                                        iface=iface, islocal=islocal)
  CALL Reallocate(ans, tsize)
  CALL obj%GetFacetConnectivity_(globalElement=globalElement, &
                                 iface=iface, islocal=islocal, ans=ans, &
                                 tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFacetConnectivity

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetConnectivity_
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFacetConnectivity_()"
#endif

  INTEGER(I4B) :: iel, temp4(4), elemType, order, &
    con(MaxNodesInElement, TypeRefelemOpt%maxFaces), &
    ii, tFaceNodes(TypeRefelemOpt%maxFaces)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)

  SELECT CASE (obj%xidim)

  CASE (1_I4B)

    tsize = 1
    IF (iface .EQ. 1) THEN
      ans(1) = obj%elementData(iel)%ptr%globalNodes(1)
    ELSE
      ans(1) = obj%elementData(iel)%ptr%globalNodes(2)
    END IF

  CASE (2_I4B)

    elemType = obj%elementData(iel)%ptr%name
    order = ElementOrder(elemType)

    tsize = order + 1
    CALL GetFaceConnectivity(elemType=elemType, con=con, order=order, &
                             opt=1_I4B)

    DO ii = 1, tsize
      ans(ii) = obj%elementData(iel)%ptr%globalNodes(con(ii, iface))
    END DO

  CASE DEFAULT

    elemType = obj%elementData(iel)%ptr%name
    temp4 = TotalEntities(elemType)
    order = ElementOrder(elemType)

    CALL RefElemGetGeoParam(elemType=elemType, faceCon=con, &
                            faceOpt=1_I4B, order=order, &
                            tFaceNodes=tFaceNodes)

    tsize = tFaceNodes(iface)
    DO ii = 1, tsize
      ans(ii) = obj%elementData(iel)%ptr%globalNodes(con(ii, iface))
    END DO

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFacetConnectivity_

!----------------------------------------------------------------------------
!                                                      GetFacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFacetConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalFacetConnectivity()"
#endif

  INTEGER(I4B) :: iel, temp4(4), elemType, order, &
    con(MaxNodesInElement, TypeRefelemOpt%maxFaces), &
    tFaceNodes(TypeRefelemOpt%maxFaces)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = 0

  SELECT CASE (obj%xidim)

  CASE (1_I4B)
    ans = 1

  CASE (2_I4B)
    elemType = obj%elementData(iel)%ptr%name
    order = ElementOrder(elemType)
    ans = order + 1

  CASE DEFAULT
    elemType = obj%elementData(iel)%ptr%name
    temp4 = TotalEntities(elemType)
    order = ElementOrder(elemType)
    CALL RefElemGetGeoParam(elemType=elemType, faceCon=con, &
                            faceOpt=1_I4B, order=order, &
                            tFaceNodes=tFaceNodes)
    ans = tFaceNodes(iface)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalFacetConnectivity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated

  IF (PRESENT(isNodeToElementsInitiated)) isNodeToElementsInitiated = &
    obj%isNodeToElementsInitiated

  IF (PRESENT(isNodeToNodesInitiated)) isNodeToNodesInitiated = &
    obj%isNodeToNodesInitiated

  IF (PRESENT(isExtraNodeToNodesInitiated)) isExtraNodeToNodesInitiated &
    = obj%isExtraNodeToNodesInitiated

  IF (PRESENT(isElementToElementsInitiated)) THEN
    isElementToElementsInitiated = obj%isElementToElementsInitiated
  END IF

  IF (PRESENT(isBoundaryDataInitiated)) isBoundaryDataInitiated = &
    obj%isBoundaryDataInitiated

  IF (PRESENT(isFacetDataInitiated)) isFacetDataInitiated = &
    obj%isFacetDataInitiated

  IF (PRESENT(uid)) uid = obj%uid

  IF (PRESENT(tElements_topology_wise)) THEN
    tElements_topology_wise = obj%tElements_topology_wise
  END IF

  IF (PRESENT(tElemTopologies)) tElemTopologies = obj%tElemTopologies

  IF (PRESENT(elemTopologies)) elemTopologies = obj%elemTopologies

  IF (PRESENT(nsd)) nsd = obj%nsd

  IF (PRESENT(maxNptrs)) maxNptrs = obj%maxNptrs

  IF (PRESENT(minNptrs)) minNptrs = obj%minNptrs

  IF (PRESENT(maxElemNum)) maxElemNum = obj%maxElemNum

  IF (PRESENT(minElemNum)) minElemNum = obj%minElemNum

  IF (PRESENT(tNodes)) tNodes = obj%tNodes

  IF (PRESENT(tElements)) tElements = obj%tElements

  IF (PRESENT(minX)) minX = obj%minX

  IF (PRESENT(minY)) minY = obj%minY

  IF (PRESENT(minZ)) minZ = obj%minZ

  IF (PRESENT(maxX)) maxX = obj%maxX

  IF (PRESENT(maxY)) maxY = obj%maxY

  IF (PRESENT(maxZ)) maxZ = obj%maxZ

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinElemNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMinElemNumber()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%minElemNum

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMinElemNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxElemNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMaxElemNumber()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%maxElemNum

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaxElemNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinNodeNumber
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMinNodeNumber()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%minNptrs

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%maxNptrs

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaxNodeNumber

!----------------------------------------------------------------------------
!                                                                 isInit
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isInit
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isInit()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isInit

!----------------------------------------------------------------------------
!                                                              isNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodeToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isNodeToElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isNodeToElementsInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isNodeToElements

!----------------------------------------------------------------------------
!                                                              isNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isNodeToNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isNodeToNodesInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isNodeToNodes

!----------------------------------------------------------------------------
!                                                         isExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isExtraNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isExtraNodeToNodesInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isExtraNodeToNodes

!----------------------------------------------------------------------------
!                                                       isElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isElementToElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isElementToElementsInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isElementToElements

!----------------------------------------------------------------------------
!                                                       isEdgeConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isEdgeConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isEdgeConnectivity()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isEdgeConnectivityInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isEdgeConnectivity

!----------------------------------------------------------------------------
!                                                         isFaceConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isFaceConnectivity
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isFaceConnectivity()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isFaceConnectivityInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isFaceConnectivity

!----------------------------------------------------------------------------
!                                                             isBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isBoundaryData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isBoundaryData()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isBoundaryDataInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isBoundaryData

!----------------------------------------------------------------------------
!                                                                 isFaceData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isFacetData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isFacetData()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isFacetDataInitiated

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isFacetData

!----------------------------------------------------------------------------
!                                                           isElementActive
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElementActive
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_isElementActive()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = obj%elementData(iel)%ptr%isActive

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_isElementActive

!----------------------------------------------------------------------------
!                                                           GetFacetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetParam
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFacetParam()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL FacetData_GetParam(obj=obj%facetData(facetElement), &
                          elementType=elementType)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFacetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntities1()"
#endif
  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = obj%GetLocalElemNumber(globalElement, islocal=islocal)
  ans = Elemdata_GetTotalEntities(obj%elementData(iel)%ptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalEntities1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEntities2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEntities2()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans(1) = obj%tNodes
  ans(2) = obj%tEdges
  ans(3) = obj%tFaces
  ans(4) = obj%tElements

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetTotalEntities2

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord2()"
#endif

  INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ncol = obj%GetTotalNodes()

!$OMP PARALLEL DO PRIVATE(ii, tsize)
  DO ii = 1, ncol
    CALL NodeData_GetNodeCoord(obj=obj%nodeData(ii)%ptr, &
                               ans=nodeCoord(:, ii), tsize=tsize)
  END DO
!$OMP END PARALLEL DO

! tsize is private in the parallel loop, so the number of rows is read
! again from the first node
  nrow = 0
  IF (ncol .GT. 0) CALL NodeData_GetNodeCoord(obj=obj%nodeData(1)%ptr, &
                                              ans=nodeCoord(:, 1), &
                                              tsize=nrow)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord2

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord3()"
#endif

  INTEGER(I4B), POINTER :: globalNode(:)
  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  iel = &
    obj%GetLocalElemNumber(globalelement=globalelement, islocal=islocal)
  globalNode => Elemdata_GetGlobalNodesPointer(obj%elementData(iel)%ptr)

  CALL obj%GetNodeCoord(nodeCoord=nodeCoord, globalNode=globalNode, &
                        islocal=math%no, nrow=nrow, ncol=ncol)

  globalNode => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNodeCoord3

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord4()"
#endif

  INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  nrow = 0
  ncol = SIZE(globalNode)

  DO ii = 1, ncol
    jj = obj%GetLocalNodeNumber(globalNode(ii), islocal=islocal)
    CALL NodeData_GetNodeCoord(obj=obj%nodeData(jj)%ptr, &
                               ans=nodeCoord(:, ii), tsize=nrow)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetNodeCoord4

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeCoord5
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNodeCoord5()"
#endif

  INTEGER(I4B) :: jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  jj = obj%GetLocalNodeNumber(globalNode, islocal=islocal)
  CALL NodeData_GetNodeCoord(obj=obj%nodeData(jj)%ptr, &
                             ans=nodeCoord, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetNodeCoord5

!----------------------------------------------------------------------------
!                                                             GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode1()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            'AbstractMesh_::obj%kdtree is not ' // &
                            'initiating, initing it.')
#endif

    CALL obj%InitiateKdtree()

  END IF

  CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=1, &
                         results=obj%kdresult)

! INFO: This is a local node number
  globalNode = obj%kdresult(1)%idx

  CALL NodeData_GetNodeCoord(obj=obj%nodeData(globalNode)%ptr, &
                             ans=x, tsize=tsize)

  globalNode = obj%GetGlobalNodeNumber(globalNode)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNearestNode1

!----------------------------------------------------------------------------
!                                                             GetNearestNode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNearestNode2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetNearestNode2()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  isok = ALLOCATED(obj%kdresult) .AND. (ASSOCIATED(obj%kdtree))
  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            'AbstractMesh_::obj%kdtree is not ' // &
                            'initiating, initing it.')
#endif

    CALL obj%InitiateKdtree()
  END IF

  CALL Kdtree2_n_nearest(tp=obj%kdtree, qv=qv(1:obj%nsd), nn=nn, &
                         results=obj%kdresult)

  DO ii = 1, nn
    globalNode(ii) = obj%kdresult(ii)%idx

    CALL NodeData_GetNodeCoord(obj=obj%nodeData(globalNode(ii))%ptr, &
                               ans=x(:, ii), tsize=tsize)

    globalNode(ii) = obj%GetGlobalNodeNumber(localnode=globalNode(ii))
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetNearestNode2

!----------------------------------------------------------------------------
!                                                       GetMaxNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMaxNodeToElements()"
#endif

  INTEGER(I4B) :: ii, tsize, tnodes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  tnodes = obj%GetTotalNodes()

  IF (.NOT. obj%isNodeToElementsInitiated) CALL &
    obj%InitiateNodeToElements()

  DO ii = 1, tnodes
    tsize = NodeData_GetTotalGlobalElements(obj%nodeData(ii)%ptr)
    ans = MAX(ans, tsize)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaxNodeToElements

!----------------------------------------------------------------------------
!                                                       GetMaxNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMaxNodeToNodes()"
#endif

  INTEGER(I4B) :: ii, tsize, tnodes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  tnodes = obj%GetTotalNodes()

  IF (.NOT. obj%isNodeToNodesInitiated) CALL obj%InitiateNodeToNodes()

  DO ii = 1, tnodes
    tsize = NodeData_GetTotalGlobalNodes(obj%nodeData(ii)%ptr)
    ans = MAX(ans, tsize)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaxNodeToNodes

!----------------------------------------------------------------------------
!                                                    GetMaxElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxElementToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetMaxElementToElements()"
#endif

  INTEGER(I4B) :: ii, tsize, tElements

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = 0
  tElements = obj%GetTotalElements()

  IF (.NOT. obj%isElementToElementsInitiated) THEN
    CALL obj%InitiateElementToElements()
  END IF

  DO ii = 1, tElements
    ! globalElements has three entries per neighbour: element number,
    ! local facet in this element, local facet in the neighbour
    tsize = Elemdata_GetTotalGlobalElements(obj%ElementData(ii)%ptr) / 3
    ans = MAX(ans, tsize)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMaxElementToElements

!----------------------------------------------------------------------------
!                                                GetVtkUnstructuredGridData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredGridData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredGridData()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B), ALLOCATABLE :: cellcon(:), vtkindx(:)
  INTEGER(I4B) :: ii, jj, nrow, ncol, maxnne, nne, tsize, icell, &
    elemtype, nPoints, nCells
  INTEGER(INT8) :: vtktype

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  nPoints = obj%GetTotalNodes()
  nCells = obj%GetTotalElements()

! points are set to zero first, the missing components (e.g., z in 2D)
! remain zero. nrow is not used, because GetNodeCoord2 does not return
! a reliable nrow when it is compiled with OpenMP.
  CALL Reallocate(points, 3, nPoints)
  points = 0.0_DFP
  CALL obj%GetNodeCoord(nodeCoord=points, nrow=nrow, ncol=ncol)

  maxnne = obj%GetMaxNNE()
  CALL Reallocate(cellcon, maxnne)
  CALL Reallocate(vtkindx, MAX(maxnne, MAX_VTK_NODES))
  CALL Reallocate(connectivity, maxnne * nCells)
  CALL Reallocate(offsets, nCells + 1)
  IF (ALLOCATED(types)) DEALLOCATE(types)
  ALLOCATE(types(nCells))

  offsets(1) = 0
  tsize = 0
  icell = 0

  cellLoop: DO ii = 1, nCells

    IF (.NOT. obj%isElementActive(globalElement=ii, &
                                  islocal=math%yes)) THEN
      CYCLE cellLoop
    END IF

    elemtype = obj%GetElemType(globalElement=ii, islocal=math%yes)
    CALL GetVTKElementType_(elemtype=elemtype, vtk_type=vtktype, &
                            nptrs=vtkindx, tsize=nne)

#ifdef DEBUG_VER
    isok = vtktype .GE. 0_INT8
    CALL AssertError1(isok, myName, &
                      'No VTK cell type for element type ' // &
                      ToString(elemtype))
#endif

    CALL obj%GetConnectivity_(globalElement=ii, islocal=math%yes, &
                              ans=cellcon, tsize=jj)
    CALL obj%GetLocalNodeNumber_(globalNode=cellcon(1:jj), &
                                 ans=cellcon(1:jj), islocal=math%no)

    DO jj = 1, nne
      connectivity(tsize + jj) = cellcon(vtkindx(jj)) - 1
    END DO

    tsize = tsize + nne
    icell = icell + 1
    offsets(icell + 1) = tsize
    types(icell) = vtktype
  END DO cellLoop

  connectivity = connectivity(1:tsize)
  offsets = offsets(1:icell + 1)
  types = types(1:icell)

  DEALLOCATE(cellcon, vtkindx)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetVtkUnstructuredGridData

!----------------------------------------------------------------------------
!                                                           Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
