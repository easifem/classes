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

SUBMODULE(Mesh_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_display
CALL Display(msg, unitno=unitno)

IF (.NOT. obj%isInitiated) THEN
  CALL Display("Mesh object is empty, noting to display", msg, &
    & unitno=unitno)
  RETURN
END IF

! readFromFile
CALL Display(obj%readFromFile, "readFromFile : ", unitno=unitno)

! isNodeToElementsInitiated
CALL Display(obj%isNodeToElementsInitiated,  &
  & "isNodeToElementsInitiated : ", unitno=unitno)

! isNodeToNodesInitiated
CALL Display(obj%isNodeToNodesInitiated,  &
  & "isNodeToNodesInitiated : ", unitno=unitno)

! isElementToElementsInitiated
CALL Display(obj%isElementToElementsInitiated,  &
  & "isElementToElementsInitiated : ", unitno=unitno)

! isBoundaryDataInitiated
CALL Display(obj%isBoundaryDataInitiated,  &
  & "isBoundaryDataInitiated : ", unitno=unitno)

! isFacetDataInitiated
CALL Display(obj%isFacetDataInitiated,  &
  & "isFacetDataInitiated : ", unitno=unitno)

! uid
CALL Display(obj%uid,  &
  & "uid : ", unitno=unitno)

! xidim
CALL Display(obj%xidim,  &
  & "xidim : ", unitno=unitno)

! elemType
CALL Display(obj%elemType,  &
  & "elemType : ", unitno=unitno)

! nsd
CALL Display(obj%nsd,  &
  & "nsd : ", unitno=unitno)

! maxNptrs
CALL Display(obj%maxNptrs,  &
  & "maxNptrs : ", unitno=unitno)

! minNptrs
CALL Display(obj%minNptrs,  &
  & "minNptrs : ", unitno=unitno)

! maxElemNum
CALL Display(obj%maxElemNum,  &
  & "maxElemNum : ", unitno=unitno)

! minElemNum
CALL Display(obj%minElemNum,  &
  & "minElemNum : ", unitno=unitno)

! tNodes
CALL Display(obj%tNodes,  &
  & "tNodes : ", unitno=unitno)

! tIntNodes
CALL Display(obj%tIntNodes,  &
  & "tIntNodes : ", unitno=unitno)

! tElements
CALL Display(obj%tElements,  &
  & "tElements : ", unitno=unitno)

! minX
CALL Display(obj%minX,  &
  & "minX : ", unitno=unitno)

! maxX
CALL Display(obj%maxX,  &
  & "maxX : ", unitno=unitno)

! minY
CALL Display(obj%minY,  &
  & "minY : ", unitno=unitno)

! maxY
CALL Display(obj%maxY,  &
  & "maxY : ", unitno=unitno)

! minZ
CALL Display(obj%minZ,  &
  & "minZ : ", unitno=unitno)

! maxZ
CALL Display(obj%maxZ,  &
  & "maxZ : ", unitno=unitno)

! X
CALL Display(obj%X,  &
  & "X : ", unitno=unitno)

! Y
CALL Display(obj%Y,  &
  & "Y : ", unitno=unitno)

! Z
CALL Display(obj%Z,  &
  & "Z : ", unitno=unitno)

! physicalTag
IF (ALLOCATED(obj%physicalTag)) THEN
  CALL Display(obj%physicalTag,  &
    & "physicalTag : ", unitno=unitno)
ELSE
  CALL Display("physicalTag : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! boundingEntity
IF (ALLOCATED(obj%boundingEntity)) THEN
  CALL Display(obj%boundingEntity,  &
    & "boundingEntity : ", unitno=unitno)
ELSE
  CALL Display("boundingEntity : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! local_elemNumber

IF (ALLOCATED(obj%local_elemNumber)) THEN
  CALL Display("local_elemNumber : ALLOCATED ", unitno=unitno)
ELSE
  CALL Display("local_elemNumber : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! local_Nptrs
IF (ALLOCATED(obj%local_Nptrs)) THEN
  CALL Display("local_Nptrs : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("local_Nptrs : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! material
IF (ALLOCATED(obj%material)) THEN
  CALL Display(obj%material,  &
    & "material : ", unitno=unitno)
ELSE
  CALL Display("material : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! facetElements
IF (ALLOCATED(obj%facetElements)) THEN
  CALL Display("facetElements : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("facetElements : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! facetElementType
IF (ALLOCATED(obj%facetElementType)) THEN
  CALL Display("facetElementType : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("facetElementType : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! nodeData
IF (ALLOCATED(obj%nodeData)) THEN
  CALL Display("nodeData : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("nodeData : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! elementData
IF (ALLOCATED(obj%elementData)) THEN
  CALL Display("elementData : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("elementData : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! internalFacetData
IF (ALLOCATED(obj%internalFacetData)) THEN
  CALL Display("internalFacetData : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("internalFacetData : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! boundaryFacetData
IF (ALLOCATED(obj%boundaryFacetData)) THEN
  CALL Display("boundaryFacetData : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("boundaryFacetData : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! refElem
IF (ASSOCIATED(obj%refElem)) THEN
  CALL Display("refElem : ASSOCIATED", unitno=unitno)
ELSE
  CALL Display("refElem : NOT ASSOCIATED", &
    & unitno=unitno)
END IF

! ipType
CALL Display(obj%ipType, "ipType = ", unitno=unitno)

! quadForTime
CALL Display(obj%quadForTime, "quadForTime = ", unitno=unitno)

! linTimeElemSD
CALL Display(obj%linTimeElemSD, "linTimeElemSD = ", unitno=unitno)

! timeElemSD
CALL Display(obj%timeElemSD, "timeElemSD = ", unitno=unitno)

! quadTypeForTime
CALL Display(obj%quadTypeForTime, "quadTypeForTime = ", unitno=unitno)

! continuityTypeForTime
CALL Display(obj%continuityTypeForTime, "continuityTypeForTime = ", &
  & unitno=unitno)

! continuityTypeForTime
CALL Display(obj%continuityTypeForTime, "continuityTypeForTime = ", &
  & unitno=unitno)

! interpolTypeForTime
CALL Display(obj%interpolTypeForTime, "interpolTypeForTime = ", &
  & unitno=unitno)

! orderTime
CALL Display(obj%orderTime, "orderTime = ", &
  & unitno=unitno)

! quadForSpace
CALL Display(obj%quadForSpace, "quadForSpace = ", &
  & unitno=unitno)

! linSpaceElemSD
CALL Display(obj%linSpaceElemSD, "linSpaceElemSD = ", &
  & unitno=unitno)

! spaceElemSD
CALL Display(obj%spaceElemSD, "spaceElemSD = ", &
  & unitno=unitno)

! stelemsd
IF (ALLOCATED(obj%stelemsd)) THEN
  CALL Display("stelemsd : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("stelemsd : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! quadTypeForSpace
CALL Display(obj%quadTypeForSpace, "quadTypeForSpace = ", &
  & unitno=unitno)

! continuityTypeForSpace
CALL Display(obj%continuityTypeForSpace, "continuityTypeForSpace = ", &
  & unitno=unitno)

! interpolTypeForSpace
CALL Display(obj%interpolTypeForSpace, "interpolTypeForSpace = ", &
  & unitno=unitno)

! orderSpace
CALL Display(obj%orderSpace, "orderSpace = ", &
  & unitno=unitno)

! quadForFacet
IF (ALLOCATED(obj%quadForFacet)) THEN
  CALL Display("quadForFacet : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("quadForFacet : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! quadForFacetCell
IF (ALLOCATED(obj%quadForFacetCell)) THEN
  CALL Display("quadForFacetCell : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("quadForFacetCell : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! linFacetElemSD
IF (ALLOCATED(obj%linFacetElemSD)) THEN
  CALL Display("linFacetElemSD : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("linFacetElemSD : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! linFacetCellElemSD
IF (ALLOCATED(obj%linFacetCellElemSD)) THEN
  CALL Display("linFacetCellElemSD : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("linFacetCellElemSD : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! facetElemSD
IF (ALLOCATED(obj%facetElemSD)) THEN
  CALL Display("facetElemSD : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("facetElemSD : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! facetCellElemSD
IF (ALLOCATED(obj%facetCellElemSD)) THEN
  CALL Display("facetCellElemSD : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("facetCellElemSD : NOT ALLOCATED", &
    & unitno=unitno)
END IF

! facetSTelemsd
IF (ALLOCATED(obj%facetSTelemsd)) THEN
  CALL Display("facetSTelemsd : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("facetSTelemsd : NOT ALLOCATED", unitno=unitno)
END IF

! quadTypeForFacet
CALL Display(obj%quadTypeForFacet, "quadTypeForFacet = ", &
  & unitno=unitno)

! continuityTypeForFacet
CALL Display(obj%continuityTypeForFacet, "continuityTypeForFacet = ", &
  & unitno=unitno)

! interpolTypeForFacet
CALL Display(obj%interpolTypeForFacet, "interpolTypeForFacet = ", &
  & unitno=unitno)

! orderFacet
CALL Display(obj%orderFacet, "orderFacet = ", unitno=unitno)

END PROCEDURE mesh_display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Import
CHARACTER(*), PARAMETER :: myName = "mesh_Import"
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
  & internalNptrs(:)
TYPE(String) :: dsetname
INTEGER(I4B) :: ii, dummy, jj
LOGICAL(LGT) :: isok

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')

dsetname = TRIM(group)

! CALL Display('Making dsetname', unitno=stdout)
! CALL Display('dsetname = '//TRIM(dsetname), stdout)

isok = hdf5%isOpen()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file is not opened')
  RETURN
END IF

isok = hdf5%isRead()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file does not have read permission')
  RETURN
END IF

isok = hdf5%isGroup(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)//  &
    & ' is not a group; it should be a group which contains the meshEntity')
  RETURN
END IF

isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)//  &
    & ' path does not exists')
  RETURN
END IF

!> read Uid
isok = hdf5%pathExists(TRIM(dsetname)//"/uid")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[INTERNAL ERROR] :: "//TRIM(dsetname)// &
    & "/uid"//' path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/uid", obj%Uid)
  ! CALL Display('uid = '//TRIM(str(obj%Uid, .TRUE.)), stdout)
END IF

!> read xidim
isok = hdf5%pathExists(TRIM(dsetname)//"/xidim")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[INTERNAL ERROR] :: "//TRIM(dsetname)//"/xidim"//  &
    & ' path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/xidim", obj%xidim)
  ! CALL Display('xidim = '//TRIM(str(obj%xidim, .TRUE.)), stdout)
END IF

!> reading elemtype
isok = hdf5%pathExists(TRIM(dsetname)//"/elemType")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[INTERNAL ERROR] :: "//TRIM(dsetname)//"/elemType"// &
    & ' path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/elemType", obj%elemType)
  ! CALL Display('elemType = '//TRIM(str(obj%elemType, .TRUE.)), stdout)
END IF

!> reading nsd
isok = hdf5%pathExists(TRIM(dsetname)//"/nsd")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)//"/nsd"// &
    & ' path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/nsd", obj%nsd)
  ! CALL Display('nsd = '//TRIM(str(obj%elemType, .TRUE.)), stdout)
END IF

!> reading tIntNodes
isok = hdf5%pathExists(TRIM(dsetname)//"/tIntNodes")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/tIntNodes path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/tIntNodes", obj%tIntNodes)
  ! CALL Display('tIntNodes = '//TRIM(str(obj%tIntNodes, .TRUE.)), stdout)
END IF

!> reading tElements, allocate obj%elementData
isok = hdf5%pathExists(TRIM(dsetname)//"/tElements")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/tElements path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/tElements", obj%tElements)
  ! CALL Display('tElements = '//TRIM(str(obj%tElements, .TRUE.)), stdout)
END IF

IF (ALLOCATED(obj%elementData)) DEALLOCATE (obj%elementData)
IF (obj%tElements .NE. 0) THEN
  ALLOCATE (obj%elementData(obj%tElements))
END IF

!>reading minX
isok = hdf5%pathExists(TRIM(dsetname)//"/minX")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/minX path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/minX", obj%minX)
  ! CALL Display('minX = '//TRIM(str(obj%minX, .TRUE.)), stdout)
END IF

!> reading minY
isok = hdf5%pathExists(TRIM(dsetname)//"/minY")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/minY path does not exists')
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/minY", obj%minY)
  ! CALL Display('minY = '//TRIM(str(obj%minY, .TRUE.)), stdout)
END IF

!> reading minZ
isok = hdf5%pathExists(TRIM(dsetname)//"/minZ")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[INTERNAL ERROR] :: "//TRIM(dsetname)// &
    & '/minZ path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/minZ", obj%minZ)
  ! CALL Display('minZ = '//TRIM(str(obj%minZ, .TRUE.)), stdout)
END IF

!> reading maxX
isok = hdf5%pathExists(TRIM(dsetname)//"/maxX")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/maxX path does not exists.')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/maxX", obj%maxX)
  ! CALL Display('maxX = '//TRIM(str(obj%maxX, .TRUE.)), stdout)
END IF

!> reading maxY
isok = hdf5%pathExists(TRIM(dsetname)//"/maxY")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)//  &
    & '/maxY path does not exists.')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/maxY", obj%maxY)
  ! CALL Display('maxY = '//TRIM(str(obj%maxY, .TRUE.)), stdout)
END IF

!> reading maxZ
isok = hdf5%pathExists(TRIM(dsetname)//"/maxZ")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/maxZ path does not exists.')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/maxZ", obj%minZ)
  ! CALL Display('maxZ = '//TRIM(str(obj%maxZ, .TRUE.)), stdout)
END IF

!> reading x
isok = hdf5%pathExists(TRIM(dsetname)//"/x")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/x path does not exists.')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/x", obj%x)
  ! CALL Display('x = '//TRIM(str(obj%x, .TRUE.)), stdout)
END IF

!> reading y
isok = hdf5%pathExists(TRIM(dsetname)//"/y")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] ::'//TRIM(dsetname)//  &
    & '/y path does not exists.')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/y", obj%y)
  ! CALL Display('y = '//TRIM(str(obj%y, .TRUE.)), stdout)
END IF

!> reading z
isok = hdf5%pathExists(TRIM(dsetname)//"/z")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/z path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/z", obj%z)
  ! CALL Display('z = '//TRIM(str(obj%z, .TRUE.)), stdout)
END IF

!> reading physicalTag
! CALL Display('reading physicalTag', stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/physicalTag")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/physicalTag path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/physicalTag", obj%physicalTag)
  ! IF (e%isLogActive()) THEN
  !   CALL Display(obj%physicalTag, "physicalTag = ",  &
  !   & unitno=e%getLogFileUnit())
  ! END IF
END IF

!> Reading elemNumber, maxElemNum, minElemNum, local_elemNumber,
!> elementData%globalElemNum, elementData%localElemNum

! CALL Display('reading elemNumber', stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/elemNumber")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/elemNumber path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/elemNumber", elemNumber)
  ! IF (e%isLogActive()) THEN
  !   CALL Display(elemNumber, "elemNumber = ", &
  !     & unitno=e%getLogFileUnit())
  ! END IF
  IF (ALLOCATED(elemNumber) .AND. SIZE(elemNumber) .NE. 0) THEN
    obj%maxElemNum = MAXVAL(elemNumber)
    obj%minElemNum = MINVAL(elemNumber)
  END IF
  CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)
  DO CONCURRENT(ii=1:obj%tElements)
    obj%local_elemNumber(elemNumber(ii)) = ii
    obj%elementData(ii)%globalElemNum = elemNumber(ii)
    obj%elementData(ii)%localElemNum = ii
  END DO
  ! CALL Display('maxElemNum = '//TRIM(str(obj%maxElemNum, .TRUE.)), stdout)
  ! CALL Display('minElemNum = '//TRIM(str(obj%minElemNum, .TRUE.)), stdout)
END IF

!> reading connectivity, maxNptrs, minNptrs, tNodes
!> elementData%globalNodes, local_nptrs,
!> nodeData%globalNodenum,  nodeData%localNodenum

! CALL Display('reading connectivity', stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/connectivity")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/connectivity path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/connectivity", connectivity)
  isok = (obj%elemType .EQ. Point1) .OR. (obj%elemType .EQ. 0)
  IF (isok) THEN
    obj%tNodes = 1
    IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
    ALLOCATE (obj%nodeData(obj%tNodes))
    obj%nodeData(1)%globalNodeNum = 1
    obj%nodeData(1)%localNodeNum = 1
    obj%nodeData(1)%nodeType = BOUNDARY_NODE
  ELSE
    obj%maxNptrs = MAXVAL(connectivity)
    obj%minNptrs = MINVAL(connectivity)
    CALL Reallocate(obj%Local_Nptrs, obj%maxNptrs)

    DO CONCURRENT(ii=1:obj%tElements)
      obj%elementData(ii)%globalNodes = connectivity(:, ii)
      obj%Local_Nptrs(connectivity(:, ii)) = connectivity(:, ii)
    END DO

    obj%tNodes = COUNT(obj%Local_Nptrs .NE. 0)
    IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
    ALLOCATE (obj%nodeData(obj%tNodes))
    dummy = 0

    DO ii = 1, obj%maxNptrs
      IF (obj%Local_Nptrs(ii) .NE. 0) THEN
        dummy = dummy + 1
        obj%nodeData(dummy)%globalNodeNum = obj%Local_Nptrs(ii)
        obj%nodeData(dummy)%localNodeNum = dummy
        obj%nodeData(dummy)%nodeType = BOUNDARY_NODE
        ! The above step is unusual, but we know the position of
        ! internal nptrs, so later we will set the
        ! those nodes as INTERNAL_NODE, in this way we can
        ! identify the boundary nodes
        obj%Local_Nptrs(ii) = dummy
      END IF
    END DO

  END IF
  ! IF (e%isLogActive()) THEN
  !   CALL Display('connectivity(1:NNS, 1:tElements) is given below = ', stdout)
  !   DO ii = 1, SIZE(connectivity, 1)
  !     CALL DISP(title="Node-"//TRIM(str(ii, .TRUE.)), &
  !       & x=connectivity(ii, :), &
  !       & unit=e%getLogFileUnit(), style="UNDERLINE", &
  !       & advance="NO")
  !   END DO
  !   CALL DISP(title='', x='', unit=e%getLogFileUnit(), &
  !     & advance="DOUBLE")
  ! END IF

  ! CALL Display('maxNptrs = '//TRIM(str(obj%maxNptrs, .TRUE.)), stdout)
  ! CALL Display('minNptrs = '//TRIM(str(obj%minNptrs, .TRUE.)), stdout)
  ! CALL Display('tNodes = '//TRIM(str(obj%tNodes, .TRUE.)), stdout)

END IF

!> reading InternalNptrs, nodeData%globalNodeNumber,
!> nodeData%localNodeNumber, nodeData%nodeType
!> mark INTERNAL_NODE

! CALL Display('reading InternalNptrs', stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/intNodeNumber")
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: '//TRIM(dsetname)// &
    & '/intNodeNumber path does not exists')
  RETURN
ELSE
  CALL hdf5%READ(TRIM(dsetname)//"/intNodeNumber", InternalNptrs)
  ! IF (e%isLogActive()) THEN
  !   CALL Display(InternalNptrs, "InternalNptrs = ", &
  !     & unitno=e%getLogFileUnit())
  ! END IF
  IF (obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0) THEN
    obj%nodeData(1)%globalNodeNum = InternalNptrs(1)
    obj%nodeData(1)%nodeType = INTERNAL_NODE
  ELSE
    DO ii = 1, SIZE(InternalNptrs)
      jj = obj%getLocalNodeNumber(InternalNptrs(ii))
      obj%nodeData(jj)%globalNodeNum = InternalNptrs(ii)
      obj%nodeData(jj)%nodeType = INTERNAL_NODE
    END DO
  END IF
END IF

!> reading boundingEntity
! CALL Display('reading boundingEntity', stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/boundingEntity")
IF (isok) THEN
  CALL hdf5%READ(TRIM(dsetname)//"/boundingEntity", obj%boundingEntity)
  ! IF (e%isLogActive()) THEN
  !   CALL Display(obj%boundingEntity, "boundingEntity = ", &
  !     & unitno=e%getLogFileUnit())
  ! END IF
END IF

!> set Reference Element
! CALL Display("Setting reference element", unitno=stdout)
isok = hdf5%pathExists(TRIM(dsetname)//"/ipType")
IF (isok) THEN
  CALL hdf5%READ(TRIM(dsetname)//"/ipType", obj%ipType)
ELSE
  obj%ipType = Equidistance
END IF

isok = obj%tElements .GT. 0
IF (isok) THEN
  obj%refelem => ReferenceElement_Pointer( &
    & xidim=obj%xidim, &
    & nsd=obj%nsd, &
    & elemType=obj%elemType, &
    & ipType=obj%ipType)

  ! CALL Display("Setting facetElements", stdout)
  IF (obj%xidim .GT. 0) THEN
    IF (.NOT. ALLOCATED(obj%facetElements)) THEN
      obj%facetElements = FacetElements(obj%refelem)
    END IF
  END IF
END IF

IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(InternalNptrs)) DEALLOCATE (InternalNptrs)

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')

END PROCEDURE mesh_Import

!----------------------------------------------------------------------------
!                                                              getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord
CHARACTER(*), PARAMETER :: myName = "mesh_getNodeCoord"
TYPE(String) :: dsetname
INTEGER(I4B) :: ii, jj
REAL(DFP), ALLOCATABLE :: xij(:, :)
!
! main
!
dsetname = TRIM(group)
!
! check
!
IF (.NOT. hdf5%isOpen()) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
!
! check
!
IF (.NOT. hdf5%isRead()) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
!
! check
!
IF (.NOT. hdf5%pathExists(dsetname%chars())) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & TRIM(dsetname)//' path does not exists')
!
! build nodeCoord
!
CALL hdf5%READ(dsetname%chars(), xij)
CALL Reallocate(nodeCoord, 3_I4B, obj%getTotalNodes())
jj = SIZE(xij, 1)
DO ii = 1, SIZE(nodeCoord, 2)
  nodeCoord(1:jj, ii) = xij(1:jj, obj%getGlobalNodeNumber(ii))
END DO
IF (ALLOCATED(xij)) DEALLOCATE (xij)
!
END PROCEDURE mesh_getNodeCoord

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Export
CHARACTER(*), PARAMETER :: myName = "mesh_Export"
CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[WIP]: This routine has not been implemented yet.")
END PROCEDURE mesh_Export

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "mesh_ExportToVTK"
LOGICAL(LGT) :: OpenTag_, CloseTag_, Content_
INTEGER(INT8) :: vtkType
INTEGER(INT8), ALLOCATABLE :: types(:)
INTEGER(I4B) :: nCells, nPoints, ii, jj, nne
INTEGER(I4B), ALLOCATABLE :: vtkIndx(:), connectivity(:), &
  & offsets(:), localNptrs(:)
! main
IF (.NOT. vtkFile%isInitiated) THEN
  IF (.NOT. PRESENT(filename)) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "VTKFile_ is not initiated, and filename is not present.")
  ELSE
    CALL vtkFile%InitiateVTKFile(filename=filename, &
      & mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
      & DataStructureType=VTK_UnStructuredGrid)
  END IF
END IF
!
nCells = obj%getTotalElements()
nPoints = obj%getTotalNodes()
OpenTag_ = INPUT(default=.TRUE., option=OpenTag)
CloseTag_ = INPUT(default=.TRUE., option=CloseTag)
Content_ = INPUT(default=.TRUE., option=Content)
! Write piece information if OpenTag is true
IF (OpenTag_) CALL vtkFile%WritePiece(nPoints=nPoints, nCells=nCells)
! Write Points information
IF (PRESENT(nodeCoord)) THEN
  IF (ANY(SHAPE(nodeCoord) .NE. [3, nPoints])) &
      & CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "Shape of nodeCoord should be [3, nPoints]")
  CALL vtkFile%WritePoints(x=nodeCoord)
END IF
! Write Cells
IF (Content_) THEN
  CALL getVTKelementType(elemType=obj%elemType, &
    & vtk_type=vtkType, Nptrs=vtkIndx)
  nne = SIZE(vtkIndx)
  ALLOCATE (types(nCells), offsets(nCells), &
    & connectivity(nne * nCells))
  types = vtkType; offsets(1) = nne; jj = 0
  DO ii = 2, nCells
    offsets(ii) = offsets(ii - 1) + nne
  END DO
  DO ii = obj%minElemNum, obj%maxElemNum
    IF (obj%isElementPresent(ii)) THEN
      jj = jj + 1
      localNptrs = obj%getLocalNodeNumber( &
        & obj%getConnectivity(globalElement=ii))
     connectivity(offsets(jj) - nne + 1:offsets(jj)) = localNptrs(vtkIndx) - 1
    END IF
  END DO
  CALL vtkFile%WriteCells(connectivity=connectivity, offsets=offsets, &
    & types=types)
END IF
IF (CloseTag_) CALL vtkFile%WritePiece()
! clean up
IF (ALLOCATED(types)) DEALLOCATE (types)
IF (ALLOCATED(vtkIndx)) DEALLOCATE (vtkIndx)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
IF (ALLOCATED(localNptrs)) DEALLOCATE (localNptrs)
END PROCEDURE mesh_ExportToVTK

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE elemData_Display
CALL Display(TRIM(msg), unitno=unitno)
CALL Display(obj%globalElemNum, msg="# globalElemNum=", unitno=unitno)
CALL Display(obj%localElemNum, msg="# localElemNum=", unitno=unitno)
!
SELECT CASE (obj%elementType)
CASE (INTERNAL_ELEMENT)
  CALL Display("# elementType=INTERNAL_ELEMENT", unitno=unitno)
CASE (BOUNDARY_ELEMENT)
  CALL Display("# elementType=BOUNDARY_ELEMENT", unitno=unitno)
CASE (DOMAIN_BOUNDARY_ELEMENT)
  CALL Display("# elementType=DOMAIN_BOUNDARY_ELEMENT", unitno=unitno)
CASE (GHOST_ELEMENT)
  CALL Display("# elementType=GHOST_ELEMENT", unitno=unitno)
END SELECT
!
! globalNodes
!
IF (ALLOCATED(obj%globalNodes)) THEN
  CALL Display(obj%globalNodes, msg="# globalNodes=", unitno=unitno)
END IF
!
! globalElements
!
IF (ALLOCATED(obj%globalElements)) THEN
  CALL Display(obj%globalElements, msg="# globalElements=", unitno=unitno)
END IF
!
! boundaryData
!
IF (ALLOCATED(obj%boundaryData)) THEN
  CALL Display(obj%boundaryData, msg="# boundaryData=", unitno=unitno)
END IF
!
END PROCEDURE elemData_Display

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE nodeData_Display
!
CALL Display(TRIM(msg), unitno=unitno)
CALL Display(obj%globalNodeNum, msg="# globalNodeNum=", unitno=unitno)
CALL Display(obj%localNodeNum, msg="# localNodeNum=", unitno=unitno)
!
SELECT CASE (obj%nodeType)
CASE (INTERNAL_NODE)
  CALL Display("# nodeType=INTERNAL_NODE", unitno=unitno)
CASE (BOUNDARY_NODE)
  CALL Display("# nodeType=BOUNDARY_NODE", unitno=unitno)
CASE (DOMAIN_BOUNDARY_NODE)
  CALL Display("# nodeType=DOMAIN_BOUNDARY_NODE", unitno=unitno)
CASE (GHOST_NODE)
  CALL Display("# nodeType=GHOST_NODE", unitno=unitno)
END SELECT
!
CALL Display(obj%localNodeNum, msg="# localNodeNum=", unitno=unitno)
!
! globalNodes
!
IF (ALLOCATED(obj%globalNodes)) THEN
  CALL Display(obj%globalNodes, msg="# globalNodes=", unitno=unitno)
END IF
!
! extraGlobalNodes
!
IF (ALLOCATED(obj%extraGlobalNodes)) THEN
  CALL Display(obj%extraGlobalNodes, msg="# extraGlobalNodes=", &
    & unitno=unitno)
END IF
!
! globalElements
!
IF (ALLOCATED(obj%globalElements)) THEN
  CALL Display(obj%globalElements, msg="# globalElements=", unitno=unitno)
END IF
!
END PROCEDURE nodeData_Display

!----------------------------------------------------------------------------
!                                                 InternalFacetData_Display
!----------------------------------------------------------------------------

MODULE PROCEDURE InternalFacetData_Display
!
CALL Display(TRIM(msg), unitno=unitno)
CALL Display("# elementType=INTERNAL_ELEMENT", unitno=unitno)
CALL Display(obj%masterCellNumber, msg="# masterCellNumber=", &
  & unitno=unitno)
CALL Display(obj%slaveCellNumber, msg="# slaveCellNumber=", &
  & unitno=unitno)
CALL Display(obj%masterlocalFacetID, msg="# masterlocalFacetID=", &
  & unitno=unitno)
CALL Display(obj%slavelocalFacetID, msg="# slavelocalFacetID=", &
  & unitno=unitno)
!
END PROCEDURE InternalFacetData_Display

!----------------------------------------------------------------------------
!                                                   BoundaryFacetData_Display
!----------------------------------------------------------------------------

MODULE PROCEDURE BoundaryFacetData_Display
!
CALL Display(TRIM(msg), unitno=unitno)
!
SELECT CASE (obj%elementType)
CASE (INTERNAL_ELEMENT)
  CALL Display("# elementType=INTERNAL_ELEMENT", unitno=unitno)
CASE (BOUNDARY_ELEMENT)
  CALL Display("# elementType=BOUNDARY_ELEMENT", unitno=unitno)
CASE (DOMAIN_BOUNDARY_ELEMENT)
  CALL Display("# elementType=DOMAIN_BOUNDARY_ELEMENT", unitno=unitno)
END SELECT
!
CALL Display(obj%masterCellNumber, msg="# masterCellNumber=", &
  & unitno=unitno)
CALL Display(obj%masterLocalFacetID, msg="# masterlocalFacetID=", &
  & unitno=unitno)
!
END PROCEDURE BoundaryFacetData_Display

!----------------------------------------------------------------------------
!                                                        DisplayElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayElementData
!
INTEGER(I4B) :: ii, telements
!
telements = obj%getTotalElements()
!
CALL Display(TRIM(msg), unitno=unitno)
!
DO ii = 1, telements
  CALL obj%elementData(ii)%Display(msg="# elementData( "//tostring(ii) &
    & //" )=", unitno=unitno)
  CALL BlankLines(nol=2, unitno=unitno)
END DO
!
END PROCEDURE mesh_DisplayElementData

!----------------------------------------------------------------------------
!                                                            DisplayNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayNodeData
!
INTEGER(I4B) :: ii, tNodes
!
tNodes = obj%getTotalNodes()
!
CALL Display(TRIM(msg), unitno=unitno)
!
DO ii = 1, tNodes
  CALL obj%nodeData(ii)%Display(msg="# nodeData( "//tostring(ii) &
    & //" )=", unitno=unitno)
  CALL BlankLines(nol=2, unitno=unitno)
END DO
!
END PROCEDURE mesh_DisplayNodeData

!----------------------------------------------------------------------------
!                                                  DisplayInternalFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayInternalFacetData
INTEGER(I4B) :: ii, telements

CALL Display(TRIM(msg), unitno=unitno)

IF (ALLOCATED(obj%internalFacetData)) THEN

  telements = SIZE(obj%internalFacetData)

  DO ii = 1, telements
    CALL obj%internalFacetData(ii)%Display( &
      & msg="# internalFacetData( "//tostring(ii) &
      & //" )=", unitno=unitno)
    CALL BlankLines(nol=2, unitno=unitno)
  END DO

ELSE
  CALL Display("# internalFacetData NOT ALLOCATED", unitno=unitno)
END IF

END PROCEDURE mesh_DisplayInternalFacetData

!----------------------------------------------------------------------------
!                                                   DisplayBoundaryFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayBoundaryFacetData
INTEGER(I4B) :: ii

CALL Display(TRIM(msg), unitno=unitno)

IF (ALLOCATED(obj%boundaryFacetData)) THEN

  DO ii = 1, SIZE(obj%boundaryFacetData)
    CALL obj%boundaryFacetData(ii)%Display( &
      & msg="# boundaryFacetData( "//tostring(ii) &
      & //" )=", unitno=unitno)
    CALL BlankLines(nol=2, unitno=unitno)
  END DO

ELSE
  CALL Display("# boundaryFacetData NOT ALLOCATED", unitno=unitno)
END IF

END PROCEDURE mesh_DisplayBoundaryFacetData

!----------------------------------------------------------------------------
!                                                         DisplayFacetElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayFacetElemSD
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)

IF (ALLOCATED(obj%facetElemSD)) THEN

  DO ii = 1, SIZE(obj%facetElemSD)

    CALL Display( &
      & obj%facetElemSD(ii), &
      & "# obj%facetElemSD( "//tostring(ii)//" )=", &
      & unitno=unitno)

    CALL BlankLines(nol=2, unitno=unitno)

    CALL Display( &
      & obj%facetCellElemSD(ii), &
      & "# obj%facetCellElemSD( "//tostring(ii)//" )=", &
      & unitno=unitno)

    CALL BlankLines(nol=2, unitno=unitno)

  END DO

ELSE

  CALL Display("# facetElemSD : NOT ALLOCATED", unitno=unitno)

END IF

END PROCEDURE mesh_DisplayFacetElemSD

!----------------------------------------------------------------------------
!                                                      DisplayFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_DisplayFacetElements
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)

IF (ALLOCATED(obj%facetElements)) THEN

  DO ii = 1, SIZE(obj%facetElements)
    CALL Display( &
      & obj%facetElements(ii), &
      & "# obj%facetElements( "//tostring(ii)//" )=", &
      & unitno=unitno)
    CALL BlankLines(nol=2, unitno=unitno)
  END DO

ELSE

  CALL Display("# facetElements : NOT ALLOCATED", unitno=unitno)

END IF

END PROCEDURE mesh_DisplayFacetElements

END SUBMODULE IOMethods
