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

SUBMODULE(mshEntity_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_Final
CALL obj%DEALLOCATE()
END PROCEDURE ent_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_Deallocate
obj%uid = 0
obj%xiDim = 0
obj%elemType = 0
obj%minX = 0.0
obj%minY = 0.0
obj%minZ = 0.0
obj%maxX = 0.0
obj%maxY = 0.0
obj%maxZ = 0.0
obj%x = 0.0
obj%y = 0.0
obj%z = 0.0
IF (ALLOCATED(obj%physicalTag)) DEALLOCATE (obj%physicalTag)
IF (ALLOCATED(obj%IntNodeNumber)) DEALLOCATE (obj%IntNodeNumber)
IF (ALLOCATED(obj%ElemNumber)) DEALLOCATE (obj%ElemNumber)
IF (ALLOCATED(obj%Connectivity)) DEALLOCATE (obj%Connectivity)
IF (ALLOCATED(obj%BoundingEntity)) DEALLOCATE (obj%BoundingEntity)
IF (ALLOCATED(obj%NodeCoord)) DEALLOCATE (obj%NodeCoord)
END PROCEDURE ent_Deallocate

!----------------------------------------------------------------------------
!                                                               gotoEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_GotoTag
! Define internal variables
INTEGER(I4B) :: IOSTAT, Reopen, unitNo
CHARACTER(100) :: Dummy, IOMSG
CHARACTER(*), PARAMETER :: myName = "ent_GotoTag"
!
! Find $meshFormat

  IF( .NOT. mshFile%isOpen() .OR. .NOT. mshFile%isRead() .OR. .NOT. mshFile%isInitiated() ) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'mshFile is either not opened or does not have read access!')
  error = -1
ELSE
  Reopen = 0; error = 0; CALL mshFile%REWIND()
  DO
    unitNo = mshFile%getUnitNo(); Dummy = ""
    READ (unitNo, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG) Dummy
    IF (IS_IOSTAT_END(IOSTAT)) THEN
      CALL mshFile%setEOFStat(.TRUE.)
      Reopen = Reopen + 1
    END IF
    IF (IOSTAT .GT. 0 .AND. Reopen .GT. 1) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Could not find $Entities !'//' :: Reopen='// &
      & TOSTRING(Reopen)//' :: IOSTAT='//TOSTRING(IOSTAT) &
      & //" :: IOMSG="//TRIM(IOMSG))
      error = -2
      EXIT
    ELSE IF (TRIM(Dummy) .EQ. '$Entities') THEN
      EXIT
    END IF
  END DO
END IF
END PROCEDURE ent_GotoTag

!----------------------------------------------------------------------------
!                                                           Write
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_Write
SELECT CASE (dim)
CASE (0)
  CALL WritePointEntity(obj, afile)
CASE (1)
  CALL WriteCurveEntity(obj, afile)
CASE (2)
  CALL WriteSurfaceEntity(obj, afile)
CASE (3)
  CALL WriteVolumeEntity(obj, afile)
END SELECT
END PROCEDURE ent_Write

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WritePointEntity(obj, afile)
  CLASS(mshEntity_), INTENT(INOUT) :: obj
  CLASS(TxtFile_), INTENT(INOUT) :: afile

  ! Define internal variables
  INTEGER(I4B) :: unitNo
  INTEGER(I4B) :: ii
  INTEGER(I4B) :: numPhysicalTags
  TYPE(String) :: astr

  astr = tostring(obj%uid)//" " &
  & //tostring(obj%x)//" " &
  & //tostring(obj%y)//" " &
  & //tostring(obj%z)

  IF (ALLOCATED(obj%physicalTag)) THEN
    numPhysicalTags = SIZE(obj%physicalTag)
    astr = astr//" "//tostring(numPhysicalTags)//" "
    DO ii = 1, numPhysicalTags
      astr = astr//" "// &
        & tostring(obj%physicalTag(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  unitNo = afile%getUnitNo()

  WRITE (unitNo, "(A)") astr%chars()

END SUBROUTINE WritePointEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WriteCurveEntity(obj, afile)
  CLASS(mshEntity_), INTENT(INOUT) :: obj
  CLASS(TxtFile_), INTENT(INOUT) :: afile

  ! Define internal variables
  INTEGER(I4B) :: unitNo
  INTEGER(I4B) :: ii
  INTEGER(I4B) :: n
  TYPE(String) :: astr

  astr = tostring(obj%uid)//" " &
  & //tostring(obj%minx)//" " &
  & //tostring(obj%miny)//" " &
  & //tostring(obj%minz)//" " &
  & //tostring(obj%maxx)//" " &
  & //tostring(obj%maxy)//" " &
  & //tostring(obj%maxz)

  IF (ALLOCATED(obj%physicalTag)) THEN
    n = SIZE(obj%physicalTag)
    astr = astr//" "//tostring(n)//" "
    DO ii = 1, n
      astr = astr//" "// &
        & tostring(obj%physicalTag(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  IF (ALLOCATED(obj%boundingEntity)) THEN
    n = SIZE(obj%boundingEntity)
    astr = astr//" "//tostring(n)
    DO ii = 1, n
      astr = astr//" "// &
        & tostring(obj%boundingEntity(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  unitNo = afile%getUnitNo()

  WRITE (unitNo, "(A)") astr%chars()

END SUBROUTINE WriteCurveEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WriteSurfaceEntity(obj, afile)
  CLASS(mshEntity_), INTENT(INOUT) :: obj
  CLASS(TxtFile_), INTENT(INOUT) :: afile

  ! Define internal variables
  INTEGER(I4B) :: unitNo
  INTEGER(I4B) :: ii
  INTEGER(I4B) :: n
  TYPE(String) :: astr

  astr = tostring(obj%uid)//" " &
  & //tostring(obj%minx)//" " &
  & //tostring(obj%miny)//" " &
  & //tostring(obj%minz)//" " &
  & //tostring(obj%maxx)//" " &
  & //tostring(obj%maxy)//" " &
  & //tostring(obj%maxz)

  IF (ALLOCATED(obj%physicalTag)) THEN
    n = SIZE(obj%physicalTag)
    astr = astr//" "//tostring(n)//" "
    DO ii = 1, n
      astr = astr//" "// &
        & tostring(obj%physicalTag(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  IF (ALLOCATED(obj%boundingEntity)) THEN
    n = SIZE(obj%boundingEntity)
    astr = astr//" "//tostring(n)
    DO ii = 1, n
      astr = astr//" "// &
        & tostring(obj%boundingEntity(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  unitNo = afile%getUnitNo()

  WRITE (unitNo, "(A)") astr%chars()

END SUBROUTINE WriteSurfaceEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WriteVolumeEntity(obj, afile)
  CLASS(mshEntity_), INTENT(INOUT) :: obj
  CLASS(TxtFile_), INTENT(INOUT) :: afile

  ! Define internal variables
  INTEGER(I4B) :: unitNo
  INTEGER(I4B) :: ii
  INTEGER(I4B) :: n
  TYPE(String) :: astr

  astr = tostring(obj%uid)//" " &
  & //tostring(obj%minx)//" " &
  & //tostring(obj%miny)//" " &
  & //tostring(obj%minz)//" " &
  & //tostring(obj%maxx)//" " &
  & //tostring(obj%maxy)//" " &
  & //tostring(obj%maxz)

  IF (ALLOCATED(obj%physicalTag)) THEN
    n = SIZE(obj%physicalTag)
    astr = astr//" "//tostring(n)//" "
    DO ii = 1, n
      astr = astr//" "// &
        & tostring(obj%physicalTag(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  IF (ALLOCATED(obj%boundingEntity)) THEN
    n = SIZE(obj%boundingEntity)
    astr = astr//" "//tostring(n)
    DO ii = 1, n
      astr = astr//" "// &
        & tostring(obj%boundingEntity(ii))
    END DO
  ELSE
    astr = astr//" 0"
  END IF

  unitNo = afile%getUnitNo()

  WRITE (unitNo, "(A)") astr%chars()

END SUBROUTINE WriteVolumeEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_WriteNodeBlock
TYPE(String) :: astr
INTEGER(I4B) :: unitNo
INTEGER(I4B) :: n
INTEGER(I4B) :: jj

unitNo = afile%getUnitNo()
astr = tostring(obj%xidim)//" "// &
  & tostring(obj%uid)//" "// &
  & "0 "

IF (ALLOCATED(obj%intNodeNumber)) THEN
  n = SIZE(obj%intNodeNumber)
  astr = astr//tostring(n)
ELSE
  astr = astr//"0 "
  n = 0
END IF

WRITE (unitNo, "(A)") astr%chars()

astr = ""

DO jj = 1, n
  WRITE (unitNo, "(A)") tostring(obj%intNodeNumber(jj))
END DO

DO jj = 1, n
  WRITE (unitNo, "(A)") &
    & str(obj%NodeCoord(1, jj))//" "// &
    & str(obj%NodeCoord(2, jj))//" "// &
    & str(obj%NodeCoord(3, jj))
END DO

END PROCEDURE ent_WriteNodeBlock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_WriteElementBlock
TYPE(String) :: astr
INTEGER(I4B) :: unitNo
INTEGER(I4B) :: n
INTEGER(I4B) :: jj
INTEGER(I4B) :: ii

IF (ALLOCATED(obj%ElemNumber)) THEN
  n = SIZE(obj%elemNumber)
  IF (n .GT. 0) THEN
    unitNo = afile%getUnitNo()
    astr = tostring(obj%xidim)//" "// &
      & tostring(obj%uid)//" "// &
      & tostring(obj%elemType)//" "//tostring(n)

    WRITE (unitNo, "(A)") astr%chars()

    DO jj = 1, n
      astr = tostring(obj%elemNumber(jj))
      DO ii = 1, SIZE(obj%Connectivity, 1)
        astr = astr//" "//tostring(obj%Connectivity(ii, jj))
      END DO
      WRITE (unitNo, "(A)") astr%chars()
    END DO
  END IF
END IF

END PROCEDURE ent_WriteElementBlock

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_display
CALL Display(Msg, UnitNo=UnitNo)
CALL Display(obj%UiD, Msg="Tag: ", unitNo=unitNo)
SELECT CASE (obj%XiDim)
CASE (0)
  CALL Display("Type: Point", unitNo=unitNo)
  CALL Display(obj%X, "X: ", unitNo=unitNo)
  CALL Display(obj%Y, "Y: ", unitNo=unitNo)
  CALL Display(obj%Z, "Z: ", unitNo=unitNo)
CASE (1)
  CALL Display("Type: Curve", unitNo=unitNo)
  CALL Display(obj%minX, "minX: ", unitNo=unitNo)
  CALL Display(obj%minY, "minY: ", unitNo=unitNo)
  CALL Display(obj%minZ, "minZ: ", unitNo=unitNo)
  CALL Display(obj%maxX, "maxX: ", unitNo=unitNo)
  CALL Display(obj%maxY, "maxY: ", unitNo=unitNo)
  CALL Display(obj%maxZ, "maxZ: ", unitNo=unitNo)
  CALL Display(obj%BoundingEntity, "Bounding points: ", unitNo=unitNo)
CASE (2)
  CALL Display("Type: Surface", unitNo=unitNo)
  CALL Display(obj%minX, "minX: ", unitNo=unitNo)
  CALL Display(obj%minY, "minY: ", unitNo=unitNo)
  CALL Display(obj%minZ, "minZ: ", unitNo=unitNo)
  CALL Display(obj%maxX, "maxX: ", unitNo=unitNo)
  CALL Display(obj%maxY, "maxY: ", unitNo=unitNo)
  CALL Display(obj%maxZ, "maxZ: ", unitNo=unitNo)
  CALL Display(obj%BoundingEntity, "Bounding curves: ", unitNo=unitNo)
CASE (3)
  CALL Display("Type: Surface", unitNo=unitNo)
  CALL Display(obj%minX, "minX: ", unitNo=unitNo)
  CALL Display(obj%minY, "minY: ", unitNo=unitNo)
  CALL Display(obj%minZ, "minZ: ", unitNo=unitNo)
  CALL Display(obj%maxX, "maxX: ", unitNo=unitNo)
  CALL Display(obj%maxY, "maxY: ", unitNo=unitNo)
  CALL Display(obj%maxZ, "maxZ: ", unitNo=unitNo)
  CALL Display(obj%BoundingEntity, "Bounding surfaces: ", unitNo=unitNo)
END SELECT
! Physical Tag
IF (ALLOCATED(obj%physicalTag)) THEN
  CALL Display(obj%physicalTag, "physicalTag: ", unitNo=unitNo)
END IF
! Nodes
IF (ALLOCATED(obj%IntNodeNumber)) THEN
  CALL Display(obj%IntNodeNumber, "Internal Node Number: ", unitNo=unitNo)
  CALL Display(TRANSPOSE(obj%NodeCoord), "Nodal Coordinates: ", &
    & unitNo=unitNo)
END IF
! Elements
IF (ALLOCATED(obj%ElemNumber)) THEN
  CALL Display(obj%ElemNumber, "Element number: ", unitNo=unitNo)
  CALL Display(TRANSPOSE(obj%Connectivity), "Connectivity: ", &
    & unitNo=unitNo)
END IF
END PROCEDURE ent_display

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_Read
SELECT CASE (dim)
CASE (0)
  CALL ReadPointEntity(obj, mshFile, readTag, error)
CASE (1)
  CALL ReadCurveEntity(obj, mshFile, readTag, error)
CASE (2)
  CALL ReadSurfaceEntity(obj, mshFile, readTag, error)
CASE (3)
  CALL ReadVolumeEntity(obj, mshFile, readTag, error)
END SELECT
END PROCEDURE ent_Read

!----------------------------------------------------------------------------
!                                                           ReadPointEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ReadPointEntity
! Define internal variables
INTEGER(I4B) :: dummyierr
INTEGER(I4B) :: Intvec(100), n, i

! go to tag
IF (ReadTag) THEN
  CALL obj%GotoTag(mshFile, error)
ELSE
  error = 0
END IF

IF (error .EQ. 0) THEN
  obj%XiDim = 0
  READ (mshFile%getUnitNo(), *) obj%Uid, obj%X, obj%Y, obj%Z, &
    & n, (Intvec(i), i=1, n)
  IF (ALLOCATED(obj%physicalTag)) DEALLOCATE (obj%physicalTag)
  IF (n .NE. 0) THEN
    ALLOCATE (obj%physicalTag(n))
    obj%physicalTag(1:n) = Intvec(1:n)
  END IF
END IF

END PROCEDURE ReadPointEntity

!----------------------------------------------------------------------------
!                                                           ReadCurveEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ReadCurveEntity
INTEGER(I4B) :: Intvec1(100), n, i, m, Intvec2(100)

IF (ReadTag) THEN
  CALL obj%GotoTag(mshFile, error)
ELSE
  error = 0
END IF

IF (error .EQ. 0) THEN
  obj%XiDim = 1
  READ (mshFile%getUnitNo(), *) &
    & obj%Uid, obj%minX, obj%minY, obj%minZ, &
    & obj%maxX, obj%maxY, obj%maxZ, &
    & n, (Intvec1(i), i=1, n), &
    & m, (Intvec2(i), i=1, m)

  IF (ALLOCATED(obj%physicalTag)) DEALLOCATE (obj%physicalTag)
  IF (ALLOCATED(obj%BoundingEntity)) DEALLOCATE (obj%BoundingEntity)

  IF (n .NE. 0) THEN
    ALLOCATE (obj%physicalTag(n))
    obj%physicalTag(1:n) = Intvec1(1:n)
  END IF
  IF (m .NE. 0) THEN
    ALLOCATE (obj%BoundingEntity(m))
    obj%BoundingEntity(1:m) = Intvec2(1:m)
  END IF
END IF
END PROCEDURE ReadCurveEntity

!----------------------------------------------------------------------------
!                                                          ReadSurfaceEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ReadSurfaceEntity
! Define internal variables
INTEGER(I4B), ALLOCATABLE :: Intvec1(:), Intvec2(:)
INTEGER(I4B) :: n, i, m
TYPE(String) :: aline
TYPE(String), ALLOCATABLE :: entries(:)
!
IF (ReadTag) THEN
  CALL obj%GotoTag(mshFile, error)
ELSE
  error = 0
END IF

IF (error .EQ. 0) THEN
  obj%XiDim = 2
  CALL aline%read_line(unit=mshFile%getUnitno())
  CALL aline%split(tokens=entries, sep=' ')
  obj%Uid = entries(1)%to_number(kind=1_I4B)

  obj%minX = entries(2)%to_number(kind=1.0_DFP)
  obj%minY = entries(3)%to_number(kind=1.0_DFP)
  obj%minZ = entries(4)%to_number(kind=1.0_DFP)

  obj%maxX = entries(5)%to_number(kind=1.0_DFP)
  obj%maxY = entries(6)%to_number(kind=1.0_DFP)
  obj%maxZ = entries(7)%to_number(kind=1.0_DFP)

  n = entries(8)%to_number(kind=I4B)
  IF (n .NE. 0) THEN
    ALLOCATE (IntVec1(n))
    DO i = 1, n
      IntVec1(i) = entries(8 + i)%to_number(kind=I4B)
    END DO
  END IF
    !! check total length here
  m = entries(9 + n)%to_number(kind=I4B)
  IF (m .NE. 0) THEN
    ALLOCATE (IntVec2(m))
    DO i = 1, m
      IntVec2(i) = entries(9 + n + i)%to_number(kind=I4B)
    END DO
  END IF

  IF (ALLOCATED(obj%physicalTag)) DEALLOCATE (obj%physicalTag)
  IF (ALLOCATED(obj%BoundingEntity)) DEALLOCATE (obj%BoundingEntity)

  IF (n .NE. 0) THEN
    ALLOCATE (obj%physicalTag(n))
    obj%physicalTag(1:n) = Intvec1(1:n)
  END IF

  IF (m .NE. 0) THEN
    ALLOCATE (obj%BoundingEntity(m))
    obj%BoundingEntity(1:m) = Intvec2(1:m)
  END IF
END IF

IF (ALLOCATED(IntVec1)) DEALLOCATE (IntVec1)
IF (ALLOCATED(IntVec2)) DEALLOCATE (IntVec2)
IF (ALLOCATED(entries)) DEALLOCATE (entries)
END PROCEDURE ReadSurfaceEntity

!----------------------------------------------------------------------------
!                                                           ReadVolumeEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ReadVolumeEntity
! Define internal variables
INTEGER(I4B), ALLOCATABLE :: Intvec1(:), Intvec2(:)
INTEGER(I4B) :: n, i, m
TYPE(String) :: aline
TYPE(String), ALLOCATABLE :: entries(:)

IF (ReadTag) THEN
  CALL obj%GotoTag(mshFile, error)
ELSE
  error = 0
END IF
!
IF (error .EQ. 0) THEN
  obj%XiDim = 3

  CALL aline%read_line(unit=mshFile%getUnitno())
  CALL aline%split(tokens=entries, sep=' ')
  obj%Uid = entries(1)%to_number(kind=I4B)

  obj%minX = entries(2)%to_number(kind=1.0_DFP)
  obj%minY = entries(3)%to_number(kind=1.0_DFP)
  obj%minZ = entries(4)%to_number(kind=1.0_DFP)

  obj%maxX = entries(5)%to_number(kind=1.0_DFP)
  obj%maxY = entries(6)%to_number(kind=1.0_DFP)
  obj%maxZ = entries(7)%to_number(kind=1.0_DFP)

  n = entries(8)%to_number(kind=I4B)
  IF (n .NE. 0) THEN
    ALLOCATE (IntVec1(n))
    DO i = 1, n
      IntVec1(i) = entries(8 + i)%to_number(kind=I4B)
    END DO
  END IF
    !! check total length here
  m = entries(9 + n)%to_number(kind=I4B)
  IF (m .NE. 0) THEN
    ALLOCATE (IntVec2(m))
    DO i = 1, m
      IntVec2(i) = entries(9 + n + i)%to_number(kind=I4B)
    END DO
  END IF

  IF (ALLOCATED(obj%physicalTag)) DEALLOCATE (obj%physicalTag)
  IF (ALLOCATED(obj%BoundingEntity)) DEALLOCATE (obj%BoundingEntity)
  !
  IF (n .NE. 0) THEN
    ALLOCATE (obj%physicalTag(n))
    obj%physicalTag(1:n) = Intvec1(1:n)
  END IF
  !
  IF (m .NE. 0) THEN
    ALLOCATE (obj%BoundingEntity(m))
    obj%BoundingEntity(1:m) = Intvec2(1:m)
  END IF
END IF

IF (ALLOCATED(IntVec1)) DEALLOCATE (IntVec1)
IF (ALLOCATED(IntVec2)) DEALLOCATE (IntVec2)
IF (ALLOCATED(entries)) DEALLOCATE (Entries)

END PROCEDURE ReadVolumeEntity

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getIndex
! Define internal variables
INTEGER(I4B) :: j, tSize
ans = 0
tSize = SIZE(mshEntities)
DO j = 1, tSize
  IF (mshEntities(j)%UiD .EQ. UiD) THEN
    ans = j
    EXIT
  END IF
END DO
END PROCEDURE ent_getIndex

!----------------------------------------------------------------------------
!                                                      getTotalPhysicalTags
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getTotalPhysicalTags
IF (ALLOCATED(obj%physicalTag)) THEN
  ans = SIZE(obj%physicalTag)
ELSE
  ans = 0
END IF
END PROCEDURE ent_getTotalPhysicalTags

!----------------------------------------------------------------------------
!                                                      getTotalBoundingTags
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getTotalBoundingTags
IF (ALLOCATED(obj%BoundingEntity)) THEN
  ans = SIZE(obj%BoundingEntity)
ELSE
  ans = 0
END IF
END PROCEDURE ent_getTotalBoundingTags

!----------------------------------------------------------------------------
!                                                           getTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getTotalElements
IF (ALLOCATED(obj%ElemNumber)) THEN
  ans = SIZE(obj%ElemNumber)
ELSE
  ans = 0
END IF
END PROCEDURE ent_getTotalElements

!----------------------------------------------------------------------------
!                                                           getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getTotalIntNodes
IF (ALLOCATED(obj%IntNodeNumber)) THEN
  ans = SIZE(obj%IntNodeNumber)
ELSE
  ans = 0
END IF
END PROCEDURE ent_getTotalIntNodes

!----------------------------------------------------------------------------
!                                                           getPhysicalTag
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getPhysicalTag
IF (ALLOCATED(obj%physicalTag)) THEN
  ans = obj%physicalTag
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE ent_getPhysicalTag

!----------------------------------------------------------------------------
!                                                            setIntNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_setIntNodeNumber
obj%IntNodeNumber = IntNodeNumber
END PROCEDURE ent_setIntNodeNumber

!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_setNodeCoord
obj%NodeCoord = NodeCoord
END PROCEDURE ent_setNodeCoord

!----------------------------------------------------------------------------
!                                                                setElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_setElemType
obj%ElemType = ElemType
END PROCEDURE ent_setElemType

!----------------------------------------------------------------------------
!                                                             setElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_setElemNumber
obj%ElemNumber = ElemNumber
END PROCEDURE ent_setElemNumber

!----------------------------------------------------------------------------
!                                                            setConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_setConnectivity
obj%Connectivity = Connectivity
END PROCEDURE ent_setConnectivity

!----------------------------------------------------------------------------
!                                                                 getUid
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getUid
ans = obj%Uid
END PROCEDURE ent_getUid

!----------------------------------------------------------------------------
!                                                                 getXiDim
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getXiDim
ans = obj%XiDim
END PROCEDURE ent_getXiDim

!----------------------------------------------------------------------------
!                                                               getElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getElemType
ans = obj%ElemType
END PROCEDURE ent_getElemType

!----------------------------------------------------------------------------
!                                                                 getMinX
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getMinX
ans = obj%MinX
END PROCEDURE ent_getMinX

!----------------------------------------------------------------------------
!                                                                 getMinY
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getMinY
ans = obj%MinY
END PROCEDURE ent_getMinY

!----------------------------------------------------------------------------
!                                                                 getMinZ
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getMinZ
ans = obj%MinZ
END PROCEDURE ent_getMinZ

!----------------------------------------------------------------------------
!                                                                 getMaxX
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getMaxX
ans = obj%MaxX
END PROCEDURE ent_getMaxX

!----------------------------------------------------------------------------
!                                                                 getMaxY
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getMaxY
ans = obj%MaxY
END PROCEDURE ent_getMaxY

!----------------------------------------------------------------------------
!                                                                 getMaxZ
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getMaxZ
ans = obj%MaxZ
END PROCEDURE ent_getMaxZ

!----------------------------------------------------------------------------
!                                                                 getX
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getX
ans = obj%X
END PROCEDURE ent_getX

!----------------------------------------------------------------------------
!                                                                 getY
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getY
ans = obj%Y
END PROCEDURE ent_getY

!----------------------------------------------------------------------------
!                                                                 getZ
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getZ
ans = obj%Z
END PROCEDURE ent_getZ

!----------------------------------------------------------------------------
!                                                               getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getNodeCoord
IF (ALLOCATED(obj%NodeCoord)) THEN
  ans = obj%NodeCoord
ELSE
  ALLOCATE (ans(0, 0))
END IF
END PROCEDURE ent_getNodeCoord

!----------------------------------------------------------------------------
!                                                          getIntNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getIntNodeNumber
IF (ALLOCATED(obj%IntNodeNumber)) THEN
  ans = Obj%IntNodeNumber
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE ent_getIntNodeNumber

!----------------------------------------------------------------------------
!                                                            getElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getElemNumber
IF (ALLOCATED(obj%ElemNumber)) THEN
  ans = Obj%ElemNumber
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE ent_getElemNumber

!----------------------------------------------------------------------------
!                                                         getBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getBoundingEntity
IF (ALLOCATED(obj%boundingEntity)) THEN
  ans = Obj%boundingEntity
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE ent_getBoundingEntity

!----------------------------------------------------------------------------
!                                                           getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getConnectivity_a
Ans = obj%Connectivity(:, elemNum)
END PROCEDURE ent_getConnectivity_a

!----------------------------------------------------------------------------
!                                                           getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getConnectivity_b
IF (ALLOCATED(obj%Connectivity)) THEN
  ans = Obj%Connectivity
ELSE
  ALLOCATE (ans(0, 0))
END IF
END PROCEDURE ent_getConnectivity_b

END SUBMODULE Methods
