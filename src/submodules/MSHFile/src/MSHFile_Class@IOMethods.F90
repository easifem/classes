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

SUBMODULE(MSHFile_Class) IOMethods
USE BaseMethod
USE mshEntity_Class, ONLY: GetIndex
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Import
CHARACTER(*), PARAMETER :: myName = "msh_Import"
CALL e%RaiseError(modName//'::'//myName//" - "// &
                  '[WIP ERROR] :: This routine is under condtruction')
END PROCEDURE msh_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Export_hdf5
CHARACTER(*), PARAMETER :: myName = "msh_Export_hdf5"
INTEGER(I4B) :: ii, tsize, tNodes, count_
REAL(DFP), ALLOCATABLE :: nodeCoord(:, :)
INTEGER(I4B), ALLOCATABLE :: local_nptrs(:)
TYPE(String) :: dSetname

dSetname = TRIM(group)

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file does not have write permission')
END IF

tNodes = obj%nodes%getNumNodes()
ALLOCATE (nodeCoord(3, tNodes))
ALLOCATE (local_nptrs(obj%Nodes%getMaxNodeTag()))
count_ = 0
local_nptrs = 0

CALL hdf5%WRITE(dSetname=dSetname%chars()//"/NSD", vals=obj%nsd)
CALL ExportMeshFormat(obj, hdf5, dSetname)

IF (obj%physicalNames%isInitiated) &
  CALL ExportMeshPhysicalNames(obj, hdf5, dSetname)

CALL ExportMeshNodeInfo(obj, hdf5, dSetname)

CALL ExportMeshElementInfo(obj, hdf5, dSetname)

IF (ALLOCATED(obj%pointEntities)) THEN
  tsize = SIZE(obj%pointEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dSetname=dSetname%chars()// &
                "/numPointEntities", vals=tsize)

DO ii = 1, tsize
  CALL ExportMeshEntity(obj%pointEntities(ii), hdf5, &
                        dSetname=dSetname%chars()//"/pointEntities_"// &
                        tostring(ii), nsd=obj%nsd)

  CALL getNodeCoord(obj=obj%pointEntities(ii), nodeCoord=nodeCoord, &
                    local_nptrs=local_nptrs, count_=count_)
END DO

IF (ALLOCATED(obj%curveEntities)) THEN
  tsize = SIZE(obj%curveEntities)
ELSE
  tsize = 0
END IF

CALL hdf5%WRITE(dSetname=dSetname%chars()// &
                "/numCurveEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%curveEntities(ii), hdf5, &
    & dSetname=dSetname%chars()//"/curveEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%curveEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
IF (ALLOCATED(obj%surfaceEntities)) THEN
  tsize = SIZE(obj%surfaceEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dSetname=dSetname%chars()// &
  & "/numSurfaceEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%surfaceEntities(ii), hdf5, &
    & dSetname=dSetname%chars()//"/surfaceEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%surfaceEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
IF (ALLOCATED(obj%volumeEntities)) THEN
  tsize = SIZE(obj%volumeEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dSetname=dSetname%chars()// &
  & "/numVolumeEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%volumeEntities(ii), hdf5, &
    & dSetname=dSetname%chars()//"/volumeEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%volumeEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
CALL hdf5%WRITE(dSetname=dSetname%chars()//"/nodeCoord", &
  &vals=nodeCoord)
CALL hdf5%WRITE(dSetname=dSetname%chars()//"/local_nptrs", &
  &vals=local_nptrs)
IF (ALLOCATED(nodeCoord)) DEALLOCATE (nodeCoord)
END PROCEDURE msh_Export_hdf5

!----------------------------------------------------------------------------
!                                                          ExportMeshFormat
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshFormat(obj, hdf5, dSetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dSetname
  ! Writing mesh format
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/version", &
    & vals=obj%FORMAT%getVersion())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/majorVersion", &
    & vals=obj%FORMAT%getMajorVersion())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/minorVersion", &
    & vals=obj%FORMAT%getMinorVersion())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/engine", &
    & vals=string("GMSH 4.1 0 8"))
END SUBROUTINE ExportMeshFormat

!----------------------------------------------------------------------------
!                                                    ExportMeshPhysicalNames
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshPhysicalNames(obj, hdf5, dSetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dSetname

  ! Internal variables
  INTEGER(I4B) :: ii, tsize

  ! Physical Names
  tsize = obj%physicalNames%getTotalPhysicalEntities()
  CALL hdf5%WRITE(dSetname=dSetname%chars()// &
    & "/PhysicalNames/totalPhysicalEntities", &
    & vals=tsize)

  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/PhysicalNames/NSD", &
    & vals=obj%physicalNames%getNSD())

  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/PhysicalNames/tag", &
    & vals=obj%physicalNames%getPhysicalTags())

  CALL hdf5%WRITE(dSetname=dSetname%chars()// &
    & "/PhysicalNames/numElements", &
    & vals=obj%physicalNames%getNumElements())

  CALL hdf5%WRITE(dSetname=dSetname%chars()// &
    & "/PhysicalNames/numNodes", &
    & vals=obj%physicalNames%getNumNodes())

  CALL hdf5%WRITE(dSetname=dSetname%chars()// &
    & "/PhysicalNames/physicalName", &
    & vals=obj%physicalNames%getPhysicalNames())

  DO ii = 1, tsize
    CALL hdf5%WRITE(dSetname=dSetname%chars()// &
    & "/PhysicalNames/entities_" &
    & //TRIM(str(ii, .TRUE.)), &
    & vals=obj%physicalNames%getEntities(indx=ii))
  END DO
END SUBROUTINE ExportMeshPhysicalNames

!----------------------------------------------------------------------------
!                                                     ExportMeshNodeInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshNodeInfo(obj, hdf5, dSetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dSetname
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/tNodes", &
    & vals=obj%Nodes%getNumNodes())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/tEntitiesForNodes", &
    & vals=obj%Nodes%getnumEntityBlocks())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/minNptrs", &
    & vals=obj%Nodes%getMinNodeTag())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/maxNptrs", &
    & vals=obj%Nodes%getMaxNodeTag())
END SUBROUTINE ExportMeshNodeInfo

!----------------------------------------------------------------------------
!                                                     ExportMeshElementInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshElementInfo(obj, hdf5, dSetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dSetname
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/tElements", &
    & vals=obj%Elements%getNumElements())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/tEntitiesForElements", &
    & vals=obj%Elements%getnumEntityBlocks())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/minElemNum", &
    & vals=obj%Elements%getMinElementTag())
  CALL hdf5%WRITE(dSetname=dSetname%chars()//"/maxElemNum", &
    & vals=obj%Elements%getMaxElementTag())
END SUBROUTINE ExportMeshElementInfo

!----------------------------------------------------------------------------
!                                                          ExportMeshEntity
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshEntity(obj, hdf5, dSetname, nsd)
  TYPE(mshEntity_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: dSetname
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), ALLOCATABLE :: intvec(:)
  REAL(DFP), ALLOCATABLE :: realvec(:), realMat(:, :)

  CALL hdf5%WRITE(TRIM(dSetname)//"/uid", obj%getUid())
  CALL hdf5%WRITE(TRIM(dSetname)//"/xidim", obj%getXidim())
  CALL hdf5%WRITE(TRIM(dSetname)//"/elemType", obj%getElemType())
  CALL hdf5%WRITE(TRIM(dSetname)//"/nsd", nsd)
  CALL hdf5%WRITE(TRIM(dSetname)//"/minX", obj%getMinX())
  CALL hdf5%WRITE(TRIM(dSetname)//"/minY", obj%getMinY())
  CALL hdf5%WRITE(TRIM(dSetname)//"/minZ", obj%getMinZ())
  CALL hdf5%WRITE(TRIM(dSetname)//"/maxX", obj%getMaxX())
  CALL hdf5%WRITE(TRIM(dSetname)//"/maxY", obj%getMaxY())
  CALL hdf5%WRITE(TRIM(dSetname)//"/maxZ", obj%getMaxZ())
  CALL hdf5%WRITE(TRIM(dSetname)//"/x", obj%getX())
  CALL hdf5%WRITE(TRIM(dSetname)//"/y", obj%getY())
  CALL hdf5%WRITE(TRIM(dSetname)//"/z", obj%getZ())

  CALL hdf5%WRITE(TRIM(dSetname)//"/tElements", obj%getTotalElements())
  CALL hdf5%WRITE(TRIM(dSetname)//"/tIntNodes", obj%getTotalIntNodes())

  CALL hdf5%WRITE(TRIM(dSetname)//"/physicalTag", obj%getPhysicalTag())
  CALL hdf5%WRITE(TRIM(dSetname)//"/intNodeNumber", obj%getIntNodeNumber())
  CALL hdf5%WRITE(TRIM(dSetname)//"/elemNumber", obj%getElemNumber())
  CALL hdf5%WRITE(TRIM(dSetname)//"/connectivity", obj%getConnectivity())

  intvec = obj%getBoundingEntity()
  IF (SIZE(intvec) .NE. 0) THEN
    CALL hdf5%WRITE(TRIM(dSetname)//"/boundingEntity", &
    & intvec)
  END IF
END SUBROUTINE ExportMeshEntity

!----------------------------------------------------------------------------
!                                                           ExportNodeCoord
!----------------------------------------------------------------------------

SUBROUTINE getNodeCoord(obj, nodeCoord, local_nptrs, count_)
  TYPE(mshEntity_), INTENT(IN) :: obj
  REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
  INTEGER(I4B), INTENT(INOUT) :: local_nptrs(:)
  INTEGER(I4B), INTENT(INOUT) :: count_
  ! internal data
  REAL(DFP), ALLOCATABLE :: myNodeCoord(:, :)
  INTEGER(I4B), ALLOCATABLE :: mynptrs(:)
  INTEGER(I4B) :: ii
  myNodeCoord = obj%getNodeCoord()
  mynptrs = obj%getIntNodeNumber()
  DO ii = 1, SIZE(mynptrs)
    count_ = count_ + 1
    local_nptrs(mynptrs(ii)) = count_
    nodeCoord(:, count_) = myNodeCoord(:, ii)
  END DO
  IF (ALLOCATED(myNodeCoord)) DEALLOCATE (myNodeCoord)
  IF (ALLOCATED(mynptrs)) DEALLOCATE (mynptrs)
END SUBROUTINE getNodeCoord

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Read
! Define internal variables
CHARACTER(*), PARAMETER :: myName = "msh_Read"
INTEGER(I4B) :: unitNo, tp, tc, ts, tv, error0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (.NOT. obj%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'MSH File is not open, please open it first.')
  RETURN
END IF

! check
IF (.NOT. obj%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'MSH File does not have read permission.')
  RETURN
END IF
!
unitNo = obj%getunitNo()

CALL obj%FORMAT%READ(mshFile=obj, error=error0)

IF (error0 .NE. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Failed in Reading mesh format')
  RETURN
END IF

! reading physical group information
CALL obj%PhysicalNames%READ(mshFile=obj, error=error0)

#ifdef DEBUG_VER
IF (obj%PhysicalNames%isInitiated) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'READING: physicalNames [OK!]')
ELSE
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'READING: physicalNames [NOT FOUND!]')
END IF
#endif

! Entities

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'LOCATING: $Entities')
#endif

CALL TypemshEntity%GotoTag(mshFile=obj, error=error0)

IF (error0 .NE. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'LOCATING: $Entities [NOT FOUND!]')
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'LOCATING: $Entities [OK!]')

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'READING: $Entities')
#endif

READ (unitNo, *) tp, tc, ts, tv
IF (tp .NE. 0) obj%nsd = 0
IF (tc .NE. 0) obj%nsd = 1
IF (ts .NE. 0) obj%nsd = 2
IF (tv .NE. 0) obj%nsd = 3
IF (tp .NE. 0) THEN
  CALL obj%ReadPointEntities(te=tp)
END IF

IF (tc .NE. 0) THEN
  CALL obj%ReadCurveEntities(te=tc)
END IF

IF (ts .NE. 0) THEN
  CALL obj%ReadSurfaceEntities(te=ts)
END IF

IF (tv .NE. 0) THEN
  CALL obj%ReadVolumeEntities(te=tv)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'READING: $Entities [OK!]')
#endif

!> Nodes
CALL obj%ReadNodes()
!> Elements

CALL obj%ReadElements()
!> nodes in physical regions
CALL SetNumNodesInPhysicalNames(obj)
IF (PRESENT(error)) error = error0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE msh_Read

!----------------------------------------------------------------------------
!                                                        ReadPointEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadPointEntities
CHARACTER(*), PARAMETER :: myName = "msh_ReadPointEntities()"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

dim = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - ' &
                        //'Total Point Entities: '//TRIM(str(te, .TRUE.)))
#endif

IF (ALLOCATED(obj%PointEntities)) DEALLOCATE (obj%PointEntities)
IF (te .NE. 0) ALLOCATE (obj%PointEntities(te))

DO i = 1, te
  CALL obj%PointEntities(i)%READ(mshFile=obj, dim=dim, &
                                 readTag=.FALSE., error=error)
  ! get total physical tag
  tpt = obj%PointEntities(i)%getTotalPhysicalTags()
  IF (tpt .NE. 0) THEN
    ! get physical tag int vector
    PhysicalTag0 = obj%PointEntities(i)%getPhysicalTag()
    DO j = 1, tpt
      ! get index of physical tag
      k = obj%PhysicalNames%getIndex(dim=dim, tag=PhysicalTag0(j))
      ! append this index to entities
      CALL obj%PhysicalNames%AppendEntities(indx=k, EntityTag=[i])
    END DO
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE msh_ReadPointEntities

!----------------------------------------------------------------------------
!                                                         ReadCurveEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadCurveEntities
CHARACTER(*), PARAMETER :: myName = "msh_ReadCurveEntities()"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ReadCurveEntities()')
#endif
dim = 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - ' &
  & //'Total Curve Entities: '//TRIM(str(te, .TRUE.)))
#endif

IF (ALLOCATED(obj%CurveEntities)) DEALLOCATE (obj%CurveEntities)
IF (te .NE. 0) ALLOCATE (obj%CurveEntities(te))

DO i = 1, te
  CALL obj%CurveEntities(i)%READ(mshFile=obj, dim=dim, &
    & readTag=.FALSE., error=error)
  ! get total physical tag
  tpt = obj%CurveEntities(i)%getTotalPhysicalTags()
  IF (tpt .NE. 0) THEN
    ! get physical tag int vector
    PhysicalTag0 = obj%CurveEntities(i)%getPhysicalTag()
    DO j = 1, tpt
      ! get index of physical tag
      k = obj%PhysicalNames%getIndex(dim=dim, tag=PhysicalTag0(j))
      ! append this index to entities
      CALL obj%PhysicalNames%AppendEntities(indx=k, EntityTag=[i])
    END DO
  END IF
END DO

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ReadCurveEntities()')
END PROCEDURE msh_ReadCurveEntities

!----------------------------------------------------------------------------
!                                                        ReadSurfaceEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadSurfaceEntities
CHARACTER(*), PARAMETER :: myName = "msh_ReadSurfaceEntities"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START]: ReadSurfaceEntities()')
#endif

dim = 2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - ' &
  & //'Total Surface Entities: '//TRIM(str(te, .TRUE.)))
#endif

IF (ALLOCATED(obj%SurfaceEntities)) DEALLOCATE (obj%SurfaceEntities)
IF (te .NE. 0) ALLOCATE (obj%SurfaceEntities(te))

DO i = 1, te
  CALL obj%SurfaceEntities(i)%READ(mshFile=obj, dim=dim, &
    & readTag=.FALSE., error=error)
  ! get total physical tag
  tpt = obj%SurfaceEntities(i)%getTotalPhysicalTags()
  IF (tpt .NE. 0) THEN
    ! get physical tag int vector
    PhysicalTag0 = obj%SurfaceEntities(i)%getPhysicalTag()
    DO j = 1, tpt
      ! get index of physical tag
      k = obj%PhysicalNames%getIndex(dim=dim, tag=PhysicalTag0(j))
      ! append this index to entities
      CALL obj%PhysicalNames%AppendEntities(indx=k, EntityTag=[i])
    END DO
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]: ReadSurfaceEntities()')
#endif
END PROCEDURE msh_ReadSurfaceEntities

!----------------------------------------------------------------------------
!                                                        ReadVolumeEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadVolumeEntities
CHARACTER(*), PARAMETER :: myName = "ReadVolumeEntities"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
! main program

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ReadVolumeEntities()')
#endif

dim = 3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - ' &
  & //'Total Volume Entities: '//TRIM(str(te, .TRUE.)))
#endif

IF (ALLOCATED(obj%VolumeEntities)) DEALLOCATE (obj%VolumeEntities)
IF (te .NE. 0) ALLOCATE (obj%VolumeEntities(te))

DO i = 1, te
  CALL obj%VolumeEntities(i)%READ(mshFile=obj, dim=dim, &
    & readTag=.FALSE., error=error)
  ! get total physical tag
  tpt = obj%VolumeEntities(i)%getTotalPhysicalTags()
  IF (tpt .NE. 0) THEN
    ! get physical tag int vector
    PhysicalTag0 = obj%VolumeEntities(i)%getPhysicalTag()
    DO j = 1, tpt
      ! get index of physical tag
      k = obj%PhysicalNames%getIndex(dim=dim, tag=PhysicalTag0(j))
      ! append this index to entities
      CALL obj%PhysicalNames%AppendEntities(indx=k, EntityTag=[i])
    END DO
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ReadVolumeEntities()')
#endif
END PROCEDURE msh_ReadVolumeEntities

!----------------------------------------------------------------------------
!                                                                 ReadNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadNodes
!> internal variables
CHARACTER(*), PARAMETER :: myName = "ReadNodes"
INTEGER(I4B) :: i, j, k, l, entityDim, entityTag, parametric, &
  & numNodesInBlock, error
INTEGER(I4B), ALLOCATABLE :: NodeNumber(:)
REAL(DFP), ALLOCATABLE :: NodeCoord(:, :)
! main program

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ReadNodes()')
#endif

! we read first line of $Nodes block
CALL obj%Nodes%READ(mshFile=obj, mshFormat=obj%FORMAT, &
  & error=error)

IF (error .NE. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Error has occured in reading '//  &
    & 'the header of nodes.')
END IF

!start reading each entity block
DO i = 1, obj%Nodes%getnumEntityBlocks()

  ! read entity dimension and entity tag (uid)
  READ (obj%GetUnitNo(), *) entityDim, entityTag, parametric, numNodesInBlock
  CALL Reallocate(NodeNumber, numNodesInBlock)

  ! now we read node numbers in NodeNumber( : )
  DO k = 1, numNodesInBlock
    READ (obj%GetUnitNo(), *) NodeNumber(k)
  END DO
  CALL Reallocate(NodeCoord, [3, numNodesInBlock])

  ! now we read node coordinates
  DO k = 1, numNodesInBlock
    READ (obj%GetUnitNo(), *) (NodeCoord(l, k), l=1, 3)
  END DO

  !make case based on entity dimension
  SELECT CASE (entityDim)
  CASE (0)
    j = getIndex(obj%PointEntities, entityTag)

    IF (j .LE. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: error in getting index of surfaceEntities')
    END IF

    CALL obj%PointEntities(j)%SetIntNodeNumber(NodeNumber)
    CALL obj%PointEntities(j)%SetNodeCoord(NodeCoord)

  CASE (1)
    j = getIndex(obj%CurveEntities, entityTag)

    IF (j .LE. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: error in getting index of surfaceEntities')
    END IF

    CALL obj%CurveEntities(j)%SetIntNodeNumber(NodeNumber)
    CALL obj%CurveEntities(j)%SetNodeCoord(NodeCoord)
  CASE (2)
    j = getIndex(obj%SurfaceEntities, entityTag)

    IF (j .LE. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: error in getting index of surfaceEntities')
    END IF

    CALL obj%SurfaceEntities(j)%SetIntNodeNumber(NodeNumber)
    CALL obj%SurfaceEntities(j)%SetNodeCoord(NodeCoord)
  CASE (3)
    j = getIndex(obj%VolumeEntities, entityTag)

    IF (j .LE. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: error in getting index of surfaceEntities')
    END IF

    CALL obj%VolumeEntities(j)%SetIntNodeNumber(NodeNumber)
    CALL obj%VolumeEntities(j)%SetNodeCoord(NodeCoord)
  END SELECT

END DO

IF (ALLOCATED(NodeNumber)) DEALLOCATE (NodeNumber)
IF (ALLOCATED(NodeCoord)) DEALLOCATE (NodeCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ReadNodes()')
#endif

END PROCEDURE msh_ReadNodes

!----------------------------------------------------------------------------
!                                                               ReadElements
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadElements
! define internal variables
CHARACTER(*), PARAMETER :: myName = "msh_ReadElements"
INTEGER(I4B) :: i, j, k, l, entityDim, entityTag, elemType, &
  & numElementsInBlock, tNodes, tpt, unitNo, error
INTEGER(I4B), ALLOCATABLE :: ElemNumber(:), nptrs(:, :), PhyTag(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

unitNo = obj%getunitNo()

CALL obj%Elements%READ(mshFile=obj, mshFormat=obj%FORMAT, &
                       error=error)

IF (error .NE. 0) &
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'Error has occured in reading the header of elements.')
! start reading each entity block
DO i = 1, obj%Elements%getnumEntityBlocks()
  ! read entity dimension and entity tag (uid)
  READ (unitNo, *) entityDim, entityTag, elemType, numElementsInBlock
  ! get the total number of nodes in element
  tNodes = TotalNodesInElement(elemType)
  CALL Reallocate(ElemNumber, numElementsInBlock)
  CALL Reallocate(nptrs, [tNodes, numElementsInBlock])
  IF (tNodes .EQ. 0 .OR. numElementsInBlock .EQ. 0) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      'Error in reading elements, found tnodes = 0 or numElementsInBlock = 0')
  END IF
  ! now we read ElemNumber and nptrs
  DO k = 1, numElementsInBlock
    READ (unitNo, *) ElemNumber(k), (nptrs(l, k), l=1, tNodes)
  END DO
  ! make case based on entity dimension
  SELECT CASE (entityDim)
  CASE (0)
    j = getIndex(obj%PointEntities, entityTag)
    ! Set the element type
    CALL obj%PointEntities(j)%SetElemType(elemType)
    CALL obj%PointEntities(j)%SetElemNumber(ElemNumber)
    CALL obj%PointEntities(j)%SetConnectivity(nptrs)
    ! counting nodes in each physical group
    tpt = obj%PointEntities(j)%getTotalPhysicalTags()
    IF (tpt .NE. 0) THEN
      ! get the physical tag in nptrs
      PhyTag = obj%PointEntities(j)%getPhysicalTag()
      DO k = 1, tpt
        l = obj%PhysicalNames%getIndex(dim=0, tag=PhyTag(k))
        CALL obj%PhysicalNames%IncNumElements(indx=l, incr=numElementsInBlock)
      END DO
    END IF
  CASE (1)
    j = getIndex(obj%CurveEntities, entityTag)
    ! Set the element type
    CALL obj%CurveEntities(j)%SetElemType(ElemType)
    CALL obj%CurveEntities(j)%SetElemNumber(ElemNumber)
    CALL obj%CurveEntities(j)%SetConnectivity(nptrs)
    ! counting nodes in each physical group
    tpt = obj%CurveEntities(j)%getTotalPhysicalTags()
    IF (tpt .NE. 0) THEN
      ! get the physical tag in nptrs
      PhyTag = obj%CurveEntities(j)%getPhysicalTag()
      DO k = 1, tpt
        l = obj%PhysicalNames%getIndex(dim=1, tag=PhyTag(k))
        CALL obj%PhysicalNames%IncNumElements(indx=l, incr=numElementsInBlock)
      END DO
    END IF
  CASE (2)
    j = getIndex(obj%SurfaceEntities, entityTag)
    ! Set the element type
    CALL obj%SurfaceEntities(j)%SetElemType(ElemType)
    CALL obj%SurfaceEntities(j)%SetElemNumber(ElemNumber)
    CALL obj%SurfaceEntities(j)%SetConnectivity(nptrs)
    ! counting nodes in each physical group
    tpt = obj%SurfaceEntities(j)%getTotalPhysicalTags()
    IF (tpt .NE. 0) THEN
      ! get the physical tag in nptrs
      PhyTag = obj%SurfaceEntities(j)%getPhysicalTag()
      DO k = 1, tpt
        l = obj%PhysicalNames%getIndex(dim=2, tag=PhyTag(k))
        CALL obj%PhysicalNames%IncNumElements(indx=l, incr=numElementsInBlock)
      END DO
    END IF
  CASE (3)
    j = getIndex(obj%VolumeEntities, entityTag)
    ! Set the element type
    CALL obj%VolumeEntities(j)%SetElemType(ElemType)
    CALL obj%VolumeEntities(j)%SetElemNumber(ElemNumber)
    CALL obj%VolumeEntities(j)%SetConnectivity(nptrs)
    ! counting nodes in each physical group
    tpt = obj%VolumeEntities(j)%getTotalPhysicalTags()
    IF (tpt .NE. 0) THEN
      ! get the physical tag in nptrs
      PhyTag = obj%VolumeEntities(j)%getPhysicalTag()
      DO k = 1, tpt
        l = obj%PhysicalNames%getIndex(dim=3, tag=PhyTag(k))
        CALL obj%PhysicalNames%IncNumElements(indx=l, incr=numElementsInBlock)
      END DO
    END IF
  END SELECT
END DO
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(ElemNumber)) DEALLOCATE (ElemNumber)
IF (ALLOCATED(PhyTag)) DEALLOCATE (PhyTag)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]  ReadElements()')
#endif
END PROCEDURE msh_ReadElements

!----------------------------------------------------------------------------
!                                                               SetNumNodes
!----------------------------------------------------------------------------

SUBROUTINE SetNumNodesInPhysicalNames(obj)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "SetNumNodesInPhysicalNames()"
  INTEGER(I4B) :: tpt, i, j, k, tElements, dim
  INTEGER(I4B), ALLOCATABLE :: Indx(:), entIndx(:), nptrs(:), &
    & dummynptrs(:)
  !> main

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] SetNumNodesInPhysicalNames()')
#endif

  ALLOCATE (nptrs(obj%Nodes%getMaxNodeTag()))
  ! Points
  dim = 0
  tpt = obj%PhysicalNames%getTotalPhysicalEntities([dim])
  IF (tpt .NE. 0) THEN
    Indx = obj%PhysicalNames%getIndex(dim=dim)
    ! loop over all physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities(Indx(i))
      CALL obj%PhysicalNames%IncNumNodes(Indx=Indx(i), &
        & incr=SIZE(entIndx))
    END DO
  END IF

  ! Curve
  dim = 1
  tpt = obj%PhysicalNames%getTotalPhysicalEntities([dim])
  IF (tpt .NE. 0) THEN
    Indx = obj%PhysicalNames%getIndex(dim=dim)
    ! loop over physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities(Indx(i))
      nptrs = 0_I4B
      DO j = 1, SIZE(entIndx)
        tElements = obj%CurveEntities(entIndx(j))%getTotalElements()
        DO k = 1, tElements
          dummynptrs = obj%CurveEntities(entIndx(j))%getConnectivity(k)
          nptrs(dummynptrs) = dummynptrs
        END DO
      END DO

      ! count the nonzero nptrs
      CALL obj%PhysicalNames%SetNumNodes(indx=Indx(i),  &
        & numNode=COUNT(nptrs .NE. 0))
    END DO
  END IF

  ! Surface
  dim = 2
  tpt = obj%PhysicalNames%getTotalPhysicalEntities(dim=[dim])
  IF (tpt .NE. 0) THEN
    Indx = obj%PhysicalNames%getIndex(dim=dim)
    ! loop over physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities(indx=Indx(i))
      nptrs = 0_I4B
      DO j = 1, SIZE(entIndx)
        tElements = obj%SurfaceEntities(entIndx(j))%getTotalElements()
        DO k = 1, tElements
          dummynptrs = obj%SurfaceEntities(entIndx(j))%getConnectivity(k)
          nptrs(dummynptrs) = dummynptrs
        END DO
      END DO

      ! count the nonzero nptrs
      CALL obj%PhysicalNames%SetNumNodes(indx=Indx(i),  &
        & numNode=COUNT(nptrs .NE. 0))
    END DO
  END IF

  ! Volume
  dim = 3
  tpt = obj%PhysicalNames%getTotalPhysicalEntities(dim=[dim])
  IF (tpt .NE. 0) THEN
    Indx = obj%PhysicalNames%getIndex(dim=dim)
    ! loop over physical points
    DO i = 1, tpt
      ! get Entities associated
      entIndx = obj%PhysicalNames%getEntities(Indx(i))
      nptrs = 0_I4B
      DO j = 1, SIZE(entIndx)
        tElements = obj%VolumeEntities(entIndx(j))%getTotalElements()
        DO k = 1, tElements
          dummynptrs = obj%VolumeEntities(entIndx(j))%getConnectivity(k)
          nptrs(dummynptrs) = dummynptrs
        END DO
      END DO
      ! count the nonzero nptrs
      CALL obj%PhysicalNames%SetNumNodes(indx=Indx(i), &
        & numNode=COUNT(nptrs .NE. 0))
    END DO
  END IF
  ! add deallocate stmt
  IF (ALLOCATED(Indx)) DEALLOCATE (Indx)
  IF (ALLOCATED(entIndx)) DEALLOCATE (entIndx)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(Dummynptrs)) DEALLOCATE (Dummynptrs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] SetNumNodesInPhysicalNames()')
#endif
END SUBROUTINE SetNumNodesInPhysicalNames

! !----------------------------------------------------------------------------
! !                                                                Display
! !----------------------------------------------------------------------------

! MODULE PROCEDURE msh_display
!   ! Define internal variable
!   INTEGER( I4B ) :: I, j
!   ! output unit
!   IF( PRESENT( unitNo ) ) THEN
!     I = unitNo
!   ELSE
!     I = stdout
!   END IF
!   ! print the message
!   IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
!     WRITE( I, "(A)" ) TRIM( Msg )
!   END IF
!   ! Printiting the Gmsh Format
!   CALL BlankLines( unitNo = I, NOL = 1 )
!   CALL Display( obj%Format, "Mesh Format = ", I )

!   ! Printing the PhysicalNames
!   CALL BlankLines( unitNo = I, NOL = 1 )
!   CALL Display( obj%PhysicalNames, "Physical Names", I )

!   ! Printing the point entities
!   IF( ALLOCATED( obj%PointEntities ) ) THEN
!     CALL BlankLines( unitNo = I, NOL = 1 )

!     WRITE( I, "(A)" ) "Point Entities"
!     DO j = 1, SIZE( obj%PointEntities )
!       CALL Display( &
!         & obj%PointEntities( j ), &
!         & "PointEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing the Curve entities
!   IF( ALLOCATED( obj%CurveEntities ) ) THEN
!     CALL BlankLines( unitNo = I, NOL = 1 )
!     WRITE( I, "(A)" ) "Curve Entities"
!     DO j = 1, SIZE( obj%CurveEntities )
!       CALL Display( &
!         & obj%CurveEntities( j ), &
!         & "CurveEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing the Surface entities
!   IF ( ALLOCATED( obj%SurfaceEntities ) ) THEN
!     CALL BlankLines( unitNo = I, NOL = 1 )
!     WRITE( I, "(A)" ) "Surface Entities"
!     DO j = 1, SIZE( obj%SurfaceEntities )
!       CALL Display( &
!         & obj%SurfaceEntities( j ), &
!         & "SurfaceEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing the Volume entities
!   IF( ALLOCATED( obj%VolumeEntities ) ) THEN
!     CALL BlankLines( unitNo = I, NOL = 1 )
!     WRITE( I, "(A)" ) "Volume Entities"
!     DO j = 1, SIZE( obj%VolumeEntities )
!       CALL Display( &
!         & obj%VolumeEntities( j ), &
!         & "VolumeEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing nodes
!   CALL BlankLines( unitNo = I, NOL = 1 )
!   CALL Display( obj%Nodes, "Nodes", I )
!   ! Printing elements
!   CALL BlankLines( unitNo = I, NOL = 1 )
!   CALL Display( obj%Elements, "Elements", I )
! END PROCEDURE msh_display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Export_txtfile
! Define internal variables
CHARACTER(*), PARAMETER :: myName = "msh_Export_txtfile"
INTEGER(I4B) :: unitNo, tp, tc, ts, tv, error0
INTEGER(I4B) :: ii

IF (.NOT. afile%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'TxtFile_:: afile is not open.')
END IF

IF (.NOT. afile%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'TxtFile_:: afile does not have write permission.')
END IF

unitNo = afile%getunitNo()

! reading mesh format
CALL Display('Writing meshFormat', stdout)
CALL obj%FORMAT%WRITE(afile=afile)

! reading physical group information

CALL Display('Writing physicalNames', stdout)
CALL obj%PhysicalNames%WRITE(afile=afile)

! Entities

CALL Display('Writing Entities', stdout)

WRITE (unitNo, "(A)") "$Entities"
tp = 0; tc = 0; ts = 0; tv = 0
IF (ALLOCATED(obj%PointEntities)) THEN
  tp = SIZE(obj%PointEntities)
END IF

IF (ALLOCATED(obj%CurveEntities)) THEN
  tc = SIZE(obj%CurveEntities)
END IF

IF (ALLOCATED(obj%SurfaceEntities)) THEN
  ts = SIZE(obj%SurfaceEntities)
END IF

IF (ALLOCATED(obj%VolumeEntities)) THEN
  tv = SIZE(obj%VolumeEntities)
END IF

WRITE (unitNo, "(A)") tostring(tp)//" " &
& //tostring(tc)//" " &
& //tostring(ts)//" " &
& //tostring(tv)

DO ii = 1, tp
  CALL obj%PointEntities(ii)%WRITE( &
    & afile=afile, &
    & dim=0)
END DO

DO ii = 1, tc
  CALL obj%CurveEntities(ii)%WRITE( &
    & afile=afile, &
    & dim=1)
END DO

DO ii = 1, ts
  CALL obj%SurfaceEntities(ii)%WRITE( &
    & afile=afile, &
    & dim=2)
END DO

DO ii = 1, tv
  CALL obj%VolumeEntities(ii)%WRITE( &
    & afile=afile, &
    & dim=3)
END DO

WRITE (unitNo, "(A)") "$EndEntities"

! Nodes

CALL obj%ExportNodes(afile=afile)

! Elements

CALL obj%ExportElements(afile=afile)

! nodes in physical regions
! CALL SetNumNodesInPhysicalNames(obj)
! IF (PRESENT(error)) error = error0
END PROCEDURE msh_Export_txtfile

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ExportNodes
TYPE(String) :: astr
INTEGER(I4B) :: unitNo
INTEGER(I4B) :: ii
INTEGER(I4B) :: tp, tc, ts, tv

unitNo = afile%getunitNo()

WRITE (unitNo, "(A)") "$Nodes"
CALL obj%nodes%WRITE(afile=afile)

tp = 0; tc = 0; ts = 0; tv = 0
IF (ALLOCATED(obj%PointEntities)) THEN
  tp = SIZE(obj%PointEntities)
END IF

IF (ALLOCATED(obj%CurveEntities)) THEN
  tc = SIZE(obj%CurveEntities)
END IF

IF (ALLOCATED(obj%SurfaceEntities)) THEN
  ts = SIZE(obj%SurfaceEntities)
END IF

IF (ALLOCATED(obj%VolumeEntities)) THEN
  tv = SIZE(obj%VolumeEntities)
END IF

DO ii = 1, tp
  CALL obj%PointEntities(ii)%WriteNodeBlock(afile=afile, dim=0)
END DO

DO ii = 1, tc
  CALL obj%CurveEntities(ii)%WriteNodeBlock(afile=afile, dim=1)
END DO

DO ii = 1, ts
  CALL obj%SurfaceEntities(ii)%WriteNodeBlock(afile=afile, dim=2)
END DO

DO ii = 1, tv
  CALL obj%VolumeEntities(ii)%WriteNodeBlock(afile=afile, dim=3)
END DO

WRITE (unitNo, "(A)") "$EndNodes"

END PROCEDURE msh_ExportNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ExportElements
TYPE(String) :: astr
INTEGER(I4B) :: unitNo
INTEGER(I4B) :: ii
INTEGER(I4B) :: tp, tc, ts, tv

unitNo = afile%getunitNo()

WRITE (unitNo, "(A)") "$Elements"
CALL obj%elements%WRITE(afile=afile)

tp = 0; tc = 0; ts = 0; tv = 0
IF (ALLOCATED(obj%PointEntities)) THEN
  tp = SIZE(obj%PointEntities)
END IF

IF (ALLOCATED(obj%CurveEntities)) THEN
  tc = SIZE(obj%CurveEntities)
END IF

IF (ALLOCATED(obj%SurfaceEntities)) THEN
  ts = SIZE(obj%SurfaceEntities)
END IF

IF (ALLOCATED(obj%VolumeEntities)) THEN
  tv = SIZE(obj%VolumeEntities)
END IF

DO ii = 1, tp
  CALL obj%PointEntities(ii)%WriteElementBlock(afile=afile, dim=0)
END DO

DO ii = 1, tc
  CALL obj%CurveEntities(ii)%WriteElementBlock(afile=afile, dim=1)
END DO

DO ii = 1, ts
  CALL obj%SurfaceEntities(ii)%WriteElementBlock(afile=afile, dim=2)
END DO

DO ii = 1, tv
  CALL obj%VolumeEntities(ii)%WriteElementBlock(afile=afile, dim=3)
END DO

WRITE (unitNo, "(A)") "$EndElements"

END PROCEDURE msh_ExportElements

END SUBMODULE IOMethods
