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
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is under condtruction')
END PROCEDURE msh_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Export_hdf5
CHARACTER(*), PARAMETER :: myName = "msh_Export_hdf5"
INTEGER(I4B) :: ii, tsize, tNodes, count_
REAL(DFP), ALLOCATABLE :: nodeCoord(:, :)
INTEGER(I4B), ALLOCATABLE :: local_nptrs(:)
TYPE(String) :: dsetname
!> main
dsetname = TRIM(group)
!>check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
END IF
!>check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have write permission')
END IF

tNodes = obj%nodes%getNumNodes()
ALLOCATE (nodeCoord(3, tNodes))
ALLOCATE (local_nptrs(obj%Nodes%getMaxNodeTag()))
count_ = 0
local_nptrs = 0

CALL hdf5%WRITE(dsetname=dsetname%chars()//"/NSD", vals=obj%nsd)
CALL ExportMeshFormat(obj, hdf5, dsetname)
IF (obj%physicalNames%isInitiated) &
  & CALL ExportMeshPhysicalNames(obj, hdf5, dsetname)
CALL ExportMeshNodeInfo(obj, hdf5, dsetname)
CALL ExportMeshElementInfo(obj, hdf5, dsetname)
IF (ALLOCATED(obj%pointEntities)) THEN
  tsize = SIZE(obj%pointEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dsetname=dsetname%chars()// &
  & "/numPointEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%pointEntities(ii), hdf5, &
    & dsetname=dsetname%chars()//"/pointEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%pointEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
IF (ALLOCATED(obj%curveEntities)) THEN
  tsize = SIZE(obj%curveEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dsetname=dsetname%chars()// &
  & "/numCurveEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%curveEntities(ii), hdf5, &
    & dsetname=dsetname%chars()//"/curveEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%curveEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
IF (ALLOCATED(obj%surfaceEntities)) THEN
  tsize = SIZE(obj%surfaceEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dsetname=dsetname%chars()// &
  & "/numSurfaceEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%surfaceEntities(ii), hdf5, &
    & dsetname=dsetname%chars()//"/surfaceEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%surfaceEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
IF (ALLOCATED(obj%volumeEntities)) THEN
  tsize = SIZE(obj%volumeEntities)
ELSE
  tsize = 0
END IF
CALL hdf5%WRITE(dsetname=dsetname%chars()// &
  & "/numVolumeEntities", vals=tsize)
DO ii = 1, tsize
  CALL ExportMeshEntity(obj%volumeEntities(ii), hdf5, &
    & dsetname=dsetname%chars()//"/volumeEntities_"// &
    & TRIM(str(ii, .TRUE.)), nsd=obj%nsd)
  CALL getNodeCoord(obj=obj%volumeEntities(ii), nodeCoord=nodeCoord, &
    & local_nptrs=local_nptrs, count_=count_)
END DO
CALL hdf5%WRITE(dsetname=dsetname%chars()//"/nodeCoord", &
  &vals=nodeCoord)
CALL hdf5%WRITE(dsetname=dsetname%chars()//"/local_nptrs", &
  &vals=local_nptrs)
IF (ALLOCATED(nodeCoord)) DEALLOCATE (nodeCoord)
END PROCEDURE msh_Export_hdf5

!----------------------------------------------------------------------------
!                                                          ExportMeshFormat
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshFormat(obj, hdf5, dsetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dsetname
  ! Writing mesh format
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/version", &
    & vals=obj%FORMAT%getVersion())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/majorVersion", &
    & vals=obj%FORMAT%getMajorVersion())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/minorVersion", &
    & vals=obj%FORMAT%getMinorVersion())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/engine", &
    & vals=string("GMSH 4.1 0 8"))
END SUBROUTINE ExportMeshFormat

!----------------------------------------------------------------------------
!                                                    ExportMeshPhysicalNames
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshPhysicalNames(obj, hdf5, dsetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dsetname

  ! Internal variables
  INTEGER(I4B) :: ii, tsize

  ! Physical Names
  tsize = obj%physicalNames%getTotalPhysicalEntities()
  CALL hdf5%WRITE(dsetname=dsetname%chars()// &
    & "/PhysicalNames/totalPhysicalEntities", &
    & vals=tsize)

  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/PhysicalNames/NSD", &
    & vals=obj%physicalNames%getNSD())

  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/PhysicalNames/tag", &
    & vals=obj%physicalNames%getPhysicalTags())

  CALL hdf5%WRITE(dsetname=dsetname%chars()// &
    & "/PhysicalNames/numElements", &
    & vals=obj%physicalNames%getNumElements())

  CALL hdf5%WRITE(dsetname=dsetname%chars()// &
    & "/PhysicalNames/numNodes", &
    & vals=obj%physicalNames%getNumNodes())

  CALL hdf5%WRITE(dsetname=dsetname%chars()// &
    & "/PhysicalNames/physicalName", &
    & vals=obj%physicalNames%getPhysicalNames())

  DO ii = 1, tsize
    CALL hdf5%WRITE(dsetname=dsetname%chars()// &
    & "/PhysicalNames/entities_" &
    & //TRIM(str(ii, .TRUE.)), &
    & vals=obj%physicalNames%getEntities(indx=ii))
  END DO
END SUBROUTINE ExportMeshPhysicalNames

!----------------------------------------------------------------------------
!                                                     ExportMeshNodeInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshNodeInfo(obj, hdf5, dsetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dsetname
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/tNodes", &
    & vals=obj%Nodes%getNumNodes())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/tEntitiesForNodes", &
    & vals=obj%Nodes%getnumEntityBlocks())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/minNptrs", &
    & vals=obj%Nodes%getMinNodeTag())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/maxNptrs", &
    & vals=obj%Nodes%getMaxNodeTag())
END SUBROUTINE ExportMeshNodeInfo

!----------------------------------------------------------------------------
!                                                     ExportMeshElementInfo
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshElementInfo(obj, hdf5, dsetname)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  TYPE(String), INTENT(IN) :: dsetname
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/tElements", &
    & vals=obj%Elements%getNumElements())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/tEntitiesForElements", &
    & vals=obj%Elements%getnumEntityBlocks())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/minElemNum", &
    & vals=obj%Elements%getMinElementTag())
  CALL hdf5%WRITE(dsetname=dsetname%chars()//"/maxElemNum", &
    & vals=obj%Elements%getMaxElementTag())
END SUBROUTINE ExportMeshElementInfo

!----------------------------------------------------------------------------
!                                                          ExportMeshEntity
!----------------------------------------------------------------------------

SUBROUTINE ExportMeshEntity(obj, hdf5, dsetname, nsd)
  TYPE(mshEntity_), INTENT(IN) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: dsetname
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), ALLOCATABLE :: intvec(:)
  REAL(DFP), ALLOCATABLE :: realvec(:), realMat(:, :)

  CALL hdf5%WRITE(TRIM(dsetname)//"/uid", obj%getUid())
  CALL hdf5%WRITE(TRIM(dsetname)//"/xidim", obj%getXidim())
  CALL hdf5%WRITE(TRIM(dsetname)//"/elemType", obj%getElemType())
  CALL hdf5%WRITE(TRIM(dsetname)//"/nsd", nsd)
  CALL hdf5%WRITE(TRIM(dsetname)//"/minX", obj%getMinX())
  CALL hdf5%WRITE(TRIM(dsetname)//"/minY", obj%getMinY())
  CALL hdf5%WRITE(TRIM(dsetname)//"/minZ", obj%getMinZ())
  CALL hdf5%WRITE(TRIM(dsetname)//"/maxX", obj%getMaxX())
  CALL hdf5%WRITE(TRIM(dsetname)//"/maxY", obj%getMaxY())
  CALL hdf5%WRITE(TRIM(dsetname)//"/maxZ", obj%getMaxZ())
  CALL hdf5%WRITE(TRIM(dsetname)//"/x", obj%getX())
  CALL hdf5%WRITE(TRIM(dsetname)//"/y", obj%getY())
  CALL hdf5%WRITE(TRIM(dsetname)//"/z", obj%getZ())

  CALL hdf5%WRITE(TRIM(dsetname)//"/tElements", obj%getTotalElements())
  CALL hdf5%WRITE(TRIM(dsetname)//"/tIntNodes", obj%getTotalIntNodes())

  CALL hdf5%WRITE(TRIM(dsetname)//"/physicalTag", obj%getPhysicalTag())
  CALL hdf5%WRITE(TRIM(dsetname)//"/intNodeNumber", obj%getIntNodeNumber())
  CALL hdf5%WRITE(TRIM(dsetname)//"/elemNumber", obj%getElemNumber())
  CALL hdf5%WRITE(TRIM(dsetname)//"/connectivity", obj%getConnectivity())

  intvec = obj%getBoundingEntity()
  IF (SIZE(intvec) .NE. 0) THEN
    CALL hdf5%WRITE(TRIM(dsetname)//"/boundingEntity", &
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
  INTEGER(I4B), ALLOCATABLE :: myNptrs(:)
  INTEGER(I4B) :: ii
  myNodeCoord = obj%getNodeCoord()
  myNptrs = obj%getIntNodeNumber()
  DO ii = 1, SIZE(myNptrs)
    count_ = count_ + 1
    local_nptrs(myNptrs(ii)) = count_
    nodeCoord(:, count_) = myNodeCoord(:, ii)
  END DO
  IF (ALLOCATED(myNodeCoord)) DEALLOCATE (myNodeCoord)
  IF (ALLOCATED(myNptrs)) DEALLOCATE (myNptrs)
END SUBROUTINE getNodeCoord

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Read
! Define internal variables
CHARACTER(*), PARAMETER :: myName = "msh_Read"
INTEGER(I4B) :: unitNo, tp, tc, ts, tv, error0
!> main
!> check
IF (.NOT. obj%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'MSH File is not open, please open it first.')
END IF
!> check
IF (.NOT. obj%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'MSH File does not have read permission.')
END IF
!>
unitNo = obj%getunitNo()
!> reading mesh format
CALL e%raiseDebug(modName//'::'//myName//' - '// &
  & 'READING: meshFormat')
CALL obj%FORMAT%READ(mshFile=obj, error=error0)
IF (error0 .NE. 0) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Failed in Reading mesh format')
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: meshFormat [OK!]')
END IF
!> reading physical group information
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: physicalNames')
CALL obj%PhysicalNames%READ(mshFile=obj, error=error0)
IF (obj%PhysicalNames%isInitiated) THEN
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: physicalNames [OK!]')
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: physicalNames [NOT FOUND!]')
END IF
!> Entities
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'LOCATING: $Entities')
CALL TypemshEntity%GotoTag(mshFile=obj, error=error0)
IF (error0 .NE. 0) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'LOCATING: $Entities [NOT FOUND!]')
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'LOCATING: $Entities [OK!]')
END IF
!> Entities
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: $Entities')
READ (unitNo, *) tp, tc, ts, tv
IF (tp .NE. 0) obj%nsd = 0
IF (tc .NE. 0) obj%nsd = 1
IF (ts .NE. 0) obj%nsd = 2
IF (tv .NE. 0) obj%nsd = 3
CALL obj%ReadPointEntities(te=tp)
CALL obj%ReadCurveEntities(te=tc)
CALL obj%ReadSurfaceEntities(te=ts)
CALL obj%ReadVolumeEntities(te=tv)
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: $Entities [OK!]')
!> Nodes
CALL obj%ReadNodes()
!> Elements
CALL obj%ReadElements()
!> nodes in physical regions
CALL setNumNodesInPhysicalNames(obj)
IF (PRESENT(error)) error = error0
END PROCEDURE msh_Read

!----------------------------------------------------------------------------
!                                                        ReadPointEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadPointEntities
INTEGER(I4B) :: unitNo
CHARACTER(*), PARAMETER :: myName = "ReadPointEntities"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program
dim = 0; unitNo = obj%getUnitNo()
CALL e%raiseInformation(modName//'::'//myName//' - ' &
  & //'Total Point Entities: '//TRIM(str(te, .TRUE.)))
IF (ALLOCATED(obj%PointEntities)) DEALLOCATE (obj%PointEntities)
IF (te .NE. 0) ALLOCATE (obj%PointEntities(te))
CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: PointEntities')
DO i = 1, te
  CALL obj%PointEntities(i)%READ(mshFile=obj, dim=dim, &
    & readTag=.FALSE., error=error)
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
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: pointEntities [OK!]')
END PROCEDURE msh_ReadPointEntities

!----------------------------------------------------------------------------
!                                                         ReadCurveEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadCurveEntities
INTEGER(I4B) :: unitNo
CHARACTER(*), PARAMETER :: myName = "ReadCurveEntities"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program
dim = 1; unitNo = obj%getUnitNo()
CALL e%raiseInformation(modName//'::'//myName//' - ' &
  & //'Total Curve Entities: '//TRIM(str(te, .TRUE.)))
IF (ALLOCATED(obj%CurveEntities)) DEALLOCATE (obj%CurveEntities)
IF (te .NE. 0) ALLOCATE (obj%CurveEntities(te))
CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: CurveEntities')
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
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: CurveEntities [OK!]')
END PROCEDURE msh_ReadCurveEntities

!----------------------------------------------------------------------------
!                                                        ReadSurfaceEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadSurfaceEntities
INTEGER(I4B) :: unitNo
CHARACTER(*), PARAMETER :: myName = "ReadSurfaceEntities"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program
dim = 2; unitNo = obj%getUnitNo()
CALL e%raiseInformation(modName//'::'//myName//' - ' &
  & //'Total Surface Entities: '//TRIM(str(te, .TRUE.)))
IF (ALLOCATED(obj%SurfaceEntities)) DEALLOCATE (obj%SurfaceEntities)
IF (te .NE. 0) ALLOCATE (obj%SurfaceEntities(te))
CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: SurfaceEntities')
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
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: SurfaceEntities [OK!]')
END PROCEDURE msh_ReadSurfaceEntities

!----------------------------------------------------------------------------
!                                                        ReadVolumeEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadVolumeEntities
INTEGER(I4B) :: unitNo
CHARACTER(*), PARAMETER :: myName = "ReadVolumeEntities"
INTEGER(I4B) :: i, j, k, tpt, error, dim
INTEGER(I4B), ALLOCATABLE :: PhysicalTag0(:)
!> main program
dim = 3; unitNo = obj%getUnitNo()
CALL e%raiseInformation(modName//'::'//myName//' - ' &
  & //'Total Volume Entities: '//TRIM(str(te, .TRUE.)))
IF (ALLOCATED(obj%VolumeEntities)) DEALLOCATE (obj%VolumeEntities)
IF (te .NE. 0) ALLOCATE (obj%VolumeEntities(te))
CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: VolumeEntities')
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
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: VolumeEntities [OK!]')
END PROCEDURE msh_ReadVolumeEntities

!----------------------------------------------------------------------------
!                                                                 ReadNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadNodes
!> internal variables
CHARACTER(*), PARAMETER :: myName = "ReadNodes"
INTEGER(I4B) :: i, j, k, l, entityDim, entityTag, parametric, &
  & numNodesInBlock, error, unitNo
INTEGER(I4B), ALLOCATABLE :: NodeNumber(:)
REAL(DFP), ALLOCATABLE :: NodeCoord(:, :)
!> main program
! we read first line of $Nodes block
CALL obj%Nodes%READ(mshFile=obj, mshFormat=obj%FORMAT, &
  & error=error)
IF (error .NE. 0) &
  & CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Error has occured in reading the header of nodes.')
unitNo = obj%getUnitNo()
!start reading each entity block
DO i = 1, obj%Nodes%getnumEntityBlocks()
  ! read entity dimension and entity tag (uid)
  READ (UnitNo, *) entityDim, entityTag, parametric, numNodesInBlock
  CALL Reallocate(NodeNumber, numNodesInBlock)
  ! now we read node numbers in NodeNumber( : )
  DO k = 1, numNodesInBlock; READ (UnitNo, *) NodeNumber(k); END DO
  CALL Reallocate(NodeCoord, [3, numNodesInBlock])
  ! now we read node coordinates
  DO k = 1, numNodesInBlock
    READ (UnitNo, *) (NodeCoord(l, k), l=1, 3)
  END DO
  !make case based on entity dimension
  SELECT CASE (entityDim)
  CASE (0)
    j = getIndex(obj%PointEntities, entityTag)
    CALL obj%PointEntities(j)%setIntNodeNumber(NodeNumber)
    CALL obj%PointEntities(j)%setNodeCoord(NodeCoord)
  CASE (1)
    j = getIndex(obj%CurveEntities, entityTag)
    CALL obj%CurveEntities(j)%setIntNodeNumber(NodeNumber)
    CALL obj%CurveEntities(j)%setNodeCoord(NodeCoord)
  CASE (2)
    j = getIndex(obj%SurfaceEntities, entityTag)
    CALL obj%SurfaceEntities(j)%setIntNodeNumber(NodeNumber)
    CALL obj%SurfaceEntities(j)%setNodeCoord(NodeCoord)
  CASE (3)
    j = getIndex(obj%VolumeEntities, entityTag)
    CALL obj%VolumeEntities(j)%setIntNodeNumber(NodeNumber)
    CALL obj%VolumeEntities(j)%setNodeCoord(NodeCoord)
  END SELECT
END DO
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: $Nodes [OK!]')
IF (ALLOCATED(NodeNumber)) DEALLOCATE (NodeNumber)
IF (ALLOCATED(NodeCoord)) DEALLOCATE (NodeCoord)
END PROCEDURE msh_ReadNodes

!----------------------------------------------------------------------------
!                                                               ReadElements
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_ReadElements
! define internal variables
CHARACTER(*), PARAMETER :: myName = "msh_ReadElements"
INTEGER(I4B) :: i, j, k, l, entityDim, entityTag, elemType, &
  & numElementsInBlock, tNodes, tpt, unitNo, error
INTEGER(I4B), ALLOCATABLE :: ElemNumber(:), Nptrs(:, :), PhyTag(:)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: $Elements')
unitNo = obj%getUnitNo()
CALL obj%Elements%READ(mshFile=obj, mshFormat=obj%FORMAT, &
  & error=error)
IF (error .NE. 0) &
  & CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Error has occured in reading the header of elements.')
! start reading each entity block
DO i = 1, obj%Elements%getnumEntityBlocks()
  ! read entity dimension and entity tag (uid)
  READ (UnitNo, *) entityDim, entityTag, elemType, numElementsInBlock
  ! get the total number of nodes in element
  tNodes = TotalNodesInElement(elemType)
  CALL Reallocate(ElemNumber, numElementsInBlock)
  CALL Reallocate(Nptrs, [tNodes, numElementsInBlock])
  ! now we read ElemNumber and Nptrs
  DO k = 1, numElementsInBlock
    READ (UnitNo, *) ElemNumber(k), (Nptrs(l, k), l=1, tNodes)
  END DO
  ! make case based on entity dimension
  SELECT CASE (entityDim)
  CASE (0)
    j = getIndex(obj%PointEntities, entityTag)
    ! set the element type
    CALL obj%PointEntities(j)%setElemType(elemType)
    CALL obj%PointEntities(j)%setElemNumber(ElemNumber)
    CALL obj%PointEntities(j)%setConnectivity(Nptrs)
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
    ! set the element type
    CALL obj%CurveEntities(j)%setElemType(ElemType)
    CALL obj%CurveEntities(j)%setElemNumber(ElemNumber)
    CALL obj%CurveEntities(j)%setConnectivity(Nptrs)
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
    ! set the element type
    CALL obj%SurfaceEntities(j)%setElemType(ElemType)
    CALL obj%SurfaceEntities(j)%setElemNumber(ElemNumber)
    CALL obj%SurfaceEntities(j)%setConnectivity(Nptrs)
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
    ! set the element type
    CALL obj%VolumeEntities(j)%setElemType(ElemType)
    CALL obj%VolumeEntities(j)%setElemNumber(ElemNumber)
    CALL obj%VolumeEntities(j)%setConnectivity(Nptrs)
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
IF (ALLOCATED(Nptrs)) DEALLOCATE (Nptrs)
IF (ALLOCATED(ElemNumber)) DEALLOCATE (ElemNumber)
IF (ALLOCATED(PhyTag)) DEALLOCATE (PhyTag)
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'READING: $Elements [OK!]')
END PROCEDURE msh_ReadElements

!----------------------------------------------------------------------------
!                                                               setNumNodes
!----------------------------------------------------------------------------

SUBROUTINE setNumNodesInPhysicalNames(obj)
  CLASS(MSHFile_), INTENT(INOUT) :: obj
  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "setNumNodesInPhysicalNames"
  INTEGER(I4B) :: tpt, i, j, k, tElements, dim
  INTEGER(I4B), ALLOCATABLE :: Indx(:), entIndx(:), Nptrs(:), &
    & dummyNptrs(:)
  !> main
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: Nodes in Physical entities')
  ALLOCATE (Nptrs(obj%Nodes%getMaxNodeTag()))
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
      Nptrs = 0_I4B
      DO j = 1, SIZE(entIndx)
        tElements = obj%CurveEntities(entIndx(j))%getTotalElements()
        DO k = 1, tElements
          dummyNptrs = obj%CurveEntities(entIndx(j))%getConnectivity(k)
          Nptrs(dummyNptrs) = dummyNptrs
        END DO
      END DO
      ! count the nonzero nptrs
 CALL obj%PhysicalNames%setNumNodes(indx=Indx(i), numNode=COUNT(Nptrs .NE. 0))
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
      Nptrs = 0_I4B
      DO j = 1, SIZE(entIndx)
        tElements = obj%SurfaceEntities(entIndx(j))%getTotalElements()
        DO k = 1, tElements
          dummyNptrs = obj%SurfaceEntities(entIndx(j))%getConnectivity(k)
          Nptrs(dummyNptrs) = dummyNptrs
        END DO
      END DO
      ! count the nonzero nptrs
 CALL obj%PhysicalNames%setNumNodes(indx=Indx(i), numNode=COUNT(Nptrs .NE. 0))
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
      Nptrs = 0_I4B
      DO j = 1, SIZE(entIndx)
        tElements = obj%VolumeEntities(entIndx(j))%getTotalElements()
        DO k = 1, tElements
          dummyNptrs = obj%VolumeEntities(entIndx(j))%getConnectivity(k)
          Nptrs(dummyNptrs) = dummyNptrs
        END DO
      END DO
      ! count the nonzero nptrs
      CALL obj%PhysicalNames%setNumNodes(indx=Indx(i), &
        & numNode=COUNT(Nptrs .NE. 0))
    END DO
  END IF
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING: Nodes in Physical entities [OK!]')
  ! add deallocate stmt
  IF (ALLOCATED(Indx)) DEALLOCATE (Indx)
  IF (ALLOCATED(entIndx)) DEALLOCATE (entIndx)
  IF (ALLOCATED(Nptrs)) DEALLOCATE (Nptrs)
  IF (ALLOCATED(DummyNptrs)) DEALLOCATE (DummyNptrs)
END SUBROUTINE setNumNodesInPhysicalNames

! !----------------------------------------------------------------------------
! !                                                                Display
! !----------------------------------------------------------------------------

! MODULE PROCEDURE msh_display
!   ! Define internal variable
!   INTEGER( I4B ) :: I, j
!   ! output unit
!   IF( PRESENT( UnitNo ) ) THEN
!     I = UnitNo
!   ELSE
!     I = stdout
!   END IF
!   ! print the message
!   IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
!     WRITE( I, "(A)" ) TRIM( Msg )
!   END IF
!   ! Printiting the Gmsh Format
!   CALL BlankLines( UnitNo = I, NOL = 1 )
!   CALL Display( obj%Format, "Mesh Format = ", I )

!   ! Printing the PhysicalNames
!   CALL BlankLines( UnitNo = I, NOL = 1 )
!   CALL Display( obj%PhysicalNames, "Physical Names", I )

!   ! Printing the point entities
!   IF( ALLOCATED( obj%PointEntities ) ) THEN
!     CALL BlankLines( UnitNo = I, NOL = 1 )

!     WRITE( I, "(A)" ) "Point Entities"
!     DO j = 1, SIZE( obj%PointEntities )
!       CALL Display( &
!         & obj%PointEntities( j ), &
!         & "PointEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing the Curve entities
!   IF( ALLOCATED( obj%CurveEntities ) ) THEN
!     CALL BlankLines( UnitNo = I, NOL = 1 )
!     WRITE( I, "(A)" ) "Curve Entities"
!     DO j = 1, SIZE( obj%CurveEntities )
!       CALL Display( &
!         & obj%CurveEntities( j ), &
!         & "CurveEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing the Surface entities
!   IF ( ALLOCATED( obj%SurfaceEntities ) ) THEN
!     CALL BlankLines( UnitNo = I, NOL = 1 )
!     WRITE( I, "(A)" ) "Surface Entities"
!     DO j = 1, SIZE( obj%SurfaceEntities )
!       CALL Display( &
!         & obj%SurfaceEntities( j ), &
!         & "SurfaceEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing the Volume entities
!   IF( ALLOCATED( obj%VolumeEntities ) ) THEN
!     CALL BlankLines( UnitNo = I, NOL = 1 )
!     WRITE( I, "(A)" ) "Volume Entities"
!     DO j = 1, SIZE( obj%VolumeEntities )
!       CALL Display( &
!         & obj%VolumeEntities( j ), &
!         & "VolumeEntities( "//TRIM( int2str( j ) )//" )", I )
!     END DO
!   END IF
!   ! Printing nodes
!   CALL BlankLines( UnitNo = I, NOL = 1 )
!   CALL Display( obj%Nodes, "Nodes", I )
!   ! Printing elements
!   CALL BlankLines( UnitNo = I, NOL = 1 )
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
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'TxtFile_:: afile is not open.')
END IF

IF (.NOT. afile%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
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
! CALL setNumNodesInPhysicalNames(obj)
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

unitNo = afile%getUnitNo()

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

unitNo = afile%getUnitNo()

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
