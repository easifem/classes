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

SUBMODULE( Mesh_Class ) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_display
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_display"
  CALL e%raiseError(modName//"::"//myName//" - "// &
      & "This routine has not been implemented yet.")
END PROCEDURE mesh_display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_Import"
  INTEGER( I4B ), ALLOCATABLE :: connectivity( :, : ), elemNumber( : ), &
    & InternalNptrs( : )
  TYPE( String ) :: dsetname
  INTEGER( I4B ) :: ii,  dummy, jj

  dsetname = trim(group)
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'Making dsetname')
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'dsetname = '//TRIM(dsetname) )
  !>check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'HDF5 file is not opened')
  END IF
  !>check
  IF( .NOT. hdf5%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'HDF5 file does not have read permission')
  END IF
  !>check
  IF( .NOT. hdf5%isGroup(TRIM(dsetname%chars())) ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // ' is not a group; it should be a group which contains the meshEntity' )
  END IF
  !>check
  IF( .NOT. hdf5%pathExists(TRIM(dsetname%chars())) ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // ' path does not exists' )
  END IF
  !> read Uid
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/uid") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/uid" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/uid", obj%Uid )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'uid = ' // TRIM(str(obj%Uid, .true.)) )
  END IF
  !> read xidim
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/xidim") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/xidim" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/xidim", obj%xidim )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'xidim = ' // TRIM(str(obj%xidim, .true.)) )
  END IF
  !>reading elemtype
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/elemType") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/elemType" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/elemType", obj%elemType )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'elemType = ' // TRIM(str(obj%elemType, .true.)) )
  END IF
  !>reading nsd
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/nsd") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/nsd" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/nsd", obj%nsd )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'nsd = ' // TRIM(str(obj%elemType, .true.)) )
  END IF
  !> reading tIntNodes
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/tIntNodes") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/tIntNodes" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/tIntNodes", &
      & obj%tIntNodes )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'tIntNodes = ' // TRIM(str(obj%tIntNodes, .true.)) )
  END IF
  !> reading tElements, allocate obj%elementData
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/tElements") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/tElements" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/tElements", &
      & obj%tElements )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'tElements = ' // TRIM(str(obj%tElements, .true.)) )
  END IF
  IF( ALLOCATED( obj%elementData ) ) DEALLOCATE( obj%elementData )
  IF( obj%tElements .NE. 0) &
    & ALLOCATE( obj%elementData( obj%tElements ) )
  !>reading minX
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/minX") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/minX" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/minX", obj%minX )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'minX = ' // TRIM(str(obj%minX, .true.)) )
  END IF
  !> reading minY
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/minY") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/minY" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/minY", obj%minY )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'minY = ' // TRIM(str(obj%minY, .true.)) )
  END IF
  !> reading minZ
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/minZ") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/minZ" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/minZ", obj%minZ )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'minZ = ' // TRIM(str(obj%minZ, .true.)) )
  END IF
  !> reading maxX
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/maxX") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/maxX" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/maxX", obj%maxX )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxX = ' // TRIM(str(obj%maxX, .true.)) )
  END IF
  !> reading maxY
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/maxY") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/maxY" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/maxY", obj%maxY )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxY = ' // TRIM(str(obj%maxY, .true.)) )
  END IF
  !> reading maxZ
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/maxZ") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/maxZ" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/maxZ", obj%minZ )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxZ = ' // TRIM(str(obj%maxZ, .true.)) )
  END IF
  !> reading x
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/x") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/x" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/x", obj%x )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'x = ' // TRIM(str(obj%x, .true.)) )
  END IF
  !> reading y
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/y") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/y" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/y", obj%y )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'y = ' // TRIM(str(obj%y, .true.)) )
  END IF
  !> reading z
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/z") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/z" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/z", obj%z )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'z = ' // TRIM(str(obj%z, .true.)) )
  END IF
  !> reading physicalTag
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading physicalTag' )
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/physicalTag") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/physicalTag" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/physicalTag", &
      & obj%physicalTag )
    IF( e%isLogActive() ) THEN
      CALL e%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( obj%physicalTag, "physicalTag = ", &
        & unitNo = e%getLogFileUnit() )
    END IF
  END IF
  !> Reading elemNumber, maxElemNum, minElemNum, local_elemNumber,
  !> elementData%globalElemNum, elementData%localElemNum
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading elemNumber' )
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/elemNumber") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/elemNumber" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/elemNumber", &
      & elemNumber )
    IF( e%isLogActive() ) THEN
      CALL e%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( elemNumber, "elemNumber = ", &
        & unitNo = e%getLogFileUnit() )
    END IF
    IF( ALLOCATED( elemNumber ) .AND. SIZE( elemNumber ) .NE. 0 ) THEN
      obj%maxElemNum=MAXVAL(elemNumber)
      obj%minElemNum=MINVAL(elemNumber)
    END IF
    CALL Reallocate( obj%local_elemNumber, obj%maxElemNum )
    DO CONCURRENT (ii = 1:obj%tElements)
      obj%local_elemNumber( elemNumber( ii ) ) = ii
      obj%elementData(ii)%globalElemNum = elemNumber( ii )
      obj%elementData(ii)%localElemNum = ii
    END DO
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'maxElemNum = ' // trim(str(obj%maxElemNum, .TRUE. )) )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'minElemNum = ' // trim(str(obj%minElemNum, .TRUE. )) )
  END IF
  !> reading connectivity, maxNptrs, minNptrs, tNodes
  !> elementData%globalNodes, local_nptrs,
  !> nodeData%globalNodenum,  nodeData%localNodenum
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading connectivity' )
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/connectivity") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/connectivity" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/connectivity", connectivity )
    IF( obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0 ) THEN
      obj%tNodes = 1
      IF( ALLOCATED( obj%nodeData ) ) DEALLOCATE( obj%nodeData )
      ALLOCATE( obj%nodeData( obj%tNodes ) )
      obj%nodeData( 1 )%globalNodeNum = 1
      obj%nodeData( 1 )%localNodeNum = 1
      obj%nodeData( 1 )%nodeType = BOUNDARY_NODE
    ELSE
      obj%maxNptrs=MAXVAL(connectivity)
      obj%minNptrs=MINVAL(connectivity)
      CALL Reallocate( obj%Local_Nptrs, obj%maxNptrs )
      DO CONCURRENT(ii = 1:obj%tElements)
        obj%elementData(ii)%globalNodes=connectivity(:,ii)
        obj%Local_Nptrs( connectivity(:,ii) ) = connectivity(:,ii)
      END DO
      obj%tNodes = COUNT( obj%Local_Nptrs .NE. 0 )
      IF( ALLOCATED( obj%nodeData ) ) DEALLOCATE( obj%nodeData )
      ALLOCATE( obj%nodeData( obj%tNodes ) )
      dummy = 0
      DO ii = 1, obj%maxNptrs
        IF( obj%Local_Nptrs( ii ) .NE. 0 ) THEN
          dummy = dummy + 1
          obj%nodeData( dummy )%globalNodeNum = obj%Local_Nptrs( ii )
          obj%nodeData( dummy )%localNodeNum = dummy
          obj%nodeData( dummy )%nodeType = BOUNDARY_NODE
            !! The above step is unusual, but we know the position of
            !! internal nptrs, so later we will set the
            !! those nodes as INTERNAL_NODE, in this way we can
            !! identify the boundary nodes
          obj%Local_Nptrs( ii ) = dummy
        END IF
      END DO
    END IF
    IF( e%isLogActive() ) THEN
      CALL e%raiseInformation(modName//'::'//myName// " - "// &
        & 'connectivity(1:NNS, 1:tElements) is given below = ' )
      DO ii = 1, SIZE( connectivity, 1)
        CALL DISP( title="Node-"//trim(str(ii, .true.)), &
          & x=connectivity( ii, : ), &
          & unit = e%getLogFileUnit(), style="UNDERLINE", &
          & advance="NO" )
      END DO
      CALL DISP( title='', x='', unit = e%getLogFileUnit(), &
        & advance="DOUBLE" )
    END IF
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
        & 'maxNptrs = ' // trim(str(obj%maxNptrs, .TRUE. )) )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
        & 'minNptrs = ' // trim(str(obj%minNptrs, .TRUE. )) )
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
        & 'tNodes = ' // trim(str(obj%tNodes, .TRUE. )) )
  END IF
  !> reading InternalNptrs, nodeData%globalNodeNumber,
  !> nodeData%localNodeNumber, nodeData%nodeType
  !> mark INTERNAL_NODE
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading InternalNptrs' )
  IF( .NOT. hdf5%pathExists(TRIM(dsetname) // "/intNodeNumber") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/intNodeNumber" // ' path does not exists' )
  ELSE
    CALL hdf5%read( TRIM(dsetname) // "/intNodeNumber", InternalNptrs )
    IF( e%isLogActive() ) THEN
      CALL e%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( InternalNptrs, "InternalNptrs = ", &
        & unitNo = e%getLogFileUnit() )
    END IF
    IF( obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0 ) THEN
      obj%nodeData(1)%globalNodeNum = InternalNptrs( 1 )
      obj%nodeData(1)%nodeType = INTERNAL_NODE
    ELSE
      DO ii = 1, SIZE( InternalNptrs )
        jj = obj%getLocalNodeNumber( InternalNptrs( ii ) )
        obj%nodeData(jj)%globalNodeNum = InternalNptrs( ii )
        obj%nodeData(jj)%nodeType = INTERNAL_NODE
      END DO
    END IF
  END IF
  !> reading boundingEntity
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'reading boundingEntity' )
  IF( hdf5%pathExists(TRIM(dsetname) // "/boundingEntity") ) THEN
    CALL hdf5%read( TRIM(dsetname) // "/boundingEntity", &
      & obj%boundingEntity )
    IF( e%isLogActive() ) THEN
      CALL e%raiseInformation(modName//'::'//myName// " - " )
      CALL Display( obj%boundingEntity, "boundingEntity = ", &
        & unitNo = e%getLogFileUnit() )
    END IF
  END IF
  !> set Reference Element
  CALL e%raiseInformation(modName//'::'//myName// " - " &
    & // "settting reference element" )
  obj%refelem => ReferenceElement_Pointer(xidim=obj%xidim, nsd=obj%nsd, &
    & elemType=obj%elemType)

  IF( ALLOCATED(elemNumber) ) DEALLOCATE(elemNumber)
  IF( ALLOCATED(connectivity) ) DEALLOCATE(connectivity)
  IF( ALLOCATED(InternalNptrs) ) DEALLOCATE(InternalNptrs)
END PROCEDURE mesh_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Export
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_Export"
  CALL e%raiseError(modName//"::"//myName//" - "// &
      & "This routine has not been implemented yet.")
END PROCEDURE mesh_Export

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_ExportToVTK
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_ExportToVTK"
  INTEGER( I4B ) :: nCells, nPoints
  !> main
  IF( .NOT. vtkFile%isInitiated ) THEN
    IF( .NOT. PRESENT( filename ) ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
      & "VTKFile_ is not initiated, and filename is not present.")
    ELSE
      CALL vtkFile%InitiateVTKFile(filename=filename, &
        & mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
        & DataStructureType=VTK_UnStructuredGrid )
    END IF
  END IF
  !>
  nCells = obj%getTotalElements()
  nPoints = obj%getTotalNodes( )
  CALL vtkFile%WritePiece( nPoints=nPoints, nCells=nCells )
  CALL vtkFile%WritePiece( )
END PROCEDURE mesh_ExportToVTK

END SUBMODULE IOMethods