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

SUBMODULE( MSH_Class ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 final
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Final
  CALL obj%DeallocateData()
END PROCEDURE msh_Final

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_deallocatedata
  CALL obj%Format%DeallocateData()
  CALL obj%PhysicalNames%DeallocateData()
  CALL obj%Nodes%DeallocateData()
  CALL obj%Elements%DeallocateData()
  IF( ALLOCATED( obj%PointEntities ) ) DEALLOCATE( obj%PointEntities )
  IF( ALLOCATED( obj%CurveEntities ) ) DEALLOCATE( obj%CurveEntities )
  IF( ALLOCATED( obj%SurfaceEntities ) ) DEALLOCATE( obj%SurfaceEntities )
  IF( ALLOCATED( obj%VolumeEntities ) ) DEALLOCATE( obj%VolumeEntities )
  obj%nsd = 0
  IF( ASSOCIATED( obj % buffer ) ) DEALLOCATE( obj%buffer )
  NULLIFY( obj%buffer )
END PROCEDURE msh_deallocatedata

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_initiate
  ! Define internal variables
  INTEGER( I4B ) :: error, unitNo
  CHARACTER( LEN = * ), PARAMETER :: myName = "msh_Initiate"

  CALL eMSH%setQuietMode( .TRUE. )
  CALL eMSH%setLogFileUnit( eUnitNo )
  CALL eMSH%setLogActive( .TRUE. )
  IF( .NOT. eMSH%isLogActive() ) THEN
    OPEN( Unit=eUnitNo, FILE=eLogFile, &
      & ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='REPLACE' )
  END IF
  CALL obj%e%addSurrogate(eMSH)
  CALL obj%e%setLogActive( .TRUE. )

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'READING GMSH FILE!')
  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Opening Gmsh File')

  CALL obj%mshFile%initiate(file=file, status="OLD", action="READ")
  CALL obj%mshFile%open()
  obj%NSD = NSD
  unitNo = obj%mshFile%getunitNo()

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Gmsh file is opened')

  CALL obj%e%raiseDebug(modName//'::'//myName//' - '// &
    & 'Reading mesh format')
  CALL obj%Format%Read( mshFile=obj%mshFile, error=error)

  IF( error .NE. 0 ) THEN
    CALL obj%e%raiseError(modName//'::'//myName//' - '// &
      & 'Failed in Reading mesh format')
  ELSE
    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Success in Reading mesh format')
  END IF

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Reading physical group information')
  CALL obj%PhysicalNames%Read( mshFile=obj%mshFile, error=error )

  IF( error .NE. 0 ) THEN
    CALL obj%e%raiseError(modName//'::'//myName//' - '// &
      & 'Failed in Reading physical group information')
  ELSE
    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Success in Reading physical group information')
  END IF

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Locating entity tag')

  CALL TypemshEntity%GotoTag( mshFile=obj%mshFile, error=error )
  IF( error .NE. 0 ) THEN
    CALL obj%e%raiseError(modName//'::'//myName//' - '// &
      & 'Failed in Locating entity tag')
  ELSE
    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Success in Locating entity tag')
  END IF

  !---------------------------------------------------------------------------
  ! Entities
  !---------------------------------------------------------------------------

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Reading entities')

  BLOCK
    INTEGER( I4B ) :: tp, tc, ts, tv, i, j, k, tpt
    INTEGER( I4B ), ALLOCATABLE :: PhysicalTag( : )
    ! we read header of Entities block
    READ( unitNo, * ) tp, tc, ts, tv

    CALL obj%e%raiseInformation(modName//'::'//myName//' - ' &
      & // ' total number of point entities : ' // TRIM(str(tp, .true.)) &
      & // ' total number of curve entities : ' // TRIM(str(tc, .true.)) &
      & // ' total number of surface entities : ' // TRIM(str(ts, .true.)) &
      & // ' total number of volume entities : ' // TRIM(str(tv, .true.)) )

    IF( ALLOCATED( obj%PointEntities ) ) DEALLOCATE( obj%PointEntities )
    IF( ALLOCATED( obj%CurveEntities ) ) DEALLOCATE( obj%CurveEntities )
    IF( ALLOCATED( obj%SurfaceEntities ) ) DEALLOCATE(obj%SurfaceEntities)
    IF( ALLOCATED( obj%VolumeEntities ) ) DEALLOCATE(obj%VolumeEntities)
    IF( tp .NE. 0 ) ALLOCATE( obj%PointEntities( tp ) )
    IF( tc .NE. 0 ) ALLOCATE( obj%CurveEntities( tc ) )
    IF( ts .NE. 0 ) ALLOCATE( obj%SurfaceEntities( ts ) )
    IF( tv .NE. 0 ) ALLOCATE( obj%VolumeEntities( tv ) )

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Reading point entities' )

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Creating physical point to entities map' )

    DO i = 1, tp
      CALL obj%PointEntities( i )%Read( mshFile=obj%mshFile, dim=0, &
        & readTag=.FALSE., error=error )
      ! get total physical tag
      tpt = obj%PointEntities( i )%getTotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj%PointEntities( i )%getPhysicalTag()
        DO j = 1, tpt
          ! get index of physical tag
          k = obj%PhysicalNames%getIndex( dim=0, tag=PhysicalTag( j ) )
          ! append this index to entities
          CALL obj%PhysicalNames%AppendEntities( indx=k, EntityTag = [i] )
        END DO
      END IF
    END DO

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Reading curve entities' )

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Creating physical curve to entities map' )

    DO i = 1, tc
      CALL obj%CurveEntities( i )%Read( mshFile=obj%mshFile, dim=1, &
        & readTag=.FALSE., error=error )
      ! get total physical tag
      tpt = obj%CurveEntities( i )%getTotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj%CurveEntities( i )%getPhysicalTag()
        DO j = 1, tpt
          ! get index of physical tag
          k = obj%PhysicalNames%getIndex( dim=1, tag=PhysicalTag( j ) )
          ! append this index to entities
          CALL obj%PhysicalNames%AppendEntities( indx=k, entityTag=[i] )
        END DO
      END IF
    END DO

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Reading surface entities' )

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Creating physical surface to entities map' )

    DO i = 1, ts
      CALL obj%SurfaceEntities( i )%Read( mshFile=obj%mshFile, &
        & dim=2, readTag=.false., error=error )
      ! get total physical tag
      tpt = obj%SurfaceEntities( i )%getTotalPhysicalTags( )

      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj%SurfaceEntities( i )%getPhysicalTag()
        DO j = 1, tpt
          ! get index of physical tag
          k = obj%PhysicalNames%getIndex( dim=2, tag=PhysicalTag( j ) )
          ! append this index to entities
          CALL obj%PhysicalNames%AppendEntities( indx=k, entityTag=[i] )
        END DO
      END IF
    END DO

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Reading volume entities' )

    CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
      & 'Creating physical volume to entities map' )

    DO i = 1, tv
      CALL obj%VolumeEntities( i )%Read( mshFile=obj%mshFile, &
        & dim=3, readTag=.false., error=error )
      ! get total physical tag
      tpt = obj%VolumeEntities( i )%getTotalPhysicalTags( )

      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj%VolumeEntities( i )%getPhysicalTag()
        DO j = 1, tpt
          ! get index of physical tag
          k = obj%PhysicalNames%getIndex( dim=3, tag=PhysicalTag( j ) )
          ! append this index to entities
          CALL obj%PhysicalNames%AppendEntities( indx=k, entityTag=[i] )
        END DO
      END IF
    END DO
    IF( ALLOCATED( PhysicalTag ) ) DEALLOCATE( PhysicalTag )
  END BLOCK

  !---------------------------------------------------------------------------
  ! Nodes
  !---------------------------------------------------------------------------

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Reading nodes section' )

  BLOCK
    ! define internal variable
    INTEGER( I4B ) :: i, j, k, l, entityDim, entityTag, parametric, &
      & numNodesInBlock
    INTEGER( I4B ), ALLOCATABLE ::  NodeNumber( : )
    REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )

    ! we read first line of $Nodes block
    CALL obj%Nodes%Read( mshFile=obj%mshFile, mshFormat=obj%Format, &
      & error=error )

    !start reading each entity block
    DO i = 1, obj%Nodes%getnumEntityBlocks()
      !read entity dimension and entity tag (uid)
      READ( UnitNo, * ) entityDim, entityTag, &
        & parametric, numNodesInBlock
      CALL Reallocate( NodeNumber, numNodesInBlock )
      ! now we read node numbers in NodeNumber( : )
      DO k = 1, numNodesInBlock
        READ( UnitNo, * ) NodeNumber( k )
      END DO
      CALL Reallocate( NodeCoord, [3, numNodesInBlock] )
      ! now we read node coordinates
      DO k = 1, numNodesInBlock
        READ( UnitNo, * ) (NodeCoord(l, k), l = 1, 3)
      END DO
      !make case based on entity dimension
      SELECT CASE( entityDim )
        CASE( 0 )
          j = getIndex( obj%PointEntities, entityTag )
          CALL obj%PointEntities( j )%setIntNodeNumber( NodeNumber )
          CALL obj%PointEntities( j )%setNodeCoord( NodeCoord )
        CASE( 1 )
          j = getIndex( obj%CurveEntities, entityTag )
          CALL obj%CurveEntities( j )%setIntNodeNumber( NodeNumber )
          CALL obj%CurveEntities( j )%setNodeCoord( NodeCoord )
        CASE( 2 )
          j = getIndex( obj%SurfaceEntities, entityTag )
          CALL obj%SurfaceEntities( j )%setIntNodeNumber( NodeNumber )
          CALL obj%SurfaceEntities( j )%setNodeCoord( NodeCoord )
        CASE( 3 )
          j = getIndex( obj%VolumeEntities, entityTag )
          CALL obj%VolumeEntities( j )%setIntNodeNumber( NodeNumber )
          CALL obj%VolumeEntities( j )%setNodeCoord( NodeCoord )
      END SELECT
    END DO
    DEALLOCATE( NodeNumber, NodeCoord )
  END BLOCK

  !---------------------------------------------------------------------------
  ! Elements
  !---------------------------------------------------------------------------

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Reading element section' )

  ! at this point we have read $Nodes and now we are ready to read elements
  BLOCK
    ! define internal variables
    INTEGER( I4B ) :: i, j, k, l, entityDim, entityTag, elemType, &
      & numElementsInBlock, tNodes, tpt
    INTEGER( I4B ), ALLOCATABLE :: ElemNumber( : ), Nptrs( :, : ), PhyTag( : )

    CALL obj%Elements%Read( mshFile=obj%mshFile, mshFormat=obj%Format, &
      & error=error )
    ! start reading each entity block
    DO i = 1, obj%Elements%getnumEntityBlocks()
      ! read entity dimension and entity tag (uid)
      READ( UnitNo, * ) entityDim, entityTag, ElemType, numElementsInBlock
      ! get the total number of nodes in element
      tNodes = TotalNodesInElement( ElemType )
      CALL Reallocate( ElemNumber, numElementsInBlock )
      CALL Reallocate( Nptrs, [tNodes, numElementsInBlock] )
      ! now we read ElemNumber and Nptrs
      DO k = 1, numElementsInBlock
        READ( UnitNo, * ) ElemNumber( k ), (Nptrs( l, k ), l = 1, tNodes)
      END DO
      ! make case based on entity dimension
      SELECT CASE( entityDim )
        CASE( 0 )
          j = getIndex( obj%PointEntities, entityTag )
          ! set the element type
          CALL obj%PointEntities( j )%setElemType(ElemType)
          CALL obj%PointEntities( j )%setElemNumber(ElemNumber)
          CALL obj%PointEntities( j )%setConnectivity(Nptrs)
          ! counting nodes in each physical group
          tpt = obj%PointEntities( j )%getTotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj%PointEntities( j )%getPhysicalTag()
            DO k = 1, tpt
              l = obj%PhysicalNames%getIndex(dim=0, tag=PhyTag(k))
              CALL obj%PhysicalNames%IncNumElements( indx=l, incr=numElementsInBlock )
            END DO
          END IF
        CASE( 1 )
          j = getIndex( obj%CurveEntities, entityTag )
          ! set the element type
          CALL obj%CurveEntities( j )%setElemType(ElemType)
          CALL obj%CurveEntities( j )%setElemNumber(ElemNumber)
          CALL obj%CurveEntities( j )%setConnectivity(Nptrs)
          ! counting nodes in each physical group
          tpt = obj%CurveEntities( j )%getTotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj%CurveEntities( j )%getPhysicalTag()
            DO k = 1, tpt
              l = obj%PhysicalNames%getIndex( dim=1, tag=PhyTag( k ) )
              CALL obj%PhysicalNames%IncNumElements( indx=l, incr=numElementsInBlock )
            END DO
          END IF
        CASE( 2 )
          j = getIndex( obj%SurfaceEntities, entityTag )
          ! set the element type
          CALL obj%SurfaceEntities( j )%setElemType(ElemType)
          CALL obj%SurfaceEntities( j )%setElemNumber(ElemNumber)
          CALL obj%SurfaceEntities( j )%setConnectivity(Nptrs)
          ! counting nodes in each physical group
          tpt = obj%SurfaceEntities( j )%getTotalPhysicalTags()
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj%SurfaceEntities( j )%getPhysicalTag()
            DO k = 1, tpt
              l = obj%PhysicalNames%getIndex( dim=2, tag=PhyTag( k ) )
              CALL obj%PhysicalNames%IncNumElements( indx=l, incr=numElementsInBlock )
            END DO
          END IF
        CASE( 3 )
          j = getIndex( obj%VolumeEntities, entityTag )
          ! set the element type
          CALL obj%VolumeEntities( j )%setElemType(ElemType)
          CALL obj%VolumeEntities( j )%setElemNumber(ElemNumber)
          CALL obj%VolumeEntities( j )%setConnectivity(Nptrs)
          ! counting nodes in each physical group
          tpt = obj%VolumeEntities( j )%getTotalPhysicalTags()
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj%VolumeEntities( j )%getPhysicalTag()
            DO k = 1, tpt
              l = obj%PhysicalNames%getIndex( dim=3, tag=PhyTag( k ) )
              CALL obj%PhysicalNames%IncNumElements( indx=l, incr=numElementsInBlock )
            END DO
          END IF
      END SELECT
    END DO
    IF(ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
    IF( ALLOCATED( ElemNumber ) ) DEALLOCATE( ElemNumber )
    IF( ALLOCATED( PhyTag ) ) DEALLOCATE( PhyTag )
  END BLOCK

  !---------------------------------------------------------------------------
  ! Counting number of nodes in physical region
  !---------------------------------------------------------------------------

  CALL obj%e%raiseInformation(modName//'::'//myName//' - '// &
    & 'Reading Nodes in Physical groups' )

  BLOCK
    ! define internal variables
    INTEGER( I4B ) :: tpt, i, j, k, tElements
    INTEGER( I4B ), ALLOCATABLE :: Indx( : ), entIndx( : ), Nptrs( : ), &
      & dummyNptrs( : )

    ALLOCATE( Nptrs( obj%Nodes%getMaxNodeTag() ) )
    ! Points
    tpt = obj%PhysicalNames%getTotalPhysicalEntities( [0] )
    IF( tpt .NE. 0 ) THEN
      Indx = obj%PhysicalNames%getIndex( dim=0 )
      ! loop over all physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = obj%PhysicalNames%getEntities( Indx( i ) )
        CALL obj%PhysicalNames%IncNumNodes( Indx=Indx( i ), &
          & incr=SIZE( entIndx ) )
      END DO
    END IF
    ! Curve
    tpt = obj%PhysicalNames%getTotalPhysicalEntities( [1] )
    IF( tpt .NE. 0 ) THEN
      Indx = obj%PhysicalNames%getIndex( dim=1 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = obj%PhysicalNames%getEntities( Indx( i ) )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = obj%CurveEntities( entIndx( j ) )%getTotalElements( )
          DO k = 1, tElements
            dummyNptrs = obj%CurveEntities( entIndx( j ) )%getConnectivity( k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        CALL obj%PhysicalNames%setNumNodes( indx=Indx( i ), numNode = COUNT( Nptrs .NE. 0 ) )
      END DO
    END IF
    ! Surface
    tpt = obj%PhysicalNames%getTotalPhysicalEntities( dim=[2] )
    IF( tpt .NE. 0 ) THEN
      Indx = obj%PhysicalNames%getIndex( dim=2 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = obj%PhysicalNames%getEntities( indx=Indx( i ) )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = obj%SurfaceEntities( entIndx( j ) )%getTotalElements( )
          DO k = 1, tElements
            dummyNptrs = obj%SurfaceEntities( entIndx( j ) )%getConnectivity( k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        CALL obj%PhysicalNames%setNumNodes( indx=Indx( i ), numNode=COUNT( Nptrs .NE. 0 ) )
      END DO
    END IF
    ! Volume
    tpt = obj%PhysicalNames%getTotalPhysicalEntities( dim=[3] )
    IF( tpt .NE. 0 ) THEN
      Indx = obj%PhysicalNames%getIndex( dim=3 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = obj%PhysicalNames%getEntities( Indx( i ) )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = obj%VolumeEntities( entIndx( j ) )%getTotalElements()
          DO k = 1, tElements
            dummyNptrs = obj%VolumeEntities( entIndx( j ) )%getConnectivity( k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        CALL obj%PhysicalNames%setNumNodes( indx=Indx( i ), &
          & numNode = COUNT( Nptrs .NE. 0 ) )
      END DO
    END IF
    ! add deallocate stmt
    IF( ALLOCATED( Indx ) ) DEALLOCATE( Indx )
    IF( ALLOCATED( entIndx ) ) DEALLOCATE( entIndx )
    IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
    IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
  END BLOCK
END PROCEDURE msh_initiate

! !----------------------------------------------------------------------------
! !                                                                        msh4
! !----------------------------------------------------------------------------

! MODULE PROCEDURE msh_constuctor1
!   CALL ans%Initiate( Path, FileName, Extension, NSD )
! END PROCEDURE msh_constuctor1

! !----------------------------------------------------------------------------
! !                                                                    Display
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

END SUBMODULE Methods