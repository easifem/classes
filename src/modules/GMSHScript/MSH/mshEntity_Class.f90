MODULE mshEntity_Class
  !! This module contains a class to handle entities in mesh file
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 mshEntity_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This class handles the mesh entities defined in msh file

TYPE :: mshEntity_
  INTEGER( I4B ) :: uid = 0
    !! unique id of entity
  INTEGER( I4B ) :: XiDim = 0
    !! for point=0, curve=1, surface = 2, volume = 3
  INTEGER( I4B ) :: ElemType = 0
    !! element type in meshing
  INTEGER( I4B ), ALLOCATABLE :: PhysicalTag( : )
    !! Physical tags associated
  INTEGER( I4B ), ALLOCATABLE :: NodeNumber( : )
    !! node numbers in mesh
  INTEGER( I4B ), ALLOCATABLE :: ElemNumber( : )
    !! element numbers in mesh
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( :, : )
    !! connectivity
  INTEGER( I4B ), ALLOCATABLE :: BoundingEntity( : )
    !! tag of bounding entity

  REAL( DFP ) :: minX = 0.0_DFP
    !! bounding box of entity
  REAL( DFP ) :: minY = 0.0_DFP
    !! bounding box of entity
  REAL( DFP ) :: minZ = 0.0_DFP
    !! bounding box of entity
  REAL( DFP ) :: maxX = 0.0_DFP
    !! bounding box of entity
  REAL( DFP ) :: maxY = 0.0_DFP
    !! bounding box of entity
  REAL( DFP ) :: maxZ = 0.0_DFP
    !! bounding box of entity
  REAL( DFP ) :: X = 0.0_DFP
    !! used only for point entity
  REAL( DFP ) :: Y = 0.0_DFP
    !! used only for point entity
  REAL( DFP ) :: Z = 0.0_DFP
    !! used only for point entity
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
    !! nodal coordinates in xiJ format

  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Finalize => ent_deallocateData
      !! To deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: GotoTag => ent_goto
      !! To find tag
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadPointEntity => ent_read_point
      !! Read the entry from file for point
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadCurveEntity => ent_Read_Curve
      !! Read the entry from file for curve
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadSurfaceEntity => ent_Read_Surface
      !! Read the entry from file for surface
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadVolumeEntity => ent_Read_Volume
      !! Read the entry from file for volume
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalPhysicalTags => ent_tphysicaltag
      !! Return total physical tags associated
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalBoundingTags => ent_tboundingtag
      !! Returns the total bounding tags
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalElements => ent_telements
      !! Returns the total elements
    PROCEDURE, PUBLIC, PASS( obj ) :: WriteToFile => ent_write_file
      !! Write data to a file
END TYPE mshEntity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: mshEntity_

TYPE( mshEntity_ ), PUBLIC, PARAMETER :: &
  & TypeMshEntity = &
  & mshEntity_( &
    & PhysicalTag = NULL( ), &
    & NodeNumber = NULL( ), &
    & ElemNumber = NULL( ), &
    & Nptrs = NULL( ), &
    & NodeCoord = NULL( ), &
    & BoundingEntity = NULL( ) )

!----------------------------------------------------------------------------
!                                                          mshEntityPointer_
!----------------------------------------------------------------------------

TYPE :: mshEntityPointer_
    CLASS( mshEntity_ ), POINTER :: Ptr => NULL( )
END TYPE mshEntityPointer_

PUBLIC :: mshEntityPointer_

!----------------------------------------------------------------------------
!                                                         GotoTag@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine finds the tag in the mesh file

MODULE SUBROUTINE ent_goto( obj, mshFile, ierr )
  CLASS( mshEntity_ ), INTENT( IN ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE ent_goto
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ReadPointEntity@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine reads the entry for point entity

!> authors: Dr. Vikas Sharma
!
! This subroutine reads the entry for point entity

MODULE SUBROUTINE ent_read_point( obj, mshFile, readTag, ierr )
  CLASS( mshEntity_ ), INTENT( INOUT) :: obj
  TYPE( File_ ), INTENT( INOUT) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT) :: ierr
  LOGICAL( LGT ), INTENT( IN ) :: readTag
END SUBROUTINE ent_read_point
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ReadCurveEntity@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine reads the entry for curve entity

!> authors: Dr. Vikas Sharma
!
! This subroutine reads the entry for curve entity

MODULE SUBROUTINE ent_read_Curve( obj, mshFile, readTag, ierr )
  CLASS( mshEntity_ ), INTENT( INOUT) :: obj
  TYPE( File_ ), INTENT( INOUT) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT) :: ierr
  LOGICAL( LGT ), INTENT( IN ) :: readTag
END SUBROUTINE ent_read_Curve
END INTERFACE

!----------------------------------------------------------------------------
!                                               ReadSurfaceEntity@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine reads the entry for surface entity

!> authors: Dr. Vikas Sharma
!
! This subroutine reads the entry for surface entity

MODULE SUBROUTINE ent_read_Surface( obj, mshFile, readTag, ierr )
  CLASS( mshEntity_ ), INTENT( INOUT) :: obj
  TYPE( File_ ), INTENT( INOUT) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT) :: ierr
  LOGICAL( LGT ), INTENT( IN ) :: readTag
END SUBROUTINE ent_read_Surface
END INTERFACE

!----------------------------------------------------------------------------
!                                                ReadVolumeEntity@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine reads the entry for volume entity

!> authors: Dr. Vikas Sharma
!
! This subroutine reads the entry for volume entity

MODULE SUBROUTINE ent_read_Volume( obj, mshFile, readTag, ierr )
  CLASS( mshEntity_ ), INTENT( INOUT) :: obj
  TYPE( File_ ), INTENT( INOUT) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT) :: ierr
  LOGICAL( LGT ), INTENT( IN ) :: readTag
END SUBROUTINE ent_read_Volume
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteToFile@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine write the data to a file

!> authors: Dr. Vikas Sharma
!
! This subroutine write the data to a file

MODULE SUBROUTINE ent_write_file( obj, mshFile, Str, EndStr )
  CLASS( mshEntity_ ), INTENT( INOUT ) :: obj
  TYPE( File_ ), INTENT( INOUT) :: mshFile
  CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL :: Str, EndStr
END SUBROUTINE ent_write_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@mshEntity
!----------------------------------------------------------------------------

INTERFACE
! This subroutine writes the content of [[mshEntity_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine writes the content of [[mshEntity_]]

MODULE SUBROUTINE ent_display( obj, Msg, UnitNo )
  CLASS( mshEntity_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE ent_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE ent_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                         getIndex@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This function finds the index of a tag/uid in list of entities

!> authors: Dr. Vikas Sharma
!
! This function finds the index of a tag/uid in the list of entities

MODULE PURE FUNCTION ent_getIndex_a( mshEntities, Uid ) RESULT( ans )
  TYPE( mshEntity_ ), INTENT( IN ) :: mshEntities( : )
  INTEGER( I4B ), INTENT( IN ) :: Uid
  INTEGER( I4B ) :: ans
END FUNCTION ent_getIndex_a
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE ent_getIndex_a
END INTERFACE getIndex

PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                TotalPhysicalTags@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total number of physical tags in entity

!> authors: Dr. Vikas Sharma
!
! This function returns the total number of physical tags in entity

MODULE PURE FUNCTION ent_tphysicaltag( obj ) RESULT( ans )
  CLASS( mshEntity_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION ent_tphysicaltag
END INTERFACE

!----------------------------------------------------------------------------
!                                                TotalBoundingTags@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total number of bounding tags in entity

!> authors: Dr. Vikas Sharma
!
! This function returns the total number of bounding tags in entity

MODULE PURE FUNCTION ent_tBoundingtag( obj ) RESULT( ans )
  CLASS( mshEntity_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION ent_tBoundingtag
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalElements@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total number of elements in entity

!> authors: Dr. Vikas Sharma
!
! This function returns the total number of elements in entity

MODULE PURE FUNCTION ent_telements( obj ) RESULT( ans )
  CLASS( mshEntity_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION ent_telements
END INTERFACE

!----------------------------------------------------------------------------
!                                                    DeallocateData@mshEntity
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine deallocate the data from [[mshentity_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocate the data from [[mshentity_]]

MODULE SUBROUTINE ent_deallocatedata( obj )
  CLASS( mshEntity_ ), INTENT( INOUT) :: obj
END SUBROUTINE ent_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE ent_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE mshEntity_Class