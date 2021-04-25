MODULE mshPhysicalNames_Class
  !! This module defines a data type for handling gmsh physical names
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          mshPhysicalNames_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This data type contains the Physical Names generate by gmsh

TYPE :: mshPhysicalNames_
  INTEGER( I4B ), ALLOCATABLE :: NSD( : )
    !! spatial dimension of each physical group
  INTEGER( I4B ), ALLOCATABLE :: Tag( : )
    !! Unit ID of each physical group
  INTEGER( I4B ), ALLOCATABLE :: numElements( : )
    !! Number of elements in each physical group
  INTEGER( I4B ), ALLOCATABLE :: numNodes( : )
    !! Number of nodes in each physical nodes
  TYPE( IntVector_ ), ALLOCATABLE :: Entities( : )
    !! Tags of Entities in each physical group
  TYPE( String ), ALLOCATABLE :: PhysicalName( : )
    !! Physical name of each physical group

  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Finalize => pn_deallocatedata
      !! To deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: GotoTag => pn_goto
      !! Search tag for physical group in mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: ReadFromFile => pn_read_file
      !! Read contents from mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: WriteToFile => pn_write_file
      !! Write contents to a mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: SIZE => pn_get_size
      !! Returns total number of physical groups
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalPhysicalPoints => pn_size_point
      !! Returns total number of physical points in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalPhysicalCurves => pn_size_Curve
      !! Returns total number of physical curves
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalPhysicalSurfaces => pn_size_Surface
      !! Returns total number of physical surface
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalPhysicalVolumes => pn_size_Volume
      !! Returns total number of physical volumes
    GENERIC, PUBLIC :: &
      & getIndex => &
      & pn_index_a, &
      & pn_index_b, &
      & pn_index_c, &
      & pn_index_d
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_a
      !! Returns the index of a physical group
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_b
      !! Returns the index of a physical group
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_c
      !! Returns the index of a physical group
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_d
      !! Returns the index of a physical group

    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalPointNames => pn_point_names
      !! Returns the names of physical points
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalCurveNames => pn_Curve_names
      !! Returns names of a physical curves
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalSurfaceNames => pn_Surface_names
      !! Returns names of Physical surface
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalVolumeNames => pn_Volume_names
      !! Returns names of physical volumes
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalPointTags => pn_Point_tags
      !! Returns tags of physical points
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalCurveTags => pn_Curve_tags
      !! Returns tags of physical points
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalSurfaceTags => pn_Surface_tags
      !! Returns tags of physical surfaces
    PROCEDURE, PUBLIC, PASS( obj ) :: PhysicalVolumeTags => pn_Volume_tags
      !! Returns tags of physical volumes
    PROCEDURE, PUBLIC, PASS( obj ) :: WhoAmI => pn_who_am_i
      !! Enquire about "volume, surface, curve, point'

    GENERIC, PUBLIC :: IndexOfPhysicalPoint => pn_index_point, &
      & pn_index_point_2
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_point
      !! Return index of physical points
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_point_2
      !! Return index of physical points

    GENERIC, PUBLIC :: IndexOfPhysicalCurve => &
      & pn_index_Curve, &
      & pn_index_Curve_2
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_Curve
      !! Return index of physical curve
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_Curve_2
      !! Return index of physical curve

    GENERIC, PUBLIC :: IndexOfPhysicalSurface => &
      & pn_index_Surface, &
      & pn_index_Surface_2
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_Surface
      !! Return index of physical surface
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_Surface_2
      !! Return index of physical surface

    GENERIC, PUBLIC :: IndexOfPhysicalVolume => pn_index_Volume, &
      & pn_index_Volume_2
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_Volume
    PROCEDURE, PRIVATE, PASS( obj ) :: pn_index_Volume_2

    PROCEDURE, PUBLIC, PASS( obj ) :: OutputFileName => pn_output_file
END TYPE mshPhysicalNames_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: mshPhysicalNames_

TYPE( mshPhysicalNames_ ), PARAMETER, PUBLIC :: &
  & TypeMSHPhysicalNames = &
    & mshPhysicalNames_( &
      & NSD = NULL( ), &
      & Tag =  NULL( ), &
      & numElements = NULL( ), &
      & numNodes = NULL( ), &
      & PhysicalName = NULL( ), &
      & Entities = NULL( ) )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: mshPhysicalNamesPointer_
  CLASS( mshPhysicalNames_ ), POINTER :: Ptr => NULL( )
END TYPE mshPhysicalNamesPointer_

PUBLIC :: mshPhysicalNamesPointer_

!----------------------------------------------------------------------------
!                                                  GotoTag@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This suboutine find the tag for Physical groups in mesh

!> authors: Dr. Vikas Sharma
!
! This suboutine find the tag for Physical groups in mesh

MODULE SUBROUTINE pn_goto( obj, mshFile, ierr )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE pn_goto
END INTERFACE

!----------------------------------------------------------------------------
!                                              ReadFromFile@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine reads physical groupds info from mesh file

!> authors: Dr. Vikas Sharma
!
! This subroutine reads physical groupds info from mesh file

MODULE SUBROUTINE pn_read_file( obj, mshFile, ierr )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  LOGICAL( LGT ), INTENT( INOUT ) :: ierr
END SUBROUTINE pn_read_file
END INTERFACE

!----------------------------------------------------------------------------
!                                               WriteToFile@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine writes physical groupds info in mesh file

!> authors: Dr. Vikas Sharma
!
! This subroutine writes physical groupds info in mesh file

MODULE SUBROUTINE pn_write_file( obj, mshFile, Str, EndStr )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
  TYPE( File_ ), INTENT( INOUT ) :: mshFile
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: Str, EndStr
END SUBROUTINE pn_write_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Display@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine displays the content of [[mshphysicalnames_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine displays the content of [[mshphysicalnames_]]

MODULE SUBROUTINE pn_display( obj, Msg, UnitNo )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE pn_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE pn_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                     Size@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns total number of physical entities

!> authors: Dr. Vikas Sharma
!
! This function returns total number of physical entities

MODULE PURE FUNCTION pn_get_size( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION pn_get_size
END INTERFACE

!----------------------------------------------------------------------------
!                                       TotalPhysicalPoints@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns total number of physical points

!> authors: Dr. Vikas Sharma
!
! This function returns total number of physical points

MODULE PURE FUNCTION pn_size_point( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION pn_size_point
END INTERFACE

!----------------------------------------------------------------------------
!                                       TotalPhysicalCurves@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns total number of physical curves

!> authors: Dr. Vikas Sharma
!
! This function returns total number of physical curves

MODULE PURE FUNCTION pn_size_Curve( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION pn_size_Curve
END INTERFACE

!----------------------------------------------------------------------------
!                                     TotalPhysicalSurfaces@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns total number of physical surfaces

!> authors: Dr. Vikas Sharma
!
! This function returns total number of physical surfaces

MODULE PURE FUNCTION pn_size_Surface( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION pn_size_Surface
END INTERFACE

!----------------------------------------------------------------------------
!                                      TotalPhysicalVolumes@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns total number of physical volumes

!> authors: Dr. Vikas Sharma
!
! This function returns total number of physical volumes

MODULE PURE FUNCTION pn_size_Volume( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION pn_size_Volume
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getIndex@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns index of a given physical name

!> authors: Dr. Vikas Sharma
!
! This function returns index of a given physical name

MODULE PURE FUNCTION pn_index_a( obj, Name ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: ans
END FUNCTION pn_index_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_index_b( obj, Name ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Name( : )
  INTEGER( I4B ) :: ans( SIZE( Name ) )
END FUNCTION pn_index_b
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

INTERFACE
!! This function returns index of a given physical name

!> authors: Dr. Vikas Sharma
!
! This function returns index of a given physical name
!
! - `XiDimTag( 1 )` denotes the XiDImension of physical entity
!     - 0 => point
!     - 1 => line
!     - 2 => surface
!     - 3 => volume
! - `XiDimTag( 2 )` denotes the Physical Tag of physical entity

MODULE PURE FUNCTION pn_index_c( obj, XiDimTag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: XiDimTag( 2 )
  INTEGER( I4B ) :: ans
END FUNCTION pn_index_c
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

INTERFACE
!! This function returns index of a given physical name

!> authors: Dr. Vikas Sharma
!
! This function returns index of a given physical name
!
! - `XiDim` denotes the XiDImension of physical entity
! - `XiDim` = 0 => returns indices of points
! - `XiDim` = 1 => returns indices of lines
! - `XiDim` = 2 => returns indices of surfaces
! - `XiDim` = 3 => returns indices of volumes

MODULE PURE FUNCTION pn_index_d( obj, XiDimTag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: XiDimTag
  INTEGER( I4B ), ALLOCATABLE  :: ans( : )
END FUNCTION pn_index_d
END INTERFACE

!----------------------------------------------------------------------------
!                                       PhysicalPointNames@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the names of physical poins

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the names of physical points

MODULE PURE FUNCTION pn_point_names( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION pn_point_names
END INTERFACE

!----------------------------------------------------------------------------
!                                       PhysicalCurveNames@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the names of physical curves

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the names of physical curves

MODULE PURE FUNCTION pn_Curve_names( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Curve_names
END INTERFACE

!----------------------------------------------------------------------------
!                                      PhysicalSurfaceNames@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the names of physical surfaces

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the names of physical surfaces

MODULE PURE FUNCTION pn_Surface_names( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Surface_names
END INTERFACE

!----------------------------------------------------------------------------
!                                       PhysicalVolumeNames@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the names of physical volumes

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the names of physical volumes

MODULE PURE FUNCTION pn_Volume_names( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Volume_names
END INTERFACE

!----------------------------------------------------------------------------
!                                         PhysicalPointTags@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the physical tags of all physical points

!> authors: Dr. Vikas Sharma
!
! This function returns the physical tags of all physical points

MODULE PURE FUNCTION pn_Point_tags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Point_tags
END INTERFACE

!----------------------------------------------------------------------------
!                                        PhysicalCurveTags@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the physical tags of all physical curves

!> authors: Dr. Vikas Sharma
!
! This function returns the physical tags of all physical curves

MODULE PURE FUNCTION pn_Curve_tags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Curve_tags
END INTERFACE

!----------------------------------------------------------------------------
!                                       PhysicalSurfaceTags@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the physical tags of all physical surfaces

!> authors: Dr. Vikas Sharma
!
! This function returns the physical tags of all physical surfaces

MODULE PURE FUNCTION pn_Surface_tags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Surface_tags
END INTERFACE

!----------------------------------------------------------------------------
!                                       PhysicalVolumeTags@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the physical tags of all physical volumes

!> authors: Dr. Vikas Sharma
!
! This function returns the physical tags of all physical volumes

MODULE PURE FUNCTION pn_Volume_tags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_Volume_tags
END INTERFACE

!----------------------------------------------------------------------------
!                                                    WhoAmI@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_who_am_i( obj, I ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: I
  TYPE( String ) :: ans
END FUNCTION pn_who_am_i
END INTERFACE

!----------------------------------------------------------------------------
!                                      IndexOfPhysicalPoint@mshPhysicalNames
!----------------------------------------------------------------------------

! return the index of physical point from its physical id

INTERFACE
MODULE PURE FUNCTION pn_index_point( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag
  INTEGER( I4B ) :: ans
END FUNCTION pn_index_point
END INTERFACE

INTERFACE
MODULE PURE FUNCTION pn_index_point_2( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag( : )
  INTEGER( I4B ) :: ans( SIZE( Tag ) )
END FUNCTION pn_index_point_2
END INTERFACE

!----------------------------------------------------------------------------
!                                      IndexOfPhysicalCurve@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function return the index of physical Curve from its physical id

!> authors: Dr. Vikas Sharma
!
! This function returns the index of physical Curve from its physical id

MODULE PURE FUNCTION pn_index_Curve( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag
  INTEGER( I4B ) :: ans
END FUNCTION pn_index_Curve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
!! This function return the index of physical Curve from its physical id

!> authors: Dr. Vikas Sharma
!
! This function returns the index of physical Curve from its physical id

MODULE PURE FUNCTION pn_index_Curve_2( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag( : )
  INTEGER( I4B ) :: ans( SIZE( Tag ) )
END FUNCTION pn_index_Curve_2
END INTERFACE

!----------------------------------------------------------------------------
!                                    IndexOfPhysicalSurface@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function return the index of physical Surface from its physical id

!> authors: Dr. Vikas Sharma
!
! This function returns the index of physical Surface from its physical id

MODULE PURE FUNCTION pn_index_Surface( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag
  INTEGER( I4B ) :: ans
END FUNCTION pn_index_Surface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_index_Surface_2( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag( : )
  INTEGER( I4B ) :: ans( SIZE( Tag ) )
END FUNCTION pn_index_Surface_2
END INTERFACE

!----------------------------------------------------------------------------
!                                    IndexOfPhysicalVolume@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function return the index of physical Volume from its physical id

!> authors: Dr. Vikas Sharma
!
! This function returns the index of physical Volume from its physical id

MODULE PURE FUNCTION pn_index_Volume( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag
  INTEGER( I4B ) :: ans
END FUNCTION pn_index_Volume
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
!! This function return the index of physical Volume from its physical id

!> authors: Dr. Vikas Sharma
!
! This function returns the index of physical Volume from its physical id

MODULE PURE FUNCTION pn_index_Volume_2( obj, Tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Tag( : )
  INTEGER( I4B ) :: ans( SIZE( Tag ) )
END FUNCTION pn_index_Volume_2
END INTERFACE

!----------------------------------------------------------------------------
!                                            OutputFileName@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This function retunrns the name of output file

MODULE PURE FUNCTION pn_output_file( obj, mshFile, indx ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( File_ ), INTENT( IN ) :: mshFile
  INTEGER( I4B ), INTENT( IN ) :: indx
  TYPE( String ) :: ans
END FUNCTION pn_output_file
END INTERFACE

!----------------------------------------------------------------------------
!                                             DeallocateData@mshPhysicalNames
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine deallocates the data stored in [[mshPhysicalNames_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocates the data stored in [[mshPhysicalNames_]]

MODULE SUBROUTINE pn_deallocatedata( obj )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT) :: obj
END SUBROUTINE pn_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE pn_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

END MODULE mshPhysicalNames_Class
