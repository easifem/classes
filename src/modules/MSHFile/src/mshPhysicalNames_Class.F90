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

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This module defines a data type for handling gmsh physical names

MODULE mshPhysicalNames_Class
USE BaseType
USE GlobalData
USE TxtFile_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE
TYPE( ExceptionHandler_ ) :: e
CHARACTER( LEN = * ), PARAMETER :: modName = "MSHPHYSICALNAMES_CLASS"
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!                                                          mshPhysicalNames_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This data type contains the Physical Names generate by gmsh

TYPE :: mshPhysicalNames_
  PRIVATE
  LOGICAL( LGT ), PUBLIC :: isInitiated = .FALSE.
    !! True if mshPhysicalNames_ is initiated.
  INTEGER( I4B ), ALLOCATABLE :: NSD( : )
    !! spatial dimension of each physical group
  INTEGER( I4B ), ALLOCATABLE :: tag( : )
    !! Unique ID of each physical group
  INTEGER( I4B ), ALLOCATABLE :: numElements( : )
    !! Number of elements in each physical group
  INTEGER( I4B ), ALLOCATABLE :: numNodes( : )
    !! Number of nodes in each physical nodes
  TYPE( IntVector_ ), ALLOCATABLE :: entities( : )
    !! Tags of Entities in each physical group
  TYPE( String ), ALLOCATABLE :: physicalName( : )
    !! Physical name of each physical group
  CONTAINS
    PRIVATE
    FINAL :: pn_final
      !! Finalizer
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => pn_deallocatedata
      !! To deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: GotoTag => pn_GotoTag
      !! Search tag for physical group in mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: Read => pn_Read
      !! Read contents from mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: Write => pn_Write
      !! Write contents to a mesh file
    PROCEDURE, PUBLIC, PASS( Obj ) :: Display => pn_Display
      !! Displays the content
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalPhysicalEntities => pn_getTotalPhysicalEntities
      !! Returns total number of physical entities, points, surface, volumes
    GENERIC, PUBLIC :: getIndex => pn_getIndex_a, pn_getIndex_b, &
      & pn_getIndex_c, pn_getIndex_d, pn_getIndex_e
    PROCEDURE, PASS( obj ) :: pn_getIndex_a
      !! Returns the index of a physical group
    PROCEDURE, PASS( obj ) :: pn_getIndex_b
      !! Returns the index of a physical group
    PROCEDURE, PASS( obj ) :: pn_getIndex_c
      !! Returns the index of a physical group
    PROCEDURE, PASS( obj ) :: pn_getIndex_d
      !! Returns the index of a physical group
    PROCEDURE, PASS( Obj ) :: pn_getIndex_e
    PROCEDURE, PUBLIC, PASS( Obj ) :: getPhysicalNames => pn_getPhysicalNames
      !! Returns the physical names
    PROCEDURE, PUBLIC, PASS( Obj ) :: getPhysicalTags => pn_getPhysicalTags
      !! Returns tags of physical entities
    PROCEDURE, PUBLIC, PASS( obj ) :: WhoAmI => pn_WhoAmI
      !! Enquire about "volume, surface, curve, point'
    PROCEDURE, PUBLIC, PASS( obj ) :: OutputFileName => pn_OutputFileName
      !! Returns output files
    PROCEDURE, PUBLIC, PASS( Obj ) :: AppendEntities => pn_AppendEntities
      !! Append entries to entities
    PROCEDURE, PUBLIC, PASS( Obj ) :: IncNumElements => pn_IncNumElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: IncNumNodes => pn_IncNumNodes
    PROCEDURE, PUBLIC, PASS( Obj ) :: getEntities => pn_getEntities
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNSD => pn_getNSD
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNumElements => pn_getNumElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNumNodes => pn_getNumNodes
    PROCEDURE, PUBLIC, PASS( Obj ) :: setNumNodes => pn_setNumNodes
END TYPE mshPhysicalNames_

PUBLIC :: mshPhysicalNames_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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
  CLASS( mshPhysicalNames_ ), POINTER :: ptr => NULL( )
END TYPE mshPhysicalNamesPointer_

PUBLIC :: mshPhysicalNamesPointer_

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE pn_final(obj)
  TYPE( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
END SUBROUTINE pn_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This subroutine deallocates the data stored in [[mshPhysicalNames_]]

INTERFACE
MODULE SUBROUTINE pn_deallocatedata( obj )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
END SUBROUTINE pn_deallocatedata
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE pn_deallocatedata
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                    GotoTag
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This suboutine find the tag for Physical groups in msh file

INTERFACE
MODULE SUBROUTINE pn_GotoTag( obj, mshFile, error )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  CLASS( TxtFile_ ), INTENT( INOUT ) :: mshFile
  INTEGER( I4B ), INTENT( INOUT ) :: error
END SUBROUTINE pn_GotoTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This subroutine reads physical groupds info from mesh file

INTERFACE
MODULE SUBROUTINE pn_Read( obj, mshFile, error )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
  CLASS( TxtFile_ ), INTENT( INOUT ) :: mshFile
  INTEGER( I4B ), INTENT( INOUT ) :: error
END SUBROUTINE pn_Read
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This subroutine writes physical groupds info in mesh file

INTERFACE
MODULE SUBROUTINE pn_Write( obj, mshFile, StartStr, EndStr )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
  CLASS( TxtFile_ ), INTENT( INOUT ) :: mshFile
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: StartStr, EndStr
END SUBROUTINE pn_Write
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This subroutine displays the content of [[mshphysicalnames_]]

INTERFACE
MODULE SUBROUTINE pn_Display( obj, Msg, UnitNo )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE pn_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE pn_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                  getTotalPhysicalEntities
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns total number of physical entities
!
!# Introduction
! 	Following usage
!
!### Usage
!
!```fortran
!	m = getTotalPhysicalEntities()
!	m = getTotalPhysicalEntities([0])
!	m = getTotalPhysicalEntities([1])
!	m = getTotalPhysicalEntities([2])
!	m = getTotalPhysicalEntities([3])
!	m = getTotalPhysicalEntities([0,1])
!	m = getTotalPhysicalEntities([0,1,2])
!	m = getTotalPhysicalEntities([0,1,3])
!
!```

INTERFACE
MODULE PURE FUNCTION pn_getTotalPhysicalEntities( obj, dim ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim(:)
  INTEGER( I4B ) :: ans
END FUNCTION pn_getTotalPhysicalEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getTotalPhysicalPoints
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns total number of physical points

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalPoints( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getTotalPhysicalCurves
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This function returns total number of physical curves

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalCurves( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalCurves
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getTotalPhysicalSurfaces
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns total number of physical surfaces

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalSurfaces( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalSurfaces
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getTotalPhysicalVolumes
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This function returns total number of physical volumes

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalVolumes( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalVolumes
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getIndex@mshPhysicalNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns index of a given physical name

INTERFACE
MODULE PURE FUNCTION pn_getIndex_a( obj, Name ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: ans
END FUNCTION pn_getIndex_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns index of a given physical name

INTERFACE
MODULE PURE FUNCTION pn_getIndex_b( obj, Name ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Name( : )
  INTEGER( I4B ) :: ans( SIZE( Name ) )
END FUNCTION pn_getIndex_b
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	20 June 2021
! summary: 	 This function returns index of a given physical name
!
!# Introduction
! This function returns index of a given physical name
!
! - `dim` denotes the XiDImension of physical entity
!     - 0 => point
!     - 1 => line
!     - 2 => surface
!     - 3 => volume
! - `tag` denotes the Physical Tag of physical entity

INTERFACE
MODULE PURE FUNCTION pn_getIndex_c( obj, dim, tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  INTEGER( I4B ) :: ans
END FUNCTION pn_getIndex_c
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	20 June 2021
! summary: 	This function returns index of a given physical name
!
!# Introduction
!
! This function returns index of a given physical name
!
! - `dim` denotes the XiDImension of physical entity
! - `dim` = 0 => returns indices of points
! - `dim` = 1 => returns indices of lines
! - `dim` = 2 => returns indices of surfaces
! - `dim` = 3 => returns indices of volumes

INTERFACE
MODULE PURE FUNCTION pn_getIndex_d( obj, dim ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), ALLOCATABLE  :: ans( : )
END FUNCTION pn_getIndex_d
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	20 June 2021
! summary: 	 This function returns index of a given physical name
!
!# Introduction
! This function returns index of a given physical name
!
! - `dim` denotes the XiDImension of physical entity
!     - 0 => point
!     - 1 => line
!     - 2 => surface
!     - 3 => volume
! - `tag` denotes the Physical TagS of physical entity

INTERFACE
MODULE PURE FUNCTION pn_getIndex_e( obj, dim, tag ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag( : )
  INTEGER( I4B ) :: ans( SIZE( tag ) )
END FUNCTION pn_getIndex_e
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getPhysicalNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 June 2021
! summary: This function returns the names of physical entities

INTERFACE
MODULE PURE FUNCTION pn_getPhysicalNames( obj, dim ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION pn_getPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalPointNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical poins

INTERFACE
MODULE PURE FUNCTION getPhysicalPointNames( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalPointNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalCurveNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical names

INTERFACE
MODULE PURE FUNCTION getPhysicalCurveNames( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalCurveNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPhysicalSurfaceNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical surface

INTERFACE
MODULE PURE FUNCTION getPhysicalSurfaceNames( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalSurfaceNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getPhysicalVolumeNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical volume

INTERFACE
MODULE PURE FUNCTION getPhysicalVolumeNames( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalVolumeNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getPhysicalTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This routine returns the physical tags of a physical entities

INTERFACE
MODULE PURE FUNCTION pn_getPhysicalTags( obj, dim ) RESULT( Ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_getPhysicalTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalPointTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical points

INTERFACE
MODULE PURE FUNCTION getPhysicalPointTags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalPointTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalCurveTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical curves

INTERFACE
MODULE PURE FUNCTION getPhysicalCurveTags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalCurveTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPhysicalSurfaceTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical surfaces

INTERFACE
MODULE PURE FUNCTION getPhysicalSurfaceTags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalSurfaceTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getPhysicalVolumeTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical volumes

INTERFACE
MODULE PURE FUNCTION getPhysicalVolumeTags( obj ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalVolumeTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    WhoAmI
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_WhoAmI( obj, I ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: I
  TYPE( String ) :: ans
END FUNCTION pn_WhoAmI
END INTERFACE

!----------------------------------------------------------------------------
!                                                            OutputFileName
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_OutputFileName( obj, mshFile, indx ) RESULT( ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  CLASS( TxtFile_ ), INTENT( IN ) :: mshFile
  INTEGER( I4B ), INTENT( IN ) :: indx
  TYPE( String ) :: ans
END FUNCTION pn_OutputFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                           AppendEntities
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE pn_AppendEntities( obj, indx, Entitytag )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: EntityTag( : ), indx
END SUBROUTINE pn_AppendEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                            IncNumElements
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE pn_IncNumElements( obj, indx, incr )
  CLASS( mshPhysicalNames_  ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: incr
END SUBROUTINE pn_IncNumElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                                getEntities
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_getEntities( obj, indx ) RESULT( Ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION pn_getEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                            IncNumNodes
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE pn_IncNumNodes( obj, indx, incr )
  CLASS( mshPhysicalNames_  ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: incr
END SUBROUTINE pn_IncNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getNSD
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_getNSD( obj ) RESULT( Ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_getNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getNumElements
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_getNumElements( obj ) RESULT( Ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_getNumElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                               getNumNodes
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pn_getNumNodes( obj ) RESULT( Ans )
  CLASS( mshPhysicalNames_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION pn_getNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                               setNumNodes
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE pn_setNumNodes( obj, indx, numNode )
  CLASS( mshPhysicalNames_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), INTENT( IN ) :: numNode
END SUBROUTINE pn_setNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE mshPhysicalNames_Class
