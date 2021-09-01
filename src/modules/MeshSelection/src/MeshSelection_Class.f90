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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

MODULE MeshSelection_Class
USE easifemBase
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE HDF5File_Class, ONLY: HDF5File_
USE Domain_Class, ONLY: Domain_
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MESHSELECTION_CLASS"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

TYPE :: MeshSelection_
  PRIVATE
  LOGICAL( LGT ), PUBLIC :: isInitiated = .FALSE.
  LOGICAL( LGT ), PUBLIC :: isSelectionByMeshID = .FALSE.
  LOGICAL( LGT ), PUBLIC :: isSelectionByElemID = .FALSE.
  LOGICAL( LGT ), PUBLIC :: isSelectionByBox = .FALSE.
  TYPE( IntVector_ ) :: PointMeshID
  !! It denotes the IDs of mesh which has xidim = 0 (point-mesh)
  TYPE( IntVector_ ) :: CurveMeshID
  !! It denotes the IDs of mesh which has xidim = 1 (curve-mesh)
  TYPE( IntVector_ ) :: SurfaceMeshID
  !! It denotes the IDs of mesh which has xidim = 2 (surface-mesh)
  TYPE( IntVector_ ) :: VolumeMeshID
  !! It denotes the IDs of mesh which has xidim = 3 (volume-mesh)
  TYPE( IntVector_ ) :: PointElemNum
  TYPE( IntVector_ ) :: CurveElemNum
  TYPE( IntVector_ ) :: SurfaceElemNum
  TYPE( IntVector_ ) :: VolumeElemNum
  !! Element number sorted based on xiDim of mesh
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => meshSelect_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => meshSelect_Initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => meshSelect_DeallocateData
  FINAL :: meshSelect_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Add => meshSelect_Add
  PROCEDURE, PUBLIC, PASS( obj ) :: Set => meshSelect_Set
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => meshSelect_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => meshSelect_Export
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => meshSelect_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: getMeshID => meshSelect_getMeshID
  PROCEDURE, PUBLIC, PASS( obj ) :: getElemNum => meshSelect_getElemNum
END TYPE MeshSelection_

PUBLIC :: MeshSelection_

TYPE( MeshSelection_ ), PUBLIC, PARAMETER :: TypeMeshSelection = &
  & MeshSelection_( )

!----------------------------------------------------------------------------
!                                                     MeshSelectionPointer_
!----------------------------------------------------------------------------

TYPE :: MeshSelectionPointer_
  CLASS( MeshSelection_ ), POINTER :: ptr => NULL()
END TYPE MeshSelectionPointer_

PUBLIC :: MeshSelectionPointer_

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Add surrogate to the module exceptionHandler_

INTERFACE
MODULE SUBROUTINE meshSelect_addSurrogate( obj, UserObj )
  CLASS( MeshSelection_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE meshSelect_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Initiate an instance of `MeshSelection`

INTERFACE
MODULE SUBROUTINE meshSelect_Initiate( obj, isSelectionByMeshID, &
  & isSelectionByElemID, isSelectionByBox )
  CLASS( MeshSelection_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isSelectionByMeshID
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isSelectionByElemID
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isSelectionByBox
END SUBROUTINE meshSelect_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine deallocates the data stored inside the instance

INTERFACE
MODULE SUBROUTINE meshSelect_DeallocateData( obj )
  CLASS( MeshSelection_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshSelect_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This is finalizer for instance of meshSelection

INTERFACE
MODULE SUBROUTINE meshSelect_Final( obj )
  TYPE( MeshSelection_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshSelect_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection

INTERFACE
MODULE SUBROUTINE meshSelect_Add( obj, dom, xidim, meshID, box, elemNum )
  CLASS( MeshSelection_ ), INTENT( INOUT ) :: obj
  TYPE( Domain_ ), OPTIONAL, INTENT( IN ) :: dom
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: xidim
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: meshID( : )
  TYPE( BoundingBox_ ), OPTIONAL, INTENT( IN ) :: box
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: elemNum( : )
END SUBROUTINE meshSelect_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection

INTERFACE
MODULE SUBROUTINE meshSelect_Set( obj )
  CLASS( MeshSelection_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshSelect_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by import

INTERFACE
MODULE SUBROUTINE meshSelect_Import( obj, hdf5, group, dom )
  CLASS( MeshSelection_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), OPTIONAL, INTENT( IN ) :: dom
END SUBROUTINE meshSelect_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Export
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by Export

INTERFACE
MODULE SUBROUTINE meshSelect_Export( obj, hdf5, group )
  CLASS( MeshSelection_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE meshSelect_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by Export

INTERFACE
MODULE SUBROUTINE meshSelect_Display( obj, msg, unitNo )
  CLASS( MeshSelection_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE meshSelect_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getMeshID
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
MODULE PURE FUNCTION meshSelect_getMeshID( obj, xidim ) RESULT( Ans )
  CLASS( MeshSelection_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), ALLOCATABLE :: ans(:)
END FUNCTION meshSelect_getMeshID
END INTERFACE


!----------------------------------------------------------------------------
!                                                                 getElemNum
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
MODULE PURE FUNCTION meshSelect_getElemNum( obj, xidim ) RESULT( Ans )
  CLASS( MeshSelection_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), ALLOCATABLE :: ans(:)
END FUNCTION meshSelect_getElemNum
END INTERFACE

END MODULE MeshSelection_Class