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
! date: 25 March 2021
! summary: `Mesh_Class` module contains data type for handling the mesh.

MODULE Mesh_Class
USE BaseType
USE GlobalData
USE ElementFactory
USE ExceptionHandler_Class
USE ElementPointerVector_Class, ONLY: ElementPointerVector_, &
  & ElementPointerIterator_
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
IMPLICIT NONE
PRIVATE
REAL( DFP ), PARAMETER :: default_factor = 1.5_DFP
CHARACTER( LEN = * ), PARAMETER :: modName = "MESH_CLASS"

!----------------------------------------------------------------------------
!                                                                 MeshData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	13 June 2021
! summary: 	This datatype contains the meta data of a mesh

TYPE :: MeshData_
  ! PRIVATE
  INTEGER( I4B ) :: uid = 0
    !! Unique id of the mesh
  INTEGER( I4B ) :: xidim = 0
    !! xidimension of elements present inside the mesh
  INTEGER( I4B ) :: elemType = 0
    !! type of element present inside the mesh
  INTEGER( I4B ) :: nsd = 0
    !! number of spatial dimension
  INTEGER( I4B ) :: maxNptrs = 0
    !! largest node number present inside the mesh
  INTEGER( I4B ) :: minNptrs = 0
    !! minimum node number present inside the mesh
  INTEGER( I4B ) :: tNodes = 0
    !! total number of nodes present iside the mesh
  INTEGER( I4B ) :: tIntNodes = 0
    !! total number of internal nodes inside the mesh
  INTEGER( I4B ) :: tElements = 0
    !! total number of elements present inside the mesh
  REAL( DFP ) :: minX = 0.0
    !! minimum value of x coordinate
  REAL( DFP ) :: maxX = 0.0
    !! maximum value of x coordinate
  REAL( DFP ) :: minY = 0.0
    !! minimum value of y coordinate
  REAL( DFP ) :: maxY = 0.0
    !! maximum value of y coordinate
  REAL( DFP ) :: minZ = 0.0
    !! minimum value of z coordinate
  REAL( DFP ) :: maxZ = 0.0
    !! maximum value of z coordinate
  REAL( DFP ) :: X = 0.0
    !! x coorindate of centroid
  REAL( DFP ) :: Y = 0.0
    !! y coordinate of centroid
  REAL( DFP ) :: Z = 0.0
    !! z coordinate of centroid
  LOGICAL( LGT ) :: isInitiated = .FALSE.
    !! logical flag denoting for whether mesh data is initiated or not
  CLASS( ReferenceElement_ ), POINTER :: refelem => NULL()
    !! Reference element of the mesh
  TYPE( ReferenceElement_ ), ALLOCATABLE :: FacetElements( : )
    !! Facet Elements in the reference element
  INTEGER( I4B ), ALLOCATABLE :: local_elemNumber( : )
    !! Local element number
  REAL( DFP ), ALLOCATABLE :: nodeCoord( :, : )
    !! Nodal coordinates
  INTEGER( I4B ), ALLOCATABLE :: physicalTag( : )
    !! physical entities associated with the entity
  INTEGER( I4B ), ALLOCATABLE :: elemNumber( : )
    !! element number present inside the mesh
  INTEGER( I4B ), ALLOCATABLE :: boundingEntity( : )
    !! uid of bounding entity
  INTEGER( I4B ), ALLOCATABLE :: connectivity( :, : )
    !! connectivity matrix for the mesh
    !! each column of connectivity matrix denotes the node number

  INTEGER( I4B ), ALLOCATABLE :: LBndyIndex( : )
    !! For a given element if `LBndyIndex(iel) .eq. 0` then `iel` is not a
    !! a boundary element else it a boundary element which represents the
    !! index of `iel` in `BoundaryData()`.

  INTEGER( I4B ), ALLOCATABLE :: Nptrs( :, : )
    !! Node number of mesh `Nptrs( 1 : maxNptrs, 2 )`
    !! The first column contains the global node number
    !! The second column contains the indicator, >0 or -1
    !! If >zero then it is an internal node, and it contains the
    !! information of local node number
    !! if <zero, then it is an boundary node, and it contains the
    !! information of local node number

  INTEGER( I4B ), ALLOCATABLE :: BoundaryNptrs( : )
    !! Node number of boundary of mesh
  INTEGER( I4B ), ALLOCATABLE :: InternalNptrs( : )
    !! Node number of internal nodes
  INTEGER( I4B ), ALLOCATABLE :: Local_Nptrs( : )
    !! This converts a given global node into local-node which can be
    !! used for accessing data inside `NodeToElem, NodeToNode`
  TYPE( IntVector_ ), ALLOCATABLE :: NodeToElem( : )
    !! `NodeToElem( iLocalNode )` denotes indx of elems in mesh which are
    !! directly connected to node `GlobalNptrs( iLocalNode )`
  TYPE( IntVector_ ), ALLOCATABLE :: ElemToElem( : )
    !! `ElemToElem( iel )` denotes data of elements
    !! connected to the element`iel`
  TYPE( IntVector_ ), ALLOCATABLE :: NTN( : )
    !! `NTN( iLocalNode )` denotes global node-ids that are connected
    !! to a `GlobalNode( iLocalNode )`
  TYPE( IntVector_ ), ALLOCATABLE :: BoundaryData( : )
    !! If `iel` is boundary element;
    !! then `Vec=BoundaryData( LBndyIndex(iel) ) `
    !! contains boundary data, where `vec(1)` is equal to `iel`, and
    !! `vec(2:)` are ids of local facets which are boundaries of mesh
  TYPE( IntVector_ ), ALLOCATABLE :: InternalBndyElemNum( : )
    !! To do
  TYPE( IntVector_ ), ALLOCATABLE :: InternalBoundaryData( : )
    !! To do
  TYPE( ExceptionHandler_ ) :: e
    !! Exception handler
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => meshData_DeallocateData
    Final :: meshData_Final
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalInternalNodes => meshData_getTotalInternalNodes
      !! Returns the total number of internal nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalNodes => meshData_getTotalNodes
      !! Returns the total number of nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundaryNodes => meshData_getTotalBoundaryNodes
      !! Returns the total number of boundary nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundaryElements => meshData_getTotalBoundaryElements
      !! Returns the total number of boundary element
    PROCEDURE, PUBLIC, PASS( obj ) :: getBoundingBox => meshData_getBoundingBox
      !! Returns the bounding box of the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getConnectivity => &
      & meshData_getConnectivity
      !! Returns  node numbers in an element
    PROCEDURE, PASS( obj ) :: meshData_getLocalNptrs_1
      !! Returns the local node number of a glocal node number
    PROCEDURE, PASS( obj ) :: meshData_getLocalNptrs_2
      !! Returns the local node number of a global node number
    GENERIC, PUBLIC :: getLocalNptrs => meshData_getLocalNptrs_1, &
      & meshData_getLocalNptrs_2
      !! Returns the local node number of a global node number
    PROCEDURE, PASS( obj ) :: meshData_getGlobalNptrs_1
      !! Returns the global node number of a local node number
    PROCEDURE, PASS( obj ) :: meshData_getGlobalNptrs_2
      !! Returns the global node number of a local node number
    GENERIC, PUBLIC :: getGlobalNptrs => meshData_getGlobalNptrs_1, &
      & meshData_getGlobalNptrs_2
    PROCEDURE, PASS( obj ) :: meshData_getGlobalElemNumber_1
    PROCEDURE, PASS( obj ) :: meshData_getGlobalElemNumber_2
    GENERIC, PUBLIC :: getGlobalElemNumber => &
      & meshData_getGlobalElemNumber_1, meshData_getGlobalElemNumber_2
      !! Returns the global element number for a local element number
    PROCEDURE, PASS( obj ) :: meshData_getLocalElemNumber_1
    PROCEDURE, PASS( obj ) :: meshData_getLocalElemNumber_2
    GENERIC, PUBLIC :: getLocalElemNumber => &
      & meshData_getLocalElemNumber_1, meshData_getLocalElemNumber_2
      !! Returns the local element number of a global element number
    PROCEDURE, PUBLIC, PASS( Obj ) :: isLocalElementNumbersInitiated => &
      & meshData_isLocalElementNumbersInitiated
      !! Returns true if LocalElements initiated
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodePresent => meshData_isNodePresent
      !! Returns true if a node number is present
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodeToNodesInitiated => &
      & meshData_isNodeToNodesInitiated
      !! returns `.true.` if `NToN` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodeToElementsInitiated => &
      & meshData_isNodeToElementsInitiated
      !! returns `.true.` if `NodeToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isElementToElementsInitiated => &
      & meshData_isElementToElementsInitiated
      !! returns `.true.` if `ElemToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isConnectivityInitiated => &
      & meshData_isConnectivityInitiated
      !! returns `.true.` if `Connectivity` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryDataInitiated => &
      & meshData_isBoundaryDataInitiated
      !! returns `.true.` if `BoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isInternalNptrsInitiated => &
      & meshData_isInternalNptrsInitiated
      !! returns `.true.` if `InternalNptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryNptrsInitiated => &
      & meshData_isBoundaryNptrsInitiated
    PROCEDURE, PUBLIC, PASS( obj ) :: isLocalNptrsInitiated => &
      & meshData_isLocalNptrsInitiated
      !! returns `.true.` if `Local_Nptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isInternalBoundaryDataInitiated => &
      & meshData_isInternalBoundaryDataInitiated
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateLocalNptrs => meshData_InitiateLocalNptrs
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNptrs => meshData_InitiateNptrs
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateLocalElementNumbers => &
      & meshData_InitiateLocalElementNumbers
      !! returns `.true.` if `InternalBoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNodeToElements => &
      & meshData_InitiateNodeToElements
      !! Initiate node to node data
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToElements => &
      & meshData_getNodeToElements
      !! Returns the element attached to a given global node number
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNodeToNodes => &
      & meshData_InitiateNodetoNodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToNodes => &
      & meshData_getNodeToNodes
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateElementToElements => &
      & meshData_InitiateElementToElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: getElementToElements => &
      & meshData_getElementToElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateBoundaryData => &
      & meshData_InitiateBoundaryData
END TYPE MeshData_

!----------------------------------------------------------------------------
!                                                                      Mesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Data type for mesh
!
!{!pages/Mesh.md}

TYPE :: Mesh_
  PRIVATE
  LOGICAL( LGT ) :: readFromFile = .TRUE.
  TYPE( ElementPointerVector_ ), PUBLIC :: list
    !! A dynamic vector of element pointer
  TYPE( MeshData_ ), PUBLIC :: MeshData
    !! Mesh data
  TYPE( ExceptionHandler_ ), PUBLIC :: e
    !! Exception handler for mesh
  CONTAINS
    PRIVATE
    PROCEDURE, PASS( obj ) :: Read => mesh_Read
      !! Read mesh from mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => mesh_initiate
      !! Allocate size of a mesh
    FINAL :: mesh_final
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mesh_DeallocateData
      !! Deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => mesh_display
      !! Display the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: Prune => mesh_PruneMesh
      !! Check the mesh, clean the broken link
    PROCEDURE, PUBLIC, PASS( obj ) :: Pushback => mesh_Pushback
      !! Append an element to a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: SetElement => mesh_SetElement
      !! Set an element to a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getElementPointer => mesh_getElementPointer
      !! Get Pointer to an element in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: RemoveElement => mesh_RemoveElement
      !! Remove an element from a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => mesh_size
      !! returns the SIZE of mesh
    ! PROCEDURE, PUBLIC, PASS( obj ) :: getNptrs => mesh_getNptrs
    !   !! Get node numbers in a mesh
    ! PROCEDURE, PUBLIC, PASS( obj ) :: setMaterialType => mesh_setMaterialType
    !   !! Set material type of a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: GenerateMeshData => mesh_GenerateMeshData
END TYPE Mesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: Mesh_
TYPE( Mesh_ ), PARAMETER, PUBLIC :: TypeMesh = Mesh_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
! `MeshPointer_` is a userdefine datatype which contains the pointer to
! a mesh
TYPE :: MeshPointer_
  TYPE( Mesh_ ), POINTER :: Ptr => NULL( )
END TYPE MeshPointer_

PUBLIC :: MeshPointer_

!----------------------------------------------------------------------------
!                                                          Read@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mesh_Read( obj, meshFile, xidim, id )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), INTENT( IN ) :: id
END SUBROUTINE mesh_Read
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Allocate the size of the mesh
!
!### Introduction
!
! Allocate the size of the mesh. Generic name ---> Initiate(). Param is [[ParameterList_]], it should contain:
! - `nsd`
! - `size`
!
!### Usage
!
!```fortran
! call obj % initiate( N )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_initiate( obj, meshFile, xidim, id )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! mesh object
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
    !! Mesh file
  INTEGER( I4B ), INTENT( IN ) :: xidim
    !! Xidimension of the mesh
  INTEGER( I4B ), INTENT( IN ) :: id
    !! Id of mesh
END SUBROUTINE mesh_initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Mesh@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Function for constructing the instance of [[Mesh_]]
!
!### Introduction
!
!  Function for constructing the instance of [[Mesh_]]
!
!### Usage
!
!```fortran
!obj = Mesh( NSD = 2, tELements = 10 )
!```end fortran

INTERFACE
MODULE FUNCTION Constructor1( meshFile, xidim, id ) RESULT( ans )
  TYPE( Mesh_ ) :: ans
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), INTENT( IN ) :: id
END FUNCTION Constructor1
END INTERFACE

!>
! Generic function for constructing [[mesh_]]
INTERFACE Mesh
  MODULE PROCEDURE Constructor1
END INTERFACE Mesh

PUBLIC :: Mesh

!----------------------------------------------------------------------------
!                                                   Mesh_Pointer@MeshMethods
!----------------------------------------------------------------------------

!> authos: Dr Vikas Sharma
!
!  Function for constructing pointer to the instance of [[Mesh_]] datatype
!
!### Usage
!
!```fortran
! class( mesh_ ), pointer :: obj
! obj => mesh_pointer( NSD = 2, tELements = 10 )
!```end fortran

INTERFACE
MODULE FUNCTION Constructor_1( meshFile, xidim, id ) RESULT( ans )
  CLASS( Mesh_ ), POINTER :: ans
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), INTENT( IN ) :: id
END FUNCTION Constructor_1
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Generic function for constructing pointer to the instance of [[mesh_]] datatype

INTERFACE Mesh_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE Mesh_Pointer

PUBLIC :: Mesh_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Free up the memory stored in [[mesh_]]
!
!### Introduction
!
! Free up the memory stored in [[mesh_]] data type
!
!### Usage
!
!```fortran
!call obj%deallocateData( )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_DeallocateData( obj )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
END SUBROUTINE mesh_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE mesh_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mesh_final( obj )
  TYPE( Mesh_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mesh_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	12 June 2021
! summary: 	Displays the content of [[mesh_]] datatype
!
!### Introduction
! 	This routine displays the content of [[mesh_]] datatype
!
!### Usage
!
!```fortran
!	call display( obj, 'mesh', stdout )
!	call obj%display( 'mesh', stdout )
!```

INTERFACE
MODULE SUBROUTINE mesh_display( obj, msg, UnitNo )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
    !! message on screen
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
    !! unit number of ouput file
END SUBROUTINE mesh_display
END INTERFACE

!>
! generic routine to display content of mesh
INTERFACE Display
  MODULE PROCEDURE mesh_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                            Prune@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Prune mesh object
!
!### Introduction
!
! This subroutine prune the mesh, that is, removing any broken links.
!
!@note
! This routine runs through the element array and counts element pointers
! that are associated, and return the total number of associated elements.
! Therefore, it should be called only after appending/removing an element
! from the mesh. This routine also check for broken links and remove them.
!@endnote
!
!### Usage
!
!```fortran
!call obj%prune( )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_PruneMesh( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object which will be pruned
END SUBROUTINE mesh_PruneMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                 AppendElement@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Append an element to the mesh
!
!### Introduction
!
!  Append an element, and increase the total elements in mesh by one
!
!### Usage
!```fortran
! call obj%pushBack( Elem )
!```


INTERFACE
MODULE SUBROUTINE mesh_Pushback( obj, Elem )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh obj
  CLASS( Element_ ), TARGET, INTENT( INOUT ) :: Elem
    !! finite element to be added
END SUBROUTINE mesh_Pushback
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetElement@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Set an element to a mesh
!
!### Introduction
!
! Seting element; total number of elements remain same
! Size of mesh should be sufficient while using this.
!
!### Usage
!
!```fortran
! call obj % setElement( Elem )
!```

INTERFACE
MODULE SUBROUTINE mesh_SetElement( obj, Elem, iel )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! Mesh object which will be modified
  CLASS( Element_ ), TARGET, INTENT( INOUT ) :: Elem
    !! Finite element to be set inside the mesh
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
END SUBROUTINE mesh_SetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ElementPointer@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	12 June 2021
! summary: Return the pointer to an element `obj % Elem(iel)`
!
!### Introduction
!
! Return the pointer to an element `obj % Elem(iel)`
!
! @warning
! make sure `iel` should be less that `obj%telements`
! @endwarning
!
!### Usage
!
!```fortran
! class( element_ ), pointer :: elem
! elem => obj % ElementPointer( iel )
!```

INTERFACE
MODULE FUNCTION mesh_getElementPointer( obj, iel ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  CLASS( Element_ ), POINTER :: ans
    !! pointer to finite element
END FUNCTION mesh_getElementPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                 RemoveElement@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 June 2021
! summary: Remove an element from the mesh
!
!### Introduction
! Remove an element from the mesh. If you want to free the memory occupied with the entry which is going to be removed then set `freeMem` to `.TRUE.`, otherwise set it to `.FALSE.`.
!
!### Usage
!
!```fortran
! call obj % removeElement( iel = iel, freeMem= )
!```

INTERFACE
MODULE SUBROUTINE mesh_RemoveElement( obj, iel, freeMem )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem
END SUBROUTINE mesh_RemoveElement
END INTERFACE

!----------------------------------------------------------------------------
!                                              getTotalElements@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 June 2021
! summary: Returns total elements in the mesh
!
!### Introduction
!
! Returns the total number of elements in the mesh.
!
!### Usage
!
!```fortran
!	telem = obj % SIZE( obj )
!```

INTERFACE
MODULE FUNCTION mesh_size( obj ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh object
  INTEGER( I4B ) :: ans
END FUNCTION mesh_size
END INTERFACE
! !----------------------------------------------------------------------------
! !                                                        getNptrs@MeshMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! Returns the node numbers in mesh

! !> authors: Dr. Vikas Sharma
! !
! ! Returns the node numbers in mesh
! !
! !### Usage
! !
! !```fortran
! !	call obj % getNptrs( Nptrs )
! !```

! MODULE SUBROUTINE mesh_getNptrs( obj, Nptrs )
!   CLASS( Mesh_ ), INTENT( INOUT ) :: obj
!     !! mesh object
!   INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Nptrs( : )
!     !! node numbers
! END SUBROUTINE mesh_getNptrs
! END INTERFACE

!----------------------------------------------------------------------------
!                                          GenerateMeshData@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mesh_GenerateMeshData( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mesh_GenerateMeshData
END INTERFACE

!----------------------------------------------------------------------------
!                                             DeallocateData@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	13 June 2021
! summary: 	Deallocate data stored inside [[meshdata_]]
!
!### Introduction
! 	This routine clears the space occupied by the mesh data object. It will
! Deallocate all the arrays and set scalar to zero value.
!
!### Usage
!
! ```fortran
!	call deallocateData( obj )
! obj%DeallocateData()
! ```

INTERFACE
MODULE SUBROUTINE meshData_DeallocateData( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data object
END SUBROUTINE meshData_DeallocateData
END INTERFACE

!>
! Generic subroutine to deallocate data stored inside [[MeshData_]]
INTERFACE DeallocateData
  MODULE PROCEDURE meshData_DeallocateData
END INTERFACE DeallocateData

!----------------------------------------------------------------------------
!                                                      Final@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE meshData_Final( obj )
  TYPE( MeshData_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshData_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE meshData_InitiateLocalNptrs( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshData_InitiateLocalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                              InitiateNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE meshData_InitiateNptrs( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshData_InitiateNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                      getTotalInternalNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_getTotalInternalNodes( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION meshData_getTotalInternalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                             getTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_getTotalNodes( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION meshData_getTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                     getTotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_getTotalBoundaryNodes( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION meshData_getTotalBoundaryNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  getTotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_getTotalBoundaryElements( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION meshData_getTotalBoundaryElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            getBoundingBox@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_getBoundingBox( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  TYPE( BoundingBox_ ) :: ans
END FUNCTION meshData_getBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                          getConnectivity@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: This routine returns node number in an element

INTERFACE
MODULE PURE FUNCTION meshData_getConnectivity( obj, iel ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iel
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION meshData_getConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                          getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local node number from global node num
!
!### Introduction
!
! This function returns the local node numbers from global node numbers.

INTERFACE
MODULE PURE FUNCTION meshData_getLocalNptrs_1( obj, GlobalNode ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  INTEGER( I4B ) :: Ans( SIZE( GlobalNode ) )
END FUNCTION meshData_getLocalNptrs_1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the local node number from a global node number

INTERFACE
MODULE PURE FUNCTION meshData_getLocalNptrs_2( obj, GlobalNode ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  INTEGER( I4B ) :: Ans
END FUNCTION meshData_getLocalNptrs_2
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
MODULE PURE FUNCTION meshData_getGlobalNptrs_1( obj, LocalNode ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalNode( : )
  INTEGER( I4B ) :: Ans( SIZE( LocalNode ) )
END FUNCTION meshData_getGlobalNptrs_1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
MODULE PURE FUNCTION meshData_getGlobalNptrs_2( obj, LocalNode ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalNode
  INTEGER( I4B ) :: Ans
END FUNCTION meshData_getGlobalNptrs_2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
MODULE PURE FUNCTION meshData_getGlobalElemNumber_1( obj, LocalElem ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalElem( : )
  INTEGER( I4B ) :: Ans( SIZE( LocalElem ) )
END FUNCTION meshData_getGlobalElemNumber_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
MODULE PURE FUNCTION meshData_getGlobalElemNumber_2( obj, LocalElem ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalElem
  INTEGER( I4B ) :: Ans
END FUNCTION meshData_getGlobalElemNumber_2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getLocalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
MODULE PURE FUNCTION meshData_getLocalElemNumber_1( obj, GlobalElem ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElem( : )
  INTEGER( I4B ) :: Ans( SIZE( GlobalElem ) )
END FUNCTION meshData_getLocalElemNumber_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       getLocalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
MODULE PURE FUNCTION meshData_getLocalElemNumber_2( obj, GlobalElem ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElem
  INTEGER( I4B ) :: Ans
END FUNCTION meshData_getLocalElemNumber_2
END INTERFACE

!----------------------------------------------------------------------------
!                             isLocalElementNumbersInitiated@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns true if LocalElementNumbers data initiated

INTERFACE
MODULE PURE FUNCTION meshData_isLocalElementNumbersInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isLocalElementNumbersInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodePresent@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: Returns  true if given global node number is present

INTERFACE
MODULE PURE FUNCTION meshData_isNodePresent( obj, GlobalNode ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                    isNodeToNodesInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isNodeToNodesInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isNodeToNodesInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isNodeToElementsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isNodeToElementsInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isNodeToElementsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isElementToElementsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isElementToElementsInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isElementToElementsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                   isConnectivityInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isConnectivityInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isConnectivityInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isBoundaryDataInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isBoundaryDataInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isBoundaryDataInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isInternalNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isInternalNptrsInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isInternalNptrsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isBoundaryNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isBoundaryNptrsInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isBoundaryNptrsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isLocalNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isLocalNptrsInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isLocalNptrsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                            isInternalBoundaryDataInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION meshData_isInternalBoundaryDataInitiated( obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION meshData_isInternalBoundaryDataInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateLocalElementNumbers@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Initiate local element numbers

INTERFACE
MODULE SUBROUTINE meshData_InitiateLocalElementNumbers( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
END SUBROUTINE meshData_InitiateLocalElementNumbers
END INTERFACE

!----------------------------------------------------------------------------
!                                      initiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	generate Elements surrounding a node mapping
!
!### Introduction
!
! This subroutine generate Elements surrounding a node mapping
! - The mapping is stored in the array called `NodeToElem`
! - `NodeToElem` is a vector of type [[IntVector_]]
! - The size of `NodeToElem` vector is same as `obj%Nptrs`, that is total number of nodes in the mesh
! - In `NodeToElem(i)`, i denotes the local node number, and it will give the local element number surrounding the local node `i`
! - Always use method called `getNodeToElements()` to access this information
!
! @warning
! Always use the mapping between global node number and local node number to
! avoid segmentation fault
! @endwarning
!
!### Usage
!
! ```fortran
!	call obj%initiateNodeToElements()
! ```

INTERFACE
MODULE SUBROUTINE meshData_InitiateNodeToElements( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE meshData_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                         getNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: Returns the element numbers which are connected to the a global node
!

INTERFACE
MODULE FUNCTION meshData_getNodeToElements( obj, GlobalNode ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
    !! global node number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! A vector of local element number
END FUNCTION meshData_getNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateNodeToNode@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate node to node connectivity data

INTERFACE
MODULE SUBROUTINE meshData_InitiateNodetoNodes( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE meshData_InitiateNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                           getNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: 	Returns the node surrounding a node

INTERFACE
MODULE FUNCTION meshData_getNodeToNodes( obj, GlobalNode, IncludeSelf ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ), INTENT( IN ) :: IncludeSelf
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION meshData_getNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate boundary data

INTERFACE
MODULE SUBROUTINE meshData_InitiateElementToElements( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE meshData_InitiateElementToElements
END INTERFACE


!----------------------------------------------------------------------------
!                                      getElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns element to element connectivity information
!
!### Introduction
! This routine returns element to element connectivity information for a given local element number `iel`
!
! If OnlyElements is absent or it is set to FALSE then, this routine returns
! **full information** about elements surrounding
! element `iel`
! - Each Row of `ans` denotes the element to which `iel` is connected to
! - Column-1 of `ans` denotes element number
! - Column-2 denotes the local face number of element `iel`, and
! - Column-3 denotes the local face number of element whose element number is
! given in the column-1
!
! If `OnlyElements` is present and it is TRUE then, this routine returns only the information of elements connectved ot an element.
!

INTERFACE
MODULE PURE FUNCTION meshData_getElementToElements( obj, iel, onlyElements ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: onlyElements
    !! If onlyElements is absent or it is FALSE then full information about the elements connected to element iel is given
    !! If onlyElements is present and it is TRUE then only the information about the elements connected to element iel is given
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
    !! list of elements surrounding elements
END FUNCTION meshData_getElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateBoundaryData@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate boundary data

INTERFACE
MODULE SUBROUTINE meshData_InitiateBoundaryData( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE meshData_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Mesh_Class
