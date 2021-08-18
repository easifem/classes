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
!
!{!pages/MeshData.md}
!{!pages/Mesh.md}

MODULE Mesh_Class
USE BaseType
USE GlobalData
USE ElementFactory
USE ExceptionHandler_Class
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MESH_CLASS"
TYPE( ExceptionHandler_ ), PUBLIC :: eMesh
INTEGER( I4B ), PARAMETER :: eUnitNo = 1003
CHARACTER( LEN = * ), PARAMETER :: eLogFile = "MESH_CLASS_EXCEPTION.txt"
  !! Exception handler
INTEGER( I4B ), PARAMETER, PUBLIC :: INTERNAL_NODE = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: BOUNDARY_NODE = -1
INTEGER( I4B ), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_NODE = -2
INTEGER( I4B ), PARAMETER, PUBLIC :: GHOST_NODE = -3
INTEGER( I4B ), PARAMETER, PUBLIC :: INTERNAL_ELEMENT = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: BOUNDARY_ELEMENT = -1
INTEGER( I4B ), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_ELEMENT = -2
INTEGER( I4B ), PARAMETER, PUBLIC :: GHOST_ELEMENT = -3

!----------------------------------------------------------------------------
!                                                                 NodeData_
!----------------------------------------------------------------------------

TYPE :: NodeData_
  INTEGER( I4B ) :: globalNodeNum = 0
  INTEGER( I4B ) :: localNodeNum = 0
  INTEGER( I4B ), ALLOCATABLE :: globalNodes(:)
    !! It contains the global node number surrouding an element
    !! It does not contain self global node number
  INTEGER( I4B ), ALLOCATABLE :: globalElements(:)
  INTEGER( I4B ) :: nodeType = INTERNAL_NODE
END TYPE NodeData_

!----------------------------------------------------------------------------
!                                                                 NodeData_
!----------------------------------------------------------------------------

TYPE :: ElemData_
  INTEGER( I4B ) :: globalElemNum = 0
  INTEGER( I4B ) :: localElemNum = 0
  INTEGER( I4B ), ALLOCATABLE :: globalNodes(:)
  INTEGER( I4B ), ALLOCATABLE :: globalElements(:)
    !! Contains the information about the element surrounding an element
    !! Lets us say that globalElem1, globalElem2, globalElem3 surrounds a
    !! local element ielem (its global element number is globalElem), then
    !! globalElements( [1,2,3] ) contains globalElem1, pFace, nFace
    !! globalElements( [4,5,6] ) contains globalElem2, pFace, nFace
    !! globalElements( [7,8,9] ) contains globalElem3, pFace, nFace
    !! Here, pFace is the local facet number of parent element globalElem (ielem) which is connected to the nFace of the neighbor element
    !! All element numbers are global element number
  INTEGER( I4B ), ALLOCATABLE :: boundaryData(:)
    !! If `iel` is boundary element;
    !! then `Vec=BoundaryData( LBndyIndex(iel) ) `
    !! contains boundary data, where `vec(1)` is equal to `iel`, and
    !! `vec(2:)` are ids of local facets which are boundaries of mesh
  INTEGER( I4B ) :: elementType = INTERNAL_ELEMENT
END TYPE ElemData_

!----------------------------------------------------------------------------
!                                                                 Mesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	13 June 2021
! summary: 	This datatype contains the meta data of a mesh
!
!{!pages/Mesh.md}

TYPE :: Mesh_
  PRIVATE
  LOGICAL( LGT ) :: readFromFile = .TRUE.
    !! True if the mesh is read from a file
  LOGICAL( LGT ) :: isInitiated = .FALSE.
    !! logical flag denoting for whether mesh data is initiated or not
  LOGICAL( LGT ) :: isNodeToElementsInitiated = .FALSE.
    !! Node to elements mapping
  LOGICAL( LGT ) :: isNodeToNodesInitiated = .FALSE.
    !! Node to nodes mapping
  LOGICAL( LGT ) :: isElementToElementsInitiated = .FALSE.
    !! Element to elements mapping
  LOGICAL( LGT ) :: isBoundaryDataInitiated = .FALSE.
    !! Boundary data
  INTEGER( I4B ) :: uid = 0
    !! Unique id of the mesh
  INTEGER( I4B ) :: xidim = 0
    !! xidimension of elements present inside the mesh
  INTEGER( I4B ) :: elemType = 0
    !! type of element present inside the mesh
  INTEGER( I4B ) :: nsd = 0
    !! number of spatial dimension of the mesh
  INTEGER( I4B ), PUBLIC :: maxNptrs = 0
    !! largest node number present inside the mesh
  INTEGER( I4B ), PUBLIC :: minNptrs = 0
    !! minimum node number present inside the mesh
  INTEGER( I4B ), PUBLIC :: maxElemNum = 0
    !! largest element number present inside the mesh
  INTEGER( I4B ), PUBLIC :: minElemNum = 0
    !! minimum element number present inside the mesh
  INTEGER( I4B ) :: tNodes = 0
    !! total number of nodes present inside the mesh
  INTEGER( I4B ) :: tIntNodes = 0
    !! total number of internal nodes inside the mesh
  INTEGER( I4B ) :: tElements = 0
    !! total number of elements present inside the mesh
    !! It is the size of elemNumber vector
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
  INTEGER( I4B ), ALLOCATABLE :: physicalTag( : )
    !! Physical entities associated with the current entity (mesh)
  INTEGER( I4B ), ALLOCATABLE :: boundingEntity( : )
    !! Bounding entity numbers of the current entity
  INTEGER( I4B ), ALLOCATABLE :: local_elemNumber( : )
    !! List of local element numbers, the lowerbound is `minElemNum`
    !! and upper bound is `maxElemNum`. In this way, local_elemNumber(iel)
    !! returns the local element number of global element number iel.
  INTEGER( I4B ), ALLOCATABLE :: Local_Nptrs( : )
    !! Returns local node number from a global node number
    !! Its length is from 1 to maxNptrs
    !! Helpul in finding if a global node is present inside the mesh or not
  CLASS( ReferenceElement_ ), POINTER :: refelem => NULL()
    !! Reference element of the mesh
  TYPE( ReferenceElement_ ), ALLOCATABLE :: FacetElements( : )
    !! Facet Elements in the reference element
  TYPE( NodeData_ ), ALLOCATABLE :: nodeData( : )
  TYPE( ElemData_ ), ALLOCATABLE :: elementData( : )
  CONTAINS
    PRIVATE
    PROCEDURE, PASS( obj ) :: Import => mesh_Import
      !! Read mesh from hdf5 file
    ! PROCEDURE, PUBLIC, PASS( obj ) :: Export => mesh_Export
      !! Export mesh to an hdf5 file
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => mesh_display
      !! Display the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => mesh_initiate
      !! Allocate size of a mesh
    FINAL :: mesh_final
    PROCEDURE, PASS( obj ) :: InitiateNodeToElements => &
      & mesh_InitiateNodeToElements
      !! Initiate node to node data
    PROCEDURE, PASS( obj ) :: InitiateNodeToNodes => &
      & mesh_InitiateNodetoNodes
      !! Initiate Node to nodes mapping
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateElementToElements => &
      & mesh_InitiateElementToElements
      !! Initiate element to elements mapping
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateBoundaryData => &
      & mesh_InitiateBoundaryData
      !! Initiate boundary data
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mesh_DeallocateData
      !! Deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryNode => &
      & mesh_isBoundaryNode
      !! Returns true if a given global node number is a boundary node
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryElement => &
      & mesh_isBoundaryElement
      !! Returns true if a given global element number is a boundary element
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodePresent => &
      & mesh_isNodePresent
      !! Returns true if a node number is present
    PROCEDURE, PUBLIC, PASS( obj ) :: isElementPresent => &
      & mesh_isElementPresent
      !! Returns true if a given element number is present
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => mesh_size
      !! Returns the size of the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getRefElemPointer => mesh_getRefElemPointer
      !! Returns pointer to the reference element
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalElements => mesh_size
      !! Returns the size of the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getBoundingEntity => &
      & mesh_getBoundingEntity
      !! Returns the nodal coordinates
    PROCEDURE, PUBLIC, PASS( obj ) :: getNptrs => mesh_getNptrs
      !! Returns the node number of mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getInternalNptrs => &
      & mesh_getInternalNptrs
      !! Returns a vector of internal node numbers
    PROCEDURE, PUBLIC, PASS( obj ) :: getBoundaryNptrs => &
      & mesh_getBoundaryNptrs
      !! Returns a vector of boundary node numbers
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalInternalNodes => mesh_getTotalInternalNodes
      !! Returns the total number of internal nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalNodes => mesh_getTotalNodes
      !! Returns the total number of nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundaryNodes => mesh_getTotalBoundaryNodes
      !! Returns the total number of boundary nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundaryElements => mesh_getTotalBoundaryElements
      !! Returns the total number of boundary element
    PROCEDURE, PUBLIC, PASS( obj ) :: getBoundingBox => mesh_getBoundingBox
      !! Returns the bounding box of the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getConnectivity => &
      & mesh_getConnectivity
      !! Returns  node numbers in an element
    PROCEDURE, PASS( obj ) :: mesh_getLocalNodeNumber1
      !! Returns the local node number of a glocal node number
    PROCEDURE, PASS( obj ) :: mesh_getLocalNodeNumber2
      !! Returns the local node number of a global node number
    GENERIC, PUBLIC :: getLocalNodeNumber => mesh_getLocalNodeNumber1, &
      & mesh_getLocalNodeNumber2
      !! Returns the local node number of a global node number
    PROCEDURE, PASS( obj ) :: mesh_getGlobalNodeNumber1
      !! Returns the global node number of a local node number
    PROCEDURE, PASS( obj ) :: mesh_getGlobalNodeNumber2
      !! Returns the global node number of a local node number
    GENERIC, PUBLIC :: getGlobalNodeNumber => mesh_getGlobalNodeNumber1, &
      & mesh_getGlobalNodeNumber2
    PROCEDURE, PASS( obj ) :: mesh_getGlobalElemNumber_1
    PROCEDURE, PASS( obj ) :: mesh_getGlobalElemNumber_2
    GENERIC, PUBLIC :: getGlobalElemNumber => &
      & mesh_getGlobalElemNumber_1, mesh_getGlobalElemNumber_2
      !! Returns the global element number for a local element number
    PROCEDURE, PASS( obj ) :: mesh_getLocalElemNumber_1
    PROCEDURE, PASS( obj ) :: mesh_getLocalElemNumber_2
    GENERIC, PUBLIC :: getLocalElemNumber => &
      & mesh_getLocalElemNumber_1, mesh_getLocalElemNumber_2
      !! Returns the local element number of a global element number
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToElements => &
      & mesh_getNodeToElements
      !! Returns the element attached to a given global node number
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToNodes => &
      & mesh_getNodeToNodes
      !! Returns nodes connected to a given node number
    PROCEDURE, PUBLIC, PASS( obj ) :: getElementToElements => &
      & mesh_getElementToElements
      !! Returns local element number connected to a given local
      !! element number, it also gives information about the local
      !! facet number
    PROCEDURE, PUBLIC, PASS( obj ) :: getBoundaryElementData => &
      & mesh_getBoundaryElementData
      !! Returns boundary element data
    PROCEDURE, PRIVATE, PASS( obj ) :: setSparsity1 => mesh_setSparsity1
    PROCEDURE, PRIVATE, PASS( obj ) :: setSparsity2 => mesh_setSparsity2
    GENERIC, PUBLIC :: setSparsity => setSparsity1, setSparsity2
END TYPE Mesh_

!----------------------------------------------------------------------------
!                                                                     Mesh
!----------------------------------------------------------------------------

PUBLIC :: Mesh_
! TYPE( Mesh_ ), PARAMETER, PUBLIC :: TypeMesh = Mesh_( )

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
!                                                                 Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: This routine reads the mesh from a meshFile which is an hdf5
! file
!
!### Introduction
!
! This routine reads the following
!
! meshdata%uid,  meshdata%xidim, meshData%elemType, meshData%minX, meshData%minY, meshData%minZ, meshData%maxX, meshData%maxY, meshData%maxZ, meshData%X,meshData%Y, meshData%Z, meshData%tElements, meshData%tIntNodes,  meshData%physicalTag, meshData%InternalNptrs, meshData%elemNumber, meshData%connectivity, meshData%boundingEntity
!
! This routine initiate the local_nptrs data in mesh.
! This routine also sets the number of nodes in the mesh (tNodes)
! This routine allocate obj%nodeData
! This routine set localNodeNum and globalNodeNum data inside the
! nodeData

INTERFACE
MODULE SUBROUTINE mesh_Import( obj, hdf5, group )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE mesh_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
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
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Allocate the size of the mesh
!
!### Introduction
!
! This routine initiate the the mesh by reading the data stored inside
! the HDF5 file.
! It calls following routines
!
! - obj%import()
! - obj%InitiateLocalNptrs()
! - obj%InitiateLocalElementNumbers()
! - obj%InitiateNodeToElements()
! - obj%InitiateNodeToNodes()
! - obj%InitiateElementToElements()
! - obj%InitiateBoundaryData()

INTERFACE
MODULE SUBROUTINE mesh_initiate( obj, hdf5, group )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! mesh object
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
    !! Mesh file
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE mesh_initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Mesh@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Function for constructing an instance of [[Mesh_]]

INTERFACE
MODULE FUNCTION Mesh_Constructor1( hdf5, group ) RESULT( ans )
  TYPE( Mesh_ ) :: ans
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END FUNCTION Mesh_Constructor1
END INTERFACE

!>
! Generic function for constructing [[mesh_]]
INTERFACE Mesh
  MODULE PROCEDURE Mesh_Constructor1
END INTERFACE Mesh

PUBLIC :: Mesh

!----------------------------------------------------------------------------
!                                                   Mesh_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This function returns a pointer to an instance of mesh_ object

INTERFACE
MODULE FUNCTION Mesh_Constructor_1( hdf5, group ) RESULT( ans )
  CLASS( Mesh_ ), POINTER :: ans
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END FUNCTION Mesh_Constructor_1
END INTERFACE

INTERFACE Mesh_Pointer
  MODULE PROCEDURE Mesh_Constructor_1
END INTERFACE Mesh_Pointer

PUBLIC :: Mesh_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
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
!                                              getTotalElements@getMethod
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

!----------------------------------------------------------------------------
!                                               getRefElemPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: REturns the pointer to the reference element

INTERFACE
MODULE FUNCTION mesh_getRefElemPointer( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  CLASS( ReferenceElement_ ), POINTER :: ans
END FUNCTION mesh_getRefElemPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                             getBoundingEntity@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns bounding entity of the mesh

INTERFACE
MODULE PURE FUNCTION mesh_getBoundingEntity( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getBoundingEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getNptrs@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns the vector of global node numbers
!
!### Introduction
! This vector returns a vector of global node numbers
!
!### Usage
!
!```fortran
  ! type( mesh_ ) :: obj
  ! integer( I4B ) :: ierr, ii
  ! type( HDF5File_ ) :: meshfile
  ! call display( colorize('TEST:', color_fg='pink', style='underline_on') &
  !   & // colorize('getNptrs, getInternalNptrs, getBoundaryNptrs :', color_fg='blue', &
  !   & style='underline_on') )
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  ! call display( obj%getNptrs(), "getNptrs = ")
  ! call display( obj%getInternalNptrs(), "getInternalNptrs = ")
  ! call display( obj%getBoundaryNptrs(), "getBoundaryNptrs = ")
  ! call obj%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
!```

INTERFACE
MODULE PURE FUNCTION mesh_getNptrs( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInternalNptrs@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns the vector of global node numbers of internal nodes
!
!### Introduction
! This vector returns a vector of global node numbers of internal nodes
!
!### Usage
!
!```fortran
  ! type( mesh_ ) :: obj
  ! integer( I4B ) :: ierr, ii
  ! type( HDF5File_ ) :: meshfile
  ! call display( colorize('TEST:', color_fg='pink', style='underline_on') &
  !   & // colorize('getNptrs, getInternalNptrs, getBoundaryNptrs :', color_fg='blue', &
  !   & style='underline_on') )
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  ! call display( obj%getNptrs(), "getNptrs = ")
  ! call display( obj%getInternalNptrs(), "getInternalNptrs = ")
  ! call display( obj%getBoundaryNptrs(), "getBoundaryNptrs = ")
  ! call obj%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
!```

INTERFACE
MODULE PURE FUNCTION mesh_getInternalNptrs( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                getBoundaryNptrs@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns the vector of global node numbers of boundary nodes
!
!### Introduction
! This vector returns a vector of global node numbers of boundary nodes
!
!### Usage
!
!```fortran
  ! type( mesh_ ) :: obj
  ! integer( I4B ) :: ierr, ii
  ! type( HDF5File_ ) :: meshfile
  ! call display( colorize('TEST:', color_fg='pink', style='underline_on') &
  !   & // colorize('getNptrs, getInternalNptrs, getBoundaryNptrs :', color_fg='blue', &
  !   & style='underline_on') )
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  ! call display( obj%getNptrs(), "getNptrs = ")
  ! call display( obj%getInternalNptrs(), "getInternalNptrs = ")
  ! call display( obj%getBoundaryNptrs(), "getBoundaryNptrs = ")
  ! call obj%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
!```

INTERFACE
MODULE PURE FUNCTION mesh_getBoundaryNptrs( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getBoundaryNptrs
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
! - This subroutine generates Elements surrounding a node mapping.
! - Elements numbers are global element number.
! - This mapping is stored inside obj%nodeData array
! - For a local node number ii, obj%nodeData(ii)%globalElements(:) contains the global element numbers.
!
!@note
! Always use method called `getNodeToElements()` to access this information. This methods requires global Node number
!@endnote
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
MODULE SUBROUTINE mesh_InitiateNodeToElements( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mesh_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateNodeToNode@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate node to node connectivity data
!
!### Introduction
! This routine generate the node to nodes mapping
! This mapping is stored inside `obj%nodeData%globalNodeNum`
!
! For a local node number i, obj%nodeData(i)%globalNodeNum denotes the global node data surrounding the local node number. This list does not include self node.

INTERFACE
MODULE SUBROUTINE mesh_InitiateNodetoNodes( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate boundary data

INTERFACE
MODULE SUBROUTINE mesh_InitiateElementToElements( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateBoundaryData@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate boundary data

INTERFACE
MODULE SUBROUTINE mesh_InitiateBoundaryData( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isBoundaryNode@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns true if given global node is a boundary node

INTERFACE
MODULE PURE FUNCTION mesh_isBoundaryNode( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isBoundaryNode
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Returns true if an global element number is a boundary element
!
!### Introduction
! This routine returns true if a global element number is a boundary element.
! A boundary element is one which contains a boundary node.

INTERFACE
MODULE PURE FUNCTION mesh_isBoundaryElement( obj, globalElemNumber ) &
  & RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalElemNumber
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isNodePresent@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: Returns  TRUE if a given global node number is present

INTERFACE
MODULE PURE FUNCTION mesh_isNodePresent( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isElementPresent@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: Returns  TRUE if a given global Element number is present

INTERFACE
MODULE PURE FUNCTION mesh_isElementPresent( obj, GlobalElement ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElement
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                           getTotalInternalNodes@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: Returns total number of internal nodes inside the mesh

INTERFACE
MODULE PURE FUNCTION mesh_getTotalInternalNodes( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalInternalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                             getTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns total number of nodes in the mesh

INTERFACE
MODULE PURE FUNCTION mesh_getTotalNodes( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                     getTotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns total number of boundary nodes in the mesh

INTERFACE
MODULE PURE FUNCTION mesh_getTotalBoundaryNodes( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalBoundaryNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  getTotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns total number of boundary elements

INTERFACE
MODULE PURE FUNCTION mesh_getTotalBoundaryElements( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalBoundaryElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            getBoundingBox@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns bounding box of the mesh

INTERFACE
MODULE PURE FUNCTION mesh_getBoundingBox( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  TYPE( BoundingBox_ ) :: ans
END FUNCTION mesh_getBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getConnectivity@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: This routine returns global node numbers in a given global elem

INTERFACE
MODULE PURE FUNCTION mesh_getConnectivity( obj, globalElemNumber ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalElemNumber
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION mesh_getConnectivity
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
MODULE PURE FUNCTION mesh_getLocalNodeNumber1( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  INTEGER( I4B ) :: Ans( SIZE( GlobalNode ) )
END FUNCTION mesh_getLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the local node number from a global node number

INTERFACE
MODULE PURE FUNCTION mesh_getLocalNodeNumber2( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalNodeNumber1( obj, LocalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalNode( : )
  INTEGER( I4B ) :: Ans( SIZE( LocalNode ) )
END FUNCTION mesh_getGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalNodeNumber2( obj, LocalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalNode
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalElemNumber_1( obj, LocalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalElem( : )
  INTEGER( I4B ) :: Ans( SIZE( LocalElem ) )
END FUNCTION mesh_getGlobalElemNumber_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalElemNumber_2( obj, LocalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalElem
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getGlobalElemNumber_2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getLocalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
MODULE PURE FUNCTION mesh_getLocalElemNumber_1( obj, GlobalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElem( : )
  INTEGER( I4B ) :: Ans( SIZE( GlobalElem ) )
END FUNCTION mesh_getLocalElemNumber_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       getLocalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
MODULE PURE FUNCTION mesh_getLocalElemNumber_2( obj, GlobalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElem
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getLocalElemNumber_2
END INTERFACE

!----------------------------------------------------------------------------
!                                         getNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: Returns the element numbers which are connected to the a global node
!
!### Introduction
! This function returns the elements containing the global node number `GlobalNode`
! The element number are global element numbers
!
!@note
! 	If the node number `GlobalNode` is not present inside the mesh then
! the returned vector of integer has size 0
!@endnote
!
!
!### Usage
!
!```fortran
  ! type( mesh_ ) :: obj
  ! integer( I4B ) :: ierr, ii
  ! type( HDF5File_ ) :: meshfile
  ! call display( colorize('TEST:', color_fg='pink', style='underline_on') &
  !   & // colorize('NODE TO ELEMENTS:', color_fg='blue', &
  !   & style='underline_on') )
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  ! do ii = obj%minElemNum, obj%maxElemNum
  !   if( .not. obj%isElementPresent( ii ) ) cycle
  ! end do
  ! do ii = obj%minNptrs, obj%maxNptrs
  !   if( .not. obj%isNodePresent( ii ) ) cycle
  !   call display( obj%getNodeToElements( ii ), "node = " // trim( string( ii ) ) // ' is connected to global elements = '  )
  ! end do
  ! call obj%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
!```

INTERFACE
MODULE PURE FUNCTION mesh_getNodeToElements( obj, GlobalNode ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
    !! global node number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! A vector of local element number
END FUNCTION mesh_getNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           getNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: 	Returns the node surrounding a node
!
!### Introduction
! This fucntion returns the vector of node numbers which surrounds a given
! node number `GlobalNode`.
! - If `IncludeSelf` is true then, in the returned vector of integer, node number GlobalNode is also present
!- If `IncludeSelf` is false then, in the returned vector of integer, node number `GlobalNode` is not present
!
!@note
! 	If the node number `GlobalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote
!
!
!### Usage
!
!```fortran
  ! type( mesh_ ) :: obj
  ! integer( I4B ) :: ierr, ii
  ! type( HDF5File_ ) :: meshfile
  ! call display( colorize('TEST:', color_fg='pink', style='underline_on') &
  !   & // colorize('NODE TO NODES:', color_fg='blue', &
  !   & style='underline_on') )
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  ! do ii = obj%minNptrs, obj%maxNptrs
  !   if( .not. obj%isNodePresent( ii ) ) cycle
  !   call display( obj%getNodeToNodes( ii, .true. ), &
  !     & "node = " // trim( string( ii ) ) &
  !     & // ' is connected to global nodes = ' )
  ! end do
  ! call obj%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
!```

INTERFACE
MODULE PURE FUNCTION mesh_getNodeToNodes( obj, GlobalNode, IncludeSelf ) &
  & RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ), INTENT( IN ) :: IncludeSelf
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION mesh_getNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                      getElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns element to element connectivity information
!
!### Introduction
! This routine returns element to element connectivity information for a given global element number `globalElemNumber`
!
! If `OnlyElements` is absent or it is set to FALSE then, this routine returns the **full information** about elements surrounding the global element `globalElemNumber`. In this case,
!
! - Each Row of `ans` denotes the element to which `globalElemNumber` is connected to
! - Column-1 of `ans` denotes global element number of the neighbour
! - Column-2 denotes the local face number of element `globalElemNumber`
! - Column-3 denotes the local face number of global element given by the column number 1 (same row)
!
! If `OnlyElements` is present and it is TRUE then, this routine returns only the global element numbers surrouding the given element `globalElemNumber`
!

INTERFACE
MODULE PURE FUNCTION mesh_getElementToElements( obj, globalElemNumber, &
  & onlyElements ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: globalElemNumber
    !! Global element number
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: onlyElements
    !! If onlyElements is absent or it is FALSE then full information about the elements connected to element iel is given
    !! If onlyElements is present and it is TRUE then only the information about the elements connected to element iel is given
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
    !! list of elements surrounding elements
END FUNCTION mesh_getElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                     getBoundaryElementData@MeshDataMethods
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 22 July 2021
! summary: This routine returns the boundary element data
!
!### Introduction
! This routine returns the boundry element data. It contains the local index of facet element which is boundary
!
!
!### Usage
!
!```fortran
  ! type( mesh_ ) :: obj
  ! integer( I4B ) :: ierr, ii
  ! type( HDF5File_ ) :: meshfile
  ! call display( colorize('TEST:', color_fg='pink', style='underline_on') &
  !   & // colorize('testing  :', color_fg='blue', &
  !   & style='underline_on') )
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  ! do ii = obj%minElemNum,obj%maxElemNum
  !   if( .NOT. obj%isElementPresent(ii ) ) cycle
  !   if( obj%isBoundaryElement(ii) ) then
  !     call display( obj%getBoundaryElementData( ii ), &
  !     & "element = " // trim( string( ii ) ) &
  !     & // ' is connected to global elements = ' )
  !   end if
  ! end do
  ! call obj%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
!```

INTERFACE
MODULE PURE FUNCTION mesh_getBoundaryElementData( obj, globalElemNumber ) &
  & RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalElemNumber
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getBoundaryElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
MODULE SUBROUTINE mesh_setSparsity1( obj, mat, localNodeNumber, lbound, &
  & ubound )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! Mesh_ class
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: mat
    !! CSRMatrix object
  INTEGER( I4B ), INTENT( IN ) :: lbound
  INTEGER( I4B ), INTENT( IN ) :: ubound
  INTEGER( I4B ), INTENT( IN ) :: LocalNodeNumber( lbound:ubound )
    !! Global to local node number map
END SUBROUTINE mesh_setSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                setSparsity@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
MODULE SUBROUTINE mesh_setSparsity2( obj, mat )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! Mesh_ class
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: mat
    !! CSRMatrix object
END SUBROUTINE mesh_setSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE Mesh_Class
