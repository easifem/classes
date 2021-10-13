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
! date: 12 Oct 2021
! summary: Domain connectivity class

MODULE DomainConnectivity_Class
USE BaseType
USE GlobalData
USE Mesh_Class
USE Domain_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "DomainConnectivity_Class"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                        FacetConnectivity_
!----------------------------------------------------------------------------

TYPE :: FacetConnectivity_
  INTEGER( I4B ) :: facetID=0
    !! global element number of facet element in facet cell
  INTEGER( I4B ) :: masterGlobalCellNum=0
    !! global element number of master cell
  INTEGER( I4B ) :: masterLocalFacetID=0
    !! Local facet of master cell which is connected to the facet mesh
    !! element
  INTEGER( I4B ) :: slaveCellID=0
    !! global element number of slave cell
  INTEGER( I4B ) :: slaveLocalFacetID=0
    !! Local facet of slave cell which is connected to the facet mesh element
END TYPE FacetConnectivity_

!----------------------------------------------------------------------------
!                                                        NodeConnectivity_
!----------------------------------------------------------------------------

TYPE :: NodeConnectivity_
  INTEGER( I4B ) :: masterGlobalNodeNum=0
  INTEGER( I4B ) :: slaveGlobalNodeNum=0
END TYPE NodeConnectivity_

!----------------------------------------------------------------------------
!                                                      ElementConnectivity_
!----------------------------------------------------------------------------

TYPE :: ElementConnectivity_
  INTEGER( I4B ) :: masterGlobalElemNum = 0
  INTEGER( I4B ) :: masterLocalFacetID = 0
  INTEGER( I4B ) :: slaveGlobalElemNum = 0
  INTEGER( I4B ) :: slaveLocalFacetID = 0
END TYPE ElementConnectivity_

!----------------------------------------------------------------------------
!                                                       DomainConnectivity_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This data type contains connectivity data between two [[domain_]]

TYPE :: DomainConnectivity_
  PRIVATE
  LOGICAL( LGT ), PUBLIC :: isInitiated = .FALSE.
  LOGICAL( LGT ), PUBLIC :: isFacetToCell = .FALSE.
  LOGICAL( LGT ), PUBLIC :: isNodeToNode = .FALSE.
  LOGICAL( LGT ), PUBLIC :: isElemToElem = .FALSE.
  TYPE( FacetConnectivity_ ), ALLOCATABLE :: FacetToCell( : )
    !! Facet connectivity, Facet to cell data
  INTEGER( I4B ), ALLOCATABLE :: NodeToNode( : )
    !! Node to node connectivity
    !! There are two rows
    !! The number of columns are equal to the
    !! total number of nodes in mesh1, domain1
  TYPE( ElementConnectivity_ ), ALLOCATABLE :: ElemToElem( : )
    !! Cell to cell connectivity data
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: AddSurrogate => dc_AddSurrogate
    !! Add surrogate to the module error handler
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => dc_DeallocateData
    !! Deallocate data stored in the object
    FINAL :: dc_Final
    !! finalizer
    PROCEDURE, PRIVATE, PASS( obj ) :: dc_InitiateFacetToCellData1
    !! Initiate facet to cell connectivity for [[Mesh_]]
    GENERIC, PUBLIC :: InitiateFacetToCellData1 =>  &
      & dc_InitiateFacetToCellData1
    !! Initiate [[DomainConnectivity_:FacetConnectivity]]
    PROCEDURE, PASS( obj ) :: dc_CellNumber1
    !! Return the cell number of a given facet
    PROCEDURE, PASS( obj ) :: dc_CellNumber2
    !! Return the cell numbers of given facet elements
    GENERIC, PUBLIC :: CellNumber =>  &
      & dc_CellNumber1, &
      & dc_CellNumber2
    !! Return the cell numbers of given facet elements
    PROCEDURE, PUBLIC, PASS( obj ) :: dc_FacetLocalID1
    !! Return the facet local id in cell element
    PROCEDURE, PUBLIC, PASS( obj ) :: dc_FacetLocalID2
    !! Return the facet local id in cell element
    GENERIC, PUBLIC :: FacetLocalID =>  &
      & dc_FacetLocalID1, &
      & dc_FacetLocalID2
    !! Return the facet local id in cell element
    PROCEDURE, PASS( obj ) :: dc_InitiateNodeToNodeData1
    !! Initiate the node to node connectivity between two meshes
    GENERIC, PUBLIC :: InitiateNodeToNodeData => &
      & dc_InitiateNodeToNodeData1
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToNodePointer => &
      & dc_getNodeToNodePointer
END TYPE DomainConnectivity_

PUBLIC :: DomainConnectivity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DomainConnectivityPointer_
  CLASS( DomainConnectivity_ ), POINTER :: Ptr => NULL( )
END TYPE DomainConnectivityPointer_

PUBLIC :: DomainConnectivityPointer_

!----------------------------------------------------------------------------
!                                            AddSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This routine addes surrogate to module exception handler

INTERFACE
MODULE SUBROUTINE dc_AddSurrogate( obj, userObj )
  CLASS( DomainConnectivity_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE dc_AddSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                          DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Deallocates the data stored in [[DomainConnectivity_]]
!
!# Introduction
!
! This subroutine deallocate the data stored in [[DomainConnectivity_]]
!
!# Usage
!
!```fortran
!call DeallocateData( obj )
!call obj%DeallocateData()
!```

INTERFACE
MODULE PURE SUBROUTINE dc_DeallocateData( obj )
  CLASS( DomainConnectivity_ ), INTENT( INOUT ) :: obj
    !! Mesh connectivity object
END SUBROUTINE dc_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dc_Final( obj )
  TYPE( DomainConnectivity_ ), INTENT( INOUT ) :: obj
END SUBROUTINE dc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateFacetToCellData@FacetToCellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Generate the connectivity matrix between cell and facet mesh.
!
!# Introduction
!
! This subroutine generate the connectivity matrix called obj % CellFacet
! between cell and facet mesh.
!
!  - The output result will be an integer array with 2 rows
!  - First row contains the element number of `CellMesh`
!  - Second row contains the local facet number of cell element which
!  connects to the facet mesh element.
!  - Each column of `obj % CellFacet` corresponds to an Element of
!  `FacetMesh`; total number of columns are same as total number of elem
!  in the `FacetMesh`
!  - if an element of `FacetMesh` is orphan then its corresponding entry
!  is set to zero in `obj % CellFacet` matrix
!
!### Usage
!
!```fortran
!```

INTERFACE
MODULE SUBROUTINE dc_InitiateFacetToCellData1( obj, Facet, Cell )
  CLASS( DomainConnectivity_ ), INTENT( INOUT ) :: obj
  !! Mesh connectivity data
  CLASS( Mesh_ ), INTENT( INOUT ) :: Facet
  !! Mesh of cell elements
  CLASS( Mesh_ ), INTENT( INOUT ) :: Cell
  !! Mesh of facet mesh
END SUBROUTINE dc_InitiateFacetToCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                              CellNumber@FacetToCellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns cell number of given facet number
!
!# Introduction
!
! - Returns cell number of given facet number
! - if cell number is zero it means facet element is an orphan
!
!# Usage
!
!```fortran
!id = obj % CellNumber( facetNum )
!```

INTERFACE
MODULE PURE FUNCTION dc_CellNumber1( obj,  FacetNum ) RESULT( ans )
  CLASS( DomainConnectivity_ ), INTENT( IN ) :: obj
    !! Mesh connectivity data
  INTEGER( I4B ), INTENT( IN ) :: FacetNum
    !! Facet element number
  INTEGER( I4B ) :: ans
    !! Cell number
END FUNCTION dc_CellNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              CellNumber@FacetToCellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns cell number of given facet number
!
!# Introduction
!
! - Returns cell number of given facet number
! - if cell number is zero it means facet element is an orphan
!
!# Usage
!
!```fortran
!	id = obj % CellNumber( facetNum )
!```

INTERFACE
MODULE PURE FUNCTION dc_CellNumber2( obj, FacetNum ) RESULT( ans )
  CLASS( DomainConnectivity_ ), INTENT( IN ) :: obj
    !! Mesh connectivity data
  INTEGER( I4B ), INTENT( IN ) :: FacetNum( : )
    !! List of facet element numbers
  INTEGER( I4B ) :: ans( SIZE( FacetNum ) )
    !! List of cell element numbers
END FUNCTION dc_CellNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                            FacetLocalID@FacetToCellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id of cell element
!
!# Introduction
!
! Returns the local facet id of cell element which is in contact with
! facet element
!
!## Usage
!
!```fortran
! id = obj % FacetLocalID( FacetNum )
!```

INTERFACE
MODULE PURE FUNCTION dc_FacetLocalID1( obj, FacetNum ) RESULT( ans )
  CLASS( DomainConnectivity_ ), INTENT( IN ) :: obj
    !! Mesh connectivity object
  INTEGER( I4B ), INTENT( IN ) :: FacetNum
    !! Facet element number
  INTEGER( I4B ) :: ans
    !! Local facet ID
END FUNCTION dc_FacetLocalID1
END INTERFACE

!----------------------------------------------------------------------------
!                                            FacetLocalID@FacetToCellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id of cell element
!
!# Introduction
!
! Returns the local facet id of cell element which is in contact with
! facet element
!
!## Usage
!
!```fortran
!	id = obj % FacetLocalID( FacetNum )
!```

INTERFACE
MODULE PURE FUNCTION dc_FacetLocalID2( obj, FacetNum ) RESULT( ans )
  CLASS( DomainConnectivity_ ), INTENT( IN ) :: obj
    !! Mesh connectivity data
  INTEGER( I4B ), INTENT( IN ) :: FacetNum( : )
    !! List of facet element numbers
  INTEGER( I4B ) :: ans( SIZE( FacetNum ) )
    !! List of local facet IDs
END FUNCTION dc_FacetLocalID2
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateNodeToNodeData@NodeToNodeMethods
!----------------------------------------------------------------------------

INTERFACE
!! Generate the connectivity matrix between two meshes

!> authors: Dr. Vikas Sharma
!
!	 This subroutine generate the connectivity matrix between two meshes
!  The output result will be an integer array with 2 columns
!
! - first column: contains the node number of Mesh1
! - second column: contains the node number of Mesh2 which is
! directly connected to the node 1

MODULE SUBROUTINE dc_InitiateNodeToNodeData1( obj, domain1, domain2, &
  & dim1, dim2, entityNum1, entityNum2 )
  CLASS( DomainConnectivity_ ), INTENT( INOUT ) :: obj
    !! mesh connectivity object
  CLASS( Domain_ ), INTENT( IN ) :: domain1
    !! Domain1
  CLASS( Domain_ ), INTENT( IN ) :: domain2
    !! domain2
  INTEGER( I4B ), INTENT( IN ) :: dim1
    !! dimension of mesh in domain-1
  INTEGER( I4B ), INTENT( IN ) :: dim2
    !! dimension of mesh in domain-2
  INTEGER( I4B ), INTENT( IN ) :: entityNum1
    !! entity num of mesh in domain-1
  INTEGER( I4B ), INTENT( IN ) :: entityNum2
    !! entity num of mesh in domain-2
END SUBROUTINE dc_InitiateNodeToNodeData1
END INTERFACE

!----------------------------------------------------------------------------
!                                           getNodeToNodePointer@NodeMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION dc_getNodeToNodePointer( obj ) RESULT( Ans )
  CLASS( DomainConnectivity_ ), TARGET, INTENT( IN ) :: obj
  INTEGER( I4B ), POINTER :: ans( : )
END FUNCTION dc_getNodeToNodePointer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DomainConnectivity_Class