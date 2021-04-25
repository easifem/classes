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
!

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: 	 `Mesh_Class` module contains three data user defined data types related to finite element meshes: [[Mesh_]], [[MeshData_]], and [[MeshConnectivity_]].

MODULE Mesh_Class
USE BaseType
USE GlobalData
USE ElementFactory
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE ElementPointerVector_Class, ONLY: ElementPointerVector_, &
  & ElementPointerIterator_
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
REAL( DFP ), PARAMETER :: default_factor = 1.5_DFP
TYPE( ExceptionHandler_ ), SAVE, PUBLIC :: eMesh
!$OMP THREADPRIVATE(eMesh)
CHARACTER( LEN = * ), PARAMETER :: modName = "MESH_CLASS"

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
  TYPE( ElementPointerVector_ ), PUBLIC :: list
    !! A dynamic vector of element pointer
  INTEGER( I4B ) :: NSD = 0
    !! spatial dimension
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => mesh_initiate
      !! Allocate size of a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: Prune => mesh_PruneMesh
      !! Check the mesh, clean the broken link
    PROCEDURE, PUBLIC, PASS( obj ) :: Pushback => mesh_Pushback
      !! Append an element to a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: SetElement => mesh_SetElement
      !! Set an element to a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getElementPointer => mesh_getElementPointer
      !! Get Pointer to an element in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: EraseElement => mesh_EraseElement
      !! Remove an element from a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => mesh_size
      !! returns the SIZE of mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getNptrs => mesh_getNptrs
      !! Get node numbers in a mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: setMaterialType => mesh_setMaterialType
      !! Set material type of a mesh
    FINAL :: mesh_final
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mesh_DeallocateData
      !! Deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => mesh_display
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
! call obj % initiate( NSD = 2, tELements = 10 )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_initiate( obj, param )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! mesh object
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE mesh_initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Mesh@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: 	  Function for constructing [[Mesh_]]
!
!### Introduction
!
!  Function for constructing [[Mesh_]]
!
!### Usage
!
!```fortran
!obj = Mesh( NSD = 2, tELements = 10 )
!```end fortran

INTERFACE
MODULE FUNCTION Constructor1( param ) RESULT( ans )
  TYPE( Mesh_ ) :: ans
  TYPE( ParameterList_ ), INTENT( IN ) :: param
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
!  Function for constructing pointer to [[Mesh_]]
!
!### Usage
!
!```fortran
! class( mesh_ ), pointer :: obj
! obj => mesh_pointer( NSD = 2, tELements = 10 )
!```end fortran

INTERFACE
MODULE FUNCTION Constructor_1( param ) RESULT( ans )
  CLASS( Mesh_ ), POINTER :: ans
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END FUNCTION Constructor_1
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Generic function for constructing pointer to [[mesh_]]

INTERFACE Mesh_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE Mesh_Pointer

PUBLIC :: Mesh_Pointer

!----------------------------------------------------------------------------
!                                                         SetSize@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Set total elements in mesh object
!
!### Introduction
!
!  Set total elements in mesh object
!
!@note
! this routine runs through the element array and counts element pointers
! that are associated, and return the total number of associated elements.
! Therefore, it should be called only after appending/removing an element
! from the mesh. This routine also check for broken links and remove them.
!@endnote
!
!### Usage
!
!```fortran
!call obj % SetSize( )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_PruneMesh( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object
END SUBROUTINE mesh_PruneMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                 AppendElement@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Add an element to mesh
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
! summary: 	 Set an element to a mesh
!
!### Introduction
!
! Seting element; total number of elements remain same
! Size of mesh should be sufficient while using this.
!
!### Usage
!```fortran
! call obj % setElement( Elem )
!```

INTERFACE
MODULE SUBROUTINE mesh_SetElement( obj, Elem, iel )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! Mesh object
  CLASS( Element_ ), TARGET, INTENT( INOUT ) :: Elem
    !! Finite element to be put in mesh
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
END SUBROUTINE mesh_SetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ElementPointer@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return the pointer to an element `obj % Elem(iel)`

!> authos: Dr Vikas Sharma
!
! Return the pointer to an element `obj % Elem(iel)`
!
! @warning
! make sure `iel ` should be less that `obj%telements`
! @endwarning
!
!### Usage
!```fortran
! class( element_ ), pointer :: elem
! elem => obj % ElementPointer( iel )
!```

MODULE FUNCTION mesh_getElementPointer( obj, iel ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  CLASS( Element_ ), POINTER :: ans
    !! pointer to finite element
END FUNCTION mesh_getElementPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                  RemoveElement@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Remove an element from the mesh

!> authors: Dr. Vikas Sharma
!
! Remove an element from the mesh
!
! - extraOption = 1 then `obj % elem( iel )` will be nullified and deallocated
! - extraOption = 2 then `obj % elem( iel )` will be nullified and elements in
! mesh will be rearranged
! - extraoption = 3 then `obj % elem( iel )` will be nullified and deallocated
!  and elements in the mesh will be rearranged
!
!### Usage
!
!```fortran
! call obj % removeElement( iel = iel, extraoption = 2 )
!```

MODULE SUBROUTINE mesh_EraseElement( obj, iel )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
END SUBROUTINE mesh_EraseElement
END INTERFACE

!----------------------------------------------------------------------------
!                                              getTotalElements@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns total elements in the mesh

!> authors: Dr. Vikas Sharma
!
! Returns total elements in the mesh
!
!### Usage
!
!```fortran
!	telem = obj % SIZE( obj )
!```

MODULE FUNCTION mesh_size( obj ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh object
  INTEGER( I4B ) :: ans
END FUNCTION mesh_size
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getNptrs@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns the node numbers in mesh

!> authors: Dr. Vikas Sharma
!
! Returns the node numbers in mesh
!
!### Usage
!
!```fortran
!	call obj % getNptrs( Nptrs )
!```

MODULE SUBROUTINE mesh_getNptrs( obj, Nptrs )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Nptrs( : )
    !! node numbers
END SUBROUTINE mesh_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getNptrs@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Returns a vec of node numbers present in a collection of [[MeshPointer_]]

!> authors: Dr. Vikas Sharma
!
! Returns the vec of node numbers present in a collection of [[MeshPointer_]]
!
!### Usage
!
!```fortran
!	call getNptrs( obj, Nptrs )
!```

MODULE SUBROUTINE meshPointer_getNptrs( obj, Nptrs )
  TYPE( MeshPointer_ ), INTENT( INOUT ) :: obj( : )
    !! Collection of pointer to [[Mesh_]]
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Nptrs( : )
    !! Node present in the collection of mesh
END SUBROUTINE meshPointer_getNptrs
END INTERFACE

!> authors: Dr. Vikas Sharma
!
! Generic subroutine to get `Nptrs` in [[MeshPointer_]]
INTERFACE getNptrs
  MODULE PROCEDURE meshPointer_getNptrs
END INTERFACE getNptrs

PUBLIC :: getNptrs

!----------------------------------------------------------------------------
!                                                setMaterialType@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Set material propertie

!> authors: Dr. Vikas Sharma
!
! set material properties of element in the mesh. Currently, this routine
! add same material property to all elements
!
! @todo
! - add material properties by providing MatType as a vector of material type
! @endtodo
!
!### Usage
!
!```fortran
!	call obj % setMaterialType( MatType = 1 )
!```

MODULE SUBROUTINE mesh_setMaterialType(  obj, MatType )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !!
  INTEGER( I4B ), INTENT( IN ) :: MatType
END SUBROUTINE mesh_setMaterialType
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Deallocate data stored in [[mesh_]]
!
!### Introduction
!
! Deallocate data stored in [[mesh_]]
!
!### Usage
!
!```fortran
!call deallocateData( obj = obj )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_DeallocateData( obj )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
END SUBROUTINE mesh_DeallocateData
END INTERFACE

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

INTERFACE
!! display content of [[mesh_]]

!> authors: Dr. Vikas Sharma
!
!  Display content of [[mesh_]]
!
!### Usage
!
!```fortran
!	call display( obj, 'mesh', stdout )
!```

MODULE SUBROUTINE mesh_display( obj, Msg, UnitNo )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
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

END MODULE Mesh_Class
