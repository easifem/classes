!>
! `Mesh_Class` module contains three data user defined data types related to
!  finite element meshes: [[Mesh_]], [[MeshData_]], and [[MeshConnectivity_]].

MODULE Mesh_Class
  !! This module contains `Mesh_` `MeshData_` and `MeshConnectivity_`
USE BaseType
USE GlobalData
USE FE
IMPLICIT NONE

PRIVATE
REAL( DFP ), PARAMETER :: default_factor = 1.5_DFP

!----------------------------------------------------------------------------
!                                                                      Mesh_
!----------------------------------------------------------------------------

!> author: Dr Vikas Sharma
!
! In **EASIFEM** `Mesh_` datatype is simply a collection of finite elements.
! To avoid working with linked list I have encapsulated a vector of
! `ElementPointer_`. This way, adding or removing of an element
! to existing mesh becomes simple.
!
! @warning
!   Dont use `Obj % SetTotalElements()` method in external programs
! @endwarning

TYPE :: Mesh_
  TYPE( ElementPointer_ ), ALLOCATABLE :: Elem( : )
    !! Collection of finite elements
  INTEGER( I4B ) :: NSD
    !! spatial dimension
  INTEGER( I4B ) :: tElements
    !! total elements in mesh
  INTEGER( I4B ) :: maxElements
    !! maximum size of the wrapper

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => Deallocate_Data
      !! Deallocate data
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => allocateMeshSize
      !! Allocate size of a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: SetTotalElements => set_total_elements
      !! Alias of SetSize
    PROCEDURE, PUBLIC, PASS( Obj ) :: Append => add_element
      !! Append an element to a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: SetSize => set_total_elements
      !! Set total elements in a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: SetElement => Set_element
      !! Set an element to a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: ElementPointer => getElement_Pointer
      !! Get Pointer to an element in mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: RemoveElement => remove_element
      !! Remove an element from a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: SIZE => total_elements
      !! Returns the total elements in a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalElements => total_elements
      !! Alias of SIZE
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNptrs => get_nptrs
      !! Get node numbers in a mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: setMaterialType => setMaterialType_1
      !! Set material type of a mesh
END TYPE Mesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: Mesh_

TYPE( Mesh_ ), PARAMETER, PUBLIC :: TypeMesh = Mesh_( &
  & Elem = NULL( ), &
  & NSD = 0, tElements = 0, &
  & maxElements = 0 )

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

INTERFACE
!!  Allocate the size of the mesh

!> authos: Dr Vikas Sharma
!
!  Allocate the size of the mesh. Generic name ---> Initiate()
!
!### Usage
!
! ```fortran
!call obj % initiate( NSD = 2, tELements = 10 )
! ```end fortran

MODULE PURE SUBROUTINE allocateMeshSize( Obj, NSD, tElements, factor )
  !! allocate the size of the mesh
  CLASS( Mesh_ ), INTENT( INOUT) :: Obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: tElements
    !! total number of elements in mesh
  INTEGER( I4B ), INTENT( IN ) :: NSD
    !! spatial dimension
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: factor
    !! maxLength = factor * telements
END SUBROUTINE allocateMeshSize
END INTERFACE

!>
! Generic routine for constructing [[Mesh_]]
INTERFACE Initiate
  MODULE PROCEDURE allocateMeshSize
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                           Mesh@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!!  Function for constructing [[Mesh_]]

!> authos: Dr Vikas Sharma
!
!  Function for constructing [[Mesh_]]
!
!### Usage
!
! ```fortran
!obj = Mesh( NSD = 2, tELements = 10 )
! ```end fortran

MODULE PURE FUNCTION Constructor1( NSD, tElements, factor ) RESULT( Ans )
  TYPE( Mesh_ ) :: Ans
    !! Mesh object
  INTEGER( I4B ), INTENT( IN ) :: tElements
    !! totat number of elements in mesh
  INTEGER( I4B ), INTENT( IN ) :: NSD
    !! spatial dimension
  REAL( DFP ), INTENT( IN ), OPTIONAL :: factor
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

!>
! Generic function for constructing pointer to [[mesh_]]
INTERFACE Mesh_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE Mesh_Pointer

PUBLIC :: Mesh_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!!  Deallocate data stored in [[mesh_]]

!> authos: Dr Vikas Sharma
!
!  Deallocate data stored in [[mesh_]]
!
!### Usage
!
! ```fortran
!call deallocateData( Obj = Obj )
! ```end fortran

MODULE SUBROUTINE Deallocate_Data( Obj )
  CLASS( Mesh_ ), INTENT( INOUT) :: Obj
    !! mesh object
END SUBROUTINE Deallocate_Data
END INTERFACE

!>
! Generic subroutine for deallcating data stored in [[mesh_]]
INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         SetSize@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Set total elements in mesh object

!> authos: Dr Vikas Sharma
!
!  Set total elements in mesh object
!
! @note
! this routine runs through the element array and counts element pointers
! that are associated, and return the total number of associated elements.
! Therefore, it should be called only after appending/removing an element
! from the mesh.
! @endnote
!
!### Usage
! ```fortran
!call obj % SetSize( )
! ```end fortran

MODULE PURE SUBROUTINE set_total_elements( Obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: Obj
    !! mesh object
END SUBROUTINE set_total_elements
END INTERFACE

!----------------------------------------------------------------------------
!                                                 AppendElement@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Add an element to mesh

!> authos: Dr Vikas Sharma
!
!  Append an element, increate total elements in mesh by one
!
!### Usage
! ```fortran
! call obj % append( Elem )
! ```

MODULE SUBROUTINE add_element( Obj, Elem )
  CLASS( Mesh_ ), INTENT( INOUT ) :: Obj
    !! mesh obj
  CLASS( Element_ ), TARGET, INTENT( INOUT ) :: Elem
    !! finite element to be added
END SUBROUTINE add_element
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetElement@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Set an element to a mesh

!> authos: Dr Vikas Sharma
!
! Seting element; total number of elements remain same
! Size of mesh should be sufficient while using this.
!
!### Usage
! ```fortran
! call obj % setElement( Elem )
! ```

MODULE PURE SUBROUTINE Set_element( Obj, Elem, iel )
  CLASS( Mesh_ ), INTENT( INOUT ) :: Obj
    !! Mesh object
  CLASS( Element_ ), TARGET, INTENT( INOUT ) :: Elem
    !! Finite element to be put in mesh
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
END SUBROUTINE Set_element
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ElementPointer@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return the pointer to an element `Obj % Elem(iel)`

!> authos: Dr Vikas Sharma
!
! Return the pointer to an element `Obj % Elem(iel)`
!
! @warning
! make sure `iel ` should be less that `Obj%telements`
! @endwarning
!
!### Usage
! ```fortran
! class( element_ ), pointer :: elem
! elem => obj % ElementPointer( iel )
! ```

MODULE FUNCTION getElement_Pointer( Obj, iel ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: Obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  CLASS( Element_ ), POINTER :: Ans
    !! pointer to finite element
END FUNCTION getElement_Pointer
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
! - extraOption = 1 then `Obj % elem( iel )` will be nullified and deallocated
! - extraOption = 2 then `Obj % elem( iel )` will be nullified and elements in
! mesh will be rearranged
! - extraoption = 3 then `Obj % elem( iel )` will be nullified and deallocated
!  and elements in the mesh will be rearranged
!
!### Usage
!
! ```fortran
! call obj % removeElement( iel = iel, extraoption = 2 )
! ```

MODULE SUBROUTINE remove_element( Obj, iel, extraoption )
  CLASS( Mesh_ ), INTENT( INOUT) :: Obj
    !! mesh object
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), INTENT( IN ) :: extraoption
END SUBROUTINE remove_element
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
! ```fortran
!	telem = Obj % SIZE( Obj )
! ```

MODULE PURE FUNCTION total_elements( Obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: Obj
    !! mesh object
  INTEGER( I4B ) :: Ans
END FUNCTION total_elements
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
! ```fortran
!	call display( obj, 'mesh', stdout )
! ```

MODULE SUBROUTINE display_mesh( Obj, Msg, UnitNo )
  CLASS( Mesh_ ), INTENT( INOUT ) :: Obj
    !! mesh object
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
    !! message on screen
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
    !! unit number of ouput file
END SUBROUTINE display_mesh
END INTERFACE

!>
! generic routine to display content of mesh
INTERFACE Display
  MODULE PROCEDURE display_mesh
END INTERFACE Display

PUBLIC :: Display

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
! ```fortran
!	call obj % getNptrs( Nptrs )
! ```

MODULE PURE SUBROUTINE get_nptrs( Obj, Nptrs )
  CLASS( Mesh_ ), INTENT( INOUT ) :: Obj
    !! mesh object
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Nptrs( : )
    !! node numbers
END SUBROUTINE get_nptrs
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
! ```fortran
!	call getNptrs( Obj, Nptrs )
! ```

MODULE PURE SUBROUTINE mesh_pointer_get_nptrs( Obj, Nptrs )
  TYPE( MeshPointer_ ), INTENT( INOUT ) :: Obj( : )
    !! Collection of pointer to [[Mesh_]]
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Nptrs( : )
    !! Node present in the collection of mesh
END SUBROUTINE mesh_pointer_get_nptrs
END INTERFACE

!> authors: Dr. Vikas Sharma
!
! Generic subroutine to get `Nptrs` in [[MeshPointer_]]
INTERFACE getNptrs
  MODULE PROCEDURE mesh_pointer_get_nptrs
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
! ```fortran
!	call Obj % setMaterialType( MatType = 1 )
! ```

MODULE PURE SUBROUTINE setMaterialType_1(  Obj, MatType )
  CLASS( Mesh_ ), INTENT( INOUT ) :: Obj
    !!
  INTEGER( I4B ), INTENT( IN ) :: MatType
END SUBROUTINE setMaterialType_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

CONTAINS

!> authos: Dr Vikas Sharma
!
!  Function for constructing pointer to [[Mesh_]]
!
!### Usage
!
! ```fortran
! class( mesh_ ), pointer :: obj
! obj => mesh_pointer( NSD = 2, tELements = 10 )
! ```end fortran

FUNCTION Constructor_1( NSD, tElements, factor ) RESULT( Ans )
  CLASS( Mesh_ ), POINTER :: Ans
  INTEGER( I4B ), INTENT( IN ) :: tElements, NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: factor

  ALLOCATE( Ans )
  IF( PRESENT( factor ) ) THEN
    CALL Ans % Initiate( NSD = NSD, tElements = tElements, factor = factor )
  ELSE
    CALL Ans % Initiate( NSD = NSD, tElements = tElements )
  END IF

END FUNCTION Constructor_1

END MODULE Mesh_Class
