MODULE QuadratureVariables_Class
  !! This module defines a class `MatProps_` which contians variables
  !! and parameters defined inside a finite element
  !! (i.e., at quadrature points)

USE BaseType
USE GlobalData
USE Mesh_Class
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                       QuadratureVariables_
!----------------------------------------------------------------------------

!>
!> authors: Dr. Vikas Sharma
!
! [[QuadratureVariables_]] class contains the variables and parameters defined
! in a collection of finite elements at quadrature points.
! - If all variables inside an element do not change then
! one can initiate the object with the ColID = 'M' which then indicates
! that material-parameters are stored
! - If one or all variables in elements chages then one must
! initiate the obhect with the ColID = 'E' which then indicates that
! element variables are stored
!
!### Usage
!
! ```fortran
!	type( QuadratureVariables_ ) :: obj
!
! !initiate obj-----------
!                        |
! call obj % initiate( tprop = 3, tpoint = 1, telem=100, names=names )
! call obj %  setValue( ipoint, elemnum, Val, is, ie )
! Val = obj % Value( ColIndx = 1 )
! call deallocateData( Obj )
! ```
!
! @note
! When `colID = M` then `tPoint` argument in initiate call will be ignored
! @endnote
!
TYPE :: QuadratureVariables_
  INTEGER( I4B ) :: tprop = 0
    !! Total number of properties defined at a quadrature points
  INTEGER( I4B ) :: tpoint = 0
    !! Total number of quadrature points inside an element
  TYPE(String), ALLOCATABLE :: Names( : )
    !! Names of variables stored; its length should be equal to
    !! tprops
  REAL( DFP ), ALLOCATABLE :: Val( :, :, : )
    !! This array contains the varialbe/parameter information
    !! `Val( :, i )` denotes material properties or element variables

  CONTAINS

    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => elem_var_initiate
      !! Subroutine that Initiates the object
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => elem_var_deallocate_data
      !! deallocate data
    PROCEDURE, PUBLIC, PASS( Obj ) :: setValue => elem_var_set_value
      !! Subroutine that Sets values/change values
    PROCEDURE, PUBLIC, PASS( Obj ) :: AddContribution => elem_var_add_val
      !! Subroutine that Adds contribution to the existing values
    PROCEDURE, PUBLIC, PASS( Obj ) :: ArrayValues => elem_var_get_value
      !! Subroutine that gets element variables
END TYPE QuadratureVariables_

PUBLIC :: QuadratureVariables_

!> Parameter instance of type [[QuadratureVariables_]]
TYPE( QuadratureVariables_ ), PARAMETER, PUBLIC :: TypeQuadratureVariables=&
  & QuadratureVariables_( Val = NULL(), Names = NULL() )

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate [[quadraturevariables_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiates `QuadratureVariables_` object
! - If `colID=M` then object stores material properties
! - In above case `tpoint` is not considered and set to 1 internally
! - If names are present then `size( names ) == tprop`
! - If `names` are not present then variables will be named as `V1, V2,...`

MODULE PURE SUBROUTINE elem_var_initiate( Obj, tprop, tpoint, telem, names )
  CLASS( QuadratureVariables_ ), INTENT( INOUT) :: Obj
    !! Instance
  INTEGER( I4B ), INTENT( IN ) :: tprop
    !! Total number of properties
  INTEGER( I4B ), INTENT( IN ) :: tpoint
    !! Total number of quadrature points
  INTEGER( I4B ), INTENT( IN ) :: telem
    !! Total number of columns
  TYPE( String ), OPTIONAL, INTENT( IN ) :: names( : )
    !! Names of variables
END SUBROUTINE elem_var_initiate
END INTERFACE

!> Generic method to initiate [[quadraturevariables_]]
INTERFACE Initiate
  MODULE PROCEDURE elem_var_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                           setValue@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that sets values in [[quadraturevariables_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set values of `Obj % Val( :, ipoint, icol )`
! - `Obj % Val( i_s:i_e, ipoint, icol) = Val( : )`

MODULE PURE SUBROUTINE elem_var_set_value( Obj, ipoint, elemnum, Val, is, ie )
  CLASS( QuadratureVariables_ ), INTENT( INOUT) :: Obj
    !! Instance of [[quadraturevariables_]]
  INTEGER( I4B ), INTENT( IN ) :: ipoint
    !! Quadrature point number
  INTEGER( I4B ), INTENT( IN ) :: elemnum
    !! element number
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! Values
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: is
    !! `is` starting index
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ie
    !! `ie` ending index
END SUBROUTINE elem_var_set_value
END INTERFACE

!----------------------------------------------------------------------------
!                                                     addContribution@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that adds values in [[quadraturevariables_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine adds values of `Obj % Val( :, ipoint, icol )`

MODULE PURE SUBROUTINE elem_var_add_val( Obj, ipoint, elemnum, Scale, Val, &
  & is, ie )
  CLASS( QuadratureVariables_ ), INTENT( INOUT) :: Obj
    !! Instance of [[quadraturevariables_]]
  INTEGER( I4B ), INTENT( IN ) :: elemnum
    !! It represents id of element or material depending on storage type
  INTEGER( I4B ), INTENT( IN ) :: ipoint
    !! Quadrature point number
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! Values
  REAL( DFP ), INTENT( IN ) :: Scale
    !! Scaling used for Values
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: is
    !! `is` starting index
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ie
    !! `ie` ending index
END SUBROUTINE elem_var_add_val
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Value@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Return values stored inside [[quadraturevariables_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine returns `Obj % Val(:,:,elemnum) `

MODULE PURE FUNCTION elem_var_get_value( Obj, elemnum ) RESULT( Ans )
  CLASS( QuadratureVariables_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: elemnum
    !! Element number or material number
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION elem_var_get_value
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Display the content of [[quadraturevariables_]]

MODULE SUBROUTINE elem_var_disp( Obj, msg, unitno )
  CLASS( QuadratureVariables_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE elem_var_disp
END INTERFACE

!> Generic interaface to display content of [[quadraturevariables_]]
INTERFACE Display
  MODULE PROCEDURE elem_var_disp
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                     DeallocateData@Method
!----------------------------------------------------------------------------

INTERFACE
!! Deallocat data stored inside [[quadraturevariables_]] object

MODULE PURE SUBROUTINE elem_var_deallocate_data( Obj )
  CLASS( QuadratureVariables_ ), INTENT( INOUT) :: Obj
END SUBROUTINE elem_var_deallocate_data
END INTERFACE

!> genenric interface for deallocating data in [[quadraturevariables_]]
INTERFACE DeallocateData
  MODULE PROCEDURE elem_var_deallocate_data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                QuadratureVariablesPointer_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[QuadratureVariablesPointer_]] contains a pointer to
! [[QuadratureVariables_]]

TYPE :: QuadratureVariablesPointer_
  CLASS( QuadratureVariables_ ), POINTER :: Ptr => NULL( )
END TYPE QuadratureVariablesPointer_

PUBLIC :: QuadratureVariablesPointer_

END MODULE QuadratureVariables_Class



