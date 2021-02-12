MODULE Material_Class
USE BaseType
USE GlobalData
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                  Material_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! `Material_` type contains materials properties
TYPE :: Material_
  TYPE( KeyValue_ ), ALLOCATABLE :: Props( : )
    !! Each entry in prop denotes a material parameter

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => mat_initiate
      !! Construct the object
    PROCEDURE, PUBLIC, PASS( Obj ) :: Append => mat_append
      !! Append value to the list
    PROCEDURE, PUBLIC, PASS( Obj ) :: Size => mat_size
      !! get total number of properties/parameters
    PROCEDURE, PUBLIC, PASS( Obj ) :: Property => mat_get_prop
      !! Returns property as a [keyvalue_] instance
END TYPE Material_

PUBLIC :: Material_

TYPE( Material_ ), PARAMETER, PUBLIC :: TypeMaterial = &
  & Material_( Props = NULL( ) )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that constructs [[material_]]

MODULE PURE SUBROUTINE mat_initiate( Obj, tprop )
  CLASS( Material_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tprop
END SUBROUTINE mat_initiate
END INTERFACE

!> Generic subroutine to construct [[material_]]
INTERFACE Initiate
  MODULE PROCEDURE mat_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

INTERFACE
!! Append keyval object to [[material_]]

MODULE PURE SUBROUTINE mat_append( Obj, KeyValObj )
  CLASS( Material_ ), INTENT( INOUT) :: Obj
  TYPE( KeyValue_ ), INTENT( IN ) :: KeyValObj
END SUBROUTINE mat_append
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

INTERFACE
!! Get total number of parameters stored in [[material_]]

MODULE PURE FUNCTION mat_size( Obj ) RESULT( Ans )
  CLASS( Material_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION mat_size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Property
!----------------------------------------------------------------------------

INTERFACE
!! Function that return a property as an instance of [[keyvalue_]]

MODULE PURE FUNCTION mat_get_prop( Obj, Key ) RESULT( Ans )
  CLASS( Material_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  TYPE( KeyValue_ ) :: Ans
END FUNCTION mat_get_prop
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

INTERFACE
!! Deallocate data stored in [[material_]]

MODULE PURE SUBROUTINE mat_deallocate(  Obj )
  CLASS( Material_ ), INTENT( INOUT) :: Obj
END SUBROUTINE mat_deallocate
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE mat_deallocate
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mat_display( Obj, msg, unitno )
  CLASS( Material_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE mat_display
END INTERFACE

INTERFACE
MODULE SUBROUTINE mat_display_vec( Obj, msg, unitno )
  TYPE( Material_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE mat_display_vec
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE mat_display, mat_display_vec
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: MatetrialPointer_
  CLASS( Material_ ), POINTER :: Ptr => NULL( )
END TYPE MatetrialPointer_

PUBLIC :: MatetrialPointer_

END MODULE Material_Class