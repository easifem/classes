MODULE gmshEntity_Class
  !! This module defines an abstract class for `gmshEntity_`

USE GlobalData
USE BaseType

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                gmshEntity_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! `gmshEntity_` is an abstract class

TYPE, ABSTRACT :: gmshEntity_
  INTEGER( I4B ) :: uid = 0
  INTEGER( I4B ) :: dim = 0
    !! Unique id of entity
  CONTAINS
  PROCEDURE(en_encode), PUBLIC, DEFERRED, PASS( obj ) :: encodedStr
    !! Encode the information stored in gmshEntity_ in sring and returns
END TYPE gmshEntity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: gmshEntity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
FUNCTION en_encode( obj ) RESULT( ans )
  IMPORT :: gmshEntity_, String
  CLASS( gmshEntity_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans
END FUNCTION en_encode
END INTERFACE

END MODULE gmshEntity_Class