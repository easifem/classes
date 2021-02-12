MODULE gmshVolume_Class
  !! This module defines a class for gmsh Volume entities

USE GlobalData
USE BaseType
USE gmshEntity_Class

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                               gmshVolume_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshEntity_ ) :: gmshVolume_
  INTEGER( I4B ), ALLOCATABLE :: shellTags( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => Volume_encode
END TYPE gmshVolume_

TYPE :: gmshVolumePointer_
  CLASS( gmshVolume_ ), POINTER :: Ptr => Null( )
END TYPE gmshVolumePointer_

PUBLIC :: gmshVolume_
PUBLIC :: gmshVolumePointer_
PUBLIC :: gmshVolume_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                          gmsVolume_Pointer@VolumeMethods
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Add a volume (a region) defined by one or more shells shellTags.
! The first surface loop defines the exterior boundary; additional surface
! loop define holes. If tag is positive, set the tag explicitly; otherwise a
!  new tag is selected automatically. Return the tag of the volume.

FUNCTION gmshVolume_Pointer ( shellTags, uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: shellTags( : )
  INTEGER( I4B ), INTENT( IN ) :: uid
  CLASS( gmshVolume_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % shellTags = shellTags
  Ans % uid = uid
  Ans % dim = 3

END FUNCTION gmshVolume_Pointer

FUNCTION Volume_encode( Obj ) RESULT( Ans )
  CLASS( gmshVolume_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % shellTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim(str( obj % shellTags( ii ), no_sign=.true. ))
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Volume( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " };"

END FUNCTION Volume_encode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE gmshVolume_Class