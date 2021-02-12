MODULE gmshSurface_Class
  !! This module defines a class for gmsh Surface entities

USE GlobalData
USE BaseType
USE gmshEntity_Class

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                 gmshSurface_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshEntity_ ) :: gmshSurface_
  INTEGER( I4B ), ALLOCATABLE :: wireTags( : )
  INTEGER( I4B ) :: sphereCenterTag = -1
    !! wireTags contains id of curve loops
    !! The first curve loop defines the exterior contour;
    !! additional curve loop define holes.
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => Surface_encode
END TYPE gmshSurface_

TYPE :: gmshSurfacePointer_
  CLASS( gmshSurface_ ), POINTER :: Ptr => Null( )
END TYPE gmshSurfacePointer_

PUBLIC :: gmshSurface_
PUBLIC :: gmshSurfacePointer_
PUBLIC :: gmshSurface_Pointer

!----------------------------------------------------------------------------
!                                                          gmshPlaneSurface_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshSurface_ ) :: gmshPlaneSurface_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => PlaneSurface_encode
END TYPE gmshPlaneSurface_

TYPE :: gmshPlaneSurfacePointer_
  CLASS( gmshPlaneSurface_ ), POINTER :: Ptr => NULL( )
END TYPE gmshPlaneSurfacePointer_

PUBLIC :: gmshPlaneSurface_
PUBLIC :: gmshPlaneSurfacePointer_
PUBLIC :: gmshPlaneSurface_Pointer

!----------------------------------------------------------------------------
!                                                          gmshSurfaceLoop_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshSurface_ ) :: gmshSurfaceLoop_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => SurfaceLoop_encode
END TYPE gmshSurfaceLoop_

TYPE :: gmshSurfaceLoopPointer_
  CLASS( gmshSurfaceLoop_ ), POINTER :: Ptr => NULL( )
END TYPE gmshSurfaceLoopPointer_

PUBLIC :: gmshSurfaceLoop_
PUBLIC :: gmshSurfaceLoopPointer_
PUBLIC :: gmshSurfaceLoop_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                          gmsSurface_Pointer@SurfaceMethods
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Creates a surface filling. With the built-in kernel, the first curve
! loop should be composed of either three or four curves.
! With the built-in kernel, the optional In Sphere argument forces
! the surface to be a spherical patch (the extra parameter gives the
! tag of the center of the sphere).

FUNCTION gmshSurface_Pointer ( wireTags, uid, sphereCenterTag ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: wireTags( : )
  INTEGER( I4B ), INTENT( IN ) :: uid
  INTEGER( I4B ), INTENT( IN ) :: sphereCenterTag
  CLASS( gmshSurface_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % wireTags = wireTags
  Ans % sphereCenterTag = sphereCenterTag
  Ans % uid = uid
  Ans % dim = 2

END FUNCTION gmshSurface_Pointer

FUNCTION Surface_encode( Obj ) RESULT( Ans )
  CLASS( gmshSurface_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % wireTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % wireTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Surface( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " }"

  IF( obj % sphereCenterTag .LT. 0 ) THEN
    ans = trim( ans ) // ";"
  ELSE
    ans = trim( ans ) &
      & // " In Sphere " &
      & // str( obj % sphereCenterTag, no_sign = .true. ) &
      & // ";"
  END IF

END FUNCTION Surface_encode

!----------------------------------------------------------------------------
!                                 gmshPlaneSurface_Pointer@PlaneSurfaceMethods
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Reference
! Creates a plane surface. The expression inside the parentheses is the plane
! surface’s tag; the expression-list on the right hand side should contain
! the tags of all the curve loops defining the surface. The first curve loop
! defines the exterior boundary of the surface; all other curve loops define
! holes in the surface. A curve loop defining a hole should not have any
! curves in common with the exterior curve loop (in which case it is not a
! hole, and the two surfaces should be defined separately). Likewise, a
! curve loop defining a hole should not have any curves in common with
! another curve loop defining a hole in the same surface (in which case the
! two curve loops should be combined).

FUNCTION gmshPlaneSurface_Pointer ( wireTags, uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: wireTags( : ), uid
  CLASS( gmshPlaneSurface_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % wireTags = wireTags
  Ans % uid = uid
  Ans % dim = 2
END FUNCTION gmshPlaneSurface_Pointer

FUNCTION PlaneSurface_encode( Obj ) RESULT( Ans )
  CLASS( gmshPlaneSurface_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % wireTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % wireTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Plane Surface( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " }; "

END FUNCTION PlaneSurface_encode

!----------------------------------------------------------------------------
!                                 gmshSurfaceLoop_Pointer@SurfaceLoopMethods
!----------------------------------------------------------------------------

FUNCTION gmshSurfaceLoop_Pointer ( wireTags, uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: wireTags( : ), uid
  CLASS( gmshSurfaceLoop_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % wireTags = wireTags
  Ans % uid = uid
  Ans % dim = 2
END FUNCTION gmshSurfaceLoop_Pointer

FUNCTION SurfaceLoop_encode( Obj ) RESULT( Ans )
  CLASS( gmshSurfaceLoop_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % wireTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim(str( obj % wireTags( ii ) ))
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Surface Loop( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " }; "

END FUNCTION SurfaceLoop_encode

END MODULE gmshSurface_Class