MODULE gmshCurve_Class
  !! This module defines a class for gmsh points

USE GlobalData
USE BaseType
USE gmshEntity_Class

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                 gmshCurve_
!----------------------------------------------------------------------------

TYPE, ABSTRACT, EXTENDS( gmshEntity_ ) :: gmshCurve_
END TYPE gmshCurve_

TYPE :: gmshCurvePointer_
  CLASS( gmshCurve_ ), POINTER :: Ptr => Null( )
END TYPE gmshCurvePointer_

PUBLIC :: gmshCurve_
PUBLIC :: gmshCurvePointer_

!----------------------------------------------------------------------------
!                                                                 gmshLine_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshLine_
  INTEGER( I4B ) :: startTag, endTag
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => line_encode
END TYPE gmshLine_

TYPE :: gmshLinePointer_
  CLASS( gmshLine_ ), POINTER :: Ptr => NULL( )
END TYPE gmshLinePointer_

PUBLIC :: gmshLine_
PUBLIC :: gmshLinePointer_
PUBLIC :: gmshLine_Pointer

!----------------------------------------------------------------------------
!                                                                gmshCircle_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshCircle_
  INTEGER( I4B ) :: startTag, endTag, centerTag
  REAL( DFP ) :: nx, ny, nz
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => circle_encode
END TYPE gmshCircle_

TYPE :: gmshCirclePointer_
  CLASS( gmshCircle_ ), POINTER :: Ptr => NULL( )
END TYPE gmshCirclePointer_

PUBLIC :: gmshCircle_
PUBLIC :: gmshCirclePointer_
PUBLIC :: gmshCircle_Pointer

!----------------------------------------------------------------------------
!                                                                gmshEllipse_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshEllipse_
  INTEGER( I4B ) :: startTag, endTag, majorTag, centerTag
  REAL( DFP ) :: nx, ny, nz
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => Ellipse_encode
END TYPE gmshEllipse_

TYPE :: gmshEllipsePointer_
  CLASS( gmshEllipse_ ), POINTER :: Ptr => NULL( )
END TYPE gmshEllipsePointer_

PUBLIC :: gmshEllipse_
PUBLIC :: gmshEllipsePointer_
PUBLIC :: gmshEllipse_Pointer

!----------------------------------------------------------------------------
!                                                                gmshSpline_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshSpline_
  INTEGER( I4B ), ALLOCATABLE :: pointTags( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => Spline_encode
END TYPE gmshSpline_

TYPE :: gmshSplinePointer_
  CLASS( gmshSpline_ ), POINTER :: Ptr => NULL( )
END TYPE gmshSplinePointer_

PUBLIC :: gmshSpline_
PUBLIC :: gmshSplinePointer_
PUBLIC :: gmshSpline_Pointer

!----------------------------------------------------------------------------
!                                                        gmshCompoundSpline_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshCompoundSpline_
  INTEGER( I4B ), ALLOCATABLE :: curveTags( : )
  INTEGER( I4B ) :: numIntervals
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => CompoundSpline_encode
END TYPE gmshCompoundSpline_

TYPE :: gmshCompoundSplinePointer_
  CLASS( gmshCompoundSpline_ ), POINTER :: Ptr => NULL( )
END TYPE gmshCompoundSplinePointer_

PUBLIC :: gmshCompoundSpline_
PUBLIC :: gmshCompoundSplinePointer_
PUBLIC :: gmshCompoundSpline_Pointer

!----------------------------------------------------------------------------
!                                                       gmshCompoundBSpline_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshCompoundBSpline_
  INTEGER( I4B ), ALLOCATABLE :: curveTags( : )
  INTEGER( I4B ) :: numIntervals
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => CompoundBSpline_encode
END TYPE gmshCompoundBSpline_

TYPE :: gmshCompoundBSplinePointer_
  CLASS( gmshCompoundBSpline_ ), POINTER :: Ptr => NULL( )
END TYPE gmshCompoundBSplinePointer_

PUBLIC :: gmshCompoundBSpline_
PUBLIC :: gmshCompoundBSplinePointer_
PUBLIC :: gmshCompoundBSpline_Pointer

!----------------------------------------------------------------------------
!                                                                gmshBSpline_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshBSpline_
  INTEGER( I4B ), ALLOCATABLE :: pointTags( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => BSpline_encode
END TYPE gmshBSpline_

TYPE :: gmshBSplinePointer_
  CLASS( gmshBSpline_ ), POINTER :: Ptr => NULL( )
END TYPE gmshBSplinePointer_

PUBLIC :: gmshBSpline_
PUBLIC :: gmshBSplinePointer_
PUBLIC :: gmshBSpline_Pointer

!----------------------------------------------------------------------------
!                                                                gmshBezier_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshBezier_
  INTEGER( I4B ), ALLOCATABLE :: pointTags( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => Bezier_encode
END TYPE gmshBezier_

TYPE :: gmshBezierPointer_
  CLASS( gmshBezier_ ), POINTER :: Ptr => NULL( )
END TYPE gmshBezierPointer_

PUBLIC :: gmshBezier_
PUBLIC :: gmshBezierPointer_
PUBLIC :: gmshBezier_Pointer


!----------------------------------------------------------------------------
!                                                             gmshCurveLoop_
!----------------------------------------------------------------------------

TYPE, EXTENDS( gmshCurve_ ) :: gmshCurveLoop_
  INTEGER( I4B ), ALLOCATABLE :: curveTags( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => CurveLoop_encode
END TYPE gmshCurveLoop_

TYPE :: gmshCurveLoopPointer_
  CLASS( gmshCurveLoop_ ), POINTER :: Ptr => NULL( )
END TYPE gmshCurveLoopPointer_

PUBLIC :: gmshCurveLoop_
PUBLIC :: gmshCurveLoopPointer_
PUBLIC :: gmshCurveLoop_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                gmsLine_Pointer@LineMethods
!----------------------------------------------------------------------------

FUNCTION gmshLine_Pointer ( startTag, endTag, uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, uid
  CLASS( gmshLine_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % startTag = startTag
  Ans % endTag = endTag
  Ans % uid = uid
  Ans % dim = 1
END FUNCTION gmshLine_Pointer

FUNCTION line_encode( Obj ) RESULT( Ans )
  CLASS( gmshLine_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ans = &
  & "Line( " // &
  & trim( str( obj % uid, no_sign=.true. ) ) // &
  & " ) = { " // &
  & trim( str( obj % startTag, no_sign=.true. ) ) // &
  & ", " // &
  & trim( str( obj % endTag, no_sign=.true. ) ) // &
  & " };"

END FUNCTION line_encode

!----------------------------------------------------------------------------
!                                           gmshCircle_Pointer@CircleMethods
!----------------------------------------------------------------------------

FUNCTION gmshCircle_Pointer ( startTag, centerTag, endTag, uid, nx, ny, nz ) &
  & RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, centerTag, uid
  REAL( DFP ), INTENT( IN ) :: nx,ny, nz
  CLASS( gmshCircle_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % startTag = startTag
  Ans % endTag = endTag
  Ans % centerTag = centerTag
  Ans % nx = nx
  Ans % ny = ny
  Ans % nz = nz
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshCircle_Pointer

FUNCTION Circle_encode( Obj ) RESULT( Ans )
  CLASS( gmshCircle_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ans = &
  & "Circle( " // &
  & trim( str( obj % uid, no_sign=.true. ) ) // &
  & " ) = { " // &
  & trim( str( obj % startTag, no_sign=.true. ) ) // &
  & ", " // &
  & trim( str( obj % centerTag, no_sign=.true. ) ) // &
  & ", " // &
  & trim( str( obj % endTag, no_sign=.true. ) ) // &
  & " };"

END FUNCTION Circle_encode

!----------------------------------------------------------------------------
!                                          gmshEllipse_Pointer@EllipseMethods
!----------------------------------------------------------------------------

FUNCTION gmshEllipse_Pointer ( startTag, centerTag, majorTag, endTag, uid, &
  & nx, ny, nz ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, centerTag, majorTag, uid
  REAL( DFP ), INTENT( IN ) :: nx,ny, nz
  CLASS( gmshEllipse_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % startTag = startTag
  Ans % endTag = endTag
  Ans % majorTag = majorTag
  Ans % centerTag = centerTag
  Ans % nx = nx
  Ans % ny = ny
  Ans % nz = nz
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshEllipse_Pointer

FUNCTION Ellipse_encode( Obj ) RESULT( Ans )
  CLASS( gmshEllipse_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ans = &
  & "Ellipse( " // &
  & trim( str( obj % uid, no_sign=.true. ) ) // &
  & " ) = { " // &
  & trim( str( obj % startTag, no_sign=.true. ) ) // &
  & ", " // &
  & trim( str( obj % centerTag, no_sign=.true. ) ) // &
  & ", " // &
  & trim( str( obj % majorTag, no_sign=.true. ) ) // &
  & ", " // &
  & trim( str( obj % endTag, no_sign=.true. ) ) // &
  & " };"

END FUNCTION Ellipse_encode

!----------------------------------------------------------------------------
!                                           gmshSpline_Pointer@SplineMethods
!----------------------------------------------------------------------------

FUNCTION gmshSpline_Pointer ( pointTags,  uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: uid, pointTags( : )
  CLASS( gmshSpline_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % pointTags = pointTags
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshSpline_Pointer

FUNCTION Spline_encode( Obj ) RESULT( Ans )
  CLASS( gmshSpline_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % pointTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % pointTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Spline( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " };"

END FUNCTION Spline_encode

!----------------------------------------------------------------------------
!                            gmshCompoundSpline_Pointer@CompoundSplineMethods
!----------------------------------------------------------------------------

FUNCTION gmshCompoundSpline_Pointer ( curveTags, uid, numIntervals ) &
  & RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: uid, curveTags( : ), numIntervals
  CLASS( gmshCompoundSpline_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % curveTags = curveTags
  Ans % numIntervals = numIntervals
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshCompoundSpline_Pointer

FUNCTION CompoundSpline_encode( Obj ) RESULT( Ans )
  CLASS( gmshCompoundSpline_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % curveTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % curveTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Compound Spline( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " } Using " // &
    & trim( str( obj % numIntervals, no_sign = .true. ) ) // &
    & ";"

END FUNCTION CompoundSpline_encode

!----------------------------------------------------------------------------
!                         gmshCompoundBSpline_Pointer@CompoundBSplineMethods
!----------------------------------------------------------------------------

FUNCTION gmshCompoundBSpline_Pointer ( curveTags, uid, numIntervals ) &
  & RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: uid, curveTags( : ), numIntervals
  CLASS( gmshCompoundBSpline_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % curveTags = curveTags
  Ans % numIntervals = numIntervals
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshCompoundBSpline_Pointer

FUNCTION CompoundBSpline_encode( Obj ) RESULT( Ans )
  CLASS( gmshCompoundBSpline_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % curveTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % curveTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Compound BSpline( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " } Using " // &
    & trim( str( obj % numIntervals, no_sign = .true. ) ) // &
    & ";"

END FUNCTION CompoundBSpline_encode

!----------------------------------------------------------------------------
!                                         gmshBSpline_Pointer@BSplineMethods
!----------------------------------------------------------------------------

FUNCTION gmshBSpline_Pointer ( pointTags,  uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: uid, pointTags( : )
  CLASS( gmshBSpline_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % pointTags = pointTags
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshBSpline_Pointer

FUNCTION BSpline_encode( Obj ) RESULT( Ans )
  CLASS( gmshBSpline_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % pointTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % pointTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "BSpline( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " };"

END FUNCTION BSpline_encode

!----------------------------------------------------------------------------
!                                           gmshBezier_Pointer@BezierMethods
!----------------------------------------------------------------------------

FUNCTION gmshBezier_Pointer ( pointTags,  uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: uid, pointTags( : )
  CLASS( gmshBezier_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % pointTags = pointTags
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshBezier_Pointer

FUNCTION Bezier_encode( Obj ) RESULT( Ans )
  CLASS( gmshBezier_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % pointTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % pointTags( ii ), no_sign=.true. ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Bezier( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " };"
END FUNCTION Bezier_encode

!----------------------------------------------------------------------------
!                                     gmshCurveLoop_Pointer@CurveLoopMethods
!----------------------------------------------------------------------------

FUNCTION gmshCurveLoop_Pointer ( curveTags,  uid ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: uid, curveTags( : )
  CLASS( gmshCurveLoop_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % curveTags = curveTags
  Ans % uid = uid
  Ans % dim = 1

END FUNCTION gmshCurveLoop_Pointer

FUNCTION CurveLoop_encode( Obj ) RESULT( Ans )
  CLASS( gmshCurveLoop_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )

  n = SIZE( obj % curveTags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( obj % curveTags( ii ) ) )
  END DO

  ans = ans % join( s, sep=", " )

  ans = &
    & "Curve Loop( " // &
    & trim( str( obj % uid, no_sign=.true. ) ) // &
    & " ) = { " // &
    & trim( ans ) // &
    & " };"
END FUNCTION CurveLoop_encode

END MODULE gmshCurve_Class