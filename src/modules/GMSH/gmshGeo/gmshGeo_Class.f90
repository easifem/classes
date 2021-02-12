MODULE gmshGeo_Class
  !! This class is container for a geometry

USE BaseType
USE BaseMethod
USE gmshPoint_Class
USE gmshCurve_Class
USE gmshSurface_Class
USE gmshVolume_Class
USE gmshGeoMesh_Class

IMPLICIT NONE

INTEGER( I4B ), PUBLIC, PARAMETER :: def_max_point = 100
INTEGER( I4B ), PUBLIC, PARAMETER :: def_max_curve = 100
INTEGER( I4B ), PUBLIC, PARAMETER :: def_max_Surface = 100
INTEGER( I4B ), PUBLIC, PARAMETER :: def_max_Volume = 100
INTEGER( I4B ), PUBLIC, PARAMETER :: def_max_curveLoop = 100
INTEGER( I4B ), PUBLIC, PARAMETER :: def_max_SurfaceLoop = 100

PRIVATE

!----------------------------------------------------------------------------
!                                                                  gmshGeo_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This class is defined to contain the elements of a geometry
! which will be constructed by Gmsh inbuilt kernel

TYPE :: gmshGeo_
  TYPE( Buffer_ ), POINTER :: buffer => NULL( )

  TYPE( gmshGeoMesh_ ), POINTER :: mesh => NULL( )

  TYPE( gmshPointPointer_ ), ALLOCATABLE :: Point( : )
  TYPE( gmshCurvePointer_ ), ALLOCATABLE :: Curve( : )
  TYPE( gmshSurfacePointer_ ), ALLOCATABLE :: Surface( : )
  TYPE( gmshVolumePointer_ ), ALLOCATABLE :: Volume( : )

  TYPE( gmshCurveLoopPointer_ ), ALLOCATABLE :: CurveLoop( : )
  TYPE( gmshSurfaceLoopPointer_ ), ALLOCATABLE :: SurfaceLoop( : )

  INTEGER( I4B ) :: tPoints = 0
  INTEGER( I4B ) :: tCurves = 0
  INTEGER( I4B ) :: tSurfaces = 0
  INTEGER( I4B ) :: tVolumes = 0

  INTEGER( I4B ) :: tCurveLoops = 0
  INTEGER( I4B ) :: tSurfaceLoops = 0
  ! TYPE( gmshVolumePointer_ ), ALLOCATABLE :: volume( : )

  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: write => geo_write

  PROCEDURE, PUBLIC, PASS( Obj ) :: addPoint => geo_add_point
    !! Add a point entitiy to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addLine => geo_add_line
    !! Add a line entity to geometry
  PROCEDURE, PUBLIC, PASS( Obj ) :: addCircleArc => geo_add_circle
    !! Add a Circular arc entity to geometry
  PROCEDURE, PUBLIC, PASS( Obj ) :: addEllipseArc => geo_add_Ellipse
    !! Add a Ellipse entity to geometry
  PROCEDURE, PUBLIC, PASS( Obj ) :: addSpline => geo_add_Spline
    !! Add a Spline entity to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addCompoundSpline => &
    & geo_add_CompoundSpline
    !! Add a compound Spline entity to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addBSpline => geo_add_BSpline
    !! Add a BSpline entity to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addCompoundBSpline => &
    & geo_add_CompoundBSpline
    !! Add a compound BSpline entity to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addBezier => geo_add_Bezier
    !! Add a Bezier entity to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addCurveLoop => geo_add_CurveLoop
    !! Add a Curveloop to geometry

  PROCEDURE, PUBLIC, PASS( Obj ) :: addSurfaceFilling =>geo_add_SurfaceFilling
    !! Add surfaceFilled
  PROCEDURE, PUBLIC, PASS( Obj ) :: addPlaneSurface => geo_add_PlaneSurface
    !! Add a plane surface entity to geometry
  PROCEDURE, PUBLIC, PASS( Obj ) :: addSurfaceLoop => geo_add_SurfaceLoop

END TYPE gmshGeo_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: gmshGeo_

! TYPE( gmshGeo_ ), PUBLIC, PARAMETER :: TypeGmshGeo = &
!   & gmshGeo_( point = NULL( ), curve = NULL( ) )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

FUNCTION geo_write( Obj, UnitNo ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT) ::  Obj
  INTEGER( I4B ), INTENT( IN ) :: UnitNo
  INTEGER( I4B ) :: Ans

  ! Internal variables
  INTEGER( I4B ) :: ii

  IF( .NOT. ASSOCIATED( Obj % buffer ) ) THEN
    CALL Display( "ERROR:: gmshGeo_Class.f90")
    CALL Display( "        geo_write()")
    CALL Display( "        Obj % buffer is not associated ")
    STOP
  END IF

  IF( Obj % buffer % tLine .EQ. 0 ) THEN
    CALL Display( "ERROR:: gmshGeo_Class.f90")
    CALL Display( "        geo_write()")
    CALL Display( "        Obj % buffer is empty")
    STOP
  END IF

  DO ii = 1, Obj % buffer % tLine
    IF( ASSOCIATED( Obj % buffer % Line( ii ) % ptr ) ) THEN
      ! CALL Display( "gmsh%model%geo:: writing gmsh%model%geo%buffer(" &
      !   & // trim( str( ii ) ) // " )" )
      WRITE( UnitNo, "(DT)" ) Obj % buffer % Line( ii ) % ptr
    END IF
  END DO

  WRITE( UnitNo, "(A)" ) "Coherence;"

END FUNCTION geo_write

!----------------------------------------------------------------------------
!                                                                   addPoint
!----------------------------------------------------------------------------

FUNCTION geo_add_point( Obj, x, y, z, lc, uid ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: x, y, z, lc
  INTEGER( I4B ), INTENT( IN ) :: uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! main code
  IF( ALLOCATED( obj % point ) ) THEN

    n = SIZE( obj % point )

    ! we will expand the point now to accomodate one
    IF( n .EQ. obj % tPoints ) THEN

      BLOCK

        TYPE( gmshPointPointer_ ) :: temp( n )
        INTEGER( I4B ) :: ii

        DO ii = 1, n
          temp( ii ) % ptr => obj % point( ii ) % ptr
          obj % point( ii ) % ptr => NULL( )
        END DO

        DEALLOCATE( obj % point )
        ALLOCATE( obj % point( 2*(n + 1) ) )
        obj % tPoints = n + 1
        ip = n + 1

        ! copy back
        DO ii = 1, n
          obj % point( ii ) % ptr => temp( ii ) % ptr
          temp( ii ) % ptr => NULL( )
        END DO

      END  BLOCK

    ELSE
    ! enough size is there
      obj % tPoints = obj % tPoints + 1
      ip = obj % tPoints

    END IF

  ELSE

    ALLOCATE( obj % point( def_max_point ) )
    obj % tPoints = 1
    ip = 1

  END IF

  IF( uid .GT. 0 ) THEN
    obj  % point( ip ) % ptr => gmshPoint_Pointer( x, y, z, lc, uid )
    Ans = 0
  ELSE
    obj  % point( ip ) % ptr => gmshPoint_Pointer( x, y, z, lc, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%point(ip)%ptr % encodedStr( )
  ! CALL Display( "    adding Point in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%point(ip)%ptr % encodedStr( ) )

END FUNCTION geo_add_point

!----------------------------------------------------------------------------
!                                                                   addLine
!----------------------------------------------------------------------------

FUNCTION geo_add_line( Obj, startTag, endTag, uid ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_line()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshLine_Pointer( startTag, endTag, uid )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshLine_Pointer( startTag, endTag, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%curve(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding Line in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_line

!----------------------------------------------------------------------------
!                                                               addCircleArc
!----------------------------------------------------------------------------

FUNCTION geo_add_circle( Obj, startTag, centerTag, endTag, uid, &
  & nx, ny, nz ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, centerTag, uid
  REAL( DFP ), OPTIONAL :: nx, ny, nz
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n
  REAL( DFP ) :: rval( 3 )

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_line()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  rval = 0.0
  IF( PRESENT( nx ) ) rval( 1 ) = nx
  IF( PRESENT( ny ) ) rval( 2 ) = ny
  IF( PRESENT( ny ) ) rval( 3 ) = nz

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshCircle_Pointer( startTag, &
      & centerTag, endTag, uid, rval( 1 ), rval( 2 ), rval( 3 ) )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshCircle_Pointer( startTag, &
      & centerTag, endTag, ip, rval( 1 ), rval( 2 ), rval( 3 ) )
    Ans = ip
  END IF

  ! append to buffer
  ! CALL Display( "    adding Circle in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_circle

!----------------------------------------------------------------------------
!                                                              addEllipseArc
!----------------------------------------------------------------------------

FUNCTION geo_add_Ellipse( Obj, startTag, centerTag, majorTag, endTag, uid, &
  & nx, ny, nz ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, centerTag, majorTag, uid
  REAL( DFP ), OPTIONAL :: nx, ny, nz
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n
  REAL( DFP ) :: rval( 3 )

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Ellipse()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  rval = 0.0
  IF( PRESENT( nx ) ) rval( 1 ) = nx
  IF( PRESENT( ny ) ) rval( 2 ) = ny
  IF( PRESENT( ny ) ) rval( 3 ) = nz

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshEllipse_Pointer( startTag, &
      & centerTag, majorTag, endTag, uid, rval( 1 ), rval( 2 ), rval( 3 ) )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshEllipse_Pointer( startTag, &
      & centerTag, majorTag, endTag, ip, rval( 1 ), rval( 2 ), rval( 3 ) )
    Ans = ip
  END IF

  ! append to buffer
  ! CALL Display( "    adding Ellipse in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_Ellipse

!----------------------------------------------------------------------------
!                                                                  addSpline
!----------------------------------------------------------------------------

FUNCTION geo_add_Spline( Obj, pointTags, uid ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Spline()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshSpline_Pointer( pointTags, uid )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshSpline_Pointer( pointTags, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%curve(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding Spline in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_Spline

!----------------------------------------------------------------------------
!                                                          addCompoundSpline
!----------------------------------------------------------------------------

FUNCTION geo_add_CompoundSpline( Obj, curveTags, uid, numIntervals ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags( : ), uid
  INTEGER( I4B ), OPTIONAL :: numIntervals
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n, nI

  IF( PRESENT( numIntervals ) ) THEN
    nI = numIntervals
  ELSE
    nI = 5
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_CompoundSpline()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshCompoundSpline_Pointer( curveTags, &
      & uid, nI )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshCompoundSpline_Pointer( curveTags, &
      & ip, nI)
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%curve(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding CompoundSpline in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_CompoundSpline

!----------------------------------------------------------------------------
!                                                                  addBSpline
!----------------------------------------------------------------------------

FUNCTION geo_add_BSpline( Obj, pointTags, uid ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_BSpline()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshBSpline_Pointer( pointTags, uid )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshBSpline_Pointer( pointTags, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%curve(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding BSpline in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_BSpline

!----------------------------------------------------------------------------
!                                                          addCompoundBSpline
!----------------------------------------------------------------------------

FUNCTION geo_add_CompoundBSpline( Obj, curveTags, uid, numIntervals ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags( : ), uid
  INTEGER( I4B ), OPTIONAL :: numIntervals
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n, nI

  IF( PRESENT( numIntervals ) ) THEN
    nI = numIntervals
  ELSE
    nI = 5
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_CompoundBSpline()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshCompoundBSpline_Pointer( curveTags, &
      & uid, nI )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshCompoundBSpline_Pointer( curveTags, &
      & ip, nI)
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%curve(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding CompoundBSpline in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_CompoundBSpline

!----------------------------------------------------------------------------
!                                                                  addBezier
!----------------------------------------------------------------------------

FUNCTION geo_add_Bezier( Obj, pointTags, uid ) RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Bezier()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

#include "./addcurve.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curve( ip ) % ptr => gmshBezier_Pointer( pointTags, uid )
    Ans = 0
  ELSE
    obj  % curve( ip ) % ptr => gmshBezier_Pointer( pointTags, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%curve(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding Bezier in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%curve(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_Bezier

!----------------------------------------------------------------------------
!                                                               addCurveLoop
!----------------------------------------------------------------------------

FUNCTION geo_add_CurveLoop( Obj, curveTags, uid ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_CurveLoop()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % curve ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_CurveLoop()")
    CALL Display( "       curve are not allocated")
    STOP
  END IF

#include "./addcurveloop.inc"

  IF( uid .GT. 0 ) THEN
    obj  % curveloop( ip ) % ptr => gmshCurveLoop_Pointer( curveTags, uid )
    Ans = 0
  ELSE
    obj  % curveloop( ip ) % ptr => gmshCurveLoop_Pointer( curveTags, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%CurveLoop(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding Curveloop in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%CurveLoop(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_CurveLoop

!----------------------------------------------------------------------------
!                                                                 addSurface
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Add a surface filling the curve loops in wireTags.
! Currently only a single curve loop is supported; this curve loop should be
! composed by 3 or 4 curves only. If tag is positive, set the tag explicitly;
! otherwise a new tag is selected automatically. Return the tag of the surface.

FUNCTION geo_add_SurfaceFilling( Obj, wireTags, uid, sphereCenterTag ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: wireTags( : ), uid
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: sphereCenterTag
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n, sct

  IF( PRESENT( sphereCenterTag ) ) THEN
    sct = sphereCenterTag
  ELSE
    sct = -1
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_SurfaceFilling()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % curve ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_SurfaceFilling()")
    CALL Display( "       curve are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % CurveLoop ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_SurfaceFilling()")
    CALL Display( "       CurveLoop are not allocated")
    STOP
  END IF

#include "./addsurface.inc"

  IF( uid .GT. 0 ) THEN
    obj  % surface( ip ) % ptr => gmshSurface_Pointer( wireTags, uid,sct )
    Ans = 0
  ELSE
    obj  % surface( ip ) % ptr => gmshSurface_Pointer( wireTags, ip, sct )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%surface(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding Surface in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%surface(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_SurfaceFilling

!----------------------------------------------------------------------------
!                                                            addPlaneSurface
!----------------------------------------------------------------------------

FUNCTION geo_add_PlaneSurface( Obj, wireTags, uid ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: wireTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_PlaneSurface()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % curve ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_PlaneSurface()")
    CALL Display( "       curve are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % CurveLoop ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_PlaneSurface()")
    CALL Display( "       CurveLoop are not allocated")
    STOP
  END IF

#include "./addsurface.inc"

  IF( uid .GT. 0 ) THEN
    obj  % surface( ip ) % ptr => gmshPlaneSurface_Pointer( wireTags, uid )
    Ans = 0
  ELSE
    obj  % surface( ip ) % ptr => gmshPlaneSurface_Pointer( wireTags, ip )
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%surface(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding PlaneSurface in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%surface(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_PlaneSurface

!----------------------------------------------------------------------------
!                                                            addSurfaceLoop
!----------------------------------------------------------------------------

FUNCTION geo_add_SurfaceLoop( Obj, surfaceTags, uid ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: surfaceTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_SurfaceLoop()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % curve ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_SurfaceLoop()")
    CALL Display( "       curve are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % surface ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_SurfaceLoop()")
    CALL Display( "       surfaces are not allocated")
    STOP
  END IF

#include "./addsurfaceloop.inc"

  IF( uid .GT. 0 ) THEN
    obj % Surfaceloop( ip ) % ptr => gmshSurfaceLoop_Pointer(surfaceTags,uid)
    Ans = 0
  ELSE
    obj % Surfaceloop( ip ) % ptr => gmshSurfaceLoop_Pointer(surfaceTags,ip)
    Ans = ip
  END IF

  ! append to buffer
  ! WRITE( obj % buffer % unitno, "(DT)" ) obj%SurfaceLoop(ip)%ptr%encodedStr( )
  ! CALL Display( "    adding SurfaceLoop in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%SurfaceLoop(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_SurfaceLoop

!----------------------------------------------------------------------------
!                                                                 addVolume
!----------------------------------------------------------------------------

FUNCTION geo_add_Volume( Obj, shellTags, uid ) &
  & RESULT( Ans )
  CLASS( gmshGeo_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: shellTags( : ), uid
  INTEGER( I4B ) :: ans

  ! Internal variables
  INTEGER( I4B ) :: ip, n

  ! check
  IF( .NOT. ALLOCATED( obj % point ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Volume()")
    CALL Display( "       points are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % curve ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Volume()")
    CALL Display( "       curve are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % surface ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Volume()")
    CALL Display( "       surfaces are not allocated")
    STOP
  END IF

  ! check
  IF( .NOT. ALLOCATED( obj % Surfaceloop ) ) THEN
    CALL Display( "ERROR: gmshGeo_Class.f90")
    CALL Display( "       geo_add_Volume()")
    CALL Display( "       surfaceLoop are not allocated")
    STOP
  END IF

#include "./addvolume.inc"

  IF( uid .GT. 0 ) THEN
    obj % Volume( ip ) % ptr => gmshVolume_Pointer(shellTags,uid)
    Ans = 0
  ELSE
    obj % Volume( ip ) % ptr => gmshVolume_Pointer(shellTags,ip)
    Ans = ip
  END IF

  ! append to buffer
  ! CALL Display( "    adding Volume in gmsh%model%geo%buffer")
  CALL APPEND( obj % buffer, obj%Volume(ip)%ptr%encodedStr( ) )

END FUNCTION geo_add_Volume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE gmshGeo_Class
