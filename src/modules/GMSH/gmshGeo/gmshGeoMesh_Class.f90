MODULE gmshGeoMesh_Class
USE BaseType
USE BaseMethod

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                               gmshGeoMesh_
!----------------------------------------------------------------------------

! gmsh / model / geo / mesh
TYPE :: gmshGeoMesh_

  TYPE( Buffer_ ), POINTER :: buffer => NULL( )

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: write => geoMesh_write
    PROCEDURE, PUBLIC, PASS( Obj ) :: setSize => geoMesh_setSize
    PROCEDURE, PUBLIC, PASS( Obj ) :: setTransfiniteCurve => &
      & geoMesh_setTransfiniteCurve
    PROCEDURE, PUBLIC, PASS( Obj ) :: setTransfiniteSurface => &
      & geoMesh_setTransfiniteSurface
    PROCEDURE, PUBLIC, PASS( Obj ) :: setTransfiniteVolume => &
      & geoMesh_setTransfiniteVolume
    PROCEDURE, PUBLIC, PASS( Obj ) :: setRecombine => &
      & geoMesh_setRecombine
    PROCEDURE, PUBLIC, PASS( Obj ) :: setSmoothing => &
      & geoMesh_setSmoothing
    PROCEDURE, PUBLIC, PASS( Obj ) :: setReverse => &
      & geoMesh_setReverse
    PROCEDURE, PUBLIC, PASS( Obj ) :: setAlgorithm => &
      & geoMesh_setAlgorithm
    PROCEDURE, PUBLIC, PASS( Obj ) :: setSizeFromBoundary => &
      & geoMesh_setSizeFromBoundary

END TYPE gmshGeoMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: gmshGeoMesh_

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geoMesh_write( Obj, UnitNo ) RESULT( Ans )
  CLASS( gmshGeoMesh_  ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: UnitNo
  INTEGER( I4B ) :: Ans

  INTEGER( I4B ) :: ii

  IF( ASSOCIATED( Obj % buffer ) ) THEN
    DO ii = 1, Obj % buffer % tLine
      WRITE( UnitNo, "(DT)" ) Obj % buffer % Line( ii ) % Ptr
    END DO
  END IF

END FUNCTION geoMesh_write

!----------------------------------------------------------------------------
!                                                                   setSize
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set a mesh size constraint on the model entities dimTags. Currently only
! entities of dimension 0 (points) are handled.

FUNCTION geoMesh_setSize( Obj, dim, tags, meshsize ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tags( : )
  REAL( DFP ), INTENT( IN ) :: meshsize
  INTEGER( I4B ) :: Ans

  ! internal variables
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )
  TYPE( String ) :: ss


  IF( dim .NE. 0 ) THEN
    CALL Display( "ERROR:: gmshGeoMesh_Class.f90" )
    CALL Display( "          geoMesh_setSize()" )
    CALL Display( "          only dim = 0, supported currently")
  END IF

  CALL Display( "      gmsh%model%geo%mesh%setSize()" )

  Ans = 0
  n = SIZE( tags )
  ALLOCATE( s( n ) )
  DO ii = 1, n
    s( ii ) = trim( str( tags( ii ), no_sign=.true. ) )
  END DO

  ss = ss % join( s, sep=", " )

  ss = &
    & "Characteristic Length { " // &
    & trim( ss ) // &
    & " } =  " // &
    & trim( str( meshsize ) ) // &
    & " ;"

  IF( .NOT. ASSOCIATED( Obj % buffer ) ) THEN
    ALLOCATE( Obj % buffer )
  END IF

  CALL APPEND( Obj % buffer, ss )

  DEALLOCATE( s )

END FUNCTION geoMesh_setSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> authors: Dr. Vikas Sharma
!
! Set a transfinite meshing constraint on the curve tag, with numNodes nodes
! distributed according to meshType and coef. Currently supported types
! are "Progres- sion" (geometrical progression with power coef) and "Bump"
! (refinement toward both extremities of the curve).

FUNCTION geoMesh_setTransfiniteCurve( Obj, tag, nPoints, meshType, &
  & coef ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: nPoints
  CHARACTER( LEN = * ), INTENT( IN ) :: meshType
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: coef
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setTransfiniteCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set a transfinite meshing constraint on the surface tag. arrangement
! describes the arrangement of the triangles when the surface is not
! flagged as recombined: cur- rently supported values are "Left", "Right",
! "AlternateLeft" and "AlternateRight". cornerTags can be used to specify
! the (3 or 4) corners of the transfinite interpola- tion explicitly;
! specifying the corners explicitly is mandatory if the surface has more
! that 3 or 4 points on its boundary.

FUNCTION geoMesh_setTransfiniteSurface( Obj, tag, arrangement, &
  & cornerTags ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  CHARACTER( LEN = * ), INTENT( IN ) ::  arrangement
  INTEGER( I4B ), INTENT( IN ) :: cornerTags( : )
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setTransfiniteSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set a transfinite meshing constraint on the surface tag. arrangement
! describes the arrangement of the triangles when the surface is not
! flagged as recombined: cur- rently supported values are "Left", "Right",
! "AlternateLeft" and "AlternateRight". cornerTags can be used to specify
! the (3 or 4) corners of the transfinite interpola- tion explicitly;
! specifying the corners explicitly is mandatory if the surface has more
! that 3 or 4 points on its boundary.

FUNCTION geoMesh_setTransfiniteVolume( Obj, tag, &
  & cornerTags ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cornerTags( : )
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setTransfiniteVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set a recombination meshing constraint on the model entity of dimension
! dim and tag tag. Currently only entities of dimension 2
! (to recombine triangles into quadrangles) are supporte

FUNCTION geoMesh_setRecombine( Obj, dim, tag, angle ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tag
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: angle
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setRecombine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set a smoothing meshing constraint on the model entity of dimension dim
! and tag tag. val iterations of a Laplace smoother are applied.

FUNCTION geoMesh_setSmoothing( Obj, dim, tag, val ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: val
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setSmoothing

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set a smoothing meshing constraint on the model entity of dimension dim
! and tag tag. val iterations of a Laplace smoother are applied.

FUNCTION geoMesh_setReverse( Obj, dim, tag, val ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tag
  LOGICAL( LGT ), INTENT( IN ) :: val
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setReverse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set the meshing algorithm on the model entity of dimension dim and tag tag.
! Currently only supported for dim == 2.

FUNCTION geoMesh_setAlgorithm( Obj, dim, tag, val ) RESULT( Ans )
  CLASS( gmshGeoMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: val
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setAlgorithm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Force the mesh size to be extended from the boundary, or not, for the model
! entity of dimension dim and tag tag. Currently only supported for dim == 2.

FUNCTION geoMesh_setSizeFromBoundary( Obj, dim, tag, val ) RESULT( Ans)
  CLASS( gmshGeoMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: val
  INTEGER( I4B ) :: Ans
END FUNCTION geoMesh_setSizeFromBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE gmshGeoMesh_Class