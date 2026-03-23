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

MODULE GmshModelGeoMeshInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
IMPLICIT NONE

PRIVATE
PUBLIC :: gmshModelGeoMeshSetSize
PUBLIC :: gmshModelGeoMeshSetTransfiniteCurve
PUBLIC :: gmshModelGeoMeshSetTransfiniteSurface
PUBLIC :: gmshModelGeoMeshSetTransfiniteVolume
PUBLIC :: gmshModelGeoMeshSetRecombine
PUBLIC :: gmshModelGeoMeshSetSmoothing
PUBLIC :: gmshModelGeoMeshSetReverse
PUBLIC :: gmshModelGeoMeshSetAlgorithm
PUBLIC :: gmshModelGeoMeshSetSizeFromBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a mesh size constraint on the entities `dimTags' in the built-in CAD
! kernel representation. Currently only entities of dimension 0 (points) are
! handled.
!
! GMSH_API void gmshModelGeoMeshSetSize(int *dimTags, size_t dimTags_n,
!                                       const double size,
!                                       int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetSize(dimTags, dimTags_n, size, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetSize")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: size
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a transfinite meshing constraint on the curve `tag' in the built-in CAD
! kernel representation, with `numNodes' nodes distributed according to
! `meshType' and `coef'. Currently supported types are "Progression"
! (geometrical progression with power `coef') and "Bump" (refinement toward
! both extremities of the curve).
!
! GMSH_API void gmshModelGeoMeshSetTransfiniteCurve(const int tag,
!                                                   const int nPoints,
!                                                   const char *meshType,
!                                                   const double coef,
!                                                   int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetTransfiniteCurve( &
    tag, nPoints, meshType, coef, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetTransfiniteCurve")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag, nPoints
    TYPE(C_PTR), VALUE, INTENT(IN) :: meshType
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: coef
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetTransfiniteCurve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a transfinite meshing constraint on the surface `tag' in the built-in
! CAD kernel representation. `arrangement' describes the arrangement of the
! triangles when the surface is not flagged as recombined: currently
! supported values are "Left", "Right", "AlternateLeft" and "AlternateRight".
! `cornerTags' can be used to specify the (3 or 4) corners of the transfinite
! interpolation explicitly; specifying the corners explicitly is mandatory if
! the surface has more that 3 or 4 points on its boundary.
!
! GMSH_API void gmshModelGeoMeshSetTransfiniteSurface(&
! const int tag,
! const char *arrangement,
! int *cornerTags, size_t cornerTags_n,
! int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetTransfiniteSurface( &
    tag, arrangement, cornerTags, cornerTags_n, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetTransfiniteSurface")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), VALUE, INTENT(IN) :: arrangement
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: cornerTags_n
    INTEGER(C_INT), INTENT(IN) :: cornerTags(cornerTags_n)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetTransfiniteSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a transfinite meshing constraint on the surface `tag' in the built-in
! CAD kernel representation. `cornerTags' can be used to specify the (6 or 8)
! corners of the transfinite interpolation explicitly. */
!
! GMSH_API void gmshModelGeoMeshSetTransfiniteVolume(const int tag,
! int *cornerTags, size_t cornerTags_n,
! int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetTransfiniteVolume( &
    tag, cornerTags, cornerTags_n, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetTransfiniteVolume")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: cornerTags_n
    INTEGER(C_INT), INTENT(IN) :: cornerTags(cornerTags_n)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetTransfiniteVolume
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set a recombination meshing constraint on the entity of dimension `dim'
! and
! tag `tag' in the built-in CAD kernel representation. Currently only
! entities of dimension 2 (to recombine triangles into quadrangles) are
! supported; `angle' specifies the threshold angle for the simple
! recombination algorithm.

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetRecombine(dim, tag, angle, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetRecombine")
    IMPORT :: C_INT, C_DOUBLE
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: angle
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetRecombine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a smoothing meshing constraint on the entity of dimension `dim' and tag
! `tag' in the built-in CAD kernel representation. `val' iterations of a
! Laplace smoother are applied.
!
! GMSH_API void gmshModelGeoMeshSetSmoothing(const int dim,
!                                            const int tag,
!                                            const int val,
!                                            int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetSmoothing(dim, tag, val, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetSmoothing")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag, val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetSmoothing
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a reverse meshing constraint on the entity of dimension `dim` and tag
! `tag` in the built-in CAD kernel representation. If `val` is true, the mesh
! orientation will be reversed with respect to the natural mesh orientation
! (i.e. the orientation consistent with the orientation of the geometry). If
! `val' is false, the mesh is left as-is.
!
! GMSH_API void gmshModelGeoMeshSetReverse(const int dim,
!                                          const int tag,
!                                          const int val,
!                                          int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetReverse(dim, tag, val, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetReverse")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag, val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetReverse
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the meshing algorithm on the entity of dimension `dim' and tag `tag' in
! the built-in CAD kernel representation. Currently only supported for `dim'
! == 2.
!
! GMSH_API void gmshModelGeoMeshSetAlgorithm(const int dim,
!                                            const int tag,
!                                            const int val,
!                                            int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetAlgorithm(dim, tag, val, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetAlgorithm")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag, val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetAlgorithm
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Force the mesh size to be extended from the boundary, or not, for the
! entity of dimension `dim' and tag `tag' in the built-in CAD kernel
! representation. Currently only supported for `dim' == 2.
!
! GMSH_API void gmshModelGeoMeshSetSizeFromBoundary(const int dim,
!                                                   const int tag,
!                                                   const int val,
!                                                   int *ierr);

INTERFACE
  SUBROUTINE gmshModelGeoMeshSetSizeFromBoundary(dim, tag, val, ierr) &
    BIND(C, NAME="gmshModelGeoMeshSetSizeFromBoundary")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag, val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMeshSetSizeFromBoundary
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelGeoMeshInterface

