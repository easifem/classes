! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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
SUBMODULE(GmshModelGeo_Class) Methods
USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshModelGeoAddPoint
USE GmshInterface, ONLY: GmshModelGeoAddLine
USE GmshInterface, ONLY: GmshModelGeoAddCircleArc
USE GmshInterface, ONLY: GmshModelGeoAddEllipseArc
USE GmshInterface, ONLY: GmshModelGeoAddSpline
USE GmshInterface, ONLY: GmshModelGeoAddBSpline
USE GmshInterface, ONLY: GmshModelGeoAddBezier
USE GmshInterface, ONLY: GmshModelGeoAddPolyline
USE GmshInterface, ONLY: GmshModelGeoAddCompoundSpline
USE GmshInterface, ONLY: GmshModelGeoAddCompoundBSpline
USE GmshInterface, ONLY: GmshModelGeoAddCurveLoop
USE GmshInterface, ONLY: GmshModelGeoAddCurveLoops
USE GmshInterface, ONLY: GmshModelGeoAddPlaneSurface
USE GmshInterface, ONLY: GmshModelGeoAddSurfaceFilling
USE GmshInterface, ONLY: GmshModelGeoAddSurfaceLoop
USE GmshInterface, ONLY: GmshModelGeoAddVolume
USE GmshInterface, ONLY: GmshModelGeoAddGeometry
USE GmshInterface, ONLY: GmshModelGeoAddPointOnGeometry
USE GmshInterface, ONLY: GmshModelGeoExtrude
USE GmshInterface, ONLY: GmshModelGeoRevolve
USE GmshInterface, ONLY: GmshModelGeoTwist
USE GmshInterface, ONLY: GmshModelGeoExtrudeBoundaryLayer
USE GmshInterface, ONLY: GmshModelGeoTranslate
USE GmshInterface, ONLY: GmshModelGeoRotate
USE GmshInterface, ONLY: GmshModelGeoDilate
USE GmshInterface, ONLY: GmshModelGeoMirror
USE GmshInterface, ONLY: GmshModelGeoSymmetrize
USE GmshInterface, ONLY: GmshModelGeoCopy
USE GmshInterface, ONLY: GmshModelGeoRemove
USE GmshInterface, ONLY: GmshModelGeoRemoveAllDuplicates
USE GmshInterface, ONLY: GmshModelGeoSplitCurve
USE GmshInterface, ONLY: GmshModelGeoGetMaxTag
USE GmshInterface, ONLY: GmshModelGeoSetMaxTag
USE GmshInterface, ONLY: GmshModelGeoAddPhysicalGroup
USE GmshInterface, ONLY: GmshModelGeoRemovePhysicalGroups
USE GmshInterface, ONLY: GmshModelGeoSynchronize
USE GmshUtility, ONLY: gmsh_dimtag_c2f
USE GmshUtility, ONLY: gmsh_cdouble
USE GmshUtility, ONLY: gmsh_cint
USE GmshUtility, ONLY: gmsh_opt_cint
USE GmshUtility, ONLY: gmsh_opt_cdouble
USE GmshUtility, ONLY: gmsh_intvec_c2f
USE GmshUtility, ONLY: gmsh_GetCharArray_cPtr
USE GmshUtility, ONLY: gmsh_CString
USE GmshUtility, ONLY: gmsh_size_str
USE CInterface, ONLY: optval_c_bool
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_DOUBLE
IMPLICIT NONE

INTEGER(C_INT) :: ierr
INTEGER(C_INT) :: cintvar
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
CHARACTER(*), PARAMETER :: modName = "GmshModelGeo_Class@Methods.F90"

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
IF (isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    "gmsh::Model::Geo::Mesh is already associated;")
END IF
#endif

ALLOCATE (obj%mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPoint
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPoint()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddPoint( &
          x=gmsh_cdouble(x), &
          y=gmsh_cdouble(y), &
          z=gmsh_cdouble(z), &
          meshSize=gmsh_cdouble(meshSize), &
          tag=gmsh_cint(INPUT(default=-1_I4B, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddLine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddLine()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddLine( &
          startTag=gmsh_cint(startTag), &
          endTag=gmsh_cint(endTag), &
          tag=gmsh_cint(INPUT(default=-1, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCircleArc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCircleArc()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddCircleArc( &
          startTag=gmsh_cint(startTag), &
          centerTag=gmsh_cint(centerTag), &
          endTag=gmsh_cint(endTag), &
          tag=gmsh_opt_cint(default=math%minus_one_i, option=tag), &
          nx=gmsh_opt_cdouble(math%zero, nx), &
          ny=gmsh_opt_cdouble(math%zero, ny), &
          nz=gmsh_opt_cdouble(math%zero, nz), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddEllipseArc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddEllipseArc()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddEllipseArc( &
          startTag=gmsh_cint(startTag), &
          centerTag=gmsh_cint(centerTag), &
          majorTag=gmsh_cint(majorTag), &
          endTag=gmsh_cint(endTag), &
          tag=gmsh_cint(input(default=math%minus_one_i, option=tag)), &
          nx=gmsh_cdouble(nx), &
          ny=gmsh_cdouble(ny), &
          nz=gmsh_cdouble(nz), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSpline
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddSpline()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddSpline( &
          pointTags=gmsh_cint(pointTags), &
          pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
          tag=gmsh_cint(INPUT(default=-1, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBSpline
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBSpline()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddBSpline( &
          pointTags=gmsh_cint(pointTags), &
          pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
          tag=gmsh_cint(INPUT(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBezier
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBezier()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddBezier( &
          pointTags=gmsh_cint(pointTags), &
          pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
          tag=gmsh_cint(INPUT(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPolyline
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPolyline()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddPolyline( &
          pointTags=gmsh_cint(pointTags), &
          pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
          tag=gmsh_cint(INPUT(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddPolyline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCompoundSpline
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCompoundSpline()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddCompoundSpline( &
          curveTags=gmsh_cint(curveTags), &
          curveTags_n=INT(SIZE(curveTags), KIND=C_SIZE_T), &
          numIntervals=gmsh_cint(numIntervals), &
          tag=gmsh_cint(INPUT(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCompoundSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCompoundBSpline
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCompoundBSpline()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddCompoundBSpline( &
          curveTags=gmsh_cint(curveTags), &
          curveTags_n=INT(SIZE(curveTags), KIND=C_SIZE_T), &
          numIntervals=gmsh_cint(numIntervals), &
          tag=gmsh_cint(INPUT(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AddCompoundBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCurveLoop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCurveLoop()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddCurveLoop( &
          curveTags=gmsh_cint(curveTags), &
          curveTags_n=INT(SIZE(curveTags), C_SIZE_T), &
          tag=gmsh_cint(INPUT(default=-1, option=tag)), &
          reorient=optval_c_bool(math%no, reorient), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCurveLoops
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCurveLoops()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: tags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoAddCurveLoops( &
  curveTags=gmsh_cint(curveTags), &
  curveTags_n=INT(SIZE(curveTags), C_SIZE_T), &
  tags=cptr, tags_n=tags_n, ierr=ierr)

ans = INT(ierr, I4B)
tags = gmsh_intvec_c2f(cptr, tags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCurveLoops

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPlaneSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPlaneSurface()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddPlaneSurface( &
          wireTags=gmsh_cint(wireTags), &
          wireTags_n=INT(SIZE(wireTags), C_SIZE_T), &
          tag=gmsh_cint(input(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSurfaceFilling
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddSurfaceFilling()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddSurfaceFilling( &
          wireTags=gmsh_cint(wireTags), &
          wireTags_n=INT(SIZE(wireTags), C_SIZE_T), &
          tag=gmsh_cint(input(default=math%minus_one_i, option=tag)), &
          sphereCenterTag=gmsh_cint(sphereCenterTag), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSurfaceLoop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddSurfaceLoop()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddSurfaceLoop( &
          surfaceTags=gmsh_cint(surfaceTags), &
          surfaceTags_n=INT(SIZE(surfaceTags), C_SIZE_T), &
          tag=gmsh_cint(input(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddSurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddVolume
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddVolume()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddVolume( &
          shellTags=gmsh_cint(shellTags), &
          shellTags_n=INT(SIZE(shellTags), C_SIZE_T), &
          tag=gmsh_cint(input(default=math%minus_one_i, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddGeometry
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddGeometry()"
#endif

CHARACTER(maxStrLen, kind=C_CHAR), ALLOCATABLE :: strings_strs(:)
TYPE(C_PTR), ALLOCATABLE :: strings_(:)
REAL(C_DOUBLE), ALLOCATABLE :: numbers0(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL gmsh_GetCharArray_cPtr(strings, strings_strs, strings_)

isok = PRESENT(numbers)
IF (isok) THEN
  numbers0 = gmsh_cdouble(numbers)
ELSE
  ALLOCATE (numbers0(0))
END IF

cintvar = GmshModelGeoAddGeometry( &
          geometry=gmsh_CString(geometry), &
          numbers=numbers0, &
          numbers_n=SIZE(numbers0, kind=C_SIZE_T), &
          strings=strings_, &
          strings_n=gmsh_size_str(strings), &
          tag=gmsh_cint(input(default=-1, option=tag)), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddGeometry

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPointOnGeometry
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPointOnGeometry()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddPointOnGeometry( &
          geometryTag=gmsh_cint(geometryTag), &
          x=gmsh_cdouble(x), &
          y=gmsh_cdouble(y), &
          z=gmsh_opt_cdouble(default=math%zero, option=z), &
          meshSize=gmsh_opt_cdouble(default=math%zero, option=meshSize), &
          tag=gmsh_opt_cint(default=math%minus_one_i, option=tag), &
          ierr=ierr)

ans = INT(cintvar, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddPointOnGeometry

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Extrude
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Extrude()"
#endif

INTEGER(C_INT), ALLOCATABLE :: numElements_(:)
REAL(C_DOUBLE), ALLOCATABLE :: heights_(:)
INTEGER(C_SIZE_T) :: outDimTags_n
TYPE(C_PTR) :: cptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(numElements)
IF (isok) THEN
  numElements_ = gmsh_cint(numElements)
ELSE
  CALL Reallocate(numElements_, 0)
END IF

isok = PRESENT(heights)
IF (isok) THEN
  heights_ = gmsh_cdouble(heights)
ELSE
  CALL Reallocate(heights_, 0)
END IF

CALL GmshModelGeoExtrude( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=INT(SIZE(dimTags), C_SIZE_T), &
  dx=gmsh_cdouble(dx), &
  dy=gmsh_cdouble(dy), &
  dz=gmsh_cdouble(dz), &
  outDimTags=cptr, &
  outDimTags_n=outDimTags_n, &
  numElements=gmsh_cint(numElements_), &
  numElements_n=SIZE(numElements_, KIND=C_SIZE_T), &
  heights=gmsh_cdouble(heights_), &
  heights_n=SIZE(heights_, KIND=C_SIZE_T), &
  recombine=optval_c_bool(math%no, recombine), &
  ierr=ierr)

ans = gmsh_dimtag_c2f(cptr, outDimTags_n)

DEALLOCATE (heights_, numElements_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Extrude

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Revolve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Revolve()"
#endif

INTEGER(C_SIZE_T) :: outDimTags_n
TYPE(C_PTR) :: cptr
REAL(C_DOUBLE), ALLOCATABLE :: heights0(:)
INTEGER(C_INT), ALLOCATABLE :: numElements0(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(numElements)
IF (isok) THEN
  numElements0 = gmsh_cint(numElements)
ELSE
  ALLOCATE (numElements0(0))
END IF

isok = PRESENT(heights)
IF (isok) THEN
  heights0 = gmsh_cdouble(heights)
ELSE
  ALLOCATE (heights0(0))
END IF

CALL GmshModelGeoRevolve( &
  dimTags=dimTags, &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  x=gmsh_cdouble(x), &
  y=gmsh_cdouble(y), &
  z=gmsh_cdouble(z), &
  ax=gmsh_cdouble(ax), &
  ay=gmsh_cdouble(ay), &
  az=gmsh_cdouble(az), &
  angle=gmsh_cdouble(angle), &
  outDimTags=cptr, &
  outDimTags_n=outDimTags_n, &
  numElements=numElements0, &
  numElements_n=SIZE(numElements0, KIND=C_SIZE_T), &
  heights=heights0, &
  heights_n=SIZE(heights0, KIND=C_SIZE_T), &
  recombine=optval_c_bool(math%no, recombine), &
  ierr=ierr)

ans = gmsh_dimtag_c2f(cptr, outDimTags_n)

DEALLOCATE (heights0, numElements0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Revolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Twist
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Twist()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: outDimTags_n
REAL(C_DOUBLE), ALLOCATABLE :: heights0(:)
INTEGER(C_INT), ALLOCATABLE :: numElements0(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(numElements)
IF (isok) THEN
  numElements0 = gmsh_cint(numElements)
ELSE
  ALLOCATE (numElements0(0))
END IF

isok = PRESENT(heights)
IF (isok) THEN
  heights0 = gmsh_cdouble(heights)
ELSE
  ALLOCATE (heights0(0))
END IF

CALL GmshModelGeoTwist( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  x=gmsh_cdouble(x), &
  y=gmsh_cdouble(y), &
  z=gmsh_cdouble(z), &
  dx=gmsh_cdouble(dx), &
  dy=gmsh_cdouble(dy), &
  dz=gmsh_cdouble(dz), &
  ax=gmsh_cdouble(ax), &
  ay=gmsh_cdouble(ay), &
  az=gmsh_cdouble(az), &
  angle=gmsh_cdouble(angle), &
  outDimTags=cptr, &
  outDimTags_n=outDimTags_n, &
  numElements=numElements0, &
  numElements_n=SIZE(numElements0, KIND=C_SIZE_T), &
  heights=heights0, &
  heights_n=SIZE(heights0, KIND=C_SIZE_T), &
  recombine=optval_c_bool(math%no, recombine), &
  ierr=ierr)

ans = gmsh_dimtag_c2f(cptr, outDimTags_n)

DEALLOCATE (heights0, numElements0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Twist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExtrudeBoundaryLayer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExtrudeBoundaryLayer()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: outDimTags_n
REAL(C_DOUBLE), ALLOCATABLE :: heights0(:)
INTEGER(C_INT), ALLOCATABLE :: numElements0(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(numElements)
IF (isok) THEN
  numElements0 = gmsh_cint(numElements)
ELSE
  ALLOCATE (numElements0(0))
END IF

isok = PRESENT(heights)
IF (isok) THEN
  heights0 = gmsh_cdouble(heights)
ELSE
  ALLOCATE (heights0(0))
END IF

CALL GmshModelGeoExtrudeBoundaryLayer( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
  outDimTags=cptr, &
  outDimTags_n=outDimTags_n, &
  numElements=numElements0, &
  numElements_n=SIZE(numElements0, kind=C_SIZE_T), &
  heights=heights0, &
  heights_n=SIZE(heights0, kind=C_SIZE_T), &
  recombine=optval_c_bool(math%no, recombine), &
  second=optval_c_bool(math%no, second), &
  viewIndex=optval_c_bool(math%no, viewIndex), &
  ierr=ierr)

ans = gmsh_dimtag_c2f(cptr, outDimTags_n)

DEALLOCATE (heights0, numElements0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExtrudeBoundaryLayer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Translate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Translate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoTranslate( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  dx=gmsh_cdouble(dx), &
  dy=gmsh_cdouble(dy), &
  dz=gmsh_cdouble(dz), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Translate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Rotate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Rotate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoRotate( &
  dimTags=dimTags, &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  x=gmsh_cdouble(x), &
  y=gmsh_cdouble(y), &
  z=gmsh_cdouble(z), &
  ax=gmsh_cdouble(ax), &
  ay=gmsh_cdouble(ay), &
  az=gmsh_cdouble(az), &
  angle=gmsh_cdouble(angle), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Rotate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Dilate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Dilate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoDilate( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  x=gmsh_cdouble(x), &
  y=gmsh_cdouble(y), &
  z=gmsh_cdouble(z), &
  a=gmsh_cdouble(a), &
  b=gmsh_cdouble(b), &
  c=gmsh_cdouble(c), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Dilate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Mirror
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Mirror()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMirror( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  a=gmsh_cdouble(a), &
  b=gmsh_cdouble(b), &
  c=gmsh_cdouble(c), &
  d=gmsh_cdouble(d), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Mirror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Symmetrize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Symmetrize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoSymmetrize( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  a=gmsh_cdouble(a), &
  b=gmsh_cdouble(b), &
  c=gmsh_cdouble(c), &
  d=gmsh_cdouble(d), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Symmetrize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: outDimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoCopy( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  outDimTags=cptr, &
  outDimTags_n=outDimTags_n, &
  ierr=ierr)

ans = gmsh_dimtag_c2f(cptr, outDimTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Remove
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Remove()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoRemove( &
  dimTags=dimTags, &
  dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveAllDuplicates
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RemoveAllDuplicates()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoRemoveAllDuplicates(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemoveAllDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SplitCurve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SplitCurve()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: curveTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoSplitCurve( &
  tag=gmsh_cint(tag), &
  pointTags=gmsh_cint(pointTags), &
  pointTags_n=SIZE(pointTags, kind=C_SIZE_T), &
  curveTags=cptr, &
  curveTags_n=curveTags_n, &
  ierr=ierr)

ans = gmsh_intvec_c2f(cptr, curveTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SplitCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTag
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTag()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoGetMaxTag(dim=gmsh_cint(dim), ierr=ierr)
ans = INT(cintvar, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaxTag
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMaxTag()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoSetMaxTag( &
  dim=gmsh_cint(dim), maxTag=gmsh_cint(maxTag), ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPhysicalGroup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPhysicalGroup()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshModelGeoAddPhysicalGroup( &
          dim=gmsh_cint(dim), &
          tags=gmsh_cint(tags), &
          tags_n=SIZE(tags, kind=C_SIZE_T), &
          tag=gmsh_opt_cint(default=math%minus_one_i, option=tag), &
          name=gmsh_CString(input(default="", option=name)), &
          ierr=ierr)

ans = INT(cintvar, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemovePhysicalGroups
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RemovePhysicalGroups()"
#endif
INTEGER(C_INT), ALLOCATABLE :: dimTags0(:, :)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(dimTags)

IF (isok) THEN
  dimTags0 = gmsh_cint(dimTags)
ELSE
  ALLOCATE (dimTags0(0, 0))
END IF

CALL GmshModelGeoRemovePhysicalGroups( &
  dimTags=dimTags0, dimTags_n=SIZE(dimTags0, KIND=C_SIZE_T), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Synchronize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Synchronize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoSynchronize(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Synchronize

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
