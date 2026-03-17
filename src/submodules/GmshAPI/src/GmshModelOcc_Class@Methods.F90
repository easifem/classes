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

SUBMODULE(GmshModelOcc_Class) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE ExceptionHandler_Class, ONLY: e
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_PTR
USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshModelOccAddPoint
USE GmshInterface, ONLY: GmshModelOccAddLine
USE GmshInterface, ONLY: GmshModelOccAddCircleArc
USE GmshInterface, ONLY: GmshModelOccAddCircle
USE GmshInterface, ONLY: GmshModelOccAddEllipseArc
USE GmshInterface, ONLY: GmshModelOccAddEllipse
USE GmshInterface, ONLY: GmshModelOccAddEllipse
USE GmshInterface, ONLY: GmshModelOccAddSpline
USE GmshInterface, ONLY: GmshModelOccAddBSpline
USE GmshInterface, ONLY: GmshModelOccAddBezier
USE GmshInterface, ONLY: GmshModelOccAddWire
USE GmshInterface, ONLY: GmshModelOccAddCurveLoop
USE GmshInterface, ONLY: GmshModelOccAddRectangle
USE GmshInterface, ONLY: GmshModelOccAddDisk
USE GmshInterface, ONLY: GmshModelOccAddPlaneSurface
USE GmshInterface, ONLY: GmshModelOccAddSurfaceFilling
USE GmshInterface, ONLY: GmshModelOccAddBSplineFilling
USE GmshInterface, ONLY: GmshModelOccAddBezierFilling
USE GmshInterface, ONLY: GmshModelOccAddBSplineSurface
USE GmshInterface, ONLY: GmshModelOccAddBezierSurface
USE GmshInterface, ONLY: GmshModelOccAddTrimmedSurface
USE GmshInterface, ONLY: GmshModelOccAddSurfaceLoop
USE GmshInterface, ONLY: GmshModelOccAddVolume
USE GmshInterface, ONLY: GmshModelOccAddSphere
USE GmshInterface, ONLY: GmshModelOccAddBox
USE GmshInterface, ONLY: GmshModelOccAddCylinder
USE GmshInterface, ONLY: GmshModelOccAddTorus
USE GmshInterface, ONLY: GmshModelOccAddThruSections
USE GmshInterface, ONLY: GmshModelOccSynchronize
USE GmshInterface, ONLY: GmshModelOccAddWedge
USE GmshInterface, ONLY: GmshModelOccAddCone
USE CInterface, ONLY: C_PTR_TO_INT_VEC
IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: modName = "GmshModelOcc_Class@Methods.F90"
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
REAL(DFP), PARAMETER, DIMENSION(0) :: emptyReal = 0
INTEGER(I4B), PARAMETER, DIMENSION(1) :: emptyInt = 0
INTEGER(C_INT) :: ierr

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = "obj_Initiate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    "gmsh::Model::Occ::Mesh is already associated;")
END IF
#endif

ALLOCATE (obj%Mesh)

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

ans = GmshModelOccAddPoint(x, y, z, meshSize, &
                           Input(math%minus_one_i, tag), ierr)

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

ans = GmshModelOccAddLine(startTag, endTag, &
                          Input(math%minus_one_i, tag), ierr)

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

ans = GmshModelOccAddCircleArc(startTag, centerTag, endTag, &
                               Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCircle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCircle()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddCircle( &
      x, y, z, r, Input(math%minus_one_i, tag), &
      Input(option=angle1, default=math%zero), &
      Input(option=angle2, default=math%two * math%pi), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCircle

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

ans = GmshModelOccAddEllipseArc( &
      startTag, centerTag, majorTag, endTag, &
      Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddEllipse
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddEllipse()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddEllipse( &
      x, y, z, r1, r2, Input(math%minus_one_i, tag), angle1, &
      angle2, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddEllipse

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

ans = GmshModelOccAddSpline(pointTags, SIZE(pointTags, kind=C_SIZE_T), &
                            Input(math%minus_one_i, tag), ierr)

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
LOGICAL(LGT) :: isok
INTEGER(C_SIZE_T) :: pointTags_n, weights_n, multiplicities_n, knots_n
REAL(DFP), ALLOCATABLE :: weights_(:), knots_(:)
INTEGER(I4B), ALLOCATABLE :: multiplicities_(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

pointTags_n = SIZE(pointTags)
isok = PRESENT(weights)
IF (isok) THEN
  weights_n = SIZE(weights)
  weights_ = weights
ELSE
  weights_n = 0
  weights_ = emptyReal
END IF

isok = PRESENT(multiplicities)
IF (isok) THEN
  multiplicities_n = SIZE(multiplicities)
  multiplicities_ = multiplicities
ELSE
  multiplicities_n = 0
  multiplicities_ = emptyInt
END IF

isok = PRESENT(knots)
IF (isok) THEN
  knots_n = SIZE(knots)
  knots_ = knots
ELSE
  knots_n = 0
  knots_ = emptyReal
END IF

ans = GmshModelOccAddBSpline( &
      pointTags, pointTags_n, Input(math%minus_one_i, tag), degree, &
      weights_, weights_n, knots_, knots_n, multiplicities_, &
      multiplicities_n, ierr)

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
INTEGER(C_SIZE_T) :: pointTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

pointTags_n = SIZE(pointTags)
ans = GmshModelOccAddBezier( &
      pointTags, pointTags_n, Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddWire
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddWire()"
#endif
INTEGER(C_SIZE_T) :: curveTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

curveTags_n = SIZE(curveTags)
ans = GmshModelOccAddWire( &
      curveTags, curveTags_n, Input(math%minus_one_i, tag), &
      Input(option=checkClosed, default=math%zero_i), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddWire

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCurveLoop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCurveLoop()"
#endif
INTEGER(C_SIZE_T) :: curveTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

curveTags_n = SIZE(curveTags)
ans = GmshModelOccAddCurveLoop(curveTags, curveTags_n, &
                               Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddRectangle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddRectangle()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddRectangle( &
      x, y, z, dx, dy, Input(math%minus_one_i, tag), roundedRadius, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddRectangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddDisk
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddDisk()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddDisk( &
      xc, yc, zc, rx, ry, Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddDisk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPlaneSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPlaneSurface()"
#endif
INTEGER(C_SIZE_T) :: wireTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

wireTags_n = SIZE(wireTags)
ans = GmshModelOccAddPlaneSurface(wireTags, wireTags_n, input(-1, tag), &
                                  ierr)

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
INTEGER(C_SIZE_T) :: pointTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

pointTags_n = SIZE(pointTags)
ans = GmshModelOccAddSurfaceFilling( &
      wireTag, Input(math%minus_one_i, tag), pointTags, pointTags_n, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBSplineFilling
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBSplineFilling()"
#endif
CHARACTER(LEN=maxStrLen), TARGET :: typeOfFilling_
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(typeOfFilling)
IF (isok) THEN
  typeOfFilling_ = TRIM(typeOfFilling)//C_NULL_CHAR
ELSE
  typeOfFilling_ = "Curved"//C_NULL_CHAR
END IF

ans = GmshModelOccAddBSplineFilling( &
      wireTag, Input(math%minus_one_i, tag), &
      C_LOC(typeOfFilling_), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBSplineFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBezierFilling
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBezierFilling()"
#endif
CHARACTER(LEN=maxStrLen), TARGET :: typeOfFilling_
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(typeOfFilling)) THEN
  typeOfFilling_ = TRIM(typeOfFilling)//C_NULL_CHAR
ELSE
  typeOfFilling_ = "Curved"//C_NULL_CHAR
END IF
ans = GmshModelOccAddBezierFilling( &
      wireTag, Input(math%minus_one_i, tag), C_LOC(typeOfFilling_), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBezierFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBSplineSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBSplineSurface()"
#endif
INTEGER(C_SIZE_T) :: pointTags_n, weights_n, knotsU_n, &
                     knotsV_n, multiplicitiesU_n, &
                     multiplicitiesV_n, wireTags_n
INTEGER(I4B), ALLOCATABLE :: multiplicitiesU_(:)
INTEGER(I4B), ALLOCATABLE :: multiplicitiesV_(:)
INTEGER(I4B), ALLOCATABLE :: wireTags_(:)
REAL(DFP), ALLOCATABLE :: weights_(:)
REAL(DFP), ALLOCATABLE :: knotsU_(:)
REAL(DFP), ALLOCATABLE :: knotsV_(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

pointTags_n = SIZE(pointTags)

isok = PRESENT(multiplicitiesU)
IF (isok) THEN
  multiplicitiesU_ = multiplicitiesU
  multiplicitiesU_n = SIZE(multiplicitiesU)
ELSE
  multiplicitiesU_ = emptyInt
  multiplicitiesU_n = 0
END IF

isok = PRESENT(multiplicitiesV)
IF (isok) THEN
  multiplicitiesV_ = multiplicitiesV
  multiplicitiesV_n = SIZE(multiplicitiesV)
ELSE
  multiplicitiesV_ = emptyInt
  multiplicitiesV_n = 0
END IF

isok = PRESENT(wireTags)
IF (isok) THEN
  wireTags_ = wireTags
  wireTags_n = SIZE(wireTags)
ELSE
  wireTags_ = emptyInt
  wireTags_n = 0
END IF

isok = PRESENT(weights)
IF (isok) THEN
  weights_ = weights
  weights_n = SIZE(weights)
ELSE
  weights_ = emptyReal
  weights_n = 0
END IF

isok = PRESENT(knotsU)
IF (isok) THEN
  knotsU_ = knotsU
  knotsU_n = SIZE(knotsU)
ELSE
  knotsU_ = emptyReal
  knotsU_n = 0
END IF

isok = PRESENT(knotsV)
IF (isok) THEN
  knotsV_ = knotsV
  knotsV_n = SIZE(knotsV)
ELSE
  knotsV_ = emptyReal
  knotsV_n = 0
END IF

ans = GmshModelOccAddBSplineSurface( &
      pointTags, pointTags_n, numPointsU, Input(math%minus_one_i, tag), &
      degreeU, degreeV, weights_, weights_n, knotsU_, knotsU_n, &
      knotsV_, knotsV_n, multiplicitiesU_, &
      multiplicitiesU_n, multiplicitiesV_, multiplicitiesV_n, wireTags_, &
      wireTags_n, Input(math%zero_i, wire3D), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBSplineSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBezierSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBezierSurface()"
#endif
INTEGER(C_SIZE_T) :: pointTags_n, wireTags_n
INTEGER(I4B), ALLOCATABLE :: wireTags_(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

pointTags_n = SIZE(pointTags)
isok = PRESENT(wireTags)
IF (isok) THEN
  wireTags_ = wireTags
  wireTags_n = SIZE(wireTags)
ELSE
  wireTags_ = emptyInt
  wireTags_n = 0
END IF

ans = GmshModelOccAddBezierSurface( &
      pointTags, pointTags_n, numPointsU, &
      Input(math%minus_one_i, tag), wireTags_, wireTags_n, &
      Input(math%zero_i, wire3D), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBezierSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddTrimmedSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddTrimmedSurface()"
#endif
INTEGER(C_SIZE_T) :: wireTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

wireTags_n = SIZE(wireTags)
ans = GmshModelOccAddTrimmedSurface( &
      surfaceTag, wireTags, wireTags_n, Input(math%zero_i, wire3D), &
      Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddTrimmedSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSurfaceLoop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddSurfaceLoop()"
#endif
INTEGER(C_SIZE_T) :: surfaceTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

surfaceTags_n = SIZE(surfaceTags)
ans = GmshModelOccAddSurfaceLoop( &
      surfaceTags, surfaceTags_n, Input(math%minus_one_i, tag), &
      Input(math%zero_i, sewing), ierr)

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
INTEGER(C_SIZE_T) :: shellTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

shellTags_n = SIZE(shellTags)
ans = GmshModelOccAddVolume( &
      shellTags, shellTags_n, Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSphere
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddSphere()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddSphere( &
      xc, yc, zc, radius, Input(math%minus_one_i, tag), &
      Input(-math%pi * 0.5_DFP, angle1), &
      Input(-math%pi * 0.5_DFP, angle2), &
      Input(math%pi * 2.0_DFP, angle3), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddSphere

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddBox()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddBox( &
      x, y, z, dx, dy, dz, Input(math%minus_one_i, tag), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCylinder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCylinder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddCylinder( &
      x, y, z, dx, dy, dz, r, Input(math%minus_one_i, tag), &
      Input(math%two * math%pi, angle), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCylinder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCone
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCone()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddCone( &
      x, y, z, dx, dy, dz, r1, r2, &
      Input(math%minus_one_i, tag), Input(math%two * math%pi, angle), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCone

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddWedge
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddWedge()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddWedge( &
      x, y, z, dx, dy, dz, Input(math%minus_one_i, tag), &
      Input(math%zero, ltx), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddWedge

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddTorus
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddTorus()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GmshModelOccAddTorus(x, y, z, r1, r2, Input(math%minus_one_i, tag), &
                           Input(math%two_pi, angle), ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddTorus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddThruSections
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddThruSections()"
#endif
INTEGER(C_SIZE_T) :: wireTags_n, outDimTags_n
TYPE(C_PTR) :: cptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

wireTags_n = SIZE(wireTags)
CALL GmshModelOccAddThruSections( &
  wireTags, wireTags_n, cptr, outDimTags_n, &
  Input(math%minus_one_i, tag), Input(math%one_i, makeSolid), &
  Input(math%zero_i, makeRuled), Input(math%minus_one_i, maxDegree), ierr)

CALL Reallocate(outDimTags, INT(outDimTags_n, I4B))
CALL C_PTR_TO_INT_VEC(cptr=cptr, vec=outDimTags)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddThruSections

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

CALL GmshModelOccSynchronize(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Synchronize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
