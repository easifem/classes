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

SUBMODULE(GmshModelGeoMesh_Class) Methods
USE GlobalData, ONLY: DFP, I4B, LGT
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: math => TypeMathOpt
USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshModelGeoMeshSetSize
USE GmshInterface, ONLY: GmshModelGeoMeshSetTransfiniteSurface
USE GmshInterface, ONLY: GmshModelGeoMeshSetTransfiniteVolume
USE GmshInterface, ONLY: GmshModelGeoMeshSetRecombine
USE GmshInterface, ONLY: GmshModelGeoMeshSetReverse
USE GmshInterface, ONLY: GmshModelGeoMeshSetAlgorithm
USE GmshInterface, ONLY: GmshModelGeoMeshSetSizeFromBoundary
USE GmshInterface, ONLY: GmshModelGeoMeshSetSmoothing
USE GmshInterface, ONLY: GmshModelGeoMeshSetTransfiniteCurve
USE CInterface, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "GmshModelGeoMesh_Class@Methods.F90"
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
INTEGER(C_INT) :: ierr

CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMeshSetSize( &
  dimTags, SIZE(dimTags, KIND=C_SIZE_T), meshSize, ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSize

!----------------------------------------------------------------------------
!                                                        SetTransfiniteCurve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTransfiniteCurve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTransfiniteCurve()"
#endif

! internal variables
CHARACTER(maxStrLen), TARGET :: meshType_
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(meshType)

IF (isok) THEN
  meshType_ = TRIM(meshType)//C_NULL_CHAR
ELSE
  meshType_ = "Progression"//C_NULL_CHAR
END IF

CALL GmshModelGeoMeshSetTransfiniteCurve( &
  tag, nPoints, C_LOC(meshType_), Input(option=coef, default=math%one), &
  ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTransfiniteCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTransfiniteSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTransfiniteSurface()"
#endif
CHARACTER(maxStrLen), TARGET :: arrangement_
INTEGER(C_SIZE_T) :: cornerTags_n
INTEGER(I4B), ALLOCATABLE :: cornerTags_(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(arrangement)
IF (isok) THEN
  arrangement_ = TRIM(arrangement)//C_NULL_CHAR
ELSE
  arrangement_ = "Left"//C_NULL_CHAR
END IF

isok = PRESENT(cornerTags)
IF (isok) THEN
  cornerTags_n = SIZE(cornerTags)
  cornerTags_ = cornerTags
ELSE
  cornerTags_n = math%zero_i
  ALLOCATE (cornerTags_(0))
END IF

CALL GmshModelGeoMeshSetTransfiniteSurface( &
  tag, C_LOC(arrangement_), cornerTags_, cornerTags_n, ierr)

isok = ALLOCATED(cornerTags_)
IF (isok) DEALLOCATE (cornerTags_)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTransfiniteSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTransfiniteVolume
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTransfiniteVolume()"
#endif
INTEGER(C_SIZE_T) :: cornerTags_n
INTEGER(I4B), ALLOCATABLE :: cornerTags_(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(cornerTags)

IF (isok) THEN
  cornerTags_n = SIZE(cornerTags)
  cornerTags_ = cornerTags
ELSE
  cornerTags_n = 0_I4B
  ALLOCATE (cornerTags_(0))
END IF

CALL GmshModelGeoMeshSetTransfiniteVolume( &
  tag, cornerTags_, cornerTags_n, ierr)

isok = ALLOCATED(cornerTags_)
IF (isok) DEALLOCATE (cornerTags_)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTransfiniteVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRecombine
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRecombine()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMeshSetRecombine( &
  dim, tag, Input(option=angle, default=45.0_DFP), ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRecombine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSmoothing
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSmoothing()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMeshSetSmoothing(dim, tag, val, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSmoothing

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetReverse
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetReverse()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMeshSetReverse(dim, tag, val, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetReverse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAlgorithm
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAlgorithm()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMeshSetAlgorithm(dim, tag, val, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAlgorithm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSizeFromBoundary
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSizeFromBoundary()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGeoMeshSetSizeFromBoundary(dim, tag, val, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSizeFromBoundary

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
