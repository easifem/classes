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

SUBMODULE(GmshModel_Class) Methods
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE BaseType, ONLY: math => TypeMathOpt
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_CHAR

USE GmshBasicInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshModelInterface, ONLY: GmshModelAdd
USE GmshModelInterface, ONLY: GmshModelRemove
USE GmshModelInterface, ONLY: GmshModelList
USE GmshModelInterface, ONLY: GmshModelGetCurrent
USE GmshModelInterface, ONLY: GmshModelSetCurrent
USE GmshModelInterface, ONLY: GmshModelGetFileName
USE GmshModelInterface, ONLY: GmshModelSetFileName
USE GmshModelInterface, ONLY: GmshModelGetEntities
USE GmshModelInterface, ONLY: GmshModelSetEntityName
USE GmshModelInterface, ONLY: GmshModelGetEntityName
USE GmshModelInterface, ONLY: GmshModelGetPhysicalGroups
USE GmshModelInterface, ONLY: GmshModelGetEntitiesForPhysicalGroup
USE GmshModelInterface, ONLY: GmshModelGetPhysicalGroupsForEntity
USE GmshModelInterface, ONLY: GmshModelAddPhysicalGroup
USE GmshModelInterface, ONLY: GmshModelRemovePhysicalGroups
USE GmshModelInterface, ONLY: GmshModelSetPhysicalName
USE GmshModelInterface, ONLY: GmshModelRemovePhysicalName
USE GmshModelInterface, ONLY: GmshModelGetPhysicalName
USE GmshModelInterface, ONLY: GmshModelSetTag
USE GmshModelInterface, ONLY: GmshModelGetBoundary
USE GmshModelInterface, ONLY: GmshModelGetAdjacencies
USE GmshModelInterface, ONLY: GmshModelGetEntitiesInBoundingBox
USE GmshModelInterface, ONLY: GmshModelGetBoundingBox
USE GmshModelInterface, ONLY: GmshModelGetDimension
USE GmshModelInterface, ONLY: GmshModelAddDiscreteEntity
USE GmshModelInterface, ONLY: GmshModelRemoveEntities
USE GmshModelInterface, ONLY: GmshModelRemoveEntityName
USE GmshModelInterface, ONLY: GmshModelGetType
USE GmshModelInterface, ONLY: GmshModelGetParent
USE GmshModelInterface, ONLY: GmshModelGetNumberOfPartitions
USE GmshModelInterface, ONLY: GmshModelGetPartitions
USE GmshModelInterface, ONLY: GmshModelGetValue
USE GmshModelInterface, ONLY: GmshModelGetDerivative
USE GmshModelInterface, ONLY: GmshModelGetSecondDerivative
USE GmshModelInterface, ONLY: GmshModelGetCurvature
USE GmshModelInterface, ONLY: GmshModelGetPrincipalCurvatures
USE GmshModelInterface, ONLY: GmshModelGetNormal
USE GmshModelInterface, ONLY: GmshModelGetParametrization
USE GmshModelInterface, ONLY: GmshModelGetParametrizationBounds
USE GmshModelInterface, ONLY: GmshModelIsInside
USE GmshModelInterface, ONLY: GmshModelGetClosestPoint
USE GmshModelInterface, ONLY: GmshModelReparametrizeOnSurface
USE GmshModelInterface, ONLY: GmshModelSetVisibility
USE GmshModelInterface, ONLY: GmshModelGetVisibility
USE GmshModelInterface, ONLY: GmshModelSetVisibilityPerWindow
USE GmshModelInterface, ONLY: GmshModelSetColor
USE GmshModelInterface, ONLY: GmshModelGetColor
USE GmshModelInterface, ONLY: GmshModelSetCoordinates
USE GmshModelInterface, ONLY: GmshModelGetAttributeNames
USE GmshModelInterface, ONLY: GmshModelGetAttribute
USE GmshModelInterface, ONLY: GmshModelSetAttribute
USE GmshModelInterface, ONLY: GmshModelRemoveAttribute

USE GmshUtility, ONLY: istring_
USE GmshUtility, ONLY: ovectorstring_
USE GmshUtility, ONLY: ivectorstring_
USE GmshUtility, ONLY: optval_c_int
USE GmshUtility, ONLY: optval_c_double
USE GmshUtility, ONLY: optval_c_bool
USE GmshUtility, ONLY: ovectorpair_
USE GmshUtility, ONLY: GmshFree
USE GmshUtility, ONLY: ovectorint_
USE GmshUtility, ONLY: ovectordouble_
USE GmshUtility, ONLY: size_gmsh_str

USE CInterface, ONLY: C2Fortran

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "GmshModel_Class@Methods.F90"
INTEGER(C_INT) :: ierr
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%geo)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    "gmsh::Model::Geo is already associated;")
END IF
#endif

ALLOCATE (obj%Geo)
CALL obj%Geo%Initiate()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%occ)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    "gmsh::Model::Occ is already associated;")
END IF
#endif

ALLOCATE (obj%occ)
CALL obj%occ%Initiate()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    "gmsh::Model::Mesh is already associated;")
END IF
#endif

ALLOCATE (obj%mesh)
CALL obj%mesh%Initiate()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Add()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = istring_(name)
CALL GmshModelAdd(name=C_LOC(name_), ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Add

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

CALL GmshModelRemove(ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_List
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_List()"
#endif
TYPE(C_PTR) :: names_
INTEGER(C_SIZE_T) :: names_n_
CHARACTER(maxStrLen), ALLOCATABLE :: names0(:)
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelList(names=names_, names_n=names_n_, ierr=ierr)

ans = INT(names_n_, I4B)
names0 = ovectorstring_(cptr=names_, n=names_n_)

IF (ans .GT. 0) THEN
  ALLOCATE (names(ans - 1))
END IF

DO ii = 1, ans - 1
  names(ii) = TRIM(names0(ii + 1))
END DO

ans = MAX(ans - 1, math%zero_i)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_List

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurrent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCurrent()"
#endif
TYPE(C_PTR) :: cstring

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetCurrent(name=cstring, ierr=ierr)
ans = INT(ierr, KIND=I4B)
CALL C2Fortran(C_string=cstring, F_string=name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCurrent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCurrent()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR
CALL GmshModelSetCurrent(name=C_LOC(name_), ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFileName()"
#endif
TYPE(C_PTR) :: cstring

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetFileName(fileName=cstring, ierr=ierr)
ans = INT(ierr, KIND=I4B)
CALL C2Fortran(C_string=cstring, F_string=fileName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFileName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFileName()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(fileName)//C_NULL_CHAR
CALL GmshModelSetFileName(fileName=C_LOC(name_), ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEntities()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: dimTags_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetEntities( &
  dimTags=cptr, dimTags_n=dimTags_n_, &
  dim=optval_c_int(default=input(default=math%minus_one_i, option=dim)), &
  ierr=ierr)

ans = INT(ierr, I4B)

dimTags = ovectorpair_(cptr=cptr, n=dimTags_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the name of the entity of dimension `dim' and tag `tag'.

MODULE PROCEDURE obj_SetEntityName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetEntityName()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshModelSetEntityName( &
  dim=dim, tag=tag, name=C_LOC(name_), ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the name of the entity of dimension `dim' and tag `tag'.

MODULE PROCEDURE obj_GetEntityName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEntityName()"
#endif
TYPE(C_PTR) :: cptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetEntityName(dim=dim, tag=tag, name=cptr, ierr=ierr)
ans = INT(ierr, I4B)
CALL C2Fortran(c_string=cptr, f_string=name)
CALL GmshFree(cptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get all the physical groups in the current model. If `dim' is >= 0, return
!! only the entities of the specified dimension
!! (e.g. physical points if `dim'
!! == 0). The entities are returned as a vector of (dim, tag) pairs.

MODULE PROCEDURE obj_GetPhysicalGroups
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPhysicalGroups()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: dimTags_n_
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetPhysicalGroups( &
  dimTags=cptr, dimTags_n=dimTags_n_, dim=dim, ierr=ierr)

ans = INT(ierr, I4B)

isok = dimTags_n_ .EQ. 0
IF (isok) THEN
  CALL Reallocate(dimTags, 0, 0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

dimTags = ovectorpair_(cptr=cptr, n=dimTags_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

MODULE PROCEDURE obj_GetEntitiesForPhysicalGroup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEntitiesForPhysicalGroup()"
#endif

TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: tags_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetEntitiesForPhysicalGroup( &
  dim=dim, tag=tag, tags=cptr, tags_n=tags_n_, ierr=ierr)

ans = INT(ierr, I4B)

CALL Reallocate(tags, INT(tags_n_, I4B))
CALL C2Fortran(cptr=cptr, vec=tags)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEntitiesForPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

MODULE PROCEDURE obj_GetPhysicalGroupsForEntity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPhysicalGroupsForEntity()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: physicalTags_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetPhysicalGroupsForEntity( &
  dim=dim, tag=tag, physicalTags=cptr, physicalTags_n=physicalTags_n_, &
  ierr=ierr)

ans = INT(ierr, I4B)

CALL Reallocate(physicalTags, INT(physicalTags_n_, i4b))
CALL C2Fortran(cptr=cptr, vec=physicalTags)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPhysicalGroupsForEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a physical group of dimension `dim', grouping the model entities with
!! tags `tags'. Return the tag of the physical group, equal to `tag' if `tag'
!! is positive, or a new tag if `tag' < 0.

MODULE PROCEDURE obj_AddPhysicalGroup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPhysicalGroup()"
#endif
INTEGER(C_SIZE_T) :: tags_n
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(input(option="", default=name))//C_NULL_CHAR
tags_n = SIZE(tags)

ans = GmshModelAddPhysicalGroup( &
      dim=optval_c_int(default=dim), &
      tags=optval_c_int(default=tags), &
      tags_n=tags_n, &
      tag=optval_c_int(default=Input( &
                       option=math%minus_one_i, default=tag)), &
      name=C_LOC(name_), &
      ierr=ierr)

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
INTEGER(C_SIZE_T) :: dimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dimTags_n = SIZE(dimTags)

CALL GmshModelRemovePhysicalGroups( &
  dimTags=optval_c_int(default=dimTags), dimTags_n=dimTags_n, ierr=ierr)

ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPhysicalName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPhysicalName()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshModelSetPhysicalName( &
  dim=dim, tag=optval_c_int(default=tag), name=C_LOC(name_), ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemovePhysicalName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RemovePhysicalName()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR
CALL GmshModelRemovePhysicalName(name=C_LOC(name_), ierr=ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemovePhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPhysicalName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPhysicalName()"
#endif
TYPE(C_PTR) :: cptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetPhysicalName( &
  dim=optval_c_int(default=dim), tag=optval_c_int(default=tag), &
  name=cptr, ierr=ierr)

ans = INT(ierr, I4B)

CALL C2Fortran(C_STRING=cptr, F_STRING=name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTag
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTag()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetTag(dim=optval_c_int(default=dim), &
                     tag=optval_c_int(default=tag), &
                     newtag=optval_c_int(default=newtag), ierr=ierr)

ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundary
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBoundary()"
#endif
TYPE(C_PTR) :: outDimTags_
INTEGER(C_SIZE_T) :: outDimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetBoundary( &
  dimTags=dimTags, &
  dimTags_n=optval_c_int(default=SIZE(dimTags, KIND=I4B)), &
  outDimTags=outDimTags_, &
  outDimTags_n=outDimTags_n, &
  combined=optval_c_bool(math%yes, combined), &
  oriented=optval_c_bool(math%yes, oriented), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

outDimTags = ovectorpair_(cptr=outDimTags_, n=outDimTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAdjacencies
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAdjacencies()"
#endif
TYPE(C_PTR) :: upward_
INTEGER(C_SIZE_T) :: upward_n
TYPE(C_PTR) :: downward_
INTEGER(C_SIZE_T) :: downward_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetAdjacencies( &
  dim=optval_c_int(default=dim), &
  tag=optval_c_int(default=tag), &
  upward=upward_, &
  upward_n=upward_n, &
  downward=downward_, &
  downward_n=downward_n, &
  ierr=ierr)

ans = INT(ierr, I4B)

upward = ovectorint_(cptr=upward_, n=upward_n)
downward = ovectorint_(cptr=downward_, n=downward_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetAdjacencies

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEntitiesInBoundingBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEntitiesInBoundingBox()"
#endif
TYPE(C_PTR) :: dimTags_
INTEGER(C_SIZE_T) :: dimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetEntitiesInBoundingBox( &
  xmin=optval_c_double(default=xmin), &
  xmax=optval_c_double(default=xmax), &
  ymin=optval_c_double(default=ymin), &
  ymax=optval_c_double(default=ymax), &
  zmin=optval_c_double(default=zmin), &
  zmax=optval_c_double(default=zmax), &
  tags=dimTags_, &
  tags_n=dimTags_n, &
  dim=optval_c_int(default=Input(default=math%minus_one_i, option=dim)), &
  ierr=ierr)

dimTags = ovectorpair_(dimTags_, dimTags_n)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEntitiesInBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBoundingBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBoundingBox()"
#endif
INTEGER(I4B), PARAMETER :: six_ = 6
REAL(C_DOUBLE) :: x(six_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetBoundingBox( &
  dim=optval_c_int(default=dim), &
  tag=optval_c_int(default=tag), &
  xmin=x(1), ymin=x(2), zmin=x(3), &
  xmax=x(4), ymax=x(5), zmax=x(6), ierr=ierr)

ans = INT(ierr, I4B)
xmin = REAL(x(1), DFP)
ymin = REAL(x(2), DFP)
zmin = REAL(x(3), DFP)
xmax = REAL(x(4), DFP)
ymax = REAL(x(5), DFP)
zmax = REAL(x(6), DFP)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDimension
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDimension()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = INT(GmshModelGetDimension(ierr=ierr), I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddDiscreteEntity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddDiscreteEntity()"
#endif
INTEGER(C_INT) :: ans0
INTEGER(C_SIZE_T) :: boundary_n
LOGICAL(LGT) :: isok
INTEGER(C_INT), ALLOCATABLE :: boundary0(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(boundary)

IF (isok) THEN
  boundary0 = optval_c_int(default=boundary)
  boundary_n = SIZE(boundary, kind=C_SIZE_T)

ELSE
  ALLOCATE (boundary0(0))
  boundary_n = 0_C_SIZE_T

END IF

ans0 = GmshModelAddDiscreteEntity( &
       dim=optval_c_int(default=dim), &
       tag=optval_c_int(default=Input(math%minus_one_i, tag)), &
       boundary=boundary0, &
       boundary_n=boundary_n, &
       ierr=ierr)

ans = INT(ans0, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddDiscreteEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RemoveEntities()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelRemoveEntities( &
  dimTags=optval_c_int(default=dimTags), &
  dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemoveEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveEntityName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RemoveEntityName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelRemoveEntityName(name=istring_(name), &
                               ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemoveEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetType( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), entityType=ans, &
  ierr=ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! In a partitioned model, get the parent of the entity of dimension `dim'
!! and
!! tag `tag', i.e. from which the entity is a part of, if any.
!! `parentDim' and
!! `parentTag' are Set to -1 if the entity has no parent.

MODULE PROCEDURE obj_GetParent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParent()"
#endif
INTEGER(C_INT) :: parentDim0, parentTag0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetParent( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
  parentDim=parentDim0, parentTag=parentTag0, &
  ierr=ierr)

ans = INT(ierr, i4b)
parentDim = INT(parentDim0, i4b)
parentTag = INT(parentTag0, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNumberOfPartitions
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNumberOfPartitions()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = INT(GmshModelGetNumberOfPartitions(ierr=ierr), I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNumberOfPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPartitions
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPartitions()"
#endif

TYPE(C_PTR) :: partitions_
INTEGER(C_SIZE_T) :: partitions_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetPartitions( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
  partitions=partitions_, partitions_n=partitions_n, &
  ierr=ierr)

ans = ovectorint_(partitions_, partitions_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetValue()"
#endif
TYPE(C_PTR) :: coord_
INTEGER(C_SIZE_T) :: coord_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetValue( &
  dim=INT(dim, C_INT), &
  tag=INT(tag, C_INT), &
  parametricCoord=optval_c_double(default=parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  coord=coord_, &
  coord_n=coord_n_, &
  ierr=ierr)

ans = ovectordouble_(coord_, coord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDerivative
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDerivative()"
#endif
TYPE(C_PTR) :: derivatives_
INTEGER(C_SIZE_T) :: derivatives_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetDerivative( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
  parametricCoord=parametricCoord, &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  derivatives=derivatives_, derivatives_n=derivatives_n_, &
  ierr=ierr)

derivatives = ovectordouble_(derivatives_, derivatives_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSecondDerivative
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSecondDerivative()"
#endif
TYPE(C_PTR) :: derivatives_
INTEGER(C_SIZE_T) :: derivatives_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetSecondDerivative( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
  parametricCoord=optval_c_double(default=parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  derivatives=derivatives_, derivatives_n=derivatives_n_, &
  ierr=ierr)

derivatives = ovectordouble_(derivatives_, derivatives_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSecondDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurvature
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCurvature()"
#endif
TYPE(C_PTR) :: curvatures_
INTEGER(C_SIZE_T) :: curvatures_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetCurvature( &
  dim=INT(dim, C_INT), &
  tag=INT(tag, C_INT), &
  parametricCoord=optval_c_double(default=curvatures), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  curvatures=curvatures_, &
  curvatures_n=curvatures_n_, &
  ierr=ierr)

curvatures = ovectordouble_(curvatures_, curvatures_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCurvature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrincipalCurvatures
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPrincipalCurvatures()"
#endif
TYPE(C_PTR) :: curvatureMax_
INTEGER(C_SIZE_T) :: curvatureMax_n_
TYPE(C_PTR) :: curvatureMin_
INTEGER(C_SIZE_T) :: curvatureMin_n_
TYPE(C_PTR) :: directionMax_
INTEGER(C_SIZE_T) :: directionMax_n_
TYPE(C_PTR) :: directionMin_
INTEGER(C_SIZE_T) :: directionMin_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetPrincipalCurvatures( &
  tag=INT(tag, C_INT), &
  parametricCoord=optval_c_double(default=parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  curvatureMax=curvatureMax_, &
  curvatureMax_n=curvatureMax_n_, &
  curvatureMin=curvatureMin_, &
  curvatureMin_n=curvatureMin_n_, &
  directionMax=directionMax_, &
  directionMax_n=directionMax_n_, &
  directionMin=directionMin_, &
  directionMin_n=directionMin_n_, &
  ierr=ierr)

curvatureMax = ovectordouble_(curvatureMax_, curvatureMax_n_)
curvatureMin = ovectordouble_(curvatureMin_, curvatureMin_n_)
directionMax = ovectordouble_(directionMax_, directionMax_n_)
directionMin = ovectordouble_(directionMin_, directionMin_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPrincipalCurvatures

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNormal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNormal()"
#endif
TYPE(C_PTR) :: normals_
INTEGER(C_SIZE_T) :: normals_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetNormal( &
  tag=INT(tag, C_INT), &
  parametricCoord=optval_c_double(default=parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  normals=normals_, &
  normals_n=normals_n_, &
  ierr=ierr)

ans = ovectordouble_(normals_, normals_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParametrization
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParametrization()"
#endif
TYPE(C_PTR) :: parametricCoord_
INTEGER(C_SIZE_T) :: parametricCoord_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetParametrization( &
  dim=INT(dim, C_INT), &
  tag=INT(tag, C_INT), &
  coord=optval_c_double(default=coord), &
  coord_n=SIZE(coord, kind=C_SIZE_T), &
  parametricCoord=parametricCoord_, &
  parametricCoord_n=parametricCoord_n_, &
  ierr=ierr)

ans = ovectordouble_(parametricCoord_, parametricCoord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParametrization

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParametrizationBounds
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParametrizationBounds()"
#endif
TYPE(C_PTR) :: min_
INTEGER(C_SIZE_T) :: min_n_
TYPE(C_PTR) :: max_
INTEGER(C_SIZE_T) :: max_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetParametrizationBounds( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), min=min_, min_n=min_n_, &
  max=max_, max_n=max_n_, ierr=ierr)

min = ovectordouble_(min_, min_n_)
max = ovectordouble_(max_, max_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParametrizationBounds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInside
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsInside()"
#endif
INTEGER(C_INT) :: ans0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans0 = GmshModelIsInside( &
       dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
       coord=optval_c_double(default=coord), &
       coord_n=SIZE(coord, kind=C_SIZE_T), &
       parametric=optval_c_bool(math%no, parametric), ierr=ierr)

ans = ans0 .NE. 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetClosestPoint
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetClosestPoint()"
#endif
TYPE(C_PTR) :: closestCoord_
INTEGER(C_SIZE_T) :: closestCoord_n_
TYPE(C_PTR) :: parametricCoord_
INTEGER(C_SIZE_T) :: parametricCoord_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetClosestPoint( &
  dim=INT(dim, C_INT), &
  tag=INT(tag, C_INT), &
  coord=optval_c_double(default=coord), &
  coord_n=SIZE(coord, kind=C_SIZE_T), &
  closestCoord=closestCoord_, &
  closestCoord_n=closestCoord_n_, &
  parametricCoord=parametricCoord_, &
  parametricCoord_n=parametricCoord_n_, &
  ierr=ierr)

ans = INT(ierr, I4B)
closestCoord = ovectordouble_(closestCoord_, closestCoord_n_)

parametricCoord = ovectordouble_(parametricCoord_, &
                                 parametricCoord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetClosestPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReparametrizeOnSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReparametrizeOnSurface()"
#endif
TYPE(C_PTR) :: surfaceParametricCoord_
INTEGER(C_SIZE_T) :: surfaceParametricCoord_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelReparametrizeOnSurface( &
  dim=INT(dim, C_INT), &
  tag=INT(tag, C_INT), &
  parametricCoord=optval_c_double(default=parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  surfaceTag=INT(surfaceTag, C_INT), &
  surfaceParametricCoord=surfaceParametricCoord_, &
  surfaceParametricCoord_n=surfaceParametricCoord_n_, &
  which=optval_c_int(math%zero_i, which), &
  ierr=ierr)

ans = ovectordouble_(surfaceParametricCoord_, &
                     surfaceParametricCoord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReparametrizeOnSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetVisibility
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetVisibility()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetVisibility( &
  dimTags=optval_c_int(default=dimTags), &
  dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
  VALUE=INT(VALUE, C_INT), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVisibility
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVisibility()"
#endif
INTEGER(C_INT) :: value0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetVisibility( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), VALUE=value0, &
  ierr=ierr)

ans = INT(value0, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetVisibilityPerWindow
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetVisibilityPerWindow()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetVisibilityPerWindow( &
  VALUE=INT(VALUE, C_INT), &
  windowIndex=optval_c_int(default=Input( &
                           default=math%zero_i, option=windowIndex)), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetVisibilityPerWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColor
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetColor()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetColor( &
  dimTags=optval_c_int(default=dimTags), &
  dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
  r=INT(r, C_INT), &
  g=INT(g, C_INT), &
  b=INT(b, C_INT), &
  a=INT(Input(default=255_I4B, option=a), kind=C_INT), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColor
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetColor()"
#endif
INTEGER(C_INT) :: rgba(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetColor( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), r=rgba(1), g=rgba(2), &
  b=rgba(3), a=rgba(4), ierr=ierr)

ans = INT(rgba, kind=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCoordinates
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCoordinates()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetCoordinates( &
  tag=INT(tag, C_INT), x=REAL(x, C_DOUBLE), y=REAL(y, C_DOUBLE), &
  z=REAL(z, C_DOUBLE), ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCoordinates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAttributeNames
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAttributeNames()"
#endif
CHARACTER(maxStrLen), ALLOCATABLE :: names0(:)
TYPE(C_PTR) :: names_
INTEGER(C_SIZE_T) :: names_n_
INTEGER(i4b) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetAttributeNames(names=names_, names_n=names_n_, &
                                ierr=ierr)

names0 = ovectorstring_(names_, names_n_)

ALLOCATE (names(names_n_))

DO ii = 1, INT(names_n_, i4b)
  names(ii) = TRIM(names0(ii))
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetAttributeNames

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAttribute
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAttribute()"
#endif
CHARACTER(maxStrLen), ALLOCATABLE :: values(:)
TYPE(C_PTR) :: values_
INTEGER(C_SIZE_T) :: values_n_
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetAttribute(name=istring_(name), values=values_, &
                           values_n=values_n_, ierr=ierr)
values = ovectorstring_(values_, values_n_)
ALLOCATE (ans(values_n_))

DO ii = 1, INT(values_n_, i4b)
  ans(ii) = TRIM(values(ii))
END DO

IF (ALLOCATED(values)) DEALLOCATE (values)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAttribute
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAttribute()"
#endif
CHARACTER(len=maxStrLen, kind=C_CHAR), ALLOCATABLE :: values_strs(:)
TYPE(C_PTR), ALLOCATABLE :: values_(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ivectorstring_(values, values_strs, values_)

CALL GmshModelSetAttribute( &
  name=istring_(name), values=values_, values_n=size_gmsh_str(values), &
  ierr=ierr)

ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveAttribute
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RemoveAttribute()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelRemoveAttribute(name=istring_(name), ierr=ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RemoveAttribute

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
