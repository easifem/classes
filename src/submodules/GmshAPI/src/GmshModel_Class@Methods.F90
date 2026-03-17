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
USE BaseType, ONLY: math => TypeMathOpt
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_CHAR

USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshModelAdd
USE GmshInterface, ONLY: GmshModelRemove
USE GmshInterface, ONLY: GmshModelList
USE GmshInterface, ONLY: GmshModelGetCurrent
USE GmshInterface, ONLY: GmshModelSetCurrent
USE GmshInterface, ONLY: GmshModelGetFileName
USE GmshInterface, ONLY: GmshModelSetFileName
USE GmshInterface, ONLY: GmshModelGetEntities
USE GmshInterface, ONLY: GmshModelSetEntityName
USE GmshInterface, ONLY: GmshModelGetEntityName
USE GmshInterface, ONLY: GmshModelGetPhysicalGroups
USE GmshInterface, ONLY: GmshModelGetEntitiesForPhysicalGroup
USE GmshInterface, ONLY: GmshModelGetPhysicalGroupsForEntity
USE GmshInterface, ONLY: GmshModelAddPhysicalGroup
USE GmshInterface, ONLY: GmshModelRemovePhysicalGroups
USE GmshInterface, ONLY: GmshModelSetPhysicalName
USE GmshInterface, ONLY: GmshModelRemovePhysicalName
USE GmshInterface, ONLY: GmshModelGetPhysicalName
USE GmshInterface, ONLY: GmshModelSetTag
USE GmshInterface, ONLY: GmshModelGetBoundary
USE GmshInterface, ONLY: GmshModelGetAdjacencies
USE GmshInterface, ONLY: GmshModelGetEntitiesInBoundingBox
USE GmshInterface, ONLY: GmshModelGetBoundingBox
USE GmshInterface, ONLY: GmshModelGetDimension
USE GmshInterface, ONLY: GmshModelAddDiscreteEntity
USE GmshInterface, ONLY: GmshModelRemoveEntities
USE GmshInterface, ONLY: GmshModelRemoveEntityName
USE GmshInterface, ONLY: GmshModelGetType
USE GmshInterface, ONLY: GmshModelGetParent
USE GmshInterface, ONLY: GmshModelGetNumberOfPartitions
USE GmshInterface, ONLY: GmshModelGetPartitions
USE GmshInterface, ONLY: GmshModelGetValue
USE GmshInterface, ONLY: GmshModelGetDerivative
USE GmshInterface, ONLY: GmshModelGetSecondDerivative
USE GmshInterface, ONLY: GmshModelGetCurvature
USE GmshInterface, ONLY: GmshModelGetPrincipalCurvatures
USE GmshInterface, ONLY: GmshModelGetNormal
USE GmshInterface, ONLY: GmshModelGetParametrization
USE GmshInterface, ONLY: GmshModelGetParametrizationBounds
USE GmshInterface, ONLY: GmshModelIsInside
USE GmshInterface, ONLY: GmshModelGetClosestPoint
USE GmshInterface, ONLY: GmshModelReparametrizeOnSurface
USE GmshInterface, ONLY: GmshModelSetVisibility
USE GmshInterface, ONLY: GmshModelGetVisibility
USE GmshInterface, ONLY: GmshModelSetVisibilityPerWindow
USE GmshInterface, ONLY: GmshModelSetColor
USE GmshInterface, ONLY: GmshModelGetColor
USE GmshInterface, ONLY: GmshModelSetCoordinates
USE GmshInterface, ONLY: GmshModelGetAttributeNames
USE GmshInterface, ONLY: GmshModelGetAttribute
USE GmshInterface, ONLY: GmshModelSetAttribute
USE GmshInterface, ONLY: GmshModelRemoveAttribute

USE GmshUtility, ONLY: gmsh_CString
USE GmshUtility, ONLY: gmsh_cStrings2CharArray
USE GmshUtility, ONLY: gmsh_GetCharArray_cPtr
USE GmshUtility, ONLY: gmsh_cint
USE GmshUtility, ONLY: gmsh_cdouble
USE GmshUtility, ONLY: gmsh_dimtag_c2f
USE GmshUtility, ONLY: GmshFree
USE GmshUtility, ONLY: gmsh_intvec_c2f
USE GmshUtility, ONLY: gmsh_realvec_c2f
USE GmshUtility, ONLY: gmsh_size_str

USE CInterface, ONLY: C2Fortran
USE CInterface, ONLY: optval_c_bool
USE CInterface, ONLY: optval_c_int

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "GmshModel_Class@Methods.F90"
INTEGER(C_INT) :: ierr
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_Initiate"
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
END PROCEDURE model_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_Add
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_Add()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = gmsh_CString(name)
CALL GmshModelAdd(name=C_LOC(name_), ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_Add

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_Remove
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_Remove()"
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
END PROCEDURE model_Remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_List
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_List()"
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
names0 = gmsh_cStrings2CharArray(cptr=names_, n=names_n_)

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
END PROCEDURE model_List

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetCurrent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetCurrent()"
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
END PROCEDURE model_GetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetCurrent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetCurrent()"
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
END PROCEDURE model_SetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetFileName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetFileName()"
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
END PROCEDURE model_GetFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetFileName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetFileName()"
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
END PROCEDURE model_SetFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetEntities()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: dimTags_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetEntities( &
  dimTags=cptr, dimTags_n=dimTags_n_, &
  dim=gmsh_cint(input(default=math%minus_one_i, option=dim)), &
  ierr=ierr)

ans = INT(ierr, I4B)

dimTags = gmsh_dimtag_c2f(cptr=cptr, n=dimTags_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the name of the entity of dimension `dim' and tag `tag'.

MODULE PROCEDURE model_SetEntityName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetEntityName()"
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
END PROCEDURE model_SetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the name of the entity of dimension `dim' and tag `tag'.

MODULE PROCEDURE model_GetEntityName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetEntityName()"
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
END PROCEDURE model_GetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get all the physical groups in the current model. If `dim' is >= 0, return
!! only the entities of the specified dimension (e.g. physical points if `dim'
!! == 0). The entities are returned as a vector of (dim, tag) pairs.

MODULE PROCEDURE model_GetPhysicalGroups
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetPhysicalGroups()"
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

dimTags = gmsh_dimtag_c2f(cptr=cptr, n=dimTags_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetPhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

MODULE PROCEDURE model_GetEntitiesForPhysicalGroup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetEntitiesForPhysicalGroup()"
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
END PROCEDURE model_GetEntitiesForPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

MODULE PROCEDURE model_GetPhysicalGroupsForEntity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetPhysicalGroupsForEntity()"
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
END PROCEDURE model_GetPhysicalGroupsForEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a physical group of dimension `dim', grouping the model entities with
!! tags `tags'. Return the tag of the physical group, equal to `tag' if `tag'
!! is positive, or a new tag if `tag' < 0.

MODULE PROCEDURE model_AddPhysicalGroup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_AddPhysicalGroup()"
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
      dim=gmsh_cint(dim), &
      tags=gmsh_cint(tags), &
      tags_n=tags_n, &
      tag=gmsh_cint(Input(option=math%minus_one_i, default=tag)), &
      name=C_LOC(name_), &
      ierr=ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_AddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_RemovePhysicalGroups
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_RemovePhysicalGroups()"
#endif
INTEGER(C_SIZE_T) :: dimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dimTags_n = SIZE(dimTags)

CALL GmshModelRemovePhysicalGroups( &
  dimTags=gmsh_cint(dimTags), dimTags_n=dimTags_n, ierr=ierr)

ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetPhysicalName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetPhysicalName()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshModelSetPhysicalName( &
  dim=dim, tag=gmsh_cint(tag), name=C_LOC(name_), ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_SetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_RemovePhysicalName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_RemovePhysicalName()"
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
END PROCEDURE model_RemovePhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetPhysicalName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetPhysicalName()"
#endif
TYPE(C_PTR) :: cptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetPhysicalName( &
  dim=gmsh_cint(dim), tag=gmsh_cint(tag), name=cptr, ierr=ierr)

ans = INT(ierr, I4B)

CALL C2Fortran(C_STRING=cptr, F_STRING=name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetTag
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetTag()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetTag(dim=gmsh_cint(dim), tag=gmsh_cint(tag), &
                     newtag=gmsh_cint(newtag), ierr=ierr)

ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_SetTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetBoundary
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetBoundary()"
#endif
TYPE(C_PTR) :: outDimTags_
INTEGER(C_SIZE_T) :: outDimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetBoundary( &
  dimTags=dimTags, &
  dimTags_n=gmsh_cint(SIZE(dimTags, KIND=I4B)), &
  outDimTags=outDimTags_, &
  outDimTags_n=outDimTags_n, &
  combined=optval_c_bool(math%yes, combined), &
  oriented=optval_c_bool(math%yes, oriented), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

outDimTags = gmsh_dimtag_c2f(cptr=outDimTags_, n=outDimTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetAdjacencies
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetAdjacencies()"
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
  dim=gmsh_cint(dim), &
  tag=gmsh_cint(tag), &
  upward=upward_, &
  upward_n=upward_n, &
  downward=downward_, &
  downward_n=downward_n, &
  ierr=ierr)

ans = INT(ierr, I4B)

upward = gmsh_intvec_c2f(cptr=upward_, n=upward_n)
downward = gmsh_intvec_c2f(cptr=downward_, n=downward_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetAdjacencies

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetEntitiesInBoundingBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetEntitiesInBoundingBox()"
#endif
TYPE(C_PTR) :: dimTags_
INTEGER(C_SIZE_T) :: dimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetEntitiesInBoundingBox( &
  xmin=gmsh_cdouble(xmin), &
  xmax=gmsh_cdouble(xmax), &
  ymin=gmsh_cdouble(ymin), &
  ymax=gmsh_cdouble(ymax), &
  zmin=gmsh_cdouble(zmin), &
  zmax=gmsh_cdouble(zmax), &
  tags=dimTags_, &
  tags_n=dimTags_n, &
  dim=gmsh_cint(Input(default=math%minus_one_i, option=dim)), &
  ierr=ierr)

dimTags = gmsh_dimtag_c2f(dimTags_, dimTags_n)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetEntitiesInBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetBoundingBox
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetBoundingBox()"
#endif
REAL(C_DOUBLE) :: x(6)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetBoundingBox( &
  dim=gmsh_cint(dim), tag=gmsh_cint(tag), xmin=x(1), ymin=x(2), &
  zmin=x(3), xmax=x(4), ymax=x(5), zmax=x(6), ierr=ierr)

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
END PROCEDURE model_GetBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetDimension
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetDimension()"
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
END PROCEDURE model_GetDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_AddDiscreteEntity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_AddDiscreteEntity()"
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
  boundary0 = gmsh_cint(boundary)
  boundary_n = SIZE(boundary, kind=C_SIZE_T)

ELSE
  ALLOCATE (boundary0(0))
  boundary_n = 0_C_SIZE_T

END IF

ans0 = GmshModelAddDiscreteEntity( &
       dim=gmsh_cint(dim), &
       tag=gmsh_cint(Input(math%minus_one_i, tag)), &
       boundary=boundary0, &
       boundary_n=boundary_n, &
       ierr=ierr)

ans = INT(ans0, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_AddDiscreteEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_RemoveEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_RemoveEntities()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelRemoveEntities( &
  dimTags=gmsh_cint(dimTags), &
  dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
  RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_RemoveEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_RemoveEntityName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_RemoveEntityName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelRemoveEntityName(name=gmsh_CString(name), &
                               ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_RemoveEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetType()"
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
END PROCEDURE model_GetType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> In a partitioned model, get the parent of the entity of dimension `dim' and
!! tag `tag', i.e. from which the entity is a part of, if any. `parentDim' and
!! `parentTag' are Set to -1 if the entity has no parent.

MODULE PROCEDURE model_GetParent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetParent()"
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
END PROCEDURE model_GetParent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetNumberOfPartitions
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetNumberOfPartitions()"
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
END PROCEDURE model_GetNumberOfPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetPartitions
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetPartitions()"
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

ans = gmsh_intvec_c2f(partitions_, partitions_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetValue
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetValue()"
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
  parametricCoord=gmsh_cdouble(parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  coord=coord_, &
  coord_n=coord_n_, &
  ierr=ierr)

ans = gmsh_realvec_c2f(coord_, coord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetDerivative
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetDerivative()"
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

derivatives = gmsh_realvec_c2f(derivatives_, derivatives_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetSecondDerivative
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetSecondDerivative()"
#endif
TYPE(C_PTR) :: derivatives_
INTEGER(C_SIZE_T) :: derivatives_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetSecondDerivative( &
  dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
  parametricCoord=gmsh_cdouble(parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  derivatives=derivatives_, derivatives_n=derivatives_n_, &
  ierr=ierr)

derivatives = gmsh_realvec_c2f(derivatives_, derivatives_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetSecondDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetCurvature
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetCurvature()"
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
  parametricCoord=gmsh_cdouble(curvatures), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  curvatures=curvatures_, &
  curvatures_n=curvatures_n_, &
  ierr=ierr)

curvatures = gmsh_realvec_c2f(curvatures_, curvatures_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetCurvature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetPrincipalCurvatures
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetPrincipalCurvatures()"
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
  parametricCoord=gmsh_cdouble(parametricCoord), &
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

curvatureMax = gmsh_realvec_c2f(curvatureMax_, curvatureMax_n_)
curvatureMin = gmsh_realvec_c2f(curvatureMin_, curvatureMin_n_)
directionMax = gmsh_realvec_c2f(directionMax_, directionMax_n_)
directionMin = gmsh_realvec_c2f(directionMin_, directionMin_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetPrincipalCurvatures

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetNormal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetNormal()"
#endif
TYPE(C_PTR) :: normals_
INTEGER(C_SIZE_T) :: normals_n_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetNormal( &
  tag=INT(tag, C_INT), &
  parametricCoord=gmsh_cdouble(parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  normals=normals_, &
  normals_n=normals_n_, &
  ierr=ierr)

ans = gmsh_realvec_c2f(normals_, normals_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetParametrization
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetParametrization()"
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
  coord=gmsh_cdouble(coord), &
  coord_n=SIZE(coord, kind=C_SIZE_T), &
  parametricCoord=parametricCoord_, &
  parametricCoord_n=parametricCoord_n_, &
  ierr=ierr)

ans = gmsh_realvec_c2f(parametricCoord_, parametricCoord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetParametrization

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetParametrizationBounds
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetParametrizationBounds()"
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

min = gmsh_realvec_c2f(min_, min_n_)
max = gmsh_realvec_c2f(max_, max_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetParametrizationBounds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_IsInside
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_IsInside()"
#endif
INTEGER(C_INT) :: ans0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans0 = GmshModelIsInside( &
       dim=INT(dim, C_INT), tag=INT(tag, C_INT), &
       coord=gmsh_cdouble(coord), &
       coord_n=SIZE(coord, kind=C_SIZE_T), &
       parametric=optval_c_bool(math%no, parametric), ierr=ierr)

ans = ans0 .NE. 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_IsInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetClosestPoint
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetClosestPoint()"
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
  coord=gmsh_cdouble(coord), &
  coord_n=SIZE(coord, kind=C_SIZE_T), &
  closestCoord=closestCoord_, &
  closestCoord_n=closestCoord_n_, &
  parametricCoord=parametricCoord_, &
  parametricCoord_n=parametricCoord_n_, &
  ierr=ierr)

ans = INT(ierr, I4B)
closestCoord = gmsh_realvec_c2f(closestCoord_, closestCoord_n_)

parametricCoord = gmsh_realvec_c2f(parametricCoord_, &
                                   parametricCoord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetClosestPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_ReparametrizeOnSurface
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_ReparametrizeOnSurface()"
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
  parametricCoord=gmsh_cdouble(parametricCoord), &
  parametricCoord_n=SIZE(parametricCoord, kind=C_SIZE_T), &
  surfaceTag=INT(surfaceTag, C_INT), &
  surfaceParametricCoord=surfaceParametricCoord_, &
  surfaceParametricCoord_n=surfaceParametricCoord_n_, &
  which=optval_c_int(math%zero_i, which), &
  ierr=ierr)

ans = gmsh_realvec_c2f(surfaceParametricCoord_, &
                       surfaceParametricCoord_n_)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_ReparametrizeOnSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetVisibility
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetVisibility()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetVisibility( &
  dimTags=gmsh_cint(dimTags), dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
  VALUE=INT(VALUE, C_INT), RECURSIVE=optval_c_bool(math%no, RECURSIVE), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_SetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetVisibility
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetVisibility()"
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
END PROCEDURE model_GetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetVisibilityPerWindow
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetVisibilityPerWindow()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetVisibilityPerWindow( &
  VALUE=INT(VALUE, C_INT), &
  windowIndex=gmsh_cint(input(default=math%zero_i, option=windowIndex)), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_SetVisibilityPerWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetColor
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetColor()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelSetColor( &
  dimTags=gmsh_cint(dimTags), &
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
END PROCEDURE model_SetColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetColor
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetColor()"
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
END PROCEDURE model_GetColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetCoordinates
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetCoordinates()"
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
END PROCEDURE model_SetCoordinates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetAttributeNames
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetAttributeNames()"
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

names0 = gmsh_cStrings2CharArray(names_, names_n_)

ALLOCATE (names(names_n_))

DO ii = 1, INT(names_n_, i4b)
  names(ii) = TRIM(names0(ii))
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetAttributeNames

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_GetAttribute
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_GetAttribute()"
#endif
CHARACTER(maxStrLen), ALLOCATABLE :: values(:)
TYPE(C_PTR) :: values_
INTEGER(C_SIZE_T) :: values_n_
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelGetAttribute(name=gmsh_CString(name), values=values_, &
                           values_n=values_n_, ierr=ierr)
values = gmsh_cStrings2CharArray(values_, values_n_)
ALLOCATE (ans(values_n_))

DO ii = 1, INT(values_n_, i4b)
  ans(ii) = TRIM(values(ii))
END DO

IF (ALLOCATED(values)) DEALLOCATE (values)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_GetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_SetAttribute
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_SetAttribute()"
#endif
CHARACTER(len=maxStrLen, kind=C_CHAR), ALLOCATABLE :: values_strs(:)
TYPE(C_PTR), ALLOCATABLE :: values_(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL gmsh_GetCharArray_cPtr(values, values_strs, values_)

CALL GmshModelSetAttribute( &
  name=gmsh_CString(name), values=values_, values_n=gmsh_size_str(values), &
  ierr=ierr)

ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_SetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE model_RemoveAttribute
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "model_RemoveAttribute()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshModelRemoveAttribute(name=gmsh_CString(name), ierr=ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE model_RemoveAttribute

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
