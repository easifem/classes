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

MODULE GmshModel_Class
USE GlobalData, ONLY: DFP => Real64, I4B => Int32, LGT
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE CInterface
USE String_Class
USE Utility, ONLY: Input, Reallocate
USE GmshUtility
USE GmshInterface
USE ISO_C_BINDING
USE GmshModelGeo_Class
USE GmshModelOcc_Class
USE GmshModelMesh_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "GmshModel_Class"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
TYPE(ExceptionHandler_), SAVE :: e
!$OMP THREADPRIVATE(e)
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModel_
  PRIVATE
  TYPE(GmshModelGeo_), PUBLIC, POINTER :: Geo => NULL()
  TYPE(GmshModelOcc_), PUBLIC, POINTER :: Occ => NULL()
  TYPE(GmshModelMesh_), PUBLIC, POINTER :: Mesh => NULL()
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => &
    & model_Initiate
  PROCEDURE, PUBLIC, NOPASS :: Add => model_Add
  PROCEDURE, PUBLIC, NOPASS :: Remove => model_Remove
  PROCEDURE, PUBLIC, NOPASS :: List => model_List
  PROCEDURE, PUBLIC, NOPASS :: GetCurrent => model_GetCurrent
  PROCEDURE, PUBLIC, NOPASS :: SetCurrent => model_SetCurrent
  PROCEDURE, PUBLIC, NOPASS :: GetFileName => model_GetFileName
  PROCEDURE, PUBLIC, NOPASS :: SetFileName => model_SetFileName
  PROCEDURE, PUBLIC, NOPASS :: GetEntities => model_GetEntities
  PROCEDURE, PUBLIC, NOPASS :: SetEntityName => model_SetEntityName
  PROCEDURE, PUBLIC, NOPASS :: GetEntityName => model_GetEntityName
  PROCEDURE, PUBLIC, NOPASS :: GetPhysicalGroups => model_GetPhysicalGroups
  PROCEDURE, PUBLIC, NOPASS :: GetEntitiesForPhysicalGroup => &
    & model_GetEntitiesForPhysicalGroup
  PROCEDURE, PUBLIC, NOPASS :: GetPhysicalGroupsForEntity => &
    & model_GetPhysicalGroupsForEntity
  PROCEDURE, PUBLIC, NOPASS :: AddPhysicalGroup => model_AddPhysicalGroup
  PROCEDURE, PUBLIC, NOPASS :: RemovePhysicalGroups => &
    & model_RemovePhysicalGroups
  PROCEDURE, PUBLIC, NOPASS :: SetPhysicalName => model_SetPhysicalName
  PROCEDURE, PUBLIC, NOPASS :: RemovePhysicalName => &
    & model_RemovePhysicalName
  PROCEDURE, PUBLIC, NOPASS :: GetPhysicalName => model_GetPhysicalName
  PROCEDURE, PUBLIC, NOPASS :: SetTag => model_SetTag
  PROCEDURE, PUBLIC, NOPASS :: GetBoundary => model_GetBoundary
  PROCEDURE, PUBLIC, NOPASS :: GetAdjacencies => model_GetAdjacencies
  PROCEDURE, PUBLIC, NOPASS :: GetEntitiesInBoundingBox => &
    & model_GetEntitiesInBoundingBox
  PROCEDURE, PUBLIC, NOPASS :: GetBoundingBox => &
    & model_GetBoundingBox
  PROCEDURE, PUBLIC, NOPASS :: GetDimension => &
    & model_GetDimension
  PROCEDURE, PUBLIC, NOPASS :: AddDiscreteEntity => &
    & model_AddDiscreteEntity
  PROCEDURE, PUBLIC, NOPASS :: RemoveEntities => &
    & model_RemoveEntities
  PROCEDURE, PUBLIC, NOPASS :: RemoveEntityName => &
    & model_RemoveEntityName
  PROCEDURE, PUBLIC, NOPASS :: GetType => &
    & model_GetType
  PROCEDURE, PUBLIC, NOPASS :: GetParent => &
    & model_GetParent
  PROCEDURE, PUBLIC, NOPASS :: GetNumberOfPartitions => &
    & model_GetNumberOfPartitions
  PROCEDURE, PUBLIC, NOPASS :: GetPartitions => &
    & model_GetPartitions
  PROCEDURE, PUBLIC, NOPASS :: GetValue => &
    & model_GetValue
  PROCEDURE, PUBLIC, NOPASS :: GetDerivative => &
    & model_GetDerivative
  PROCEDURE, PUBLIC, NOPASS :: GetSecondDerivative => &
    & model_GetSecondDerivative
  PROCEDURE, PUBLIC, NOPASS :: GetCurvature => &
    & model_GetCurvature
  PROCEDURE, PUBLIC, NOPASS :: GetPrincipalCurvatures => &
    & model_GetPrincipalCurvatures
  PROCEDURE, PUBLIC, NOPASS :: GetNormal => &
    & model_GetNormal
  PROCEDURE, PUBLIC, NOPASS :: GetParameterizaion => &
    & model_GetParametrization
  PROCEDURE, PUBLIC, NOPASS :: GetParametrizationBounds => &
    & model_GetParametrizationBounds
  PROCEDURE, PUBLIC, NOPASS :: IsInside => &
    & model_IsInside
  PROCEDURE, PUBLIC, NOPASS :: GetClosestPoint => &
    & model_GetClosestPoint
  PROCEDURE, PUBLIC, NOPASS :: ReparametrizeOnSurface => &
    & model_ReparametrizeOnSurface
  PROCEDURE, PUBLIC, NOPASS :: SetVisibility => model_SetVisibility
  PROCEDURE, PUBLIC, NOPASS :: GetVisibility => model_GetVisibility
  PROCEDURE, PUBLIC, NOPASS :: SetVisibilityPerWindow => &
    & model_SetVisibilityPerWindow
  PROCEDURE, PUBLIC, NOPASS :: SetColor => model_SetColor
  PROCEDURE, PUBLIC, NOPASS :: GetColor => model_GetColor
  PROCEDURE, PUBLIC, NOPASS :: SetCoordinates => model_SetCoordinates
  PROCEDURE, PUBLIC, NOPASS :: GetAttributeNames => model_GetAttributeNames
  PROCEDURE, PUBLIC, NOPASS :: SetAttribute => model_SetAttribute
  PROCEDURE, PUBLIC, NOPASS :: GetAttribute => model_GetAttribute
  PROCEDURE, PUBLIC, NOPASS :: RemoveAttribute => model_RemoveAttribute
END TYPE GmshModel_

PUBLIC :: GmshModel_
TYPE(GmshModel_), PUBLIC, PARAMETER :: Type = GmshModel_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelPointer_
  CLASS(GmshModel_), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE model_initiate(obj)
  CLASS(GmshModel_), INTENT(INOUT) :: obj
  !!
  !! internal var
  !!
  CHARACTER(LEN=*), PARAMETER :: myName = "model_initiate"
  !!
  !! main program
  !!
  IF (ASSOCIATED(obj%Geo)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "gmsh::Model::Geo is already associated;")
  END IF
  !!
  ALLOCATE (obj%Geo); CALL obj%Geo%Initiate()
  !!
  IF (ASSOCIATED(obj%Occ)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "gmsh::Model::Occ is already associated;")
  END IF
  !!
  ALLOCATE (obj%Occ); CALL obj%Occ%initiate()
  !!
  IF (ASSOCIATED(obj%Mesh)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "gmsh::Model::Mesh is already associated;")
  END IF
  !!
  ALLOCATE (obj%Mesh); CALL obj%Mesh%initiate()
  !!
END SUBROUTINE model_initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_add(name) RESULT(ans)
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  ! Internal variables
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !
  name_ = gmsh_CString(name)
  CALL gmshModelAdd(name=C_LOC(name_), ierr=ierr)
  ans = INT(ierr, KIND=I4B)
END FUNCTION model_add

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_remove() RESULT(ans)
  INTEGER(I4B) :: ans
  !
  CALL gmshModelRemove(ierr=ierr)
  ans = INT(ierr, KIND=I4B)
END FUNCTION model_remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_list(names) RESULT(ans)
  TYPE(String), ALLOCATABLE, INTENT(OUT) :: names(:)
  INTEGER(I4B) :: ans
  !! size of names is given in ans
  !!
  !! internal variables
  !!
  CHARACTER(LEN=*), PARAMETER :: myName = "model_list()"
  TYPE(C_PTR) :: names_
  INTEGER(C_SIZE_T) :: names_n_
  CHARACTER(LEN=maxStrLen), ALLOCATABLE :: names0(:)
  INTEGER(I4B) :: ii
  !!
  CALL gmshModelList( &
    & names=names_, &
    & names_n=names_n_, &
    & ierr=ierr)
  !!
  ans = INT(names_n_, I4B)
  !!
  names0 = gmsh_cStrings2CharArray(cptr=names_, n=names_n_)
  !!
  IF (ans .GT. 0) THEN
    ALLOCATE (names(ans - 1))
  END IF
  !!
  DO ii = 1, ans - 1
    names(ii) = trim(names0(ii + 1))
  END DO
  !!
  ans = MAX(ans - 1, 0_I4B)
  !!
END FUNCTION model_list

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetCurrent(name) RESULT(ans)
  CHARACTER(LEN=*), INTENT(OUT) :: name
  INTEGER(I4B) :: ans
  ! Internal variables
  TYPE(C_PTR) :: cstring
  CALL gmshModelGetCurrent(name=cstring, ierr=ierr)
  ans = INT(ierr, KIND=I4B)
  CALL C2Fortran(C_string=cstring, F_string=name)
END FUNCTION model_GetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_SetCurrent(name) RESULT(ans)
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  !! Internal variables
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshModelSetCurrent(name=C_LOC(name_), ierr=ierr)
  ans = INT(ierr, KIND=I4B)
END FUNCTION model_SetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetFileName(fileName) RESULT(ans)
  CHARACTER(LEN=*), INTENT(OUT) :: fileName
  INTEGER(I4B) :: ans
  ! Internal variables
  TYPE(C_PTR) :: cstring
  !!
  CALL gmshModelGetFileName(fileName=cstring, ierr=ierr)
  ans = INT(ierr, KIND=I4B)
  CALL C2Fortran(C_string=cstring, F_string=fileName)
END FUNCTION model_GetFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_SetFileName(fileName) RESULT(ans)
  CHARACTER(LEN=*), INTENT(IN) :: fileName
  INTEGER(I4B) :: ans
  ! Internal variables
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(fileName)//C_NULL_CHAR
  CALL gmshModelSetFileName(fileName=C_LOC(name_), ierr=ierr)
  ans = INT(ierr, KIND=I4B)
END FUNCTION model_SetFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get all the entities in the current model.
!! If `dim` is >= 0, return only
!! the entities of the specified dimension (e.g. points if `dim' == 0).
!! The entities are returned as a vector of (dim, tag) pairs.

FUNCTION model_GetEntities(dimTags, dim) &
  & RESULT(ans)
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dimTags(:, :)
  !! dimTags has two rows
  !! first row is for dim and second row is for tag
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
  !! if present should be greater than 0
  INTEGER(I4B) :: ans
  !!
  !! internal
  !!
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: dimTags_n_
  ! INTEGER(I4B) :: n
  ! INTEGER(I4B), ALLOCATABLE :: p(:)
  !!
  CALL gmshModelGetEntities( &
    & dimTags=cptr, &
    & dimTags_n=dimTags_n_, &
    & dim=gmsh_cint(input(default=-1_I4B, option=dim)), &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
  dimTags = gmsh_dimtag_c2f(cptr=cptr, n=dimTags_n_)
  !!
  ! n = INT(dimTags_n_, i4b)
  ! !!
  ! ALLOCATE (p(n))
  ! !!
  ! CALL C_PTR_TO_INT_VEC(cptr=cptr, vec=p)
  ! !!
  ! dimTags = TRANSPOSE(RESHAPE(p, [2, INT(n / 2)]))
  ! !!
  ! IF (ALLOCATED(p)) DEALLOCATE (p)
END FUNCTION model_GetEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the name of the entity of dimension `dim' and tag `tag'.

FUNCTION model_SetEntityName(dim, tag, name) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  !!
  !! Internal
  !!
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(name)//C_NULL_CHAR
  !!
  CALL gmshModelSetEntityName( &
    & dim=dim, tag=tag, name=C_LOC(name_), ierr=ierr)
  !!
  ans = INT(ierr, i4b)
END FUNCTION model_SetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the name of the entity of dimension `dim' and tag `tag'.

FUNCTION model_GetEntityName(dim, tag, name) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag
  CHARACTER(LEN=*), INTENT(OUT) :: name
  INTEGER(I4B) :: ans
  !!
  !! Internal
  !!
  TYPE(C_PTR) :: cptr
  !!
  CALL gmshModelGetEntityName(dim=dim, tag=tag, name=cptr, ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
  CALL C2Fortran(c_string=cptr, f_string=name)
  !!
  CALL gmshFree(cptr)
  !!
END FUNCTION model_GetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get all the physical groups in the current model. If `dim' is >= 0, return
!! only the entities of the specified dimension (e.g. physical points if `dim'
!! == 0). The entities are returned as a vector of (dim, tag) pairs.

FUNCTION model_GetPhysicalGroups(dimTags, dim) &
  & RESULT(ans)
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dimTags(:, :)
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: dimTags_n_
  ! INTEGER(I4B), ALLOCATABLE :: p(:)
  !!
  CALL gmshModelGetPhysicalGroups( &
    & dimTags=cptr, &
    & dimTags_n=dimTags_n_, &
    & dim=dim, &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
  IF (dimTags_n_ .EQ. 0) THEN
    CALL Reallocate(dimTags, 0, 0)
    RETURN
  END IF
  !!
  dimTags = gmsh_dimtag_c2f(cptr=cptr, n=dimTags_n_)
  !!
  ! ALLOCATE (p(dimTags_n_))
  ! !!
  ! CALL C2Fortran(cptr=cptr, vec=p)
  ! !!
  ! dimTags = TRANSPOSE(RESHAPE(p, [2, int(dimTags_n_ / 2)]))
  ! !!
  ! IF (ALLOCATED(p)) DEALLOCATE (p)
  ! !!
END FUNCTION model_GetPhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

FUNCTION model_GetEntitiesForPhysicalGroup(dim, tag, tags) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: tags(:)
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: tags_n_
  !!
  CALL gmshModelGetEntitiesForPhysicalGroup( &
    & dim=dim, &
    & tag=tag, &
    & tags=cptr, &
    & tags_n=tags_n_, &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
  CALL Reallocate(tags, INT(tags_n_, i4b))
  !!
  CALL C2Fortran(cptr=cptr, vec=tags)
  !!
END FUNCTION model_GetEntitiesForPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

FUNCTION model_GetPhysicalGroupsForEntity(dim, tag, physicalTags) &
  & RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim, tag
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: physicalTags(:)
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: physicalTags_n_
  !!
  CALL gmshModelGetPhysicalGroupsForEntity(dim=dim, tag=tag, &
    & physicalTags=cptr, &
    & physicalTags_n=physicalTags_n_, &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
  CALL Reallocate(physicalTags, INT(physicalTags_n_, i4b))
  !!
  CALL C2Fortran(cptr=cptr, vec=physicalTags)
  !!
END FUNCTION model_GetPhysicalGroupsForEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a physical group of dimension `dim', grouping the model entities with
!! tags `tags'. Return the tag of the physical group, equal to `tag' if `tag'
!! is positive, or a new tag if `tag' < 0.

FUNCTION model_AddPhysicalGroup(dim, tags, &
  & tag, name) RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: tags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: name
  INTEGER(I4B) :: ans
  !
  ! Internal
  !
  INTEGER(C_SIZE_T) :: tags_n
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(input(option="", default=name))//C_NULL_CHAR
  !!
  tags_n = SIZE(tags)
  !!
  ans = gmshModelAddPhysicalGroup(&
    & dim=gmsh_cint(dim), &
    & tags=gmsh_cint(tags), &
    & tags_n=tags_n, &
    & tag=gmsh_cint(input(option=-1_I4B, default=tag)), &
    & name=C_LOC(name_), &
    & ierr=ierr)
  !!
END FUNCTION model_AddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Remove the physical groups `dimTags' (given as a vector of (dim, tag)
!! pairs) from the current model. If `dimTags' is empty, remove all groups.

FUNCTION model_RemovePhysicalGroups(dimTags) &
  & RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dimTags(:)
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  INTEGER(C_SIZE_T) :: dimTags_n
  !!
  dimTags_n = SIZE(dimTags)
  !!
  CALL gmshModelRemovePhysicalGroups( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=dimTags_n, &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
END FUNCTION model_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the name of the physical group of dimension `dim' and tag `tag'.

FUNCTION model_SetPhysicalName(dim, tag, name) &
  & RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim, tag
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(name)//C_NULL_CHAR
  !!
  CALL gmshModelSetPhysicalName( &
    & dim=dim, &
    & tag=gmsh_cint(tag), &
    & name=C_LOC(name_), &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
END FUNCTION model_SetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Remove the physical name `name' from the current model.

FUNCTION model_RemovePhysicalName(name) &
  & RESULT(ans)

  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(name)//C_NULL_CHAR
  !!
  CALL gmshModelRemovePhysicalName( &
    & name=C_LOC(name_), ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
END FUNCTION model_RemovePhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the name of the physical group of dimension `dim' and tag `tag'.

FUNCTION model_GetPhysicalName(dim, tag, name) &
  & RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim, tag
  CHARACTER(LEN=*), INTENT(OUT) :: name
  INTEGER(I4B) :: ans
  !
  ! internal
  !
  TYPE(C_PTR) :: cptr
  !!
  CALL gmshModelGetPhysicalName( &
    & dim=gmsh_cint(dim), &
    & tag=gmsh_cint(tag), &
    & name=cptr, &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
  CALL C2Fortran(C_STRING=cptr, F_STRING=name)
  !!
END FUNCTION model_GetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the tag of the entity of dimension `dim' and tag `tag' to the new value
!! `newTag'.

FUNCTION model_SetTag(dim, tag, newTag) &
  & RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim, tag, newTag
  INTEGER(I4B) :: ans
  !
  CALL gmshModelSetTag( &
    & dim=gmsh_cint(dim), &
    & tag=gmsh_cint(tag), &
    & newtag=gmsh_cint(newtag), &
    & ierr=ierr)
  !!
  ans = INT(ierr, i4b)
  !!
END FUNCTION model_SetTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the boundary of the model entities `dimTags'. Return in `outDimTags'
! the boundary of the individual entities (if `combined' is false) or the
! boundary of the combined geometrical shape formed by all input entities (if
!  `combined' is true). Return tags multiplied by the sign of the boundary
!  entity if `oriented' is true. Apply the boundary operator recursively down
!  to dimension 0 (i.e. to points) if `recursive' is true.

FUNCTION model_GetBoundary(dimTags, outDimTags, combined, &
  & oriented, recursive) RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: outDimTags(:, :)
  LOGICAL, INTENT(IN), OPTIONAL :: combined
  LOGICAL, INTENT(IN), OPTIONAL :: oriented
  LOGICAL, INTENT(IN), OPTIONAL :: recursive
  INTEGER(I4B) :: ans
  !
  type(c_ptr) :: outDimTags_
  integer(c_size_t) :: outDimTags_n
  !
  call gmshModelGetBoundary( &
    & dimTags=dimTags, &
    & dimTags_n=gmsh_cint(size(dimTags, KIND=I4B)), &
    & outDimTags=outDimTags_, &
    & outDimTags_n=outDimTags_n, &
    & combined=optval_c_bool(.true., combined), &
    & oriented=optval_c_bool(.true., oriented), &
    & recursive=optval_c_bool(.false., recursive), &
    & ierr=ierr)
  !
  ans = INT(ierr, I4B)
  !
  outDimTags = gmsh_dimtag_c2f(cptr=outDimTags_, n=outDimTags_n)
  !
END FUNCTION model_GetBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the upward and downward adjacencies of the model entity of dimension
! `dim' and tag `tag'. The `upward' vector returns the adjacent entities of
! dimension `dim' + 1; the `downward' vector returns the adjacent entities of
! dimension `dim' - 1. */

FUNCTION model_GetAdjacencies(dim, tag, upward, downward) &
  & RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: tag
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: upward(:)
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: downward(:)
  INTEGER(I4B) :: ans
  !
  TYPE(C_PTR) :: upward_
  INTEGER(C_SIZE_T) :: upward_n
  TYPE(C_PTR) :: downward_
  INTEGER(C_SIZE_T) :: downward_n

  CALL gmshModelGetAdjacencies( &
    & dim=gmsh_cint(dim), &
    & tag=gmsh_cint(tag), &
    & upward=upward_, &
    & upward_n=upward_n, &
    & downward=downward_, &
    & downward_n=downward_n, &
    & ierr=ierr)
  ans = INT(ierr, I4B)
  !
  upward = gmsh_intvec_c2f(cptr=upward_, n=upward_n)
  downward = gmsh_intvec_c2f(cptr=downward_, n=downward_n)
  !
END FUNCTION model_GetAdjacencies

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetEntitiesInBoundingBox( &
    & xmin, ymin, zmin, xmax, ymax, zmax, &
    & dimTags, dim) RESULT(ans)

  REAL(DFP), INTENT(IN) :: xmin
  REAL(DFP), INTENT(IN) :: ymin
  REAL(DFP), INTENT(IN) :: zmin
  REAL(DFP), INTENT(IN) :: xmax
  REAL(DFP), INTENT(IN) :: ymax
  REAL(DFP), INTENT(IN) :: zmax
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dimTags(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
  INTEGER(I4B) :: ans
  !!
  TYPE(C_PTR) :: dimTags_
  INTEGER(C_SIZE_T) :: dimTags_n
  !!
  call gmshModelGetEntitiesInBoundingBox( &
    & xmin=gmsh_cdouble(xmin), &
    & xmax=gmsh_cdouble(xmax), &
    & ymin=gmsh_cdouble(ymin), &
    & ymax=gmsh_cdouble(ymax), &
    & zmin=gmsh_cdouble(zmin), &
    & zmax=gmsh_cdouble(zmax), &
    & tags=dimTags_, &
    & tags_n=dimTags_n, &
    & dim=gmsh_cint(input(default=-1, option=dim)), &
    & ierr=ierr)
  !!
  dimTags = gmsh_dimtag_c2f(dimTags_, dimTags_n)
  !!
  ans = INT(ierr, I4B)
END FUNCTION model_GetEntitiesInBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetBoundingBox(dim, tag, xmin, ymin, zmin, &
  & xmax, ymax, zmax) RESULT(ans)

  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: tag
  REAL(DFP), INTENT(OUT) :: xmin
  REAL(DFP), INTENT(OUT) :: ymin
  REAL(DFP), INTENT(OUT) :: zmin
  REAL(DFP), INTENT(OUT) :: xmax
  REAL(DFP), INTENT(OUT) :: ymax
  REAL(DFP), INTENT(OUT) :: zmax
  INTEGER(I4B) :: ans
  !!
  REAL(C_DOUBLE) :: x(6)
  !!
  CALL gmshModelGetBoundingBox(&
    & dim=gmsh_cint(dim), &
    & tag=gmsh_cint(tag), &
    & xmin=x(1), &
    & ymin=x(2), &
    & zmin=x(3), &
    & xmax=x(4), &
    & ymax=x(5), &
    & zmax=x(6), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
  xmin = REAL(x(1), DFP)
  ymin = REAL(x(2), DFP)
  zmin = REAL(x(3), DFP)
  xmax = REAL(x(4), DFP)
  ymax = REAL(x(5), DFP)
  zmax = REAL(x(6), DFP)
  !!
END FUNCTION model_GetBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return the geometrical dimension of the current model.

FUNCTION model_GetDimension() RESULT(ans)
  INTEGER(I4B) :: ans
  ans = INT(gmshModelGetDimension(ierr=ierr), I4B)
END FUNCTION model_GetDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a discrete model entity (defined by a mesh) of dimension `dim' in the
!! current model. Return the tag of the new discrete entity, equal to `tag' if
!! `tag' is positive, or a new tag if `tag' < 0. `boundary' specifies the tags
!! of the entities on the boundary of the discrete entity, if any. Specifying
!! `boundary' allows Gmsh to construct the topology of the overall model.

function model_AddDiscreteEntity(dim, tag, boundary) &
  & RESULT(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in), optional :: tag
  integer(i4b), intent(in), optional :: boundary(:)
  integer(i4b) :: ans
  !!
  integer(C_INT) :: ans0
  integer(c_size_t) :: boundary_n
  integer(c_int), allocatable :: boundary0(:)
  !!
  IF (PRESENT(boundary)) THEN
    !!
    boundary0 = gmsh_cint(boundary)
    boundary_n = SIZE(boundary, kind=c_size_t)
    !!
  ELSE
    !!
    allocate (boundary0(0))
    boundary_n = 0_c_size_t
    !!
  END IF
    !!
  ans0 = gmshModelAddDiscreteEntity( &
    & dim=gmsh_cint(dim), &
    & tag=gmsh_cint(input(-1_I4B, tag)), &
    & boundary=boundary0, &
    & boundary_n=boundary_n, &
    & ierr=ierr)
  !!
  ans = int(ans0, i4b)
    !!
end function model_AddDiscreteEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the entities `dimTags' (given as a vector of (dim, tag) pairs) of
!! the current model, provided that they are not on the boundary of (or
!! embedded in) higher-dimensional entities. If `recursive' is true, remove
!! all the entities on their boundaries, down to dimension 0.

function model_RemoveEntities(dimTags, recursive) result(ans)
  integer(i4b), intent(in) :: dimTags(:, :)
  logical, intent(in), optional :: recursive
  INTEGER(I4B) :: ans
  !!
  call gmshModelRemoveEntities( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=size(dimTags, kind=c_size_t), &
    & recursive=optval_c_bool(.false., recursive), &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
end function model_RemoveEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the entity name `name' from the current model.

function model_RemoveEntityName(name) result(ans)
  character(len=*), intent(in) :: name
  integer(i4b) :: ans
  !!
  call gmshModelRemoveEntityName( &
    & name=gmsh_CString(name), &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
  !!
end function model_RemoveEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the type of the entity of dimension `dim' and tag `tag'.

function model_GetType(dim, tag) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  character(len=:), allocatable :: ans
  !!
  call gmshModelGetType( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & entityType=ans, &
    & ierr=ierr)
  !!
end function model_GetType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> In a partitioned model, get the parent of the entity of dimension `dim' and
!! tag `tag', i.e. from which the entity is a part of, if any. `parentDim' and
!! `parentTag' are Set to -1 if the entity has no parent.

function model_GetParent(dim, tag, parentDim, parentTag) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  integer(i4b), INTENT(OUT) :: parentDim
  integer(i4b), INTENT(OUT) :: parentTag
  integer(i4b) :: ans
  !!
  integer(c_int) :: parentDim0, parentTag0
  !!
  call gmshModelGetParent( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & parentDim=parentDim0, &
    & parentTag=parentTag0, &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
  parentDim = int(parentDim0, i4b)
  parentTag = int(parentTag0, i4b)
end function model_GetParent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return the number of partitions in the model.
function model_GetNumberOfPartitions() result(ans)
  integer(i4b) :: ans
  ans = int(gmshModelGetNumberOfPartitions(ierr=ierr), i4b)
end function model_GetNumberOfPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! In a partitioned model, return the tags of the partition(s) to which the
!! entity belongs.

function model_GetPartitions(dim, tag) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  integer(i4b), allocatable :: ans(:)
  !! partitions are returned in ans
  !!
  type(c_ptr) :: partitions_
  integer(c_size_t) :: partitions_n
  !!
  call gmshModelGetPartitions( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & partitions=partitions_, &
    & partitions_n=partitions_n, &
    & ierr=ierr)
  !!
  ans = gmsh_intvec_c2f(partitions_, partitions_n)
  !!
end function model_GetPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the parametrization of the entity of dimension `dim' and tag `tag'
!! at the parametric coordinates `parametricCoord'. Only valid for `dim' equal
!! to 0 (with empty `parametricCoord'), 1 (with `parametricCoord' containing
!! parametric coordinates on the curve) or 2 (with `parametricCoord'
!! containing u, v parametric coordinates on the surface, concatenated: [p1u,
!! p1v, p2u, ...]). Return x, y, z coordinates in `coord', concatenated: [p1x,
!! p1y, p1z, p2x, ...].

function model_GetValue(dim, tag, parametricCoord) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  real(dfp), dimension(:), allocatable :: ans
  !! coord
  !!
  type(c_ptr) :: coord_
  integer(c_size_t) :: coord_n_
  !!
  call gmshModelGetValue( &
      & dim=int(dim, c_int), &
      & tag=int(tag, c_int), &
      & parametricCoord=gmsh_cdouble(parametricCoord), &
      & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
      & coord=coord_, &
      & coord_n=coord_n_, &
      & ierr=ierr)
  !!
  ans = gmsh_realvec_c2f(coord_, coord_n_)
  !!
end function model_GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the derivative of the parametrization of the entity of dimension
!! `dim' and tag `tag' at the parametric coordinates `parametricCoord'. Only
!! valid for `dim' equal to 1 (with `parametricCoord' containing parametric
!! coordinates on the curve) or 2 (with `parametricCoord' containing u, v
!! parametric coordinates on the surface, concatenated: [p1u, p1v, p2u, ...]).
!! For `dim' equal to 1 return the x, y, z components of the derivative with
!! respect to u [d1ux, d1uy, d1uz, d2ux, ...]; for `dim' equal to 2 return the
!! x, y, z components of the derivative with respect to u and v: [d1ux, d1uy,
!! d1uz, d1vx, d1vy, d1vz, d2ux, ...].

function model_GetDerivative(dim, tag, parametricCoord) result(derivatives)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  real(dfp), dimension(:), allocatable :: derivatives
  !!
  type(c_ptr) :: derivatives_
  integer(c_size_t) :: derivatives_n_
  !!
  call gmshModelGetDerivative( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & parametricCoord=parametricCoord, &
    & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
    & derivatives=derivatives_, &
    & derivatives_n=derivatives_n_, &
    & ierr=ierr)
  !!
  derivatives = gmsh_realvec_c2f(derivatives_, derivatives_n_)
  !!
end function model_GetDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Evaluate the second derivative of the parametrization of the entity of
!! dimension `dim' and tag `tag' at the parametric coordinates
!! `parametricCoord'. Only valid for `dim' equal to 1 (with `parametricCoord'
!! containing parametric coordinates on the curve) or 2 (with
!! `parametricCoord' containing u, v parametric coordinates on the surface,
!! concatenated: [p1u, p1v, p2u, ...]). For `dim' equal to 1 return the x, y,
!! z components of the second derivative with respect to u [d1uux, d1uuy,
!! d1uuz, d2uux, ...]; for `dim' equal to 2 return the x, y, z components of
!! the second derivative with respect to u and v, and the mixed derivative
!! with respect to u and v: [d1uux, d1uuy, d1uuz, d1vvx, d1vvy, d1vvz, d1uvx,
!! d1uvy, d1uvz, d2uux, ...].

function model_GetSecondDerivative(dim, tag, parametricCoord) &
  & result(derivatives)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  real(dfp), dimension(:), allocatable :: derivatives
  !!
  type(c_ptr) :: derivatives_
  integer(c_size_t) :: derivatives_n_
  !!
  call gmshModelGetSecondDerivative( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & parametricCoord=gmsh_cdouble(parametricCoord), &
    & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
    & derivatives=derivatives_, &
    & derivatives_n=derivatives_n_, &
    & ierr=ierr)
  !!
  derivatives = gmsh_realvec_c2f(derivatives_, derivatives_n_)
  !!
end function model_GetSecondDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the (maximum) curvature of the entity of dimension `dim' and tag
!! `tag' at the parametric coordinates `parametricCoord'. Only valid for `dim'
!! equal to 1 (with `parametricCoord' containing parametric coordinates on the
!! curve) or 2 (with `parametricCoord' containing u, v parametric coordinates
!! on the surface, concatenated: [p1u, p1v, p2u, ...]).

function model_GetCurvature(dim, tag, parametricCoord) &
  & result(curvatures)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  real(dfp), dimension(:), allocatable :: curvatures
  !!
  type(c_ptr) :: curvatures_
  integer(c_size_t) :: curvatures_n_
  !!
  call gmshModelGetCurvature( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & parametricCoord=gmsh_cdouble(curvatures), &
    & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
    & curvatures=curvatures_, &
    & curvatures_n=curvatures_n_, &
    & ierr=ierr)
  !!
  curvatures = gmsh_realvec_c2f(curvatures_, curvatures_n_)
  !!
end function model_GetCurvature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the principal curvatures of the surface with tag `tag' at the
!! parametric coordinates `parametricCoord', as well as their respective
!! directions. `parametricCoord' are given by pair of u and v coordinates,
!! concatenated: [p1u, p1v, p2u, ...].

function model_GetPrincipalCurvatures(tag, parametricCoord, &
  & curvatureMax, curvatureMin, directionMax, directionMin) result(ans)
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  real(dfp), dimension(:), allocatable, intent(out) :: curvatureMax
  real(dfp), dimension(:), allocatable, intent(out) :: curvatureMin
  real(dfp), dimension(:), allocatable, intent(out) :: directionMax
  real(dfp), dimension(:), allocatable, intent(out) :: directionMin
  integer(i4b) :: ans
  !!
  type(c_ptr) :: curvatureMax_
  integer(c_size_t) :: curvatureMax_n_
  type(c_ptr) :: curvatureMin_
  integer(c_size_t) :: curvatureMin_n_
  type(c_ptr) :: directionMax_
  integer(c_size_t) :: directionMax_n_
  type(c_ptr) :: directionMin_
  integer(c_size_t) :: directionMin_n_
  !!
  call gmshModelGetPrincipalCurvatures( &
    & tag=int(tag, c_int), &
    & parametricCoord=gmsh_cdouble(parametricCoord), &
    & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
    & curvatureMax=curvatureMax_, &
    & curvatureMax_n=curvatureMax_n_, &
    & curvatureMin=curvatureMin_, &
    & curvatureMin_n=curvatureMin_n_, &
    & directionMax=directionMax_, &
    & directionMax_n=directionMax_n_, &
    & directionMin=directionMin_, &
    & directionMin_n=directionMin_n_, &
    & ierr=ierr)
  !!
  curvatureMax = gmsh_realvec_c2f(curvatureMax_, curvatureMax_n_)
  curvatureMin = gmsh_realvec_c2f(curvatureMin_, curvatureMin_n_)
  directionMax = gmsh_realvec_c2f(directionMax_, directionMax_n_)
  directionMin = gmsh_realvec_c2f(directionMin_, directionMin_n_)
end function model_GetPrincipalCurvatures

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the normal to the surface with tag `tag' at the parametric coordinates
!! `parametricCoord'. The `parametricCoord' vector should contain u and v
!! coordinates, concatenated: [p1u, p1v, p2u, ...]. `normals' are returned as
!! a vector of x, y, z components, concatenated: [n1x, n1y, n1z, n2x, ...].

function model_GetNormal(tag, parametricCoord) result(normals)
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  real(dfp), dimension(:), allocatable :: normals
  !!
  type(c_ptr) :: normals_
  integer(c_size_t) :: normals_n_
  !!
  call gmshModelGetNormal( &
    & tag=int(tag, c_int), &
    & parametricCoord=gmsh_cdouble(parametricCoord), &
    & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
    & normals=normals_, &
    & normals_n=normals_n_, &
    & ierr=ierr)
  !!
  normals = gmsh_realvec_c2f(normals_, normals_n_)
  !!
end function model_GetNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the parametric coordinates `parametricCoord' for the points `coord' on
!! the entity of dimension `dim' and tag `tag'. `coord' are given as x, y, z
!! coordinates, concatenated: [p1x, p1y, p1z, p2x, ...]. `parametricCoord'
!! returns the parametric coordinates t on the curve (if `dim' = 1) or u and v
!! coordinates concatenated on the surface (if `dim' = 2), i.e. [p1t, p2t,
!! ...] or [p1u, p1v, p2u, ...].

function model_GetParametrization(dim, tag, coord) &
  & result(parametricCoord)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: coord
  real(dfp), dimension(:), allocatable :: parametricCoord
  !!
  type(c_ptr) :: parametricCoord_
  integer(c_size_t) :: parametricCoord_n_
  !!
  call gmshModelGetParametrization( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & coord=gmsh_cdouble(coord), &
    & coord_n=size(coord, kind=c_size_t), &
    & parametricCoord=parametricCoord_, &
    & parametricCoord_n=parametricCoord_n_, &
    & ierr=ierr)
  !!
  parametricCoord = gmsh_realvec_c2f(parametricCoord_,&
    & parametricCoord_n_)
  !!
end function model_GetParametrization

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the `min' and `max' bounds of the parametric coordinates for the entity
!! of dimension `dim' and tag `tag'.

function model_GetParametrizationBounds(dim, tag, min, max) &
  & result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), allocatable, intent(out) :: min
  real(dfp), dimension(:), allocatable, intent(out) :: max
  integer(i4b) :: ans
  !!
  type(c_ptr) :: min_
  integer(c_size_t) :: min_n_
  type(c_ptr) :: max_
  integer(c_size_t) :: max_n_
  !!
  call gmshModelGetParametrizationBounds(&
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & min=min_, &
    & min_n=min_n_, &
    & max=max_, &
    & max_n=max_n_, &
    & ierr=ierr)
  !!
  min = gmsh_realvec_c2f(min_, min_n_)
  max = gmsh_realvec_c2f(max_, max_n_)
  !!
end function model_GetParametrizationBounds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Check if the coordinates (or the parametric coordinates if `parametric' is
!! Set) provided in `coord' correspond to points inside the entity of
!! dimension `dim' and tag `tag', and return the number of points inside. This
!! feature is only available for a subSet of entities, depending on the
!! underlying geometrical representation.

function model_IsInside(dim, tag, coord, parametric) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: coord
  logical(lgt), intent(in), optional :: parametric
  logical(lgt) :: ans
  !!
  integer(c_int) :: ans0
  !!
  ans0 = gmshModelIsInside( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & coord=gmsh_cdouble(coord), &
    & coord_n=size(coord, kind=c_size_t), &
    & parametric=optval_c_bool(.false., parametric), &
    & ierr=ierr)
  !!
  if (ans0 .eq. 0) then
    ans = .FALSE.
  else
    ans = .TRUE.
  end if
  !!
end function model_IsInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the points `closestCoord' on the entity of dimension `dim' and tag
  !! `tag' to the points `coord', by orthogonal projection. `coord' and
  !! `closestCoord' are given as x, y, z coordinates, concatenated: [p1x, p1y,
  !! p1z, p2x, ...]. `parametricCoord' returns the parametric coordinates t on
  !! the curve (if `dim' = 1) or u and v coordinates concatenated on the surface
  !! (if `dim' = 2), i.e. [p1t, p2t, ...] or [p1u, p1v, p2u, ...].

function model_GetClosestPoint(dim, tag, coord, closestCoord, &
  & parametricCoord) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: coord
  real(dfp), dimension(:), allocatable, intent(out) :: closestCoord
  real(dfp), dimension(:), allocatable, intent(out) :: parametricCoord
  integer(i4b) :: ans
  !!
  type(c_ptr) :: closestCoord_
  integer(c_size_t) :: closestCoord_n_
  type(c_ptr) :: parametricCoord_
  integer(c_size_t) :: parametricCoord_n_
  !!
  call gmshModelGetClosestPoint( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & coord=gmsh_cdouble(coord), &
    & coord_n=size(coord, kind=c_size_t), &
    & closestCoord=closestCoord_, &
    & closestCoord_n=closestCoord_n_, &
    & parametricCoord=parametricCoord_, &
    & parametricCoord_n=parametricCoord_n_, &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
  !!
  closestCoord = gmsh_realvec_c2f(closestCoord_, closestCoord_n_)
  !!
  parametricCoord = gmsh_realvec_c2f(parametricCoord_, &
    & parametricCoord_n_)
end function model_GetClosestPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Reparametrize the boundary entity (point or curve, i.e. with `dim' == 0 or
!! `dim' == 1) of tag `tag' on the surface `surfaceTag'. If `dim' == 1,
!! reparametrize all the points corresponding to the parametric coordinates
!! `parametricCoord'. Multiple matches in case of periodic surfaces can be
!! selected with `which'. This feature is only available for a subSet of
!! entities, depending on the underlying geometrical representation.

function model_ReparametrizeOnSurface(dim, tag, &
  & parametricCoord, surfaceTag, which) result(surfaceParametricCoord)
  !!
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  real(dfp), dimension(:), intent(in) :: parametricCoord
  integer(i4b), intent(in) :: surfaceTag
  integer(i4b), intent(in), optional :: which
  real(dfp), dimension(:), allocatable :: surfaceParametricCoord
  !!
  type(c_ptr) :: surfaceParametricCoord_
  integer(c_size_t) :: surfaceParametricCoord_n_
  !!
  call gmshModelReparametrizeOnSurface( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & parametricCoord=gmsh_cdouble(parametricCoord), &
    & parametricCoord_n=size(parametricCoord, kind=c_size_t), &
    & surfaceTag=int(surfaceTag, c_int), &
    & surfaceParametricCoord=surfaceParametricCoord_, &
    & surfaceParametricCoord_n=surfaceParametricCoord_n_, &
    & which=optval_c_int(0_I4B, which), &
    & ierr=ierr)
  !!
  surfaceParametricCoord = gmsh_realvec_c2f(surfaceParametricCoord_, &
    & surfaceParametricCoord_n_)
  !!
end function model_ReparametrizeOnSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the visibility of the model entities `dimTags' (given as a vector of
!! (dim, tag) pairs) to `value'. Apply the visibility Setting recursively if
!! `recursive' is true.

function model_SetVisibility(dimTags, value, recursive) result(ans)
  integer(i4b), dimension(:, :), intent(in) :: dimTags
  integer(i4b), intent(in) :: value
  logical(lgt), intent(in), optional :: recursive
  integer(i4b) :: ans
  !!
  call gmshModelSetVisibility( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=size(dimTags, kind=c_size_t), &
    & value=int(value, c_int), &
    & recursive=optval_c_bool(.false., recursive), &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
end function model_SetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the visibility of the model entity of dimension `dim' and tag `tag'.

function model_GetVisibility(dim, tag) result(value)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  integer(i4b) :: value
  !!
  integer(c_int) :: value0
  !!
  call gmshModelGetVisibility( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & value=value0, &
    & ierr=ierr)
  !!
  value = int(value0, i4b)
  !!
end function model_GetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the global visibility of the model per window to `value', where
!! `windowIndex' identifies the window in the window list.

function model_SetVisibilityPerWindow(value, windowIndex) &
  & result(ans)
  integer(i4b), intent(in) :: value
  integer(i4b), intent(in), optional :: windowIndex
  integer(i4b) :: ans
  !!
  call gmshModelSetVisibilityPerWindow( &
    & value=int(value, c_int), &
    & windowIndex=gmsh_cint(input(default=0_I4B, option=windowIndex)), &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
end function model_SetVisibilityPerWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the color of the model entities `dimTags' (given as a vector of (dim,
!! tag) pairs) to the RGBA value (`r', `g', `b', `a'), where `r', `g', `b' and
!! `a' should be integers between 0 and 255. Apply the color Setting
!! recursively if `recursive' is true.

function model_SetColor(dimTags, r, g, b, a, recursive) result(ans)
  integer(i4b), dimension(:, :), intent(in) :: dimTags
  integer(i4b), intent(in) :: r
  integer(i4b), intent(in) :: g
  integer(i4b), intent(in) :: b
  integer(i4b), intent(in), optional :: a
  logical(lgt), intent(in), optional :: recursive
  integer(i4b) :: ans
  !!
  call gmshModelSetColor( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=size(dimTags, kind=c_size_t), &
    & r=int(r, c_int), &
    & g=int(g, c_int), &
    & b=int(b, c_int), &
    & a=int(input(default=255_I4B, option=a), kind=c_int), &
    & recursive=optval_c_bool(.false., recursive), &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
end function model_SetColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the color of the model entity of dimension `dim' and tag `tag'.

function model_GetColor(dim, tag) result(ans)
  integer(i4b), intent(in) :: dim
  integer(i4b), intent(in) :: tag
  integer(i4b) :: ans(4)
  !! r, g, b, a
  !!
  integer(c_int) :: r, g, b, a
  !!
  call gmshModelGetColor( &
    & dim=int(dim, c_int), &
    & tag=int(tag, c_int), &
    & r=r, &
    & g=g, &
    & b=b, &
    & a=a, &
    & ierr=ierr)
  !!
  ans = int([r, g, b, a], kind=i4b)
  !!
end function model_GetColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the `x', `y', `z' coordinates of a geometrical point.

function model_SetCoordinates(tag, x, y, z) result(ans)
  integer(i4b), intent(in) :: tag
  real(dfp), intent(in) :: x
  real(dfp), intent(in) :: y
  real(dfp), intent(in) :: z
  integer(i4b) :: ans
  !!
  call gmshModelSetCoordinates( &
    & tag=int(tag, c_int), &
    & x=real(x, c_double), &
    & y=real(y, c_double), &
    & z=real(z, c_double), &
    & ierr=ierr)
  !!
  ans = int(ierr, i4b)
  !!
end function model_SetCoordinates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the names of any optional attributes stored in the model.

function model_GetAttributeNames() result(names)
  type(string), allocatable :: names(:)
  !!
  character(len=maxStrLen), allocatable :: names0(:)
  type(c_ptr) :: names_
  integer(c_size_t) :: names_n_
  integer(i4b) :: ii
  !!
  call gmshModelGetAttributeNames( &
    & names=names_, &
    & names_n=names_n_, &
    & ierr=ierr)
  !!
  names0 = gmsh_cStrings2CharArray(names_, names_n_)
  !!
  allocate (names(names_n_))
  !!
  do ii = 1, int(names_n_, i4b)
    names(ii) = trim(names0(ii))
  end do
end function model_GetAttributeNames

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the values of the attribute with name `name'.

function model_GetAttribute(name) result(ans)
  character(len=*), intent(in) :: name
  type(string), allocatable :: ans(:)
  !!
  character(len=maxStrLen), allocatable :: values(:)
  type(c_ptr) :: values_
  integer(c_size_t) :: values_n_
  integer(i4b) :: ii
  !!
  call gmshModelGetAttribute( &
    & name=gmsh_CString(name), &
    & values=values_, &
    & values_n=values_n_, &
    & ierr=ierr)
  !!
  values = gmsh_cStrings2CharArray(values_, values_n_)
  !!
  ALLOCATE (ans(values_n_))
  !!
  do ii = 1, int(values_n_, i4b)
    ans(ii) = TRIM(values(ii))
  end do
  !!
  IF (ALLOCATED(values)) DEALLOCATE (values)
  !!
end function model_GetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the values of the attribute with name `name'.
function model_SetAttribute(name, values) result(ans)
  character(len=*), intent(in) :: name
  character(len=*), dimension(:), intent(in) :: values
  integer(i4b) :: ans
  !!
  character(len=maxStrLen, kind=c_char), allocatable :: values_strs(:)
  type(c_ptr), allocatable :: values_(:)
  !!
  call gmsh_GetCharArray_cPtr(values, values_strs, values_)
  call gmshModelSetAttribute(name=gmsh_CString(name), &
    & values=values_, values_n=gmsh_size_str(values), ierr=ierr)
  !!
  ans = int(ierr, i4b)
end function model_SetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the attribute with name `name'.

function model_RemoveAttribute(name) result(ans)
  character(len=*), intent(in) :: name
  integer(i4b) :: ans
  !!
  call gmshModelRemoveAttribute(name=gmsh_CString(name), ierr=ierr)
  ans = int(ierr, i4b)
end function model_RemoveAttribute

END MODULE GmshModel_Class

