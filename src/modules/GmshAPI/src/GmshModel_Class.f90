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
USE GlobalData, ONLY: DFP=> Real64, I4B => Int32, LGT
USE Utility, ONLY: Reallocate
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE GmshInterface
USE CInterface, ONLY: C_F_STRING_PTR, C_PTR_TO_INT_VEC
USE ISO_C_BINDING
USE GmshModelGeo_Class
USE GmshModelOcc_Class
USE GmshModelMesh_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHMODEL_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
TYPE( ExceptionHandler_ ), SAVE, PUBLIC :: eGmshModel
!$OMP THREADPRIVATE(eGmshModel)
INTEGER( I4B ), PARAMETER :: maxStrLen = 120

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModel_
  PRIVATE
  TYPE( GmshModelGeo_ ), PUBLIC, POINTER :: Geo => NULL()
  TYPE( GmshModelOcc_ ), PUBLIC, POINTER :: Occ => NULL()
  TYPE( GmshModelMesh_ ), PUBLIC, POINTER :: Mesh => NULL()
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( Obj ) :: add => model_add
  PROCEDURE, PUBLIC, PASS( Obj ) :: remove => model_remove
  PROCEDURE, PUBLIC, PASS( Obj ) :: list => model_list
  PROCEDURE, PUBLIC, PASS( Obj ) :: getCurrent => model_getCurrent
  PROCEDURE, PUBLIC, PASS( Obj ) :: setCurrent => model_setCurrent
  PROCEDURE, PUBLIC, PASS( Obj ) :: getFileName => model_getFileName
  PROCEDURE, PUBLIC, PASS( Obj ) :: setFileName => model_setFileName
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetEntities => model_GetEntities
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetEntityName => model_SetEntityName
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetEntityName => model_GetEntityName
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetPhysicalGroups => model_GetPhysicalGroups
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetEntitiesForPhysicalGroup => model_GetEntitiesForPhysicalGroup
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetPhysicalGroupsForEntity => model_GetPhysicalGroupsForEntity
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPhysicalGroup => model_AddPhysicalGroup
  PROCEDURE, PUBLIC, PASS( Obj ) :: RemovePhysicalGroups => model_RemovePhysicalGroups
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetPhysicalName => model_SetPhysicalName
  PROCEDURE, PUBLIC, PASS( Obj ) :: RemovePhysicalName => model_RemovePhysicalName
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetPhysicalName => model_GetPhysicalName
END TYPE GmshModel_

PUBLIC :: GmshModel_
TYPE( GmshModel_ ), PUBLIC, PARAMETER :: TypeGmshModel = GmshModel_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelPointer_
  CLASS( GmshModel_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION model_add(obj, name ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) ::  ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshModelAdd( name=C_LOC(name_), ierr=ierr)
  ans = INT( ierr, KIND=I4B)
END FUNCTION model_add

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_remove( obj ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) ::  ans

  ! Internal variables
  CALL gmshModelRemove( ierr=ierr )
  ans = INT( ierr, KIND=I4B)
END FUNCTION model_remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_list(obj, names, names_n) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = maxStrLen ), ALLOCATABLE, INTENT( OUT ) :: names( : )
  INTEGER( I4B ), INTENT( OUT ) :: names_n
  INTEGER( I4B ) ::  ans

  CHARACTER( LEN=* ), PARAMETER :: myName="model_list()"

  CALL eGmshModel%raiseError(modName//"::"//myName//" - "// &
    & "This interface is under construction")
END FUNCTION model_list

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_getCurrent( obj, name ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( OUT ) :: name
  INTEGER( I4B ) :: ans

  ! Internal variables
  TYPE( C_PTR ) :: cstring
  CALL gmshModelGetCurrent( name=cstring, ierr=ierr )
  ans = INT( ierr, KIND=I4B )
  CALL C_F_STRING_PTR(C_string=cstring, F_string=name)
END FUNCTION model_getCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_setCurrent( obj, name ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) :: ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR

  CALL gmshModelSetCurrent( name=C_LOC(name_), ierr=ierr )
  ans = INT( ierr, KIND=I4B )
END FUNCTION model_setCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_getFileName( obj, fileName ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( OUT ) :: fileName
  INTEGER( I4B ) :: ans

  ! Internal variables
  TYPE( C_PTR ) :: cstring

  CALL gmshModelGetFileName( fileName=cstring, ierr=ierr )
  ans = INT( ierr, KIND=I4B )
  CALL C_F_STRING_PTR(C_string=cstring, F_string=fileName)
END FUNCTION model_getFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_setFileName( obj, fileName ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ) :: ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(fileName) // C_NULL_CHAR

  CALL gmshModelSetFileName( fileName=C_LOC(name_), ierr=ierr )
  ans = INT( ierr, KIND=I4B )
END FUNCTION model_setFileName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetEntities(obj, dimTags, dim, dimTags_n ) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: dimTags( :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: dimTags_n
  INTEGER( I4B ), INTENT( IN ) ::  dim
  INTEGER( I4B ) :: ans

  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: dimTags_n_
  INTEGER( I4B ), ALLOCATABLE :: p( : )

  CALL gmshModelGetEntities(cptr, dimTags_n_, dim, ierr)
  ans = int( ierr, i4b )
  IF( PRESENT( dimTags_n ) ) dimTags_n = int(dimTags_n_, i4b)
  ALLOCATE( p( dimTags_n_ ) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec = p )
  dimTags = TRANSPOSE(RESHAPE( p, [2, int(dimTags_n_/2)]))
  DEALLOCATE( p )

END FUNCTION model_GetEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_SetEntityName( obj, dim, tag, name ) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) :: ans

  ! Internal
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshModelSetEntityName(dim, tag, C_LOC(name_), ierr)
  ans = int(ierr, i4b)
END FUNCTION model_SetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetEntityName( obj, dim, tag, name ) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  CHARACTER( LEN = * ), INTENT( OUT ) :: name
  INTEGER( I4B ) :: ans

  ! Internal
  TYPE( C_PTR ) :: cptr

  CALL gmshModelGetEntityName(dim, tag, cptr, ierr)
  ans = int(ierr, i4b)
  CALL C_F_STRING_PTR(c_string=cptr, f_string=name)
END FUNCTION model_GetEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetPhysicalGroups(obj,dimTags, dim, dimTags_n ) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: dimTags(:,:)
  INTEGER( I4B ), OPTIONAL :: dimTags_n
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: dimTags_n_
  INTEGER( I4B ), ALLOCATABLE :: p( : )

  CALL gmshModelGetPhysicalGroups(cptr, dimTags_n_, dim, ierr)
  ans = int( ierr, i4b )
  IF( PRESENT( dimTags_n ) ) dimTags_n = int(dimTags_n_, i4b)
  IF( dimTags_n_ .EQ. 0 ) THEN
    CALL Reallocate(dimTags, 0, 0)
    RETURN
  END IF
  ALLOCATE( p( dimTags_n_ ) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec = p )
  dimTags = TRANSPOSE(RESHAPE( p, [2, int(dimTags_n_/2)]))
  DEALLOCATE( p )
END FUNCTION model_GetPhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetEntitiesForPhysicalGroup(obj, dim, tag, tags, &
  & tags_n ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  dim, tag
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: tags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: tags_n
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: tags_n_

  CALL gmshModelGetEntitiesForPhysicalGroup(dim, tag, cptr, tags_n_, &
    & ierr)
  ans = int( ierr, i4b )
  IF( PRESENT( tags_n ) ) tags_n = int(tags_n_, i4b)
  CALL Reallocate( tags, int(tags_n_, i4b) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec = tags )
END FUNCTION model_GetEntitiesForPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetPhysicalGroupsForEntity(obj, dim, tag, physicalTags, &
  & physicalTags_n ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  dim, tag
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: physicalTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: physicalTags_n
  INTEGER( I4B ) :: ans

  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: physicalTags_n_

  CALL gmshModelGetPhysicalGroupsForEntity(dim, tag, cptr, &
    & physicalTags_n_, ierr)
  ans = int( ierr, i4b )
  IF( PRESENT( physicalTags_n ) ) physicalTags_n = int(physicalTags_n_, i4b)
  CALL Reallocate( physicalTags, int(physicalTags_n_, i4b) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec = physicalTags )
END FUNCTION model_GetPhysicalGroupsForEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_AddPhysicalGroup(obj, dim, tags, &
  & tag ) RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tags( : ), tag
  INTEGER( I4B ) :: ans
  ! Internal
  INTEGER( C_SIZE_T ) :: tags_n
  tags_n = SIZE( tags )
  ans = gmshModelAddPhysicalGroup(dim, tags, tags_n, &
    & tag, ierr )
END FUNCTION model_AddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_RemovePhysicalGroups(obj, dimTags) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags( : )
  INTEGER( I4B ) :: ans

  ! internal
  INTEGER( C_SIZE_T ) :: dimTags_n
  dimTags_n = SIZE( dimTags )
  CALL gmshModelRemovePhysicalGroups(dimTags, dimTags_n, ierr)
  ans = int( ierr, i4b )
END FUNCTION model_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_SetPhysicalName(obj, dim, tag, name) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  dim, tag
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) :: ans
  ! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM( name ) // C_NULL_CHAR
  CALL gmshModelSetPhysicalName(dim, tag, C_LOC(name_), ierr)
  ans = int( ierr, i4b )
END FUNCTION model_SetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_RemovePhysicalName(obj, name) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) :: ans
  ! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM( name ) // C_NULL_CHAR
  CALL gmshModelRemovePhysicalName(C_LOC(name_), ierr)
  ans = int( ierr, i4b )
END FUNCTION model_RemovePhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_GetPhysicalName(obj, dim, tag, name) &
  & RESULT( ans )
  CLASS( GmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  CHARACTER( LEN = * ), INTENT( OUT ) :: name
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr

  CALL gmshModelGetPhysicalName(dim, tag, cptr, ierr)
  ans = int( ierr, i4b )
  CALL C_F_STRING_PTR( C_STRING=cptr, F_STRING=name )
END FUNCTION model_GetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE GmshModel_Class