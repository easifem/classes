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

MODULE GmshOnelab_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE ISO_C_BINDING, ONLY: C_PTR
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshOnelab_
PUBLIC :: GmshOnelabPointer_
PUBLIC :: TypeGmshOnelab

!----------------------------------------------------------------------------
!                                                               GmshOnelab_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Gmsh one lab
!
!# GmshOnelab_
!
! Gmsh one lab.
!
TYPE :: GmshOnelab_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  PROCEDURE, PUBLIC, PASS(obj) :: Get => obj_Get
  PROCEDURE, PUBLIC, PASS(obj) :: GetNumber => obj_GetNumber
  PROCEDURE, PUBLIC, PASS(obj) :: GetString => obj_GetString
  PROCEDURE, PUBLIC, PASS(obj) :: SetString => obj_SetString

  ! TODO:
  ! Following methods needs to be implemented in GmshOnelab_
  ! procedure, public, pass(obj) :: getNames => onelab_getNames
  ! procedure, public, pass(obj) :: setNumber => onelab_setNumber
  ! procedure, public, pass(obj) :: getChanged => onelab_getChanged
  ! procedure, public,  pass(obj) :: setChanged => onelab_setChanged
  ! procedure, public, pass(obj) :: clear => onelab_clear
  ! procedure, public, pass(obj) :: run => onelab_run

END TYPE GmshOnelab_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(GmshOnelab_), PARAMETER :: TypeGmshOnelab = GmshOnelab_()

!----------------------------------------------------------------------------
!                                                         GmshOnelabPointer_
!----------------------------------------------------------------------------

TYPE :: GmshOnelabPointer_
  CLASS(GmshOnelab_), POINTER :: ptr => NULL()
END TYPE GmshOnelabPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  Currently, this routine does nothing

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GmshOnelab_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set one or more parameters in the ONELAB database

INTERFACE
  MODULE FUNCTION obj_Set(obj, DATA, FORMAT) RESULT(ans)
    CLASS(GmshOnelab_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: DATA
    CHARACTER(*), OPTIONAL, INTENT(IN) :: FORMAT
    INTEGER(I4B) :: ans
  END FUNCTION obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get one or more parameters from the ONELAB database.

INTERFACE
  MODULE FUNCTION obj_Get(obj, DATA, name, FORMAT) RESULT(ans)
    CLASS(GmshOnelab_), INTENT(INOUT) :: obj
    TYPE(C_PTR), TARGET, INTENT(IN) :: DATA(*)
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(IN) :: FORMAT
    INTEGER(I4B) :: ans
  END FUNCTION obj_Get
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!# GetNumber
!
! Get the value of the number parameter `name` from the ONELAB database.
!  Return an empty vector if the parameter does not exist.

INTERFACE
  MODULE FUNCTION obj_GetNumber(obj, name, VALUE, value_n) RESULT(ans)
    CLASS(GmshOnelab_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: value_n
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  SetString
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set string
!
!# SetString
!
! Set the value of the string parameter `name` in the ONELAB database. Create
! the parameter if it does not exist; update the value if the parameter
! exists.
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabSetString(const char *name,
!                                   char **value, size_t value_n,
!                                   int *ierr);
!```

INTERFACE
  MODULE FUNCTION obj_SetString(obj, name, VALUE) RESULT(ans)
    CLASS(GmshOnelab_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(IN) :: VALUE
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetString
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetString
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get the value of the string parameter `name`
!
!# GetString
!
!  Get the value of the string parameter `name` from the ONELAB database.
!  Return an empty vector if the parameter does not exist.
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabGetString(const char *name,
!                                   char ***value, size_t *value_n,
!                                   int *ierr);
!```

INTERFACE
  MODULE FUNCTION obj_GetString(obj, name, VALUE, value_n) RESULT(ans)
    CLASS(GmshOnelab_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: value_n
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshOnelab_Class
