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
USE GlobalData, ONLY: DFP, I4B
USE Utility, ONLY: Reallocate
USE GmshInterface
USE CInterface
USE ExceptionHandler_Class, ONLY: e
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "GmshOnelab_Class"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER(I4B), PARAMETER :: maxStrLen = 256
PUBLIC :: GmshOnelab_
PUBLIC :: GmshOnelabPointer_

!----------------------------------------------------------------------------
!                                                               GmshOnelab_
!----------------------------------------------------------------------------

TYPE :: GmshOnelab_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => onelab_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Set => onelab_Set
  PROCEDURE, PUBLIC, PASS(obj) :: Get => onelab_Get
  PROCEDURE, PUBLIC, PASS(obj) :: GetNumber => onelab_GetNumber
  PROCEDURE, PUBLIC, PASS(obj) :: GetString => onelab_GetString
  PROCEDURE, PUBLIC, PASS(obj) :: SetString => onelab_SetString

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

TYPE(GmshOnelab_), PUBLIC, PARAMETER :: TypeGmshOnelab = GmshOnelab_()

!----------------------------------------------------------------------------
!                                                        GmshOnelabPointer_
!----------------------------------------------------------------------------

TYPE :: GmshOnelabPointer_
  CLASS(GmshOnelab_), POINTER :: ptr => NULL()
END TYPE GmshOnelabPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  Currently, this routine does nothing

SUBROUTINE onelab_Initiate(obj)
  CLASS(GmshOnelab_), INTENT(INOUT) :: obj
END SUBROUTINE onelab_Initiate

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set one or more parameters in the ONELAB database, encoded in `format'.

FUNCTION onelab_Set(obj, DATA, FORMAT) RESULT(ans)
  CLASS(GmshOnelab_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: DATA
  CHARACTER(*), OPTIONAL, INTENT(IN) :: FORMAT
  INTEGER(I4B) :: ans
  ! internal variables
  CHARACTER(:), ALLOCATABLE, TARGET :: data_, format_
  TYPE(C_PTR) :: format_ptr
  !> main
  data_ = TRIM(DATA)//C_NULL_CHAR
  IF (PRESENT(FORMAT)) THEN
    format_ = TRIM(FORMAT)//C_NULL_CHAR
  ELSE
    format_ = "json"//C_NULL_CHAR
  END IF
  format_ptr = C_LOC(format_)
  CALL gmshOnelabSet(C_LOC(data_), format_ptr, ierr)
  ans = INT(ierr, I4B)
END FUNCTION onelab_Set

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get one or more parameters from the ONELAB database.

FUNCTION onelab_Get(obj, DATA, name, FORMAT) RESULT(ans)
  CLASS(GmshOnelab_), INTENT(INOUT) :: obj
  TYPE(C_PTR), TARGET, INTENT(IN) :: DATA(*)
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: FORMAT
  INTEGER(I4B) :: ans
  ! !> internal variables
  ! CHARACTER( LEN = : ), ALLOCATABLE, TARGET :: name_, format_
  CHARACTER(*), PARAMETER :: myName = "onelab_Get()"
  ! !> main
  ! name_ = TRIM( name ) // C_NULL_CHAR
  ! format_ = TRIM( format ) // C_NULL_CHAR
  ! CALL gmshOnelabGet( data, C_LOC( name_ ), C_LOC( format_) )
  ! TODO: 
  ! Implement onelab_Get method
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "[WIP ERROR] :: This routine is under construction.")
END FUNCTION onelab_Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!# Introduction
!
! Get the value of the number parameter `name` from the ONELAB database.
!  Return an empty vector if the parameter does not exist.

FUNCTION onelab_GetNumber(obj, name, VALUE, value_n) RESULT(ans)
  CLASS(GmshOnelab_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: value_n
  INTEGER(I4B) :: ans
  !> Internal variables
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: value_n_
  CHARACTER(:), ALLOCATABLE, TARGET :: name_
  !> main
  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshOnelabGetNumber(C_LOC(name_), cptr, value_n_, ierr)
  CALL Reallocate(VALUE, INT(value_n_, I4B))
  IF (PRESENT(value_n)) value_n = value_n_
  CALL C_PTR_TO_REAL_VEC(cptr=cptr, vec=VALUE)
  ans = INT(ierr, I4B)
END FUNCTION onelab_GetNumber

!----------------------------------------------------------------------------
!                                                                  SetString
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!# Introduction
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

FUNCTION onelab_SetString(obj, name, VALUE) RESULT(ans)
  CLASS(GmshOnelab_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: VALUE
  INTEGER(I4B) :: ans
  !> internal variables
  CHARACTER(:), ALLOCATABLE, TARGET :: name_
  CHARACTER(:), ALLOCATABLE, TARGET :: value_
  name_ = TRIM(name)//C_NULL_CHAR
  value_ = TRIM(VALUE)//C_NULL_CHAR
  CALL gmshOnelabSetString(C_LOC(name_), C_LOC(value_), &
    & LEN(value_, C_SIZE_T), ierr)
  ans = INT(ierr, I4B)
END FUNCTION onelab_SetString

!----------------------------------------------------------------------------
!                                                                 getString
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get the value of the string parameter `name`
!
!# Introduction
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

FUNCTION onelab_GetString(obj, name, VALUE, value_n) RESULT(ans)
  CLASS(GmshOnelab_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(:), ALLOCATABLE, INTENT(INOUT) :: VALUE
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: value_n
  INTEGER(I4B) :: ans
  !> internal variables
  TYPE(C_PTR) :: ptrValue
  CHARACTER(:), ALLOCATABLE, TARGET :: name_
  INTEGER(C_SIZE_T) :: value_n_
  !> main
  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshOnelabGetString(C_LOC(name_), ptrValue, value_n_, ierr)
  IF (value_n_ .EQ. 0) THEN
    VALUE = ""
  ELSE
    ALLOCATE (CHARACTER(value_n_) :: VALUE)
    CALL C2Fortran(C_String=ptrValue, F_STRING=VALUE)
  END IF
  ans = INT(ierr, I4B)
END FUNCTION onelab_GetString

END MODULE GmshOnelab_Class
