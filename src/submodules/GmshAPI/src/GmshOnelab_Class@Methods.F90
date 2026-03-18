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

SUBMODULE(GmshOnelab_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE ReallocateUtility, ONLY: Reallocate

USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshOnelabSet
USE GmshInterface, ONLY: GmshOnelabGetNumber
USE GmshInterface, ONLY: GmshOnelabSetString
USE GmshInterface, ONLY: GmshOnelabGetString

USE CInterface, ONLY: C_PTR_TO_REAL_VEC
USE CInterface, ONLY: C2Fortran

USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_SIZE_T

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "GmshOnelab_Class"
INTEGER(C_INT) :: ierr
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif
CHARACTER(:), ALLOCATABLE, TARGET :: data_, format_
TYPE(C_PTR) :: format_ptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

data_ = TRIM(DATA)//C_NULL_CHAR
isok = PRESENT(FORMAT)

IF (isok) THEN
  format_ = TRIM(FORMAT)//C_NULL_CHAR
ELSE
  format_ = "json"//C_NULL_CHAR
END IF

format_ptr = C_LOC(format_)
CALL GmshOnelabSet(C_LOC(data_), format_ptr, ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNumber()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: value_n_
CHARACTER(:), ALLOCATABLE, TARGET :: name_
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshOnelabGetNumber(C_LOC(name_), cptr, value_n_, ierr)

CALL Reallocate(VALUE, INT(value_n_, I4B))

isok = PRESENT(value_n)
IF (isok) value_n = INT(value_n_, kind=I4B)

CALL C_PTR_TO_REAL_VEC(cptr=cptr, vec=VALUE)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNumber

!----------------------------------------------------------------------------
!                                                                  SetString
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetString
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetString()"
#endif
CHARACTER(:), ALLOCATABLE, TARGET :: name_
CHARACTER(:), ALLOCATABLE, TARGET :: value_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR
value_ = TRIM(VALUE)//C_NULL_CHAR

CALL GmshOnelabSetString(C_LOC(name_), C_LOC(value_), &
                         LEN(value_, C_SIZE_T), ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetString

!----------------------------------------------------------------------------
!                                                                 GetString
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetString
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetString()"
#endif
TYPE(C_PTR) :: ptrValue
CHARACTER(:), ALLOCATABLE, TARGET :: name_
INTEGER(C_SIZE_T) :: value_n_
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshOnelabGetString(C_LOC(name_), ptrValue, value_n_, ierr)

isok = value_n_ .EQ. 0
IF (isok) THEN
  VALUE = ""
ELSE
  ALLOCATE (CHARACTER(value_n_) :: VALUE)
  CALL C2Fortran(C_String=ptrValue, F_STRING=VALUE)
END IF

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetString

!----------------------------------------------------------------------------
!                                                            Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
