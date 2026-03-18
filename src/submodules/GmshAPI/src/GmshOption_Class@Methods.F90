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

SUBMODULE(GmshOption_Class) Methods
USE ExceptionHandler_Class, ONLY: e

USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_PTR

USE GmshUtility, ONLY: gmsh_cdouble
USE GmshUtility, ONLY: gmsh_CString
USE GmshUtility, ONLY: gmsh_cint

USE CInterface, ONLY: C2Fortran

USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshOptionSetNumber
USE GmshInterface, ONLY: GmshOptionGetNumber
USE GmshInterface, ONLY: GmshOptionSetString
USE GmshInterface, ONLY: GmshOptionGetString
USE GmshInterface, ONLY: GmshOptionSetColor
USE GmshInterface, ONLY: GmshOptionGetColor

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "GmshOption_Class@Methods.F90"
INTEGER(C_INT) :: ierr
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

CONTAINS

!----------------------------------------------------------------------------
!
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
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetNumber()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshOptionSetNumber(name=C_LOC(name_), VALUE=gmsh_cdouble(VALUE), &
                         ierr=ans)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNumber()"
#endif

! Internal variables
CHARACTER(maxStrLen), TARGET :: name_
REAL(C_DOUBLE) :: val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR

CALL GmshOptionGetNumber(name=C_LOC(name_), VALUE=val, ierr=ans)

VALUE = REAL(val, KIND=DFP)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetString
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetString()"
#endif
CHARACTER(maxStrLen), TARGET :: name_, value_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR
value_ = TRIM(VALUE)//C_NULL_CHAR

CALL GmshOptionSetString(name=C_LOC(name_), VALUE=C_LOC(value_), ierr=ans)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetString
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetString()"
#endif
CHARACTER(maxStrLen), TARGET :: name_
TYPE(C_PTR) :: value_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = TRIM(name)//C_NULL_CHAR
CALL GmshOptionGetString(name=C_LOC(name_), VALUE=value_, ierr=ans)
CALL C2Fortran(C_String=value_, F_String=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColor
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetColor()"
#endif
CHARACTER(maxStrLen), TARGET :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = gmsh_CString(name)
CALL GmshOptionSetColor(name=C_LOC(name_), r=gmsh_cint(r), g=gmsh_cint(g), &
                        b=gmsh_cint(b), a=gmsh_cint(a), ierr=ans)

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
CHARACTER(maxStrLen), TARGET :: name_
INTEGER(C_INT) :: r0, g0, b0, a0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = gmsh_CString(name)

CALL GmshOptionGetColor(name=C_LOC(name_), r=r0, g=g0, b=b0, a=a0, &
                        ierr=ans)

r = INT(r0, KIND=I4B)
g = INT(g0, KIND=I4B)
b = INT(b0, KIND=I4B)
a = INT(a0, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetColor

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
