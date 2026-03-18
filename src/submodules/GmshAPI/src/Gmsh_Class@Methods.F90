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

!> authors: Vikas Sharma, Ph. D.
! date:         26 April 2021
! summary: Gmsh-Fortran Interface

SUBMODULE(Gmsh_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshInitialize
USE GmshInterface, ONLY: GmshFinalize
USE GmshInterface, ONLY: GmshIsInitialized
USE GmshInterface, ONLY: GmshFinalize
USE GmshInterface, ONLY: GmshOpen
USE GmshInterface, ONLY: GmshMerge
USE GmshInterface, ONLY: GmshWrite
USE GmshInterface, ONLY: GmshClear
USE GmshUtility, ONLY: gmsh_GetCharArray_cPtr
USE GmshUtility, ONLY: gmsh_InputStr
USE GmshUtility, ONLY: gmsh_strArraySize
USE GmshUtility, ONLY: gmsh_CString
USE CInterface, ONLY: optval_c_bool
IMPLICIT NONE

INTEGER(C_INT) :: ierr
CHARACTER(*), PARAMETER :: modName = "Gmsh_Class@Methods.F90"
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initialize
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = "obj_Initialize()"
#endif
CHARACTER(LEN=maxStrLen, KIND=C_CHAR), ALLOCATABLE :: argv_strs(:)
TYPE(C_PTR), ALLOCATABLE :: argv_cptr(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = math%zero_i

#ifdef DEBUG_VER
IF (obj%isInit) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    "Gmsh is already initiated; hint: You can run finalize(), &
    & the initialize()")
END IF
#endif

! The following code is initiated when obj%isInit is false

CALL gmsh_GetCharArray_cPtr( &
  gmsh_InputStr(default=[''], option=argv), argv_strs, argv_cptr)

CALL GmshInitialize( &
  argc=gmsh_strArraySize(argv), &
  argv=argv_cptr, &
  readConfigFiles=optval_c_bool(default=math%yes, &
                                option=readConfigFiles), &
  run=optval_c_bool(default=math%no, option=run), ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%graphics)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    "Gmsh%graphics is already associated; hint: &
    & You can try, first Nullifying it")
  ans = -1
END IF
#endif

ALLOCATE (obj%graphics)
CALL obj%graphics%Initiate()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%option)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    "Gmsh%option is already associated; &
    & hint: You can try, first Nullifying it")
  ans = -1
END IF
#endif

ALLOCATE (obj%option)
CALL obj%option%Initiate()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%FLTK)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    "Gmsh%FLTK is already associated; &
    & hint: You can try, first Nullifying it")
  ans = -1
END IF
#endif

ALLOCATE (obj%fltk)
CALL obj%fltk%Initiate()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%Onelab)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    "Gmsh%Onelab is already associated; hint: &
    & You can try, first Nullifying it")
  ans = -1
END IF
#endif

ALLOCATE (obj%onelab)
CALL obj%onelab%Initiate()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%model)
IF (isok) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    "Gmsh%model is already associated; hint: &
    & You can try, first Nullifying it")
  ans = -1
END IF
#endif

ALLOCATE (obj%model)
CALL obj%model%Initiate()
obj%isInit = math%yes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsInitiated()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%isInit

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitialized
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsInitialized()"
#endif
INTEGER(C_INT) :: ans0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans0 = GmshIsInitialized(ierr=ierr)
ans = INT(ans0, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsInitialized

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Finalize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Finalize()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%graphics)
IF (isok) THEN
  DEALLOCATE (obj%graphics)
END IF

isok = ASSOCIATED(obj%option)
IF (isok) THEN
  DEALLOCATE (obj%option)
END IF

isok = ASSOCIATED(obj%fltk)
IF (isok) THEN
  DEALLOCATE (obj%fltk)
END IF

isok = ASSOCIATED(obj%model)
IF (isok) THEN
  isok = ASSOCIATED(obj%model%geo)
  IF (isok) THEN
    DEALLOCATE (obj%model%geo)
  END IF

  isok = ASSOCIATED(obj%model%occ)
  IF (isok) THEN
    DEALLOCATE (obj%model%occ)
  END IF

  isok = ASSOCIATED(obj%model%mesh)
  IF (isok) THEN
    DEALLOCATE (obj%model%mesh)
  END IF

  DEALLOCATE (obj%model)
END IF

obj%option => NULL()
obj%model => NULL()
obj%isInit = math%no
ans = math%zero_i

CALL GmshFinalize(ierr=ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_finalize

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Finalize_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Finalize_()"
#endif

INTEGER(I4B) :: ans

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%Finalize()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Finalize_

!----------------------------------------------------------------------------
!                                                                      Open
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Open
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Open()"
#endif
CHARACTER(LEN=maxStrLen), TARGET :: C_STR

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

C_STR = TRIM(fileName)//C_NULL_CHAR
CALL GmshOpen(fileName=C_LOC(C_STR), ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Open

!----------------------------------------------------------------------------
!                                                                     Close
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Merge
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Merge()"
#endif
CHARACTER(LEN=maxStrLen), TARGET :: C_STR

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

C_STR = TRIM(fileName)//C_NULL_CHAR
CALL GmshMerge(fileName=C_LOC(C_STR), ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Merge

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Write()"
#endif
CHARACTER(LEN=maxStrLen) :: C_STR

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

C_STR = gmsh_CString(fileName)
CALL GmshWrite(fileName=C_STR, ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Write

!----------------------------------------------------------------------------
!                                                                     Clear
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Clear
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Clear()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshClear(ierr=ierr)
ans = INT(ierr, KIND=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Clear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
