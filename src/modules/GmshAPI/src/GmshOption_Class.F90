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

MODULE GmshOption_Class
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshOption_
PUBLIC :: TypeGmshOption
PUBLIC :: GmshOptionPointer_

!----------------------------------------------------------------------------
!                                                                GmshOption_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Gmsh options
!
!# GmshOption_
!
! Gmsh options.
!
TYPE :: GmshOption_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, NOPASS :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, NOPASS :: SetNumber => obj_SetNumber
  PROCEDURE, PUBLIC, NOPASS :: GetNumber => obj_GetNumber
  PROCEDURE, PUBLIC, NOPASS :: SetString => obj_SetString
  PROCEDURE, PUBLIC, NOPASS :: GetString => obj_GetString
  PROCEDURE, PUBLIC, NOPASS :: SetColor => obj_SetColor
  PROCEDURE, PUBLIC, NOPASS :: GetColor => obj_GetColor
END TYPE GmshOption_

!----------------------------------------------------------------------------
!                                                              TypeGmshOption
!----------------------------------------------------------------------------

TYPE(GmshOption_), PARAMETER :: TypeGmshOption = GmshOption_()

!----------------------------------------------------------------------------
!                                                          GmshOptionPointer_
!----------------------------------------------------------------------------

TYPE :: GmshOptionPointer_
  CLASS(GmshOption_), POINTER :: Ptr => NULL()
END TYPE GmshOptionPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate()
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_SetNumber(name, VALUE) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(*), INTENT(IN) :: VALUE
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetNumber
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetNumber(name, VALUE) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    REAL(DFP), INTENT(OUT) :: VALUE
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNumber
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_SetString(name, VALUE) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(IN) :: VALUE
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetString(name, VALUE) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(OUT) :: VALUE
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_SetColor(name, r, g, b, a) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: r, g, b, a
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetColor
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetColor(name, r, g, b, a) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(OUT) :: r, g, b, a
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetColor
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshOption_Class
