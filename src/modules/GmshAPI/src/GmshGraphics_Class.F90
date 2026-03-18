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

MODULE GmshGraphics_Class
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GmshGraphics_
PUBLIC :: TypeGmshGraphics
PUBLIC :: GmshGraphicsPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshGraphics_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(Obj) :: Draw => obj_Draw
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
END TYPE GmshGraphics_

!----------------------------------------------------------------------------
!                                                           TypeGmshGraphics
!----------------------------------------------------------------------------

TYPE(GmshGraphics_), PARAMETER :: TypeGmshGraphics = GmshGraphics_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshGraphicsPointer_
  CLASS(GmshGraphics_), POINTER :: Ptr => NULL()
END TYPE GmshGraphicsPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GmshGraphics_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_Draw(obj) RESULT(ans)
    CLASS(GmshGraphics_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Draw
END INTERFACE

END MODULE GmshGraphics_Class
