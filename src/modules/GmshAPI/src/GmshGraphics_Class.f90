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
USE GmshInterface
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHGRAPHICS_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshGraphics_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( Obj ) :: Draw=>graphics_Draw
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => graphics_Initiate
END TYPE GmshGraphics_

PUBLIC :: GmshGraphics_
TYPE( GmshGraphics_ ), PUBLIC, PARAMETER :: TypeGmshGraphics = GmshGraphics_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshGraphicsPointer_
  CLASS( GmshGraphics_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshGraphicsPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE graphics_Initiate( obj )
  CLASS( GmshGraphics_ ), INTENT( INOUT ) :: obj
END SUBROUTINE graphics_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION graphics_Draw(obj) RESULT( ans )
  CLASS( GmshGraphics_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans

  ! Internal
  CALL gmshGraphicsDraw(ierr)
  ans = int(ierr, i4b)
END FUNCTION graphics_Draw

END MODULE GmshGraphics_Class