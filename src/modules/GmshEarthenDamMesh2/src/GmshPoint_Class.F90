! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE GmshPoint_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                  GmshPoint_
!----------------------------------------------------------------------------

TYPE :: GmshPoint_
  PRIVATE
  REAL(DFP) :: x = math%zero
  REAL(DFP) :: y = math%zero
  REAL(DFP) :: z = math%zero
  REAL(DFP) :: meshSize = math%one
  INTEGER(I4B) :: indx = math%one_i

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetX => obj_SetX

END TYPE GmshPoint_

!----------------------------------------------------------------------------
!                                                          GmshPointPointer_
!----------------------------------------------------------------------------

TYPE :: GmshPointPointer_
  CLASS(GmshPoint_), POINTER :: ptr => NULL()
END TYPE GmshPointPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshPoint_Class
