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
!

MODULE AbstractEngine_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractEngine_

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractEngine_Class"
#endif

!----------------------------------------------------------------------------
!                                                           AbstractEngine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-08
! summary: Abstract engine for linear algebra operations

TYPE, ABSTRACT :: AbstractEngine_

CONTAINS
  PROCEDURE(EmptyMethod), DEFERRED, PUBLIC, PASS(obj) :: Initiate
  PROCEDURE(EmptyMethod), DEFERRED, PUBLIC, PASS(obj) :: DEALLOCATE
END TYPE AbstractEngine_

ABSTRACT INTERFACE
  SUBROUTINE EmptyMethod(obj)
    IMPORT :: AbstractEngine_
    CLASS(AbstractEngine_), INTENT(INOUT) :: obj
  END SUBROUTINE EmptyMethod
END INTERFACE

END MODULE AbstractEngine_Class
