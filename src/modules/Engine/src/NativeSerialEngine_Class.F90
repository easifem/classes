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

MODULE NativeSerialEngine_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractEngine_Class, ONLY: AbstractEngine_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: NativeSerialEngine_

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "NativeSerialEngine_Class"
#endif

!----------------------------------------------------------------------------
!                                                         NativeSerialEngine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-08
! summary: Abstract engine for linear algebra operations

TYPE, EXTENDS(AbstractEngine_) :: NativeSerialEngine_

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
END TYPE NativeSerialEngine_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(NativeSerialEngine_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(NativeSerialEngine_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

END MODULE NativeSerialEngine_Class
