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

MODULE EngineFactoryUtility
USE GlobalData, ONLY: LGT, I4B, DFP
USE AbstractEngine_Class, ONLY: AbstractEngine_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

PRIVATE
PUBLIC :: EngineFactory

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "EngineFactoryUtility"
#endif

!----------------------------------------------------------------------------
!                                                              EngineFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION EngineFactory(engine) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: engine
    CLASS(AbstractEngine_), POINTER :: ans
  END FUNCTION EngineFactory
END INTERFACE

END MODULE EngineFactoryUtility
