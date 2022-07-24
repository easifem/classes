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

SUBMODULE(FortranFile_Class) EnquireMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                isFormatted
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isFormatted
  ans = obj%formatstat
END PROCEDURE ff_isFormatted

!----------------------------------------------------------------------------
!                                                                   isDirect
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isDirect
  ans = obj%accessstat
END PROCEDURE ff_isDirect

!----------------------------------------------------------------------------
!                                                                   isPadded
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isPadded
  ans = obj%padstat
END PROCEDURE ff_isPadded

!----------------------------------------------------------------------------
!                                                                   isNew
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isNew
  ans = obj%newstat
END PROCEDURE ff_isNew

!----------------------------------------------------------------------------
!                                                               isOverwrite
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isOverwrite
  ans = obj%overwrite
END PROCEDURE ff_isOverwrite

!----------------------------------------------------------------------------
!                                                               isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isInitiated
  ans = obj%initstat
END PROCEDURE ff_isInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE EnquireMethods
