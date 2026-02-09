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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                isFormatted
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsFormatted
ans = obj%formatstat
END PROCEDURE obj_IsFormatted

!----------------------------------------------------------------------------
!                                                                   isDirect
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsDirect
ans = obj%accessstat
END PROCEDURE obj_IsDirect

!----------------------------------------------------------------------------
!                                                                   isPadded
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsPadded
ans = obj%padstat
END PROCEDURE obj_IsPadded

!----------------------------------------------------------------------------
!                                                                   isNew
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNew
ans = obj%newstat
END PROCEDURE obj_IsNew

!----------------------------------------------------------------------------
!                                                               isOverwrite
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsOverwrite
ans = obj%overwrite
END PROCEDURE obj_IsOverwrite

!----------------------------------------------------------------------------
!                                                               isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%initstat
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE EnquireMethods
