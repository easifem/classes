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

SUBMODULE(AbstractFile_Class) EnquireMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     IsOpen
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsOpen
  ans = obj%openStat
END PROCEDURE obj_IsOpen

!----------------------------------------------------------------------------
!                                                                      IsEOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsEOF
  ans = obj%eofStat
END PROCEDURE obj_IsEOF

!----------------------------------------------------------------------------
!                                                                    IsWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsWrite
  ans = obj%writeStat
END PROCEDURE obj_IsWrite

!----------------------------------------------------------------------------
!                                                                     IsRead
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsRead
  ans = obj%readStat
END PROCEDURE obj_IsRead

END SUBMODULE EnquireMethods
