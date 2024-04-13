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
!                                                                 isOpen
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_isOpen
ans = obj%openStat
END PROCEDURE aFile_isOpen

!----------------------------------------------------------------------------
!                                                                 isEOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_isEOF
ans = obj%eofStat
END PROCEDURE aFile_isEOF

!----------------------------------------------------------------------------
!                                                                 isWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_isWrite
ans = obj%writeStat
END PROCEDURE aFile_isWrite

!----------------------------------------------------------------------------
!                                                                 isRead
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_isRead
ans = obj%readStat
END PROCEDURE aFile_isRead

END SUBMODULE EnquireMethods
