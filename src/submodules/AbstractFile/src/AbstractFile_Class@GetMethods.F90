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

SUBMODULE(AbstractFile_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              getFileParts
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_getFileParts
  path=obj%path
  fileName=obj%fileName
  ext=obj%ext
END PROCEDURE aFile_getFileParts

!----------------------------------------------------------------------------
!                                                                getFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_getFilePath
  path = obj%path
END PROCEDURE aFile_getFilePath

!----------------------------------------------------------------------------
!                                                                getFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_getFileName
  fileName = obj%fileName
END PROCEDURE aFile_getFileName

!----------------------------------------------------------------------------
!                                                                getFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_getFileExt
  ext = obj%ext
END PROCEDURE aFile_getFileExt

END SUBMODULE GetMethods