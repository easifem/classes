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
!                                                               GetFileParts
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileParts
  path = obj%path
  fileName = obj%fileName
  ext = obj%ext
END PROCEDURE obj_GetFileParts

!----------------------------------------------------------------------------
!                                                                GetFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFilePath
  path = obj%path
END PROCEDURE obj_GetFilePath

!----------------------------------------------------------------------------
!                                                                GetFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileName
  fileName = obj%fileName
END PROCEDURE obj_GetFileName

!----------------------------------------------------------------------------
!                                                                 GetFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileExt
  ext = obj%ext
END PROCEDURE obj_GetFileExt

END SUBMODULE GetMethods
