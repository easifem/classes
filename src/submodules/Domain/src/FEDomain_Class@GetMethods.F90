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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(FEDomain_Class) GetMethods
USE ReallocateUtility
USE InputUtility
USE BoundingBox_Method
USE F95_BLAS, ONLY: Copy
IMPLICIT NONE
CONTAINS

MODULE PROCEDURE obj_GetNptrs
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetNptrs()
CASE (2)
  ans = obj%meshSurface%GetNptrs()
CASE (1)
  ans = obj%meshCurve%GetNptrs()
CASE (0)
  ans = obj%meshPoint%GetNptrs()
END SELECT
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
SELECT CASE (dim)
CASE (3)
  CALL obj%meshVolume%GetNptrs_(nptrs=nptrs)
CASE (2)
  CALL obj%meshSurface%GetNptrs_(nptrs=nptrs)
CASE (1)
  CALL obj%meshCurve%GetNptrs_(nptrs=nptrs)
CASE (0)
  CALL obj%meshPoint%GetNptrs_(nptrs=nptrs)
END SELECT
END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
