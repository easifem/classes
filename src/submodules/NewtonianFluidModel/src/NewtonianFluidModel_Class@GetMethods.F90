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

SUBMODULE(NewtonianFluidModel_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDynamicViscosity
ans = obj%mu
END PROCEDURE obj_GetDynamicViscosity

!----------------------------------------------------------------------------
!                                                                 GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
LOGICAL(LGT) :: isok
isok = PRESENT(dynamicViscosity)
IF (isok) dynamicViscosity = obj%mu
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                               GetDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataSize
ans = 1
END PROCEDURE obj_GetDataSize

!----------------------------------------------------------------------------
!                                                                    GetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetData
tsize = 1
DATA(1) = obj%mu
END PROCEDURE obj_GetData

END SUBMODULE GetMethods
