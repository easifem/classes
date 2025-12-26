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

SUBMODULE(NewtonianFluidModel_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetData
obj%mu = DATA(1)
END PROCEDURE obj_SetData

!----------------------------------------------------------------------------
!                                                                    SetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateData
DATA(1) = obj%mu
END PROCEDURE obj_UpdateData

!----------------------------------------------------------------------------
!                                                                    SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
LOGICAL(LGT) :: isok
isok = PRESENT(dynamicViscosity)
IF (isok) obj%mu = dynamicViscosity
END PROCEDURE obj_SetParam

END SUBMODULE SetMethods
