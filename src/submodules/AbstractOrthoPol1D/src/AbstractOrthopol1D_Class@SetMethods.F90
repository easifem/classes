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

SUBMODULE(AbstractOrthopol1D_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthoPol_SetParam
  TYPE(String) :: astr
  obj%n = n
  obj%an_1 = an_1
  obj%bn_1 = bn_1
  obj%sn_1 = sn_1
  obj%sn_2 = sn_2
  astr = obj%getstringforuid( )
  obj%uid = stringtouid( astr%chars() )
  obj%varname = String( trim(varname) )
END PROCEDURE OrthoPol_SetParam

END SUBMODULE SetMethods