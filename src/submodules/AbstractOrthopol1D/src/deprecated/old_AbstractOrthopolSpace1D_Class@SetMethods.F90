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

SUBMODULE(AbstractOrthopolSpace1D_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_SetParam
  IF( PRESENT( n ) ) obj%n=n
  IF( PRESENT( isMonic ) ) obj%isMonic=isMonic
  IF( PRESENT( isOrthonormal ) ) obj%isOrthonormal=isOrthonormal
  IF( PRESENT( coeff ) ) obj%coeff=coeff
  IF( PRESENT( scale ) ) obj%scale=scale
END PROCEDURE Orthopol_SetParam

END SUBMODULE SetMethods