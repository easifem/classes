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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBMODULE(AbstractBasis_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate1
  CALL AbstractFunction1DDeallocate(obj)
  obj%uid=0
END PROCEDURE func_Deallocate1


!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate2
  CALL AbstractFunction2DDeallocate(obj)
  obj%uid=0
END PROCEDURE func_Deallocate2


!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate3
  CALL AbstractFunction3DDeallocate(obj)
  obj%uid=0
END PROCEDURE func_Deallocate3

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_DeallocateN
  CALL AbstractFunctionNDDeallocate(obj)
  obj%uid=0
END PROCEDURE func_DeallocateN

END SUBMODULE Methods