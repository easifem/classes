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

SUBMODULE(AbstractFunction_Class) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE funcptr_Eval
  ans = obj%ptr%Eval(x)
END PROCEDURE funcptr_Eval

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE funcptr_EvalGradient
  ans = obj%ptr%EvalGradient(x)
END PROCEDURE funcptr_EvalGradient

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE funcptr_Deallocate
  CALL obj%ptr%Deallocate()
  obj%ptr => NULL()
END PROCEDURE funcptr_Deallocate


END SUBMODULE Methods