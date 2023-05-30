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

SUBMODULE(BlockNodeFieldLis_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

#ifdef USE_LIS
MODULE PROCEDURE bnField_Initiate3
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "bnField_Initiate3"
INTEGER(I4B) :: ierr

CALL BlockNodeFieldInitiate3(obj=obj, param=param, dom=dom)

CALL lis_vector_create(obj%comm, obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_vector_set_size(obj%lis_ptr, obj%local_n, &
  & obj%global_n, ierr)
CALL CHKERR(ierr)

CALL lis_vector_get_range( &
  & obj%lis_ptr, &
  & obj%is, &
  & obj%ie, &
  & ierr &
  & )
CALL CHKERR(ierr)
END PROCEDURE bnField_Initiate3

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Final
CALL obj%DEALLOCATE()
END PROCEDURE bnField_Final

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Size
ans = obj%local_n
END PROCEDURE bnField_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#endif
END SUBMODULE ConstructorMethods
