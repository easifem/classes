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

SUBMODULE(ScalarFieldLis_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Final
CALL obj%DEALLOCATE()
END PROCEDURE sField_Final

!----------------------------------------------------------------------------
!                                                                ScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Constructor1
CALL ans%initiate(param, dom)
END PROCEDURE sField_Constructor1

!----------------------------------------------------------------------------
!                                                         ScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, dom)
END PROCEDURE sField_Constructor_1

#ifdef USE_LIS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Initiate1
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "sField_Initiate"
INTEGER(I4B) :: ierr

CALL ScalarFieldInitiate1(obj=obj, param=param, dom=dom)

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

END PROCEDURE sField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Deallocate
#include "lisf.h"
INTEGER(I4B) :: ierr
CALL lis_vector_destroy(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE sField_Deallocate

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Size
ans = obj%local_n
END PROCEDURE sField_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#endif
END SUBMODULE ConstructorMethods
