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

SUBMODULE(ConstDirichletBC_Class) Methods
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Vector()"
#endif
#include "../../include/deallocate_vector.F90"
END PROCEDURE obj_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Ptr_Vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                    GetConstDirichletBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConstDirichletBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConstDirichletBCPointer()"
#endif
#include "../../include/AbstractBC/GetBCPointer.F90"
END PROCEDURE obj_GetConstDirichletBCPointer

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Vector()"
#endif
#include "../../include/display_vector.F90"
END PROCEDURE obj_Display_Vector

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Ptr_Vector()"
#endif

#include "../../include/display_vector_ptr.F90"
END PROCEDURE obj_Display_Ptr_Vector

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
