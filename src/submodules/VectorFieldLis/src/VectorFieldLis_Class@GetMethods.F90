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

SUBMODULE(VectorFieldLis_Class) GetMethods
USE GlobalData, ONLY: DOF_FMT, NODES_FMT

USE DOF_Method, ONLY: GetNodeLoc_, &
                      OPERATOR(.tNodes.), &
                      GetNodeLoc, &
                      GetIDOF

USE ScalarField_Class, ONLY: ScalarField_

USE STScalarField_Class, ONLY: STScalarField_

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE Display_Method, ONLY: ToString

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%local_n
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
ans => NULL()
CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: This method is not available for STScalarFieldLis_')
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Get9
! CHARACTER(*), PARAMETER :: myName = "obj_Get9()"
! INTEGER(I4B) :: tsize, s(3), p(3), ierr
! REAL(DFP), POINTER :: realvec(:)
!
! #include "./lis_null_error.F90"
!
! s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(obj=obj%dof, ivar=ivar, idof=idof))
!
! SELECT TYPE (VALUE)
!
! TYPE IS (ScalarField_)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
!                        tsize=tsize)
!   realvec => NULL()
!
! TYPE IS (STScalarField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   realvec => VALUE%GetPointer()
!
!   CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                        tsize=tsize)
!   realvec => NULL()
!
! TYPE IS (VectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
!                                             ivar=ivar_value, idof=idof_value))
!
!   realvec => VALUE%GetPointer()
!
!   CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                        tsize=tsize)
!   realvec => NULL()
!
! TYPE IS (ScalarFieldLis_)
!
!   tsize = obj%dof.tNodes.idof
!   CALL lis_vector_get_values_from_range3(obj%lis_ptr, s(1), s(3), tsize, &
!                                          VALUE%lis_ptr, ierr)
!
!   CALL check_lis_error
!
! TYPE IS (STScalarFieldLis_)
!
!   tsize = obj%dof.tNodes.idof
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!
!   CALL lis_vector_get_values_from_range4(obj%lis_ptr, s(1), s(3), tsize, &
!                                          VALUE%lis_ptr, p(1), p(3), ierr)
!
!   CALL check_lis_error
!
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTENRAL ERROR] :: No case found for the type of value')
!   RETURN
! END SELECT
!
! CONTAINS
!
! SUBROUTINE check_lis_error
! #ifdef DEBUG_VER
!   CALL CHKERR(ierr)
! #endif
!
! END SUBROUTINE check_lis_error
!
! END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
