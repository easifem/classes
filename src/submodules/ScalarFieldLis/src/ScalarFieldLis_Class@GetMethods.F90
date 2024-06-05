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

SUBMODULE(ScalarFieldLis_Class) GetMethods
USE AbstractField_Class, ONLY: TypeField

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE Display_Method, ONLY: ToString

USE DOF_Method, ONLY: GetNodeLoc, &
                      GetIDOF, &
                      OPERATOR(.tNodes.)

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                               GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: This method is not available for ScalarFieldLis_')
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: s(3), p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'ScalarFieldLis_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'AbstractNodeField::value is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, idof=1)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize, istart_value=p(1), iend_value=p(2), &
                       stride_value=p(3))
  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize, istart_value=p(1), iend_value=p(2), &
                       stride_value=p(3))
  realvec => NULL()

! TYPE IS (VectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   tsize = obj%dof.tNodes.1
!
!   realvec => VALUE%GetPointer()
!   CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
!                        tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                        stride_value=p(3))
!   realvec => NULL()

TYPE IS (ScalarFieldLis_)
  tsize = obj%dof.tNodes.1
  CALL lis_vector_get_values_from_range3(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr)

TYPE IS (STScalarFieldLis_)
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))

  tsize = obj%dof.tNodes.1
  CALL lis_vector_get_values_from_range4(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr, p(1), p(3))

TYPE IS (VectorFieldLis_)
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))

  tsize = obj%dof.tNodes.1
  CALL lis_vector_get_values_from_range4(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr, p(1), p(3))

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type value')
  RETURN
END SELECT

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
