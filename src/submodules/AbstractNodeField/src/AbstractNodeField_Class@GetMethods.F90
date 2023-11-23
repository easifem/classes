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

SUBMODULE(AbstractNodeField_Class) GetMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetPointer
ans => GetPointer(obj%realVec)
END PROCEDURE anf_GetPointer

!----------------------------------------------------------------------------
!                                                                     Size
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_size
CHARACTER(*), PARAMETER :: myName = "anf_size"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
! ans = obj%tSize
END PROCEDURE anf_size

!----------------------------------------------------------------------------
!                                                                     Norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Norm2
CHARACTER(*), PARAMETER :: myName = "anf_Norm2"
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  ans = NORM2(obj=obj%realvec)
ELSE
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This method has been implemented for NATIVE engines')
  ans = 0.0_DFP
END IF
END PROCEDURE anf_Norm2

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetSingle
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  VALUE = Get( &
    & obj=obj%realVec, &
    & nodenum=1, &
    & dataType=1.0_DFP)
ELSE
  VALUE = Get( &
    & obj=obj%realVec, &
    & nodenum=indx, &
    & dataType=1.0_DFP)
END IF
END PROCEDURE anf_GetSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
