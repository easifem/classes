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
!                                                       GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetFEVariable
CHARACTER(*), PARAMETER :: myName = "anf_GetFEVariable()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
 & '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE anf_GetFEVariable

!----------------------------------------------------------------------------
!                                                         GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetPhysicalNames
CHARACTER(*), PARAMETER :: myName = "anf_GetPhysicalNames()"
INTEGER(I4B) :: tnames, aint
LOGICAL(LGT) :: isOK

IF (ALLOCATED(obj%dof_names_char)) THEN
  tnames = SIZE(obj%dof_names_char)
  aint = SIZE(ans)
  isOK = tnames .EQ. aint
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The size of names ('//tostring(aint)//  &
      & ') is not same as total physical variables = '//tostring(tnames))
    RETURN
  END IF

  DO aint = 1, tnames
    ans(aint) (:) = obj%dof_names_char(aint) (1:1)
  END DO
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_names_char not'//  &
    & ' not allocated.')
  RETURN
END IF

END PROCEDURE anf_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                       GetTotalPhysicalVars
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTotalPhysicalVars
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalPhysicalVars()"
INTEGER(I4B) :: tnames, aint
LOGICAL(LGT) :: isOK

ans = 0
IF (ALLOCATED(obj%dof_names_char)) THEN
  tnames = SIZE(obj%dof_names_char)
  aint = obj%dof_tPhysicalVars
  isOK = tnames .EQ. aint
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The size of names ('//tostring(tnames)//  &
      & ') is not same as total physical variables = '//tostring(aint))
    RETURN
  END IF
  ans = aint
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_names_char not'//  &
    & ' not allocated.')
  RETURN
END IF

END PROCEDURE anf_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetSpaceCompo
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalPhysicalVars()"
INTEGER(I4B) :: tnames, aint
LOGICAL(LGT) :: isOK

ans = 0
IF (ALLOCATED(obj%dof_spaceCompo)) THEN
  tnames = SIZE(obj%dof_spaceCompo)
  aint = tPhysicalVars
  isOK = tnames .EQ. aint
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The size of spaceCompo ('//tostring(tnames)//  &
      & ') is not same as total physical variables = '//tostring(aint))
    RETURN
  END IF
  ans(1:aint) = obj%dof_spaceCompo(1:aint)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%spaceCompo not'//  &
    & ' not allocated.')
  RETURN
END IF
END PROCEDURE anf_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTimeCompo
CHARACTER(*), PARAMETER :: myName = "anf_GetTimeCompo()"
INTEGER(I4B) :: tnames, aint
LOGICAL(LGT) :: isOK

ans = 0
IF (ALLOCATED(obj%dof_timeCompo)) THEN
  tnames = SIZE(obj%dof_timeCompo)
  aint = tPhysicalVars
  isOK = tnames .EQ. aint
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The size of timeCompo ('//tostring(tnames)//  &
      & ') is not same as total physical variables = '//tostring(aint))
    RETURN
  END IF
  ans(1:aint) = obj%dof_timeCompo(1:aint)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%timeCompo not'//  &
    & ' not allocated.')
  RETURN
END IF
END PROCEDURE anf_GetTimeCompo

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
ans = obj%tSize
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
