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

MODULE PROCEDURE obj_GetFEVariable
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
 & '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                         GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPhysicalNames
CHARACTER(*), PARAMETER :: myName = "obj_GetPhysicalNames()"
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

END PROCEDURE obj_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                       GetTotalPhysicalVars
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPhysicalVars
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
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

END PROCEDURE obj_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpaceCompo
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
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
END PROCEDURE obj_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeCompo
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeCompo()"
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
END PROCEDURE obj_GetTimeCompo

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
ans => GetPointer(obj%realVec)
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                     Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_size
CHARACTER(*), PARAMETER :: myName = "obj_size"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
ans = obj%tSize
END PROCEDURE obj_size

!----------------------------------------------------------------------------
!                                                                     Norm2
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
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
END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc1
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc1()"
INTEGER(I4B), ALLOCATABLE :: spaceCompo0(:), timeCompo0(:), localNode(:)
INTEGER(I4B) :: ivar0, tsize, itime, ttime
TYPE(IntVector_), ALLOCATABLE :: int_vec_list(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] GetNodeLoc1()')
#endif

IF (PRESENT(spaceCompo)) THEN
  tsize = SIZE(spaceCompo)
  CALL Reallocate(spaceCompo0, tsize)
  spaceCompo0 = spaceCompo
ELSE
  CALL Reallocate(spaceCompo0, 1)
  spaceCompo0 = 1
END IF

IF (PRESENT(timeCompo)) THEN
  tsize = SIZE(timeCompo)
  CALL Reallocate(timeCompo0, tsize)
  timeCompo0 = timeCompo
ELSE
  CALL Reallocate(timeCompo0, 1)
  timeCompo0 = 1
END IF
ttime = SIZE(timeCompo0)

IF (PRESENT(ivar)) THEN
  ivar0 = ivar
ELSE
  ivar0 = 1
END IF

ALLOCATE (int_vec_list(ttime))
CALL Reallocate(localNode, SIZE(globalNode))

IF (ASSOCIATED(obj%domain)) THEN

  localNode = obj%domain%GetLocalNodeNumber(globalNode=globalNode)

ELSEIF (ALLOCATED(obj%domains)) THEN
  tsize = SIZE(obj%domains)
  IF (ivar0 .GT. tsize) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: ivar is greater than size of '//  &
      & ' AbstractNodeField_::obj%domains.')
    RETURN
  END IF
  localNode = obj%domains(ivar0)%ptr%GetLocalNodeNumber(globalNode)

ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%domain '//  &
    & ' obj%domains are not allocated.')
  RETURN
END IF

DO itime = 1, ttime
  int_vec_list(itime) = GetNodeLoc(obj=obj%dof, nodenum=localNode,  &
    & ivar=ivar0, spaceCompo=spaceCompo0, timeCompo=timeCompo0(itime))
END DO

ans = Get(obj=int_vec_list, dataType=1_I4B)

IF (ALLOCATED(int_vec_list)) THEN
  DO itime = 1, ttime
    CALL DEALLOCATE (int_vec_list(itime))
  END DO
  DEALLOCATE (int_vec_list)
END IF

IF (ALLOCATED(spaceCompo0)) DEALLOCATE (spaceCompo0)
IF (ALLOCATED(timeCompo0)) DEALLOCATE (timeCompo0)
IF (ALLOCATED(localNode)) DEALLOCATE (localNode)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] GetNodeLoc1()')
#endif
END PROCEDURE obj_GetNodeLoc1

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc2
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc2()"
INTEGER(I4B), ALLOCATABLE :: globalNode(:), timeCompo(:)
INTEGER(I4B) :: tPhysicalVars, spaceCompo(1), ivar0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] GetNodeLoc2()')
#endif

tPhysicalVars = obj%GetTotalPhysicalVars()
CALL Reallocate(timeCompo, tPhysicalVars)
timeCompo = obj%GetTimeCompo(tPhysicalVars)
spaceCompo(1) = dbc%GetDOFNo()
ivar0 = input(default=1_I4B, option=ivar)

CALL dbc%Get(nodeNum=globalNode)
ans = obj%GetNodeLoc(globalNode=globalNode, ivar=ivar,  &
  & spaceCompo=spaceCompo, timeCompo=arange(1_I4B, timeCompo(ivar0)))

IF (ALLOCATED(globalNode)) DEALLOCATE (globalNode)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] GetNodeLoc2()')
#endif
END PROCEDURE obj_GetNodeLoc2

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc3
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc3()"
INTEGER(I4B) :: ii, tsize
INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] GetNodeLoc3()')
#endif

tsize = SIZE(dbc)

DO ii = 1, tsize
  nptrs = obj%GetNodeLoc(dbc=dbc(ii)%ptr, ivar=ivar)
  CALL Append(ans, nptrs)
END DO

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] GetNodeLoc3()')
#endif

END PROCEDURE obj_GetNodeLoc3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
