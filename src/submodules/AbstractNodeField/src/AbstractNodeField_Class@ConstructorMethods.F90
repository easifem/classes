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

SUBMODULE(AbstractNodeField_Class) ConstructorMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                               AbstractNodeFieldCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractNodeFieldCheckError
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldCheckError()"
INTEGER(I4B) :: ivar, tvar
LOGICAL(LGT) :: isNOTOK

isNOTOK = obj%dof_tPhysicalVars .EQ. 0_I4B
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_tPhysicalVars is 0')
  RETURN
END IF

isNOTOK = .NOT. ALLOCATED(obj%dof_spaceCompo)
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_spaceCompo '// &
    & ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_spaceCompo)
isNOTOK = tvar .NE. obj%dof_tPhysicalVars
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of dof_spaceCompo ('//tostring(tvar)//  &
    & ') is not same as dof_tPhysicalVars ('//  &
    & tostring(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

isNOTOK = .NOT. ALLOCATED(obj%dof_timeCompo)
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_timeCompo '// &
    & ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_timeCompo)
isNOTOK = tvar .NE. obj%dof_tPhysicalVars
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of dof_timeCompo ('//tostring(tvar)//  &
    & ') is not same as dof_tPhysicalVars ('//  &
    & tostring(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

IF (.NOT. ALLOCATED(obj%dof_tNodes)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_tNodes '// &
    & ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_tNodes)
isNOTOK = tvar .NE. obj%dof_tPhysicalVars
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of dof_tNodes ('//tostring(tvar)//  &
    & ') is not same as dof_tPhysicalVars ('//  &
    & tostring(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

IF (.NOT. ALLOCATED(obj%dof_names_char)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_names_char '// &
    & ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_names_char)
isNOTOK = tvar .NE. obj%dof_tPhysicalVars
IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of dof_names_char ('//tostring(tvar)//  &
    & ') is not same as dof_tPhysicalVars ('//  &
    & tostring(obj%dof_tPhysicalVars)//')')
  RETURN
END IF
END PROCEDURE AbstractNodeFieldCheckError

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldInitiate1()"
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] AbstractNodeFieldInitiate()')
#endif

prefix = obj%GetPrefix()

CALL AbstractFieldInitiate(obj=obj, param=param, dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling AbstractNodeFieldCheckError()')
#endif

CALL AbstractNodeFieldCheckError(obj)

CALL Initiate( &
  & obj=obj%dof, &
  & tNodes=obj%dof_tNodes, &
  & names=obj%dof_names_char, &
  & spaceCompo=obj%dof_spaceCompo, &
  & timeCompo=obj%dof_timeCompo, &
  & storageFMT=obj%dof_storageFMT)

CALL Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = SIZE(obj%realVec)

IF (obj%local_n .EQ. 0) THEN
  obj%local_n = obj%tSize
END IF

IF (obj%global_n .EQ. 0) THEN
  obj%global_n = obj%tSize
END IF

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] AbstractNodeFieldInitiate()')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2"
INTEGER(I4B) :: ii, tsize

CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate2( &
  & obj=obj, &
  & obj2=obj2, &
  & copyFull=copyFull, &
  & copyStructure=copyStructure, &
  & usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (AbstractNodeField_)
  obj%tSize = obj2%tSize
  obj%realVec = obj2%realVec
  obj%dof = obj2%dof
END SELECT
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                            obj_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldInitiate2()"
INTEGER(I4B) :: ivar, tvar
LOGICAL(LGT) :: isNOTOK
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] AbstractNodeFieldInitiate()')
#endif

prefix = obj%GetPrefix()

CALL AbstractFieldInitiate(obj=obj, param=param, dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling AbstractNodeFieldCheckError()')
#endif

CALL AbstractNodeFieldCheckError(obj)

CALL Initiate( &
  & obj=obj%dof, &
  & tNodes=obj%dof_tNodes, &
  & names=obj%dof_names_char, &
  & spaceCompo=obj%dof_spaceCompo, &
  & timeCompo=obj%dof_timeCompo, &
  & storageFMT=obj%dof_storageFMT)

CALL Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = SIZE(obj%realVec)

IF (obj%local_n .EQ. 0) THEN
  obj%local_n = obj%tSize
END IF

IF (obj%global_n .EQ. 0) THEN
  obj%global_n = obj%tSize
END IF

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] AbstractNodeFieldInitiate()')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL AbstractFieldDeallocate(obj)
obj%dof_tPhysicalVars = 0
obj%dof_storageFMT = NODES_FMT
IF (ALLOCATED(obj%dof_spaceCompo)) DEALLOCATE (obj%dof_spaceCompo)
IF (ALLOCATED(obj%dof_timeCompo)) DEALLOCATE (obj%dof_timeCompo)
IF (ALLOCATED(obj%dof_tNodes)) DEALLOCATE (obj%dof_tNodes)
IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
obj%tSize = 0
CALL DEALLOCATE (obj%realVec)
CALL DEALLOCATE (obj%dof)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
