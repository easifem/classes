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
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Initiate2
CHARACTER(*), PARAMETER :: myName = "anf_Initiate2"
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

END PROCEDURE anf_Initiate2

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Initiate3
CHARACTER(*), PARAMETER :: myName = "anf_Initiate3"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[IMPLEMENTATION ERROR] :: Initiate3 should be implemented by the'// &
  & ' child of AbstractNodeField_')
END PROCEDURE anf_Initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Deallocate
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
END PROCEDURE anf_Deallocate

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractNodeFieldInitiate
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldInitiate"

CALL AbstractFieldInitiate(obj=obj, param=param, prefix=prefix, dom=dom)

IF (obj%dof_tPhysicalVars .EQ. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_tPhysicalVars is 0')
END IF

IF (.NOT. ALLOCATED(obj%dof_spaceCompo)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_spaceCompo '// &
    & ' is NOT ALLOCATED')
END IF

IF (.NOT. ALLOCATED(obj%dof_timeCompo)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_timeCompo '// &
    & ' is NOT ALLOCATED')
END IF

IF (.NOT. ALLOCATED(obj%dof_tNodes)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_tNodes '// &
    & ' is NOT ALLOCATED')
END IF

IF (.NOT. ALLOCATED(obj%dof_names_char)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_names_char '// &
    & ' is NOT ALLOCATED')
END IF

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

END PROCEDURE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
