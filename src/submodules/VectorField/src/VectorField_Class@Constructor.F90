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

SUBMODULE(VectorField_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_addSurrogate
CALL e%addSurrogate(UserObj)
END PROCEDURE vField_addSurrogate

!----------------------------------------------------------------------------
!                                                       setVectorFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setVectorFieldParam
INTEGER(I4B) :: ierr
ierr = param%set(key="VectorField/name", value=trim(name))
ierr = param%set(key="VectorField/spaceCompo", value=spaceCompo)
ierr = param%set(key="VectorField/fieldType",  &
  & value=INPUT(default=FIELD_TYPE_NORMAL, option=fieldType))
END PROCEDURE setVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_checkEssentialParam
CHARACTER(LEN=*), PARAMETER :: myName = "vField_checkEssentialParam"
IF (.NOT. param%isPresent(key="VectorField/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'VectorField/name should be present in param')
END IF
IF (.NOT. param%isPresent(key="VectorField/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'VectorField/spaceCompo should be present in param')
END IF
END PROCEDURE vField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Initiate1
CHARACTER(LEN=*), PARAMETER :: myName = "vField_Initiate"
INTEGER(I4B) :: ierr, storageFMT, tNodes(1), timeCompo(1), &
  & spaceCompo(1)
CHARACTER(LEN=:), ALLOCATABLE :: char_var
CHARACTER(LEN=1) :: names_char(1)

!> main program
IF (obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector field object is already initiated')
CALL obj%checkEssentialParam(param)
ALLOCATE (CHARACTER(LEN= &
  & param%DataSizeInBytes(key="VectorField/name")) :: char_var)
ierr = param%get(key="VectorField/name", value=char_var)
obj%name = char_var
names_char(1) (1:1) = char_var(1:1)
ierr = param%get(key="VectorField/spaceCompo", value=obj%spaceCompo)
IF (param%isPresent(key="VectorField/fieldType")) THEN
  ierr = param%get(key="VectorField/fieldType", value=obj%fieldType)
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF
obj%engine = "NATIVE_SERIAL"
spaceCompo = obj%spaceCompo
timeCompo = 1
storageFMT = FMT_NODES
obj%domain => dom
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  tNodes = 1
  obj%tSize = obj%domain%getTotalNodes() * obj%spaceCompo
ELSE
  tNodes = obj%domain%getTotalNodes()
  obj%tSize = tNodes(1) * obj%spaceCompo
END IF
CALL Initiate(obj=obj%dof, tNodes=tNodes, names=names_char, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT)
CALL Initiate(obj%realVec, obj%dof)
obj%isInitiated = .TRUE.
IF (ALLOCATED(char_var)) DEALLOCATE (char_var)
END PROCEDURE vField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Initiate2
CHARACTER(LEN=*), PARAMETER :: myName = "vField_Initiate2"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This method has not been implemented so far')
END PROCEDURE vField_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Deallocate
obj%spaceCompo = 0_I4B
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE vField_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Final
CALL obj%Deallocate()
END PROCEDURE vField_Final

!----------------------------------------------------------------------------
!                                                                VectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Constructor1
CALL ans%initiate(param, dom)
END PROCEDURE vField_Constructor1

!----------------------------------------------------------------------------
!                                                        VectorField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, dom)
END PROCEDURE vField_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor
