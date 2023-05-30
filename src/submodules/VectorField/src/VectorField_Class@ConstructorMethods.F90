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

SUBMODULE(VectorField_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       setVectorFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setVectorFieldParam
INTEGER(I4B) :: ierr
ierr = param%set(key=myprefix//"/name", VALUE=TRIM(name))
ierr = param%set(key=myprefix//"/engine", VALUE=TRIM(engine))
ierr = param%set(key=myprefix//"/spaceCompo", VALUE=spaceCompo)
ierr = param%set(key=myprefix//"/fieldType",  &
  & VALUE=INPUT(default=FIELD_TYPE_NORMAL, option=fieldType))
END PROCEDURE setVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "vField_checkEssentialParam"
IF (.NOT. param%isPresent(key=myprefix//"/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/name should be present in param')
END IF
IF (.NOT. param%isPresent(key=myprefix//"/engine")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/engine should be present in param')
END IF
IF (.NOT. param%isPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/spaceCompo should be present in param')
END IF
END PROCEDURE vField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Initiate1
CHARACTER(*), PARAMETER :: myName = "vField_Initiate"
INTEGER(I4B) :: ierr, storageFMT, tNodes(1), timeCompo(1), &
  & spaceCompo(1)
CHARACTER(:), ALLOCATABLE :: char_var
CHARACTER(1) :: names_char(1)

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'VectorField_::obj is already initiated')
END IF

CALL obj%checkEssentialParam(param)

! engine
ALLOCATE (CHARACTER( &
  & param%DataSizeInBytes(key=myprefix//"/engine")) :: char_var)
ierr = param%get(key=myprefix//"/engine", VALUE=char_var)
obj%engine = char_var
DEALLOCATE (char_var)

! name
ALLOCATE (CHARACTER( &
  & param%DataSizeInBytes(key=myprefix//"/name")) :: char_var)
ierr = param%get(key=myprefix//"/name", VALUE=char_var)
obj%name = char_var
names_char(1) (1:1) = char_var(1:1)
DEALLOCATE (char_var)

! spaceCompo
ierr = param%get(key=myprefix//"/spaceCompo", VALUE=obj%spaceCompo)

! fieldType
IF (param%isPresent(key=myprefix//"/fieldType")) THEN
  ierr = param%get(key=myprefix//"/fieldType", VALUE=obj%fieldType)
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF

! comm
ierr = param%get(key=myprefix//"/comm", VALUE=obj%comm)
ierr = param%get(key=myprefix//"/global_n", VALUE=obj%global_n)
ierr = param%get(key=myprefix//"/local_n", VALUE=obj%local_n)

spaceCompo = obj%spaceCompo
timeCompo = 1
storageFMT = FMT_NODES
obj%domain => dom
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  tNodes = 1
  obj%tSize = obj%domain%getTotalNodes() * obj%spaceCompo
  IF (obj%local_n .EQ. 0) THEN
    obj%local_n = obj%spaceCompo
  END IF
  IF (obj%global_n .EQ. 0) THEN
    obj%global_n = obj%spaceCompo
  END IF
ELSE
  tNodes = obj%domain%getTotalNodes()
  obj%tSize = tNodes(1) * obj%spaceCompo
  IF (obj%local_n .EQ. 0) THEN
    obj%local_n = obj%tSize
  END IF
  IF (obj%global_n .EQ. 0) THEN
    obj%global_n = obj%tSize
  END IF
END IF

CALL Initiate( &
  & obj=obj%dof, &
  & tNodes=tNodes, &
  & names=names_char, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo, &
  & storageFMT=storageFMT)

CALL Initiate(obj%realVec, obj%dof)

obj%isInitiated = .TRUE.

IF (ALLOCATED(char_var)) DEALLOCATE (char_var)
END PROCEDURE vField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Initiate2
CALL AbstractNodeFieldInitiate2(&
  & obj=obj, &
  & obj2=obj2, &
  & copyFull=copyFull, &
  & copyStructure=copyStructure, &
  & usePointer=usePointer)
SELECT TYPE (obj2)
CLASS IS (VectorField_)
  obj%spaceCompo = obj2%spaceCompo
END SELECT
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
CALL obj%DEALLOCATE()
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

END SUBMODULE ConstructorMethods
