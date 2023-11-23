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

SUBMODULE(STVectorField_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     SetSTVectorFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTVectorFieldParam
TYPE(ParameterList_), POINTER :: sublist

CALL SetAbstractFieldParam( &
  & param=param, &
  & prefix=myprefix, &
  & name=name, &
  & engine=engine, &
  & fieldType=fieldType, &
  & comm=comm, &
  & local_n=local_n, &
  & global_n=global_n)

sublist => NULL()
sublist => param%NewSubList(key=myprefix)

CALL Set( &
  & obj=sublist, &
  & datatype=TypeIntI4B, &
  & prefix=myprefix, &
  & key="spaceCompo", &
  & VALUE=spaceCompo)

CALL Set( &
  & obj=sublist, &
  & datatype=TypeIntI4B, &
  & prefix=myprefix, &
  & key="timeCompo", &
  & VALUE=timeCompo)

sublist => NULL()

END PROCEDURE SetSTVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "stvField_checkEssentialParam"
CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)
IF (.NOT. param%IsPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'spaceCompo should be present in param.')
END IF
IF (.NOT. param%IsPresent(key=myprefix//"/timeCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'timeCompo should be present in param.')
END IF
END PROCEDURE stvField_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Initiate1
CHARACTER(*), PARAMETER :: myName = "stvField_Initiate1()"
TYPE(String) :: astr
INTEGER(I4B) :: nsd, tdof, ierr, spaceCompo, tNodes, timeCompo
TYPE(ParameterList_), POINTER :: sublist

! main
sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
END IF

CALL obj%CheckEssentialParam(sublist)
CALL obj%DEALLOCATE()

CALL GetValue(obj=sublist, prefix=myprefix, key="name", VALUE=astr)
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", VALUE=spaceCompo)
CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", VALUE=timeCompo)
tNodes = dom%GetTotalNodes()
tdof = tNodes * spaceCompo * timeCompo

CALL AbstractNodeFieldSetParam(obj=obj,  &
  & dof_tPhysicalVars=1_I4B,  &
  & dof_storageFMT=NODES_FMT,  &
  & dof_spaceCompo=[spaceCompo],  &
  & dof_timeCompo=[timeCompo],  &
  & dof_tNodes=[tNodes],  &
  & dof_names_char=[astr%slice(1, 1)],  &
  & tSize=tdof)

nsd = dom%GetNSD()

CALL AbstractNodeFieldInitiate( &
  & obj=obj,  &
  & param=param,  &
  & dom=dom,  &
  & prefix=myprefix)

astr = ""
sublist => NULL()
END PROCEDURE stvField_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Initiate1_old
CHARACTER(*), PARAMETER :: myName = "stvField_Initiate1"
INTEGER(I4B) :: ierr, storageFMT, tNodes(1), spaceCompo(1), &
  & timeCompo(1)
CHARACTER(:), ALLOCATABLE :: char_var
CHARACTER(1) :: names_char(1)

! main program
IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is already initiated')
END IF

CALL obj%CheckEssentialParam(param)

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

! timeCompo
ierr = param%get(key=myprefix//"/timeCompo", VALUE=obj%timeCompo)

! spaceCompo
ierr = param%get(key=myprefix//"/spaceCompo", VALUE=obj%spaceCompo)

timeCompo = obj%timeCompo
spaceCompo = obj%spaceCompo
storageFMT = FMT_NODES
obj%domain => dom
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  tNodes = 1
  obj%tSize = obj%domain%getTotalNodes() * obj%timeCompo * obj%spaceCompo
  IF (obj%local_n .EQ. 0) THEN
    obj%local_n = tNodes(1) * obj%timeCompo * obj%spaceCompo
  END IF
  IF (obj%global_n .EQ. 0) THEN
    obj%global_n = tNodes(1) * obj%timeCompo * obj%spaceCompo
  END IF
ELSE
  tNodes = obj%domain%getTotalNodes()
  obj%tSize = tNodes(1) * obj%timeCompo * obj%spaceCompo
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
END PROCEDURE stvField_Initiate1_old

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Initiate2
CALL AbstractNodeFieldInitiate2(&
  & obj=obj, &
  & obj2=obj2, &
  & copyFull=copyFull, &
  & copyStructure=copyStructure, &
  & usePointer=usePointer)
SELECT TYPE (obj2)
CLASS IS (STVectorField_)
  obj%spaceCompo = obj2%spaceCompo
  obj%timeCompo = obj2%timeCompo
END SELECT
END PROCEDURE stvField_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Deallocate
obj%spaceCompo = 0_I4B
obj%timeCompo = 0_I4B
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE stvField_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Final
CALL obj%DEALLOCATE()
END PROCEDURE stvField_Final

!----------------------------------------------------------------------------
!                                                                STVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Constructor1
CALL ans%initiate(param, dom)
END PROCEDURE stvField_Constructor1

!----------------------------------------------------------------------------
!                                                        STVectorField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, dom)
END PROCEDURE stvField_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
