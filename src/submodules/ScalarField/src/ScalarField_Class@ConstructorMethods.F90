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

SUBMODULE(ScalarField_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            SetScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE SetScalarFieldParam
CALL SetAbstractFieldParam( &
  & param=param, &
  & prefix=myprefix,  &
  & name=name,  &
  & engine=engine,  &
  & fieldType=fieldType, &
  & comm=comm, &
  & local_n=local_n, &
  & global_n=global_n)
END PROCEDURE SetScalarFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_CheckEssentialParam
CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)
END PROCEDURE sField_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Initiate1
CHARACTER(*), PARAMETER :: myName = "sField_Initiate1()"
TYPE(String) :: astr
INTEGER(I4B) :: nsd, tdof, ierr, tNodes
TYPE(ParameterList_), POINTER :: sublist

! main
sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B ) THEN
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
tNodes = dom%GetTotalNodes()
tdof = tNodes

CALL AbstractNodeFieldSetParam(obj=obj,  &
  & dof_tPhysicalVars=1_I4B,  &
  & dof_storageFMT=NODES_FMT,  &
  & dof_spaceCompo=[1_I4B],  &
  & dof_timeCompo=[1_I4B],  &
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
END PROCEDURE sField_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Initiate_old
CHARACTER(*), PARAMETER :: myName = "sField_Initiate1"
INTEGER(I4B) :: ierr, storageFMT
INTEGER(I4B) :: tNodes(1), spaceCompo(1), timeCompo(1)
CHARACTER(:), ALLOCATABLE :: char_var
CHARACTER(1) :: names_char(1)

! main program
CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(param)

! engine
ALLOCATE (CHARACTER( &
  & param%DataSizeInBytes(key=myprefix//"/engine")) :: char_var)
ierr = param%Get(key=myprefix//"/engine", VALUE=char_var)
obj%engine = char_var
DEALLOCATE (char_var)

! name
ALLOCATE (CHARACTER( &
  & param%DataSizeInBytes(key=myprefix//"/name")) :: char_var)
ierr = param%Get(key=myprefix//"/name", VALUE=char_var)
obj%name = char_var
names_char(1) (1:1) = char_var(1:1)
DEALLOCATE (char_var)

! fieldType
IF (param%isPresent(key=myprefix//"/fieldType")) THEN
  ierr = param%Get(key=myprefix//"/fieldType", VALUE=obj%fieldType)
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF

! comm
ierr = param%Get(key=myprefix//"/comm", VALUE=obj%comm)
ierr = param%Get(key=myprefix//"/global_n", VALUE=obj%global_n)
ierr = param%Get(key=myprefix//"/local_n", VALUE=obj%local_n)

spaceCompo = [1]
timeCompo = [1]
storageFMT = FMT_NODES
obj%domain => dom
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  tNodes = 1
  obj%tSize = obj%domain%GetTotalNodes()
  IF (obj%local_n .EQ. 0) THEN
    obj%local_n = tNodes(1)
  END IF
  IF (obj%global_n .EQ. 0) THEN
    obj%global_n = tNodes(1)
  END IF
ELSE
  tNodes = obj%domain%GetTotalNodes()
  obj%tSize = tNodes(1)
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

END PROCEDURE sField_Initiate_old

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Final
CALL obj%DEALLOCATE()
END PROCEDURE sField_Final

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Deallocate
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE sField_Deallocate

!----------------------------------------------------------------------------
!                                                                ScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Constructor1
CALL ans%initiate(param, dom)
END PROCEDURE sField_Constructor1

!----------------------------------------------------------------------------
!                                                         ScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, dom)
END PROCEDURE sField_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
