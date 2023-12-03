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

SUBMODULE(STScalarField_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    SetSTScalarFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTScalarFieldParam
CHARACTER(*), PARAMETER :: myName = "SetSTScalarFieldParam()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr

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
ierr = param%GetSubList(key=myprefix, sublist=sublist)

IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
  RETURN
END IF

CALL Set( &
  & obj=sublist, &
  & datatype=TypeIntI4B, &
  & prefix=myprefix, &
  & key="timeCompo", &
  & VALUE=timeCompo)

sublist => NULL()
END PROCEDURE SetSTScalarFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "stsField_CheckEssentialParam()"
CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)
IF (.NOT. param%IsPresent(key=myprefix//"/timeCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: timeCompo should be present in param.')
END IF
END PROCEDURE stsField_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Initiate1
CHARACTER(*), PARAMETER :: myName = "stsField_Initiate1()"
CHARACTER(1) :: names(1)
TYPE(String) :: astr
INTEGER(I4B) :: nsd, tdof, ierr, tNodes
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
CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo",  &
  & VALUE=obj%timeCompo)
tNodes = dom%GetTotalNodes()
tdof = tNodes * obj%timeCompo
names(1) (:) = astr%slice(1, 1)

CALL AbstractNodeFieldSetParam(obj=obj,  &
  & dof_tPhysicalVars=1_I4B,  &
  & dof_storageFMT=NODES_FMT,  &
  & dof_spaceCompo=[1_I4B],  &
  & dof_timeCompo=[obj%timeCompo],  &
  & dof_tNodes=[tNodes],  &
  & dof_names_char=names,  &
  & tSize=tdof)

nsd = dom%GetNSD()

CALL AbstractNodeFieldInitiate(obj=obj, param=param, dom=dom)

astr = ""
sublist => NULL()
END PROCEDURE stsField_Initiate1

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Initiate2
CALL AbstractNodeFieldInitiate2(&
  & obj=obj, &
  & obj2=obj2, &
  & copyFull=copyFull, &
  & copyStructure=copyStructure, &
  & usePointer=usePointer)
SELECT TYPE (obj2)
CLASS IS (STScalarField_)
  obj%timeCompo = obj2%timeCompo
END SELECT
END PROCEDURE stsField_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Deallocate
obj%timeCompo = 0_I4B
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE stsField_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Final
CALL obj%DEALLOCATE()
END PROCEDURE stsField_Final

!----------------------------------------------------------------------------
!                                                                STScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Constructor1
CALL ans%initiate(param, dom)
END PROCEDURE stsField_Constructor1

!----------------------------------------------------------------------------
!                                                        STScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, dom)
END PROCEDURE stsField_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
