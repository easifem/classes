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
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       SetVectorFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetVectorFieldParam
CHARACTER(*), PARAMETER :: myName = "SetVectorFieldParam()"
INTEGER(I4B) :: ierr
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
ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
END IF

CALL Set( &
  & obj=sublist, &
  & datatype=TypeIntI4B, &
  & prefix=myprefix, &
  & key="spaceCompo", &
  & VALUE=spaceCompo)

sublist => NULL()

END PROCEDURE SetVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "vField_checkEssentialParam"
CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)
IF (.NOT. param%IsPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'spaceCompo should be present in param.')
END IF
END PROCEDURE vField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Initiate1
CHARACTER(*), PARAMETER :: myName = "vField_Initiate1()"
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
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo",  &
  & VALUE=obj%spaceCompo)

tNodes = dom%GetTotalNodes()
tdof = tNodes * obj%spaceCompo
names(1) (:) = astr%slice(1, 1)

CALL AbstractNodeFieldSetParam(obj=obj,  &
  & dof_tPhysicalVars=1_I4B,  &
  & dof_storageFMT=NODES_FMT,  &
  & dof_spaceCompo=[obj%spaceCompo],  &
  & dof_timeCompo=[1_I4B],  &
  & dof_tNodes=[tNodes],  &
  & dof_names_char=names,  &
  & tSize=tdof)

nsd = dom%GetNSD()

CALL AbstractNodeFieldInitiate( &
  & obj=obj,  &
  & param=param,  &
  & dom=dom,  &
  & prefix=myprefix)

astr = ""
sublist => NULL()
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
CALL ans%Initiate(param, dom)
END PROCEDURE vField_Constructor1

!----------------------------------------------------------------------------
!                                                        VectorField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Constructor_1
ALLOCATE (ans)
CALL ans%Initiate(param, dom)
END PROCEDURE vField_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
