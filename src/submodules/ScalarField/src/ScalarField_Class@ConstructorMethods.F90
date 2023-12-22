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

MODULE PROCEDURE obj_CheckEssentialParam
CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
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
tNodes = dom%GetTotalNodes()
tdof = tNodes
names(1) (:) = astr%slice(1, 1)

CALL AbstractNodeFieldSetParam(obj=obj,  &
  & dof_tPhysicalVars=1_I4B,  &
  & dof_storageFMT=NODES_FMT,  &
  & dof_spaceCompo=[1_I4B],  &
  & dof_timeCompo=[1_I4B],  &
  & dof_tNodes=[tNodes],  &
  & dof_names_char=names,  &
  & tSize=tdof)

nsd = dom%GetNSD()

CALL AbstractNodeFieldInitiate( &
  & obj=obj,  &
  & param=param,  &
  & dom=dom)

astr = ""
sublist => NULL()
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                ScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ans%initiate(param, dom)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                         ScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, dom)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_ptr_vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_ptr_vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
