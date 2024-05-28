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
USE GlobalData, ONLY: STORAGE_FMT => NODES_FMT
USE FPL_Method, ONLY: GetValue, Set
USE String_Class, ONLY: String
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldSetParam, &
                                   AbstractNodeFieldInitiate, &
                                   AbstractNodeFieldInitiate2, &
                                   AbstractNodeFieldDeallocate

USE AbstractField_Class, ONLY: AbstractFieldCheckEssentialParam, &
                               SetAbstractFieldParam

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                    SetSTScalarFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTScalarFieldParam
CHARACTER(*), PARAMETER :: myName = "SetSTScalarFieldParam()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr

CALL SetAbstractFieldParam(param=param, prefix=myprefix, name=name, &
             engine=engine, fieldType=fieldType, comm=comm, local_n=local_n, &
                           global_n=global_n)

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)

IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
  RETURN
END IF

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, key="timeCompo", &
         VALUE=timeCompo)

sublist => NULL()
END PROCEDURE SetSTScalarFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

IF (.NOT. param%IsPresent(key=myprefix//"/timeCompo")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                  '[INTERNAL ERROR] :: timeCompo should be present in param.')
  RETURN
END IF

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
CHARACTER(1) :: names(1)
TYPE(String) :: astr
INTEGER(I4B) :: tdof, ierr, tNodes(1)
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
  RETURN
END IF

CALL obj%CheckEssentialParam(sublist)
CALL obj%DEALLOCATE()

CALL GetValue(obj=sublist, prefix=myprefix, key="name", VALUE=astr)
CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", &
              VALUE=obj%timeCompo)

tNodes(1) = fedof%GetTotalDOF()
tdof = tNodes(1) * obj%timeCompo
names(1) (:) = astr%slice(1, 1)

!NOTE: STORAGE_FMT is defined at the top of this file
CALL AbstractNodeFieldSetParam(obj=obj, dof_tPhysicalVars=1_I4B, &
                         dof_storageFMT=STORAGE_FMT, dof_spaceCompo=[1_I4B], &
                           dof_timeCompo=[obj%timeCompo], dof_tNodes=tNodes, &
                               dof_names_char=names, tSize=tdof)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, fedof=fedof)

astr = ""
sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CALL AbstractNodeFieldInitiate2(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, usePointer=usePointer)
SELECT TYPE (obj2); CLASS IS (STScalarField_)
  obj%timeCompo = obj2%timeCompo
END SELECT
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%timeCompo = 0_I4B
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                              STScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                      STScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
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
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
