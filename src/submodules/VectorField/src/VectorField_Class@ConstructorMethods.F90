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
USE FPL_Method, ONLY: Set, GetValue

USE String_Class, ONLY: String

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldSetParam, &
                                   AbstractNodeFieldInitiate, &
                                   AbstractNodeFieldInitiate2, &
                                   AbstractNodeFieldDeallocate

USE AbstractField_Class, ONLY: AbstractFieldCheckEssentialParam, &
                               SetAbstractFieldParam

USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE ArangeUtility, ONLY: Arange

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       SetVectorFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetVectorFieldParam
CHARACTER(*), PARAMETER :: myName = "SetVectorFieldParam()"
INTEGER(I4B) :: ierr
TYPE(ParameterList_), POINTER :: sublist

CALL SetAbstractFieldParam(param=param, prefix=myprefix, &
                           name=name, engine=engine, fieldType=fieldType, &
                           comm=comm, local_n=local_n, global_n=global_n)

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

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, &
         key="spaceCompo", VALUE=spaceCompo)

sublist => NULL()

END PROCEDURE SetVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_checkEssentialParam()"

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

IF (.NOT. param%IsPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: spaceCompo should be present in param.')
END IF
END PROCEDURE obj_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
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

! main
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
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", &
              VALUE=obj%spaceCompo)

tNodes(1) = fedof%GetTotalDOF()
tdof = tNodes(1) * obj%spaceCompo
names(1) (:) = astr%slice(1, 1)

CALL AbstractNodeFieldSetParam(obj=obj, dof_tPhysicalVars=1_I4B, &
            dof_storageFMT=mystorageformat, dof_spaceCompo=[obj%spaceCompo], &
                               dof_timeCompo=[1_I4B], dof_tNodes=tNodes, &
                               dof_names_char=names, tSize=tdof)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, fedof=fedof)

CALL Reallocate(obj%idofs, obj%spaceCompo)
obj%idofs = Arange(1_I4B, obj%spaceCompo)

astr = ""
sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
INTEGER(I4B) :: ii, tsize

CALL AbstractNodeFieldInitiate2(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (VectorField_)
  obj%spaceCompo = obj2%spaceCompo

  tsize = SafeSize(obj2%idofs)
  CALL Reallocate(obj%idofs, tsize)
  DO ii = 1, tsize
    obj%idofs(ii) = obj2%idofs(ii)
  END DO
END SELECT
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%spaceCompo = 0_I4B
IF (ALLOCATED(obj%idofs)) DEALLOCATE (obj%idofs)
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                VectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                        VectorField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                               Deallocate
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
