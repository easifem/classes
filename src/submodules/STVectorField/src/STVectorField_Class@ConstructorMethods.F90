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
!                                                     SetSTVectorFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTVectorFieldParam
CHARACTER(*), PARAMETER :: myName = "SetSTVectorFieldParam()"
INTEGER(I4B) :: ierr
TYPE(ParameterList_), POINTER :: sublist

CALL SetAbstractFieldParam(param=param, prefix=myprefix, name=name, &
             engine=engine, fieldType=fieldType, comm=comm, local_n=local_n, &
                           global_n=global_n)

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
END IF

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, key="spaceCompo", &
         VALUE=spaceCompo)

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, key="timeCompo", &
         VALUE=timeCompo)

sublist => NULL()

END PROCEDURE SetSTVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_checkEssentialParam()"

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

IF (.NOT. param%IsPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: spaceCompo should be present in param.')
END IF

IF (.NOT. param%IsPresent(key=myprefix//"/timeCompo")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                  '[INTERNAL ERROR] :: timeCompo should be present in param.')
END IF
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
CHARACTER(1) :: names(1)
TYPE(String) :: astr
INTEGER(I4B) :: tdof, ierr, tNodes
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
CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", &
              VALUE=obj%timeCompo)

tNodes = fedof%GetTotalDOF()
tdof = tNodes * obj%spaceCompo * obj%timeCompo
names(1) (:) = astr%slice(1, 1)

CALL AbstractNodeFieldSetParam(obj=obj, dof_tPhysicalVars=1_I4B, &
            dof_storageFMT=mystorageformat, dof_spaceCompo=[obj%spaceCompo], &
                         dof_timeCompo=[obj%timeCompo], dof_tNodes=[tNodes], &
                               dof_names_char=names, tSize=tdof)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, fedof=fedof)

CALL Reallocate(obj%idofs, obj%spaceCompo * obj%timeCompo)
obj%idofs = Arange(1_I4B, obj%spaceCompo * obj%timeCompo)

CALL Reallocate(obj%space_idofs, obj%spaceCompo)
obj%space_idofs = Arange(1_I4B, obj%spaceCompo)

CALL Reallocate(obj%time_idofs, obj%timeCompo)
obj%time_idofs = Arange(1_I4B, obj%timeCompo)

astr = ""
sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
INTEGER(I4B) :: ii, tsize

CALL AbstractNodeFieldInitiate2(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (STVectorField_)
  obj%spaceCompo = obj2%spaceCompo
  obj%timeCompo = obj2%timeCompo

  tsize = SafeSize(obj2%idofs)
  CALL Reallocate(obj%idofs, tsize)
  DO ii = 1, tsize
    obj%idofs(ii) = obj2%idofs(ii)
  END DO

  tsize = SafeSize(obj2%space_idofs)
  CALL Reallocate(obj%space_idofs, tsize)
  DO ii = 1, tsize
    obj%space_idofs(ii) = obj2%space_idofs(ii)
  END DO

  tsize = SafeSize(obj2%time_idofs)
  CALL Reallocate(obj%time_idofs, tsize)
  DO ii = 1, tsize
    obj%time_idofs(ii) = obj2%time_idofs(ii)
  END DO
END SELECT
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%spaceCompo = 0_I4B
obj%timeCompo = 0_I4B
IF (ALLOCATED(obj%idofs)) DEALLOCATE (obj%idofs)
IF (ALLOCATED(obj%space_idofs)) DEALLOCATE (obj%space_idofs)
IF (ALLOCATED(obj%time_idofs)) DEALLOCATE (obj%time_idofs)
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                              STVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                      STVectorField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                           Deallocate
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
!                                                   VectorFieldSafeAllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STVectorFieldSafeAllocate1
LOGICAL(LGT) :: isalloc
INTEGER(I4B) :: tsize

isalloc = ALLOCATED(obj)

IF (.NOT. isalloc) THEN
  ALLOCATE (obj(newsize))
  RETURN
END IF

tsize = SIZE(obj)

IF (tsize .LT. newsize) THEN
  CALL STVectorFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

END PROCEDURE obj_STVectorFieldSafeAllocate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
