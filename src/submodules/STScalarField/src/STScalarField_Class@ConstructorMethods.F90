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
USE FPL_Method, ONLY: GetValue, Set
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
USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                    SetSTScalarFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTScalarFieldParam
CHARACTER(*), PARAMETER :: myName = "SetSTScalarFieldParam()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SetAbstractFieldParam(param=param, prefix=myprefix, name=name, &
             engine=engine, fieldType=fieldType, comm=comm, local_n=local_n, &
                           global_n=global_n)

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)

#ifdef DEBUG_VER
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, "error occured in getting sublist")

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
           "sublist is not associated, some error occured in getting sublist")
#endif

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, key="timeCompo", &
         VALUE=timeCompo)

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetSTScalarFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
CHARACTER(1) :: names(1)
TYPE(String) :: astr
INTEGER(I4B) :: tdof, ierr, tNodes(1), timeCompo(1)
INTEGER(I4B), PARAMETER :: spaceCompo(1) = 1_I4B, tPhysicalVars = 1_I4B
TYPE(ParameterList_), POINTER :: sublist
LOGICAL(LGT) :: isok, istimefedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  "error occured in getting sublist(1)")

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
        "sublist is not associated, some error occured in getting sublist(1)")

CALL obj%CheckEssentialParam(sublist)
CALL obj%DEALLOCATE()

CALL GetValue(obj=sublist, prefix=myprefix, key="name", VALUE=astr)

istimefedof = PRESENT(timefedof)
IF (istimefedof) THEN
  obj%timeCompo = timefedof%GetTotalDOF()

ELSE
  isok = sublist%IsPresent(key=myprefix//"/timeCompo")
  CALL AssertError1(isok, myName, &
                    "timeCompo is not present in the parameter list")
  CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", &
                VALUE=obj%timeCompo)
END IF

tNodes(1) = fedof%GetTotalDOF()
tdof = tNodes(1) * obj%timeCompo
names(1) (:) = astr%slice(1, 1)
timeCompo(1) = obj%timeCompo

CALL AbstractNodeFieldSetParam(obj=obj, dof_tPhysicalVars=tPhysicalVars, &
                  dof_storageFMT=mystorageformat, dof_spaceCompo=spaceCompo, &
                               dof_timeCompo=timeCompo, dof_tNodes=tNodes, &
                               dof_names_char=names, tSize=tdof)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, fedof=fedof, &
                               timefedof=timefedof)

CALL Reallocate(obj%idofs, obj%timeCompo)
obj%idofs = Arange(1_I4B, obj%timeCompo)

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
INTEGER(I4B) :: tsize, ii
CALL AbstractNodeFieldInitiate2(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, usePointer=usePointer)
SELECT TYPE (obj2); CLASS IS (STScalarField_)
  obj%timeCompo = obj2%timeCompo

  tsize = SafeSize(obj2%idofs)
  CALL Reallocate(obj%idofs, tsize)
  DO ii = 1, tsize
    obj%idofs(ii) = obj2%idofs(ii)
  END DO
END SELECT
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%timeCompo = 0_I4B
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
!                                                              STScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ans%Initiate(param=param, fedof=fedof, timefedof=timefedof)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                      STScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%Initiate(param=param, fedof=fedof, timefedof=timefedof)
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
!                                                           SafeAlllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STScalarFieldSafeAllocate1
LOGICAL(LGT) :: isalloc
INTEGER(I4B) :: tsize

isalloc = ALLOCATED(obj)

IF (.NOT. isalloc) THEN
  ALLOCATE (obj(newsize))
  RETURN
END IF

tsize = SIZE(obj)

IF (tsize .LT. newsize) THEN
  CALL STScalarFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

END PROCEDURE obj_STScalarFieldSafeAllocate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
