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
USE Display_Method, ONLY: ToString
USE FPL_Method, ONLY: Set, GetValue
USE String_Class, ONLY: String
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldSetParam, &
                                   AbstractNodeFieldInitiate, &
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
LOGICAL(LGT) :: isok
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SetAbstractFieldParam(param=param, prefix=myprefix, &
                           name=name, engine=engine, fieldType=fieldType, &
                           comm=comm, local_n=local_n, global_n=global_n)

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(1)')

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(2)')

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, &
         key="spaceCompo", VALUE=spaceCompo)

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetVectorFieldParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_checkEssentialParam()"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

isok = param%IsPresent(key=myprefix//"/spaceCompo")
CALL AssertError1(isok, myName, &
                  'spaceCompo should be present in param.')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

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
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main
sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
isok = ierr .EQ. 0
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(1)')

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(2)')

CALL obj%CheckEssentialParam(sublist)
CALL obj%DEALLOCATE()

CALL GetValue(obj=sublist, prefix=myprefix, key="name", VALUE=astr)
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", &
              VALUE=obj%spaceCompo)

tNodes(1) = fedof%GetTotalDOF()
tdof = tNodes(1) * obj%spaceCompo
names(1) (:) = astr%slice(1, 1)

CALL AbstractNodeFieldSetParam(obj=obj, dof_tPhysicalVars=1_I4B, &
                               dof_storageFMT=MYSTORAGEFORMAT, &
                               dof_spaceCompo=[obj%spaceCompo], &
                               dof_timeCompo=[1_I4B], dof_tNodes=tNodes, &
                               dof_names_char=names, tSize=tdof)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, fedof=fedof, &
                               timefedof=timefedof)

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

CALL AbstractNodeFieldInitiate(obj=obj, obj2=obj2, copyFull=copyFull, &
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
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
LOGICAL(LGT) :: isok
#endif

CHARACTER(1) :: dof_names(1)
INTEGER(I4B) :: dof_tNodes(1), dof_tsize, dof_spaceCompo(1), &
                dof_timeCompo(1), dof_tPhysicalVarNames

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(spaceCompo)
CALL AssertError1(isok, myName, "spaceCompo should be present.")

#endif

CALL obj%DEALLOCATE()
dof_names(1) = name(1:1)
dof_tNodes(1) = fedof%GetTotalDOF()
dof_tsize = dof_tNodes(1)
dof_spaceCompo(1) = spaceCompo(1)
dof_timeCompo(1) = 1_I4B
dof_tPhysicalVarNames = 1_I4B

CALL AbstractNodeFieldInitiate(obj=obj, name=name, engine=engine, &
                               fieldType=fieldType, comm=comm, &
                               local_n=local_n, global_n=global_n, &
                               fedof=fedof, timefedof=timefedof, &
                               storageFMT=MYSTORAGEFORMAT, &
                               spaceCompo=dof_spaceCompo, &
                               isSpaceCompo=.TRUE., &
                               isSpaceCompoScalar=.TRUE., &
                               timeCompo=dof_timeCompo, &
                               isTimeCompo=.TRUE., &
                               isTimeCompoScalar=.TRUE., &
                               tPhysicalVarNames=dof_tPhysicalVarNames, &
                               physicalVarNames=dof_names, &
                               isPhysicalVarNames=.TRUE., &
                               isPhysicalVarNamesScalar=.TRUE., &
                               tSize=dof_tsize, tNodes=dof_tNodes, &
                               isTNodes=.TRUE., isTNodesScalar=.TRUE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%spaceCompo = 0_I4B
IF (ALLOCATED(obj%idofs)) DEALLOCATE (obj%idofs)
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                       Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                 VectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                         VectorField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%Initiate(param=param, fedof=fedof)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                                  Deallocate
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
!                                                               SafeAlllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_VectorFieldSafeAllocate1
LOGICAL(LGT) :: isalloc
INTEGER(I4B) :: tsize

isalloc = ALLOCATED(obj)

IF (.NOT. isalloc) THEN
  ALLOCATE (obj(newsize))
  RETURN
END IF

tsize = SIZE(obj)

IF (tsize .LT. newsize) THEN
  CALL VectorFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

END PROCEDURE obj_VectorFieldSafeAllocate1

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
