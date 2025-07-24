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
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: STORAGE_FMT => NODES_FMT
USE FPL_Method, ONLY: FPL_GetValue => GetValue
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldSetParam, &
                                   AbstractNodeFieldInitiate, &
                                   AbstractNodeFieldDeallocate
USE AbstractField_Class, ONLY: SetAbstractFieldParam, &
                               AbstractFieldCheckEssentialParam
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            SetScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE SetScalarFieldParam
CALL SetAbstractFieldParam(param=param, prefix=myprefix, name=name, &
                           engine=engine, fieldType=fieldType, &
                           comm=comm, local_n=local_n, global_n=global_n)
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
INTEGER(I4B) :: tdof, ierr, tNodes
TYPE(ParameterList_), POINTER :: sublist
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)

#ifdef DEBUG_VER
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(1)')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(2)')
#endif

CALL obj%CheckEssentialParam(sublist)
CALL obj%DEALLOCATE()
CALL FPL_GetValue(obj=sublist, prefix=myprefix, key="name", VALUE=astr)

tNodes = fedof%GetTotalDOF()
tdof = tNodes
names(1) (:) = astr%slice(1, 1)

!note: STORAGE_FMT is defined at the top of this file
CALL AbstractNodeFieldSetParam(obj=obj, &
                               dof_tPhysicalVars=1_I4B, &
                               dof_storageFMT=STORAGE_FMT, &
                               dof_spaceCompo=[1_I4B], &
                               dof_timeCompo=[1_I4B], &
                               dof_tNodes=[tNodes], &
                               dof_names_char=names, &
                               tSize=tdof)

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

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
#endif

CHARACTER(1) :: dof_names(1)
INTEGER(I4B) :: dof_tNodes(1), dof_tsize, dof_spaceCompo(1), &
                dof_timeCompo(1), dof_tPhysicalVarNames

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
dof_names(1) = name(1:1)
dof_tNodes(1) = fedof%GetTotalDOF()
dof_tsize = dof_tNodes(1)
dof_spaceCompo(1) = 1_I4B
dof_timeCompo(1) = 1_I4B
dof_tPhysicalVarNames = 1_I4B

CALL AbstractNodeFieldInitiate(obj=obj, name=name, engine=engine, &
                               fieldType=fieldType, comm=comm, &
                               local_n=local_n, global_n=global_n, &
                               fedof=fedof, timefedof=timefedof, &
                               storageFMT=STORAGE_FMT, &
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
CALL ans%Initiate(param, fedof)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                         ScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param, fedof)
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
!                                                           SafeAlllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ScalarFieldSafeAllocate1
LOGICAL(LGT) :: isalloc
INTEGER(I4B) :: tsize

isalloc = ALLOCATED(obj)

IF (.NOT. isalloc) THEN
  ALLOCATE (obj(newsize))
  RETURN
END IF

tsize = SIZE(obj)

IF (tsize .LT. newsize) THEN
  CALL ScalarFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

END PROCEDURE obj_ScalarFieldSafeAllocate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
