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

SUBMODULE(BlockNodeField_Class) ConstructorMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldInitiate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "BlockNodeField_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%IsInitiated()) THEN
  CALL obj%DEALLOCATE()
END IF

CALL AbstractNodeFieldInitiate(obj=obj, obj2=obj2, copyFull=copyFull, &
                               copyStructure=copyStructure, &
                               usePointer=usePointer)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%IsInitiated()) THEN
  CALL obj%DEALLOCATE()
END IF

CALL AbstractNodeFieldInitiate( &
  obj=obj, name=name, engine=engine, storageFMT=storageFMT, &
  spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, &
  isSpaceCompoScalar=isSpaceCompoScalar, timeCompo=timeCompo, &
  isTimeCompo=isTimeCompo, isTimeCompoScalar=isTimeCompoScalar, &
  tPhysicalVarNames=tPhysicalVarNames, physicalVarNames=physicalVarNames, &
  isPhysicalVarNames=isPhysicalVarNames, &
  isPhysicalVarNamesScalar=isPhysicalVarNamesScalar, tNodes=tNodes, &
  isTNodes=isTNodes, isTNodesScalar=isTNodesScalar, &
  tSize=tSize, fedof=fedof, geofedof=geofedof, fieldType=fieldType, &
  comm=comm, local_n=local_n, global_n=global_n, timefedof=timefedof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

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
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
