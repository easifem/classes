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

SUBMODULE(ScalarFieldLis_Class) ConstructorMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldDeallocate
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldInitiate
USE ScalarField_Class, ONLY: ScalarFieldInitiate

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldInitiate( &
  obj=obj, obj2=obj2, copyFull=copyFull, copyStructure=copyStructure, &
  usePointer=usePointer)

CALL lis_vector_create(obj%comm, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_vector_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_vector_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ScalarFieldInitiate( &
  obj=obj, name=name, engine=engine, fieldType=fieldType, &
  storageFMT=storageFMT, comm=comm, local_n=local_n, global_n=global_n, &
  spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, &
  isSpaceCompoScalar=isSpaceCompoScalar, timeCompo=timeCompo, &
  isTimeCompo=isTimeCompo, isTimeCompoScalar=isTimeCompoScalar, &
  tPhysicalVarNames=tPhysicalVarNames, physicalVarNames=physicalVarNames, &
  isPhysicalVarNames=isPhysicalVarNames, &
  isPhysicalVarNamesScalar=isPhysicalVarNamesScalar, tNodes=tNodes, &
  isTNodes=isTNodes, isTNodesScalar=isTNodesScalar, tSize=tSize, &
  fedof=fedof, geofedof=geofedof, timefedof=timefedof)

CALL lis_vector_create(obj%comm, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_vector_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_vector_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

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
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_vector_destroy(obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL AbstractNodeFieldDeallocate(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
