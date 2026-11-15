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

SUBMODULE(STScalarFieldLis_Class) ConstructorMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#include "lisf.h"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STScalarFieldLis_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isLis
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%STScalarField_%Initiate1(obj2=obj2, copyFull=copyFull, &
                                  copyStructure=copyStructure, &
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

! Copy the values into the new LIS vector.
! A LIS obj2 keeps its values in its LIS vector (its realVec is not
! up to date), other node fields keep them in realVec, which has been
! copied into obj%realVec above. obj%engine is the engine of obj2.
isLis = obj%engine%INDEX("LIS") .EQ. math%one_i

IF (isLis) THEN
  CALL lis_vector_copy(obj2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

ELSE
  ! the engine of obj2 has been copied, obj keeps its values in LIS
  obj%engine = "LIS_OMP"
  realvec => AbstractNodeFieldGetPointer(obj)
  IF (ASSOCIATED(realvec)) THEN
    CALL lis_vector_scatter(realvec, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
    CALL CHKERR(ierr)
#endif

  END IF
  realvec => NULL()
END IF

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
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%STScalarField_%Initiate( &
  name=name, engine=engine, fieldType=fieldType, &
  storageFMT=storageFMT, comm=comm, local_n=local_n, global_n=global_n, &
  spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, &
  isSpaceCompoScalar=isSpaceCompoScalar, timeCompo=timeCompo, &
  isTimeCompo=isTimeCompo, isTimeCompoScalar=isTimeCompoScalar, &
  tPhysicalVarNames=tPhysicalVarNames, physicalVarNames=physicalVarNames, &
  isPhysicalVarNames=isPhysicalVarNames, &
  isPhysicalVarNamesScalar=isPhysicalVarNamesScalar, tNodes=tNodes, &
  isTNodes=isTNodes, isTNodesScalar=isTNodesScalar, tSize=tSize, &
  fedof=fedof, geofedof=geofedof, timefedof=timefedof)

! the values are stored in the LIS vector, whatever engine was given
obj%engine = "LIS_OMP"

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

CALL obj%STScalarField_%DEALLOCATE()

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
