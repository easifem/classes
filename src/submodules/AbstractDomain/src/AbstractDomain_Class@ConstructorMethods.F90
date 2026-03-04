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

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-16
! summary: This submodule contains methods for domain object

SUBMODULE(AbstractDomain_Class) ConstructorMethods
USE ReallocateUtility, ONLY: Reallocate
USE Kdtree2_Module, ONLY: Kdtree2_Destroy
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractDomain_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL obj%IMPORT(hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! obj%showTime = .FALSE.
obj%isInit = math%no
obj%engine = ''
obj%majorVersion = 0
obj%minorVersion = 0
obj%version = math%zero
obj%nsd = 0
obj%maxNptrs = 0
obj%minNptrs = 0
obj%tNodes = 0
obj%isNodeNumberSparse = math%no
obj%maxElemNum = 0
obj%minElemNum = 0
obj%isElemNumberSparse = math%no
obj%tEntitiesForNodes = 0
obj%tEntitiesForElements = 0
obj%tElements(0:3) = 0
obj%tEntities(0:3) = 0
IF (ALLOCATED(obj%nodeCoord)) DEALLOCATE (obj%nodeCoord)
CALL obj%DeallocateKdtree()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                           DeallocateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DeallocateKdtree
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DeallocateKdtree()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (ASSOCIATED(obj%kdtree)) THEN
  CALL Kdtree2_Destroy(obj%kdtree)
  obj%kdtree => NULL()
END IF

IF (ALLOCATED(obj%kdresult)) DEALLOCATE (obj%kdresult)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_DeallocateKdtree

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
