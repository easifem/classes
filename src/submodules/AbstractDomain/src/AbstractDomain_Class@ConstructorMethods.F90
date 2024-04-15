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
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(AbstractDomain_Class) ConstructorMethods
USE ReallocateUtility
USE CSRSparsity_Method
USE Kdtree2_Module, ONLY: Kdtree2_Destroy
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL obj%IMPORT(hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
! obj%showTime = .FALSE.
obj%isInitiated = .FALSE.
obj%engine = ''
obj%majorVersion = 0
obj%minorVersion = 0
obj%version = 0.0_DFP
obj%nsd = 0
obj%maxNptrs = 0
obj%minNptrs = 0
obj%tNodes = 0
obj%isNodeNumberSparse = .FALSE.
obj%maxElemNum = 0
obj%minElemNum = 0
obj%isElemNumberSparse = .FALSE.
obj%tEntitiesForNodes = 0
obj%tEntitiesForElements = 0
obj%tElements(0:3) = 0
obj%tEntities(0:3) = 0
CALL DEALLOCATE (obj%meshmap)

IF (ALLOCATED(obj%nodeCoord)) DEALLOCATE (obj%nodeCoord)

CALL obj%DeallocateKdtree()

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                           DeallocateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DeallocateKdtree
IF (ASSOCIATED(obj%kdtree)) THEN
  CALL Kdtree2_Destroy(obj%kdtree)
  obj%kdtree => NULL()
END IF

IF (ALLOCATED(obj%kdresult)) DEALLOCATE (obj%kdresult)
END PROCEDURE obj_DeallocateKdtree

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
