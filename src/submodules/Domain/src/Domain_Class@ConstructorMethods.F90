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

SUBMODULE(Domain_Class) ConstructorMethods
USE ReallocateUtility
USE CSRSparsity_Method
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Initiate
CHARACTER(*), PARAMETER :: myName = "Domain_Initiate()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

! Exception related to Mesh_ data type wil be printed in the
! domain only

CALL obj%IMPORT(hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE Domain_Initiate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacetData_Initiate
CALL Reallocate(obj%masterCellNumber, n)
CALL Reallocate(obj%slaveCellNumber, n)
CALL Reallocate(obj%masterLocalFacetID, n)
CALL Reallocate(obj%slaveLocalFacetID, n)
END PROCEDURE MeshFacetData_Initiate

!----------------------------------------------------------------------------
!                                                                isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacetData_isInitiated
IF (ALLOCATED(obj%masterCellNumber)) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE MeshFacetData_isInitiated

!----------------------------------------------------------------------------
!                                                                Size
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacetData_Size
IF (ALLOCATED(obj%masterCellNumber)) THEN
  ans = SIZE(obj%masterCellNumber)
ELSE
  ans = 0
END IF
END PROCEDURE MeshFacetData_Size

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Deallocate
CHARACTER(*), PARAMETER :: myName = "Domain_Deallocate()"
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
IF (ALLOCATED(obj%meshFacetData)) DEALLOCATE (obj%meshFacetData)
! BUG
CALL e%RaiseDebug(modName//'::'//myName//'-'// &
  & 'There should be better way to deallocate obj%meshList...')
IF (ALLOCATED(obj%meshList)) THEN
  DEALLOCATE (obj%meshList)
END IF
IF (ALLOCATED(obj%nodeCoord)) DEALLOCATE (obj%nodeCoord)
IF (ALLOCATED(obj%local_nptrs)) DEALLOCATE (obj%local_nptrs)
IF (ALLOCATED(obj%global_nptrs)) DEALLOCATE (obj%global_nptrs)
END PROCEDURE Domain_Deallocate

!----------------------------------------------------------------------------
!                                                              Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Final
CALL Obj%DEALLOCATE()
END PROCEDURE Domain_Final

!----------------------------------------------------------------------------
!                                                            Domain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_Constructor_1
ALLOCATE (Domain_ :: ans)
CALL ans%Initiate(hdf5=hdf5, group=group)
END PROCEDURE Domain_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
