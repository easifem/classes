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

SUBMODULE(DirichletBC_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_checkEssentialParam
  !!
CHARACTER(LEN=*), PARAMETER :: myName = "bc_checkEssentialParam"
  !!
IF (.NOT. param%isPresent(key="DirichletBC/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'DirichletBC/name should be present in param')
END IF
  !!
IF (.NOT. param%isPresent(key="DirichletBC/idof")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'DirichletBC/idof should be present in param')
END IF
  !!
IF (.NOT. param%isPresent(key="DirichletBC/nodalValueType")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'DirichletBC/nodalValueType should be present in param')
END IF
  !!
IF (.NOT. param%isPresent(key="DirichletBC/useFunction")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'DirichletBC/useFunction should be present in param')
END IF
  !!
END PROCEDURE bc_checkEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setDirichletBCParam
INTEGER(I4B) :: ierr
  !!
ierr = param%set(key="DirichletBC/name", value=trim(name))
ierr = param%set(key="DirichletBC/idof", value=idof)
ierr = param%set(key="DirichletBC/nodalValueType", value=nodalValueType)
  !!
IF (PRESENT(useFunction)) THEN
  ierr = param%set(key="DirichletBC/useFunction", value=useFunction)
ELSE
  ierr = param%set(key="DirichletBC/useFunction", value=.FALSE.)
END IF
  !!
END PROCEDURE setDirichletBCParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Initiate
  !!
CHARACTER(LEN=*), PARAMETER :: myName = "bc_Initiate"
CHARACTER(LEN=:), ALLOCATABLE :: char_var
INTEGER(I4B) :: ierr
  !!
  !! check
  !!
IF (obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'DirichletBC_ object is already initiated')
  !!
  !! check
  !!
CALL obj%checkEssentialParam(param=param)
  !!
  !!
  !!
obj%isInitiated = .TRUE.
obj%boundary = boundary
obj%dom => dom
  !!
  !! name
  !!
ALLOCATE (CHARACTER(LEN=param%DataSizeInBytes( &
  & key="DirichletBC/name")) :: char_var)
ierr = param%get(key="DirichletBC/name", value=char_var)
obj%name = char_var
DEALLOCATE (char_var)
  !!
  !! idof
  !!
ierr = param%get(key="DirichletBC/idof", value=obj%idof)
  !!
  !! nodalValueType
  !!
ierr = param%get(key="DirichletBC/nodalValueType", &
  & value=obj%nodalValueType)
  !!
  !! useFunction
  !!
ierr = param%get(key="DirichletBC/useFunction", &
  & value=obj%useFunction)
  !!
  !!
  !!
IF (boundary%isSelectionByMeshID .AND. (.NOT. obj%useFunction)) THEN
    !!
  IF (obj%nodalValueType .NE. Constant) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When meshSelection is by MeshID &
        & and `useFunction` is false, then &
        & `nodalValueType` in `DirichletBC_` &
        & object should be Constant.')
  END IF
    !!
END IF
  !!
END PROCEDURE bc_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate
CALL AbstractBCDeallocate(obj)
obj%idof = 0
obj%nodalValueType = -1
obj%useFunction = .FALSE.
IF (ALLOCATED(obj%NodalValue)) DEALLOCATE (obj%NodalValue)
obj%TimeFunction => NULL()
obj%SpaceFunction => NULL()
obj%SpaceTimeFunction => NULL()
END PROCEDURE bc_Deallocate

!----------------------------------------------------------------------------
!                                                            Final
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Final
CALL obj%Deallocate()
END PROCEDURE bc_Final

END SUBMODULE ConstructorMethods
