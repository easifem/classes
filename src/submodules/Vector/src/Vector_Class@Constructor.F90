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

SUBMODULE(Vector_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_checkEssentialParam
CHARACTER(LEN=*), PARAMETER :: myName = "vec_checkEssentialParam"
INTEGER(I4B) :: data_dim, ierr, ii, data_size
INTEGER(I4B), ALLOCATABLE :: data_shape(:)
CHARACTER(LEN=:), ALLOCATABLE :: char_var
TYPE(String) :: bigName
TYPE(String), ALLOCATABLE :: names(:)

IF (.NOT. param%isPresent(key="tNodes")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'tNodes should be present in param')
END IF

data_dim = param%getDimensions(key="tNodes")
IF (data_dim .NE. 1) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'tNodes should be a vector of integer')

ierr = param%getShape(key="tNodes", shape=data_shape)

IF (.NOT. param%isPresent(key="names")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'names should be present in param')
END IF

data_size = param%DataSizeInBytes(key="names")
ALLOCATE (CHARACTER(LEN=data_size) :: char_var)
ierr = param%get(key="names", value=char_var)
bigName = String(TRIM(char_var))
CALL bigName%split(tokens=names, sep=',')

IF (data_shape(1) .NE. SIZE(names)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'There is some mismatch between total number of variables and total number of names.')

DO ii = 1, SIZE(names)
  names(ii) = TRIM(ADJUSTL(names(ii)))
  IF (LEN_TRIM(names(ii)) .NE. 1) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Only SINGLE CHARACATER NAME is allowed for name of physical vars' &
    & //' var name found is : '//TRIM(names(ii)))
  END IF
END DO

IF (ALLOCATED(char_var)) DEALLOCATE (char_var)
IF (ALLOCATED(names)) DEALLOCATE (names)
IF (ALLOCATED(data_shape)) DEALLOCATE (data_shape)
CALL bigName%Free()
END PROCEDURE vec_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_Initiate
CHARACTER(LEN=*), PARAMETER :: myName = "vec_Initiate"
INTEGER(I4B) :: data_dim, int0, ierr, data_size, ii, storageFMT
INTEGER(I4B), ALLOCATABLE :: tNodes(:), data_shape(:), &
  & spaceCompo(:), timeCompo(:)
CHARACTER(LEN=:), ALLOCATABLE :: char_var
CHARACTER(LEN=1), ALLOCATABLE :: names_char(:)
TYPE(String) :: bigName
TYPE(String), ALLOCATABLE :: names(:)

!> main program
CALL obj%checkEssentialParam(param)
!>
data_dim = param%getDimensions(key="tNodes")
ierr = param%getShape(key="tNodes", shape=data_shape)
ALLOCATE (tNodes(data_shape(1)))
ierr = param%get(key="tNodes", value=tNodes)
!>
data_size = param%DataSizeInBytes(key="names")
ALLOCATE (CHARACTER(LEN=data_size) :: char_var)
ierr = param%get(key="names", value=char_var)
bigName = String(TRIM(char_var))
CALL bigName%split(tokens=names, sep=',')
!>
ALLOCATE (names_char(SIZE(names)))
DO ii = 1, SIZE(names)
  names_char(ii) (1:1) = TRIM(ADJUSTL(names(ii)))
END DO
!>
IF (param%isPresent(key="spaceCompo")) THEN
  data_dim = param%getDimensions(key="spaceCompo")
  ierr = param%getShape(key="spaceCompo", shape=data_shape)
  ALLOCATE (spaceCompo(data_shape(1)))
  ierr = param%get(key="spaceCompo", value=spaceCompo)
ELSE
  CALL Reallocate(spaceCompo, SIZE(names))
  spaceCompo = 1
END IF
!>
IF (param%isPresent(key="timeCompo")) THEN
  data_dim = param%getDimensions(key="timeCompo")
  ierr = param%getShape(key="timeCompo", shape=data_shape)
  ALLOCATE (timeCompo(data_shape(1)))
  ierr = param%get(key="timeCompo", value=timeCompo)
ELSE
  CALL Reallocate(timeCompo, SIZE(names))
  timeCompo = 1
END IF
!>
IF (param%isPresent(key="storageFMT")) THEN
  ierr = param%get(key="storageFMT", value=storageFMT)
ELSE
  storageFMT = FMT_DOF
END IF
!>
CALL initiate(obj=obj%dof, tNodes=tNodes, names=names_char, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT)
!>
CALL initiate(obj%realVec, obj%dof)
!>
obj%isInitiated = .TRUE.
obj%tSize = SIZE(obj%realVec)
!>
IF (ALLOCATED(tNodes)) DEALLOCATE (tNodes)
IF (ALLOCATED(data_shape)) DEALLOCATE (data_shape)
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(names_char)) DEALLOCATE (names_char)
IF (ALLOCATED(names)) DEALLOCATE (names)
IF (ALLOCATED(char_var)) DEALLOCATE (char_var)
CALL bigName%Free()
END PROCEDURE vec_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_Deallocate
CHARACTER(LEN=*), PARAMETER :: myName = "vec_Deallocate"
obj%tSize = 0_I4B
obj%isInitiated = .FALSE.
CALL Deallocate (obj%realvec)
CALL Deallocate (obj%dof)
END PROCEDURE vec_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_Final
CALL obj%Deallocate()
END PROCEDURE vec_Final

!----------------------------------------------------------------------------
!                                                                 Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_Constructor1
CALL ans%initiate(param)
END PROCEDURE vec_Constructor1

!----------------------------------------------------------------------------
!                                                             Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_Constructor_1
ALLOCATE (ans)
CALL ans%initiate(param)
END PROCEDURE vec_Constructor_1

END SUBMODULE Constructor
