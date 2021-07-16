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
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE( MatrixField_Class ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_checkEssentialParam"
  IF( .NOT. param%isPresent(key="name") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'name should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="matrixProp") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'matrixProp should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="dof-valmap") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'dof should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="dof-map") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'dof should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="dof-storageFMT") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'dof should be present in param')
  END IF
END PROCEDURE mField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate1"
  INTEGER( I4B ) :: ierr, nrow, ncol
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  TYPE( DOF_ ) :: dofobj

  !> main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Matrix field object is already initiated')
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'Checking essential parameters')
  CALL obj%checkEssentialParam(param)
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( key="name" ) ) :: char_var )
  ierr = param%get( key="name", value=char_var )
  obj%name = char_var
  IF( param%isPresent(key="fieldType") ) THEN
    ierr = param%get( key="fieldType", value=obj%fieldType )
  ELSE
    obj%fieldType = FIELD_TYPE_NORMAL
  END IF
  DEALLOCATE( char_var )
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( key="matrixProp" ) ) :: char_var )
  ierr = param%get( key="matrixProp", value=char_var )
  CALL getValue( obj=param, key="dof", value=dofobj )
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'Found all required parameters')
  nrow = .tNodes. dofobj
  ncol = nrow
  obj%domain => dom
  CALL initiate( obj=obj%mat, nrow=nrow, ncol=ncol, dof=dofobj, &
    & matrixProp=char_var )
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'CSRMatrix has been initiated')
  obj%isInitiated = .TRUE.
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'Setting sparsity...')
  !> setting the sparsity
  CALL obj%domain%setSparsity( mat=obj%mat )
END PROCEDURE mField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate2
END PROCEDURE mField_Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_DeallocateData
  obj%name = ''
  CALl DeallocateData( obj%mat )
  obj%isInitiated = .FALSE.
  obj%fieldType = 0
  obj%domain => NULL()
END PROCEDURE mField_DeallocateData

END SUBMODULE Constructor