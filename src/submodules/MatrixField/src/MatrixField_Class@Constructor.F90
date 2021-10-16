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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    setMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setMatrixFieldParam
  INTEGER( I4B ) :: ierr
  ierr = param%set( key="MatrixField/name", value=TRIM(name) )
  ierr = param%set( key="MatrixField/matrixProp", value=TRIM(matrixProp) )
  ierr = param%set( key="MatrixField/spaceCompo",  &
    &  value=INPUT( option=spaceCompo, default=1 ) )
  ierr = param%set( key="MatrixField/timeCompo",  &
    & value=INPUT( option=timeCompo, default=1 ) )
  ierr = param%set( key="MatrixField/fieldType", value=INPUT(  &
    & option=fieldType, default=FIELD_TYPE_NORMAL ) )
END PROCEDURE setMatrixFieldParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_addSurrogate
  CALL e%addSurrogate(UserObj)
END PROCEDURE mField_addSurrogate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_checkEssentialParam"
  IF( .NOT. param%isPresent(key="MatrixField/name") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'MatrixField/name should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="MatrixField/matrixProp") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'MatrixField/matrixProp should be present in param')
  END IF
END PROCEDURE mField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate1"
  INTEGER( I4B ) :: ierror, nrow, ncol, storageFMT, tNodes( 1 ), &
    & timeCompo( 1 ), spaceCompo(1)
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  CHARACTER( LEN=1 ) :: names_char( 1 )
  TYPE( DOF_ ) :: dofobj
  !> main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Matrix field object is already initiated')
  CALL obj%checkEssentialParam(param)
  !> engine
  obj%engine="NATIVE_SERIAL"
  !> name
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="MatrixField/name" ) ) :: char_var )
  ierror = param%get( key="MatrixField/name", value=char_var )
  obj%name = char_var
  names_char(1)(1:1) = char_var(1:1)
  DEALLOCATE( char_var )
  !> fieldType
  IF( param%isPresent(key="MatrixField/fieldType") ) THEN
    ierror = param%get( key="MatrixField/fieldType", value=obj%fieldType )
  ELSE
    obj%fieldType = FIELD_TYPE_NORMAL
  END IF
  !> spaceCompo
  IF( param%isPresent(key="MatrixField/spaceCompo") ) THEN
    ierror = param%get( key="MatrixField/spaceCompo", value=spaceCompo(1) )
  ELSE
    spaceCompo(1) = 1
  END IF
  !> timeCompo
  IF( param%isPresent(key="MatrixField/timeCompo") ) THEN
    ierror = param%get( key="MatrixField/timeCompo", value=timeCompo(1) )
  ELSE
    timeCompo(1) = 1
  END IF
  !> storage format
  storageFMT = FMT_NODES
  tNodes = dom%getTotalNodes()
  !> make [[DOF_]]
  CALL initiate( obj=dofobj, tNodes=tNodes, names=names_char, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT )
  !> matrixProp
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="MatrixField/matrixProp" ) ) :: char_var )
  ierror = param%get( key="MatrixField/matrixProp", value=char_var )
  nrow = tNodes(1) * spaceCompo(1) * timeCompo(1)
  ncol = nrow
  obj%domain => dom
  CALL initiate( obj=obj%mat, nrow=nrow, ncol=ncol, dof=dofobj, &
    & matrixProp=char_var )
  DEALLOCATE( char_var )
  obj%isInitiated = .TRUE.
  obj%isPmatInitiated = .FALSE.
  !> setting the sparsity
  CALL obj%domain%setSparsity( mat=obj%mat )
  CALL DeallocateData( dofobj )
END PROCEDURE mField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate2
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate2"
  SELECT TYPE (obj2)
  CLASS IS (MatrixField_)
    IF( .NOT. obj2%isInitiated .OR. obj%isInitiated ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'Either obj is already initiated or obj2 is not initiated!')
    obj%isInitiated = .TRUE.
    obj%name = obj2%name
    obj%fieldType = obj2%fieldType
    obj%mat = obj2%mat
    obj%isPmatInitiated = .FALSE.
    obj%engine=obj2%engine
  END SELECT
END PROCEDURE mField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate3
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate3"
  CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'Presently, this subroutine does nothing')
END PROCEDURE mField_Initiate3

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_DeallocateData
  INTEGER( I4B ) :: ii
  obj%name = ''
  obj%engine=""
  CALl DeallocateData( obj%mat )
  CALL DeallocateData( obj%Pmat )
  obj%isInitiated = .FALSE.
  obj%isPmatInitiated = .FALSE.
  obj%fieldType = 0
  obj%domain => NULL()
  IF( ALLOCATED( obj%domains ) ) THEN
    DO ii = 1, SIZE( obj%domains )
      obj%domains(ii)%ptr => NULL()
    END DO
    DEALLOCATE( obj%domains )
  END IF
END PROCEDURE mField_DeallocateData

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Pmat_DeallocateData
  obj%PmatName = 0
  IF( ALLOCATED( obj%A ) ) DEALLOCATE( obj%A )
  IF( ALLOCATED( obj%JA ) ) DEALLOCATE( obj%JA )
  IF( ALLOCATED( obj%IA ) ) DEALLOCATE( obj%IA )
  IF( ALLOCATED( obj%JU ) ) DEALLOCATE( obj%JU )
  IF( ALLOCATED( obj%IPERM ) ) DEALLOCATE( obj%IPERM )
  IF( ALLOCATED( obj%LEVS ) ) DEALLOCATE( obj%LEVS )
  obj%nnz = 0
  obj%ncol = 0
  obj%nrow = 0
  obj%isInitiated = .FALSE.
  obj%lfil = 0
  obj%mbloc = 0
  obj%alpha = 0
  obj%droptol = 0
  obj%permtol = 0
END PROCEDURE Pmat_DeallocateData
END SUBMODULE Constructor