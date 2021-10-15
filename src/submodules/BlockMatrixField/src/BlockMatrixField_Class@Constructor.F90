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
! summary: This module contains constructor method for [[BlockMatrixField_]]

SUBMODULE(BlockMatrixField_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_addSurrogate
  CALL e%addSurrogate(UserObj)
END PROCEDURE mField_addSurrogate

!----------------------------------------------------------------------------
!                                                    setBlockMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setBlockMatrixFieldParam
  INTEGER( I4B ) :: ierr, ii
  CHARACTER( LEN = * ), PARAMETER :: myName="setBlockMatrixFieldParam"

  IF( ANY( [SIZE(physicalVarNames), SIZE(spaceCompo), SIZE(timeCompo)]  &
    & .NE. SIZE(physicalVarNames))) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of physicalVarNames, spaceCompo, and timeCompo should be same')
  END IF
  ierr = param%set( key="BlockMatrixField/name", value=TRIM(name) )
  ierr = param%set( key="BlockMatrixField/matrixProp", &
    & value=TRIM(matrixProp) )
  ierr = param%set( key="BlockMatrixField/tPhysicalVarNames",  &
    & value=SIZE(physicalVarNames) )
  DO ii = 1, SIZE( physicalVarNames )
    ierr = param%set( key="BlockMatrixField/physicalVarName"//TOSTRING(ii),  &
      & value=TRIM(physicalVarNames(ii)) )
  END DO
  ierr = param%set( key="BlockMatrixField/spaceCompo",  &
    &  value=spaceCompo )
  ierr = param%set( key="BlockMatrixField/timeCompo",  &
    & value=timeCompo )
  ierr = param%set( key="BlockMatrixField/fieldType", value=INPUT( &
    & option=fieldType, default=FIELD_TYPE_NORMAL ) )
END PROCEDURE setBlockMatrixFieldParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_checkEssentialParam"
  INTEGER( I4B ) :: ii, n
  IF( .NOT. param%isPresent(key="BlockMatrixField/name") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockMatrixField/name should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockMatrixField/matrixProp") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockMatrixField/matrixProp should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockMatrixField/tPhysicalVarNames") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockMatrixField/tPhysicalVarNames should be present in param')
  ELSE
    ii = param%get( key='BlockMatrixField/tPhysicalVarNames', value=n )
  END IF
  IF( .NOT. param%isPresent(key="BlockMatrixField/spaceCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockMatrixField/spaceCompo should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockMatrixField/timeCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockMatrixField/timeCompo should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockMatrixField/fieldType") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockMatrixField/fieldType should be present in param')
  END IF
  DO ii=1, n
    IF( .NOT. param%isPresent(key="BlockMatrixField/physicalVarName" &
      & // TOSTRING(ii)) ) THEN
      CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'BlockMatrixField/physicalVarName' &
      & // TOSTRING(ii) &
      & // ' should be present in param')
    END IF
  END DO
END PROCEDURE mField_checkEssentialParam

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

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_DeallocateData
  INTEGER( I4B ) :: ii
  obj%name = ''
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
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate1"
  INTEGER( I4B ) :: ierror, nrow, ncol, storageFMT, tVar, ii
  INTEGER( I4B ), ALLOCATABLE :: tNodes( : ), timeCompo( : ), spaceCompo(:)
  CHARACTER( LEN=1 ), ALLOCATABLE :: physicalVarNames( : )
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  TYPE( DOF_ ) :: dofobj
  !> main program
  !> check
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Block Matrix field is already initiated')
  CALL obj%checkEssentialParam(param)
  !>engine
  obj%engine="NATIVE_SERIAL"
  !> name
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="BlockMatrixField/name" ) ) :: char_var )
  ierror = param%get( key="BlockMatrixField/name", value=char_var )
  obj%name = char_var; DEALLOCATE( char_var )
  !> fieldType
  ierror = param%get( key="BlockMatrixField/fieldType",  &
    & value=obj%fieldType )
  !> tPhysicalVarNames
  ierror=param%get( key='BlockMatrixField/tPhysicalVarNames', value=tVar )
  ALLOCATE( tNodes( tVar ), timeCompo(tVar), spaceCompo(tVar), &
    & physicalVarNames(tVar) )
  !> physicalVarName
  DO ii = 1, tVar
    ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( &
      & key="BlockMatrixField/physicalVarName"//TOSTRING(ii)))::char_var )
    ierror = param%get( key="BlockMatrixField/physicalVarName" &
      & //TOSTRING(ii), value=char_var )
    physicalVarNames(1)(1:1) = char_var(1:1); DEALLOCATE( char_var )
  END DO
  !> spaceCompo
  IF( param%isPresent(key="BlockMatrixField/spaceCompo") ) THEN
    ierror = param%get( key="BlockMatrixField/spaceCompo", value=spaceCompo)
  END IF
  !> timeCompo
  IF( param%isPresent(key="BlockMatrixField/timeCompo") ) THEN
    ierror = param%get( key="BlockMatrixField/timeCompo", value=timeCompo)
  END IF
  !> storage format
  storageFMT = FMT_DOF
  obj%domain => dom
  ALLOCATE( obj%domains( tvar ) )
  DO ii = 1, tVar
    obj%domains(ii)%ptr => dom
    tNodes( ii ) = obj%domains(ii)%ptr%getTotalNodes()
  END DO
  !> make [[DOF_]]
  CALL Initiate( obj=dofobj, tNodes=tNodes, names=physicalVarNames, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT )
  !> matrixProp
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="BlockMatrixField/matrixProp" ) ) :: char_var )
  !> #CSRMatrix/Initiate
  ierror = param%get( key="BlockMatrixField/matrixProp", value=char_var )
  nrow = .tNodes. dofobj
  ncol = nrow
  CALL Initiate( obj=obj%mat, nrow=nrow, ncol=ncol, dof=dofobj, &
    & matrixProp=char_var )
  DEALLOCATE( char_var )
  obj%isInitiated = .TRUE.
  obj%isPmatInitiated = .FALSE.
  !> setting the sparsity
  CALL obj%domain%setSparsity( mat=obj%mat, domains=obj%domains )
  CALL DeallocateData( dofobj )
  IF( ALLOCATED( tNodes ) ) DEALLOCATE( tNodes )
  IF( ALLOCATED( spaceCompo ) ) DEALLOCATE( spaceCompo )
  IF( ALLOCATED( timeCompo ) ) DEALLOCATE( timeCompo )
  IF( ALLOCATED( physicalVarNames ) ) DEALLOCATE( physicalVarNames )
END PROCEDURE mField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate2
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate2"
  CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'This routine is under construction!')
  SELECT TYPE (obj2)
  CLASS IS (BlockMatrixField_)
  END SELECT
END PROCEDURE mField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate3
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Initiate3"
  INTEGER( I4B ) :: ierror, nrow, ncol, storageFMT, tVar, ii
  INTEGER( I4B ), ALLOCATABLE :: tNodes( : ), timeCompo( : ), spaceCompo(:)
  CHARACTER( LEN=1 ), ALLOCATABLE :: physicalVarNames( : )
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  TYPE( DOF_ ) :: dofobj
  !> main program
  !> check
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Block Matrix field is already initiated')
  CALL obj%checkEssentialParam(param)
  !> engine
  obj%engine="NATIVE_SERIAL"
  !> name
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="BlockMatrixField/name" ) ) :: char_var )
  ierror = param%get( key="BlockMatrixField/name", value=char_var )
  obj%name = char_var; DEALLOCATE( char_var )
  !> fieldType
  ierror = param%get( key="BlockMatrixField/fieldType",  &
    & value=obj%fieldType )
  !> tPhysicalVarNames
  ierror=param%get( key='BlockMatrixField/tPhysicalVarNames', value=tVar )
  !> check
  IF( SIZE(dom) .NE. tVar ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of dom not equal to the total number of physical variables')
  DO ii = 1, tVar
    IF( .NOT. ASSOCIATED( dom(ii)%ptr ) ) THEN
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'dom( '// TOSTRING(ii) // ')%ptr is NOT ASSOCIATED!')
    END IF
  END DO
  !> allocate
  ALLOCATE( tNodes( tVar ), timeCompo(tVar), spaceCompo(tVar), &
    & physicalVarNames(tVar) )
  !> physicalVarName
  DO ii = 1, tVar
    ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( &
      & key="BlockMatrixField/physicalVarName"//TOSTRING(ii)))::char_var )
    ierror = param%get( key="BlockMatrixField/physicalVarName" &
      & //TOSTRING(ii), value=char_var )
    physicalVarNames(1)(1:1) = char_var(1:1); DEALLOCATE( char_var )
  END DO
  !> spaceCompo
  IF( param%isPresent(key="BlockMatrixField/spaceCompo") ) THEN
    ierror = param%get( key="BlockMatrixField/spaceCompo", value=spaceCompo)
  END IF
  !> timeCompo
  IF( param%isPresent(key="BlockMatrixField/timeCompo") ) THEN
    ierror = param%get( key="BlockMatrixField/timeCompo", value=timeCompo)
  END IF
  !> storage format
  storageFMT = FMT_DOF
  !> domains
  ALLOCATE( obj%domains( tvar ) )
  DO ii = 1, tVar
    obj%domains(ii)%ptr => dom(ii)%ptr
    tNodes( ii ) = obj%domains(ii)%ptr%getTotalNodes()
  END DO
  !> make [[DOF_]]
  CALL Initiate( obj=dofobj, tNodes=tNodes, names=physicalVarNames, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT )
  !> matrixProp
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="BlockMatrixField/matrixProp" ) ) :: char_var )
  !> #CSRMatrix/Initiate
  ierror = param%get( key="BlockMatrixField/matrixProp", value=char_var )
  nrow = .tNodes. dofobj
  ncol = nrow
  CALL Initiate( obj=obj%mat, nrow=nrow, ncol=ncol, dof=dofobj, &
    & matrixProp=char_var )
  DEALLOCATE( char_var )
  obj%isInitiated = .TRUE.
  obj%isPmatInitiated = .FALSE.
  !> setting the sparsity
  CALL obj%domains(1)%ptr%SetSparsity( mat=obj%mat, domains=obj%domains )
  CALL DeallocateData( dofobj )
  IF( ALLOCATED( tNodes ) ) DEALLOCATE( tNodes )
  IF( ALLOCATED( spaceCompo ) ) DEALLOCATE( spaceCompo )
  IF( ALLOCATED( timeCompo ) ) DEALLOCATE( timeCompo )
  IF( ALLOCATED( physicalVarNames ) ) DEALLOCATE( physicalVarNames )
END PROCEDURE mField_Initiate3

END SUBMODULE Constructor