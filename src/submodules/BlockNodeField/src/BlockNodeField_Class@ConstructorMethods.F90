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

SUBMODULE(BlockNodeField_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_addSurrogate
  CALL e%addSurrogate( Userobj )
END PROCEDURE bnField_addSurrogate

!----------------------------------------------------------------------------
!                                                         setBlockNodeField
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockNodeFieldParam
  CHARACTER( LEN = * ), PARAMETER :: myName="SetBlockNodeFieldParam"
  INTEGER( I4B ) :: ierr0, ii
  !> main
  !> check
  IF( ANY( [SIZE(physicalVarNames), SIZE(spaceCompo), SIZE(timeCompo)]  &
    & .NE. SIZE(physicalVarNames))) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of physicalVarNames, spaceCompo, and timeCompo should be &
    & same')
  END IF
  ierr0 = param%set( key="BlockNodeField/name", value=TRIM(name) )
  ierr0 = param%set( key="BlockNodeField/tPhysicalVarNames",  &
    & value=SIZE(physicalVarNames) )
  DO ii = 1, SIZE( physicalVarNames )
    ierr0 = param%set( key="BlockNodeField/physicalVarName"//TOSTRING(ii),  &
      & value=TRIM(physicalVarNames(ii)) )
  END DO
  ierr0 = param%set( key="BlockNodeField/spaceCompo",  &
    &  value=spaceCompo )
  ierr0 = param%set( key="BlockNodeField/timeCompo",  &
    & value=timeCompo )
  ierr0 = param%set( key="BlockNodeField/fieldType", value=INPUT( &
    & option=fieldType, default=FIELD_TYPE_NORMAL ) )
END PROCEDURE SetBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "bnField_checkEssentialParam"
  INTEGER( I4B ) :: ii, n
  !> main
  IF( .NOT. param%isPresent(key="BlockNodeField/name") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/name should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/tPhysicalVarNames") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/tPhysicalVarNames should be present in param')
  ELSE
    ii = param%get( key='BlockNodeField/tPhysicalVarNames', value=n )
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/spaceCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/spaceCompo should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/timeCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/timeCompo should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/fieldType") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/fieldType should be present in param')
  END IF
  DO ii=1, n
    IF( .NOT. param%isPresent(key="BlockNodeField/physicalVarName" &
      & // TOSTRING(ii)) ) THEN
      CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'BlockNodeField/physicalVarName' &
      & // TOSTRING(ii) &
      & // ' should be present in param')
    END IF
  END DO
END PROCEDURE bnField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_Initiate1"
  TYPE( DomainPointer_ ), ALLOCATABLE :: domains( : )
  INTEGER( I4B ) :: tPhysicalVarNames, ii
  !> main program
  ii=param%get(key="BlockNodeField/tPhysicalVarNames", &
    & value=tPhysicalVarNames)
  ALLOCATE( domains( tPhysicalVarNames ) )
  DO ii = 1, tPhysicalVarNames
    domains( ii )%ptr => dom
  END DO
  CALL obj%Initiate(param=param, dom=domains )
  DO ii = 1, tPhysicalVarNames
    domains( ii )%ptr => NULL()
  END DO
  IF( ALLOCATED( domains ) ) DEALLOCATE( domains )
END PROCEDURE bnField_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Initiate2
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_Initiate2"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE bnField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Initiate3
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_Initiate3"
  CHARACTER( LEN=1 ), ALLOCATABLE :: physicalVarNames( : )
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  INTEGER( I4B ) :: tVar, ii, ierror, storageFMT
  INTEGER( I4B ), ALLOCATABLE :: timeCompo(:), spaceCompo(:), tNodes(:)
  !> main
  !> check
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The instance of BlockNodeField_ is already initiated')
  CALL obj%checkEssentialParam( param )
  !> engine
  obj%engine="NATIVE_SERIAL"
  !> name
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes(  &
    & key="BlockNodeField/name" ) ) :: char_var )
  ierror = param%get( key="BlockNodeField/name", value=char_var )
  obj%name = char_var; DEALLOCATE( char_var )
  !> fieldType
  ierror = param%get( key="BlockNodeField/fieldType",  &
    & value=obj%fieldType )
  !> tPhysicalVarNames
  ierror=param%get( key='BlockNodeField/tPhysicalVarNames', value=tVar )
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
      & key="BlockNodeField/physicalVarName"//TOSTRING(ii)))::char_var )
    ierror = param%get( key="BlockNodeField/physicalVarName" &
      & //TOSTRING(ii), value=char_var )
    physicalVarNames(ii)(1:1) = char_var(1:1); DEALLOCATE( char_var )
  END DO
  !> spaceCompo
  IF( param%isPresent(key="BlockNodeField/spaceCompo") ) THEN
    ierror = param%get( key="BlockNodeField/spaceCompo", value=spaceCompo)
  END IF
  !> timeCompo
  IF( param%isPresent(key="BlockNodeField/timeCompo") ) THEN
    ierror = param%get( key="BlockNodeField/timeCompo", value=timeCompo)
  END IF
  !> storage format
  storageFMT = FMT_DOF
  !> domains, tNodes
  ALLOCATE( obj%domains( tvar ) ); obj%tSize=0
  DO ii = 1, tVar
    obj%domains(ii)%ptr => dom(ii)%ptr
    tNodes( ii ) = obj%domains(ii)%ptr%getTotalNodes()
    obj%tSize=obj%tSize+tNodes(ii)*timeCompo(ii)&
      & * spaceCompo(ii)
  END DO
  !> tNodes for constant field
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) tNodes(:) = 1
  !> [[DOF_]]
  CALL Initiate( obj=obj%dof, tNodes=tNodes, names=physicalVarNames, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT )
  !> realVec
  CALL Initiate( val=obj%realVec, obj=obj%dof )
  obj%isInitiated = .TRUE.
  IF( ALLOCATED( tNodes ) ) DEALLOCATE( tNodes )
  IF( ALLOCATED( spaceCompo ) ) DEALLOCATE( spaceCompo )
  IF( ALLOCATED( timeCompo ) ) DEALLOCATE( timeCompo )
  IF( ALLOCATED( physicalVarNames ) ) DEALLOCATE( physicalVarNames )
END PROCEDURE bnField_Initiate3

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_DeallocateData
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_DeallocateData"
  !> main program
  CALL AbstractNodeFieldDeallocateData(obj)
END PROCEDURE bnField_DeallocateData

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Final
  CALL obj%DeallocateData()
END PROCEDURE bnField_Final

END SUBMODULE ConstructorMethods