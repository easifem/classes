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
  INTEGER( I4B ) :: tPhysicalVars, ii
  !> main program
  ii=param%get(key="BlockNodeField/tPhysicalVars", &
    & value=tPhysicalVars)
  CALL OK( tPhysicalVars == 2, "debug:: "//myName//" :: " )
  ALLOCATE( domains( tPhysicalVars ) )
  DO ii = 1, tPhysicalVars
    domains( ii )%ptr => dom
  END DO
  CALL obj%initiate(param=param, dom=domains )
  CALL PASS( "debug:: "//myName )

  DO ii = 1, tPhysicalVars
    domains( ii )%ptr => NULL()
  END DO

  CALL PASS( "debug:: "//myName )
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
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  CHARACTER( LEN=1 ), ALLOCATABLE :: names_char( : )
  INTEGER( I4B ) :: tPhysicalVars, ii, ierr, storageFMT
  INTEGER( I4B ), ALLOCATABLE :: timeCompo( : ), spaceCompo( : ), &
    & tNodes(:), fieldType( : )
  !> main program
  !> check
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField object is already initiated')
  CALL obj%checkEssentialParam( param )
  !> tPhysicalVars
  ierr = param%get(key="BlockNodeField/tPhysicalVars", value=tPhysicalVars)
  !> names
  ALLOCATE( names_char(tPhysicalVars) ); obj%name=""
  DO ii = 1, tPhysicalVars
    ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( key="BlockNodeField/name"//TRIM(STR(ii,.TRUE.)) ) ) :: char_var )
    ierr = param%get( key="BlockNodeField/name"//TRIM(STR(ii,.TRUE.)), &
      & value=char_var )
    obj%name=obj%name//TRIM(char_var)
    names_char(ii)(1:1)=char_var(1:1)
    DEALLOCATE( char_var )
  END DO
  !> engine
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( &
    & key="BlockNodeField/engine" ) ) :: char_var )
  obj%engine=TRIM(char_var); DEALLOCATE( char_var )
  !> timeCompo
  CALL Reallocate( timeCompo, tPhysicalVars, spaceCompo, tPhysicalVars )
  ierr = param%get( key="BlockNodeField/timeCompo", value=timeCompo )
  !> spaceCompo
  ierr = param%get( key="BlockNodeField/spaceCompo", value=spaceCompo )
  !> fieldType
  CALL Reallocate( fieldType, tPhysicalVars, tNodes, tPhysicalVars )
  ierr = param%get( key="BlockNodeField/fieldType", value=fieldType)
  !> tNodes
  obj%tSize=0
  DO ii = 1, tPhysicalVars
    obj%tSize=obj%tSize+dom(ii)%ptr%getTotalNodes()*timeCompo(ii)&
      & * spaceCompo(ii)
    IF( fieldType(ii) .EQ. FIELD_TYPE_CONSTANT ) THEN
      tNodes( ii ) = 1
    ELSE
      tNodes( ii ) = dom(ii)%ptr%getTotalNodes( )
    END IF
  END DO
  !> dof
  storageFMT=FMT_DOF
  CALL initiate( obj=obj%dof, tNodes=tNodes, names=names_char, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT )
  !> initiate realVec
  CALL initiate( val=obj%realVec, obj=obj%dof )
  !> domain
  IF( SIZE(dom) .NE. tPhysicalVars ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'SIZE of doms should be equal to total physical variables' )
  IF( ALLOCATED( obj%domains ) ) THEN
    IF( SIZE(obj%domains) .NE. tPhysicalVars ) &
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'obj%domains is already allocated, and some of the entries &
        & are already associated!')
    DO ii = 1, tPhysicalVars
      IF( ASSOCIATED(obj%domains( ii )%ptr) ) THEN
        CALL e%raiseError(modName//'::'//myName// " - "// &
          & 'obj%domains is already allocated, and some of the entries &
          & are already associated!')
      ELSE
        obj%domains(ii)%ptr=>dom(ii)%ptr
      END IF
    END DO
  ELSE
    ALLOCATE( obj%domains( tPhysicalVars ) )
    DO ii = 1, tPhysicalVars
      obj%domains(ii)%ptr=>dom(ii)%ptr
    END DO
  END IF
  obj%isInitiated = .TRUE.
END PROCEDURE bnField_Initiate3

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_DeallocateData
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_DeallocateData"
  INTEGER( I4B ) :: ii
  !> main program
  obj%tSize=0
  obj%isInitiated=.FALSE.
  obj%fieldType=FIELD_TYPE_NORMAL
  CALL DeallocateData( obj%realVec )
  CALL DeallocateData( obj%dof )
  NULLIFY( obj%domain )
  IF( ALLOCATED( obj%domains ) ) THEN
    DO  ii=1,SIZE(obj%domains)
      obj%domains(ii)%ptr=>NULL()
    END DO
    DEALLOCATE( obj%domains )
  END IF
END PROCEDURE bnField_DeallocateData

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Final
  CALL obj%DeallocateData()
END PROCEDURE bnField_Final

END SUBMODULE ConstructorMethods