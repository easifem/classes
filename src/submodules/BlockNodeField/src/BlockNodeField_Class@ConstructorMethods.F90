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

SUBMODULE( BlockNodeField_Class ) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_addSurrogate
  CALL e%addSurrogate( Userobj )
END PROCEDURE Block_addSurrogate

!----------------------------------------------------------------------------
!                                                         setBlockNodeField
!----------------------------------------------------------------------------

MODULE PROCEDURE setBlockNodeFieldParam
  CHARACTER( LEN = * ), PARAMETER :: myName="setBlockNodeFieldParam"
  INTEGER( I4B ) :: ierr, tPhysicalVars, ii
  INTEGER( I4B ) :: spaceCompo_(SIZE(name))
  INTEGER( I4B ) :: timeCompo_(SIZE(name))
  INTEGER( I4B ) :: fieldType_(SIZE(name))

  !> tPhysicalVars
  tPhysicalVars=SIZE(name)
  DO ii=1, tPhysicalVars
    ierr = param%set( key="BlockNodeField/name"//TRIM(STR(ii, .TRUE.)), &
      & value=name(ii)%chars() )
  END DO
  ierr = param%set( key="BlockNodeField/tPhysicalVars", value=tPhysicalVars )
  !>
  IF( PRESENT( spaceCompo ) ) THEN
    IF( SIZE( spaceCompo ) .NE. tPhysicalVars ) &
      & CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Size of spaceCompo should be equal to the total number of physical &
      & variables.')
    spaceCompo_ = spaceCompo
  ELSE
    spaceCompo_ = 1
  END IF
  IF( PRESENT( timeCompo ) ) THEN
    IF( SIZE( timeCompo ) .NE. tPhysicalVars ) &
      & CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Size of timeCompo should be equal to the total number of physical &
      & variables.')
    timeCompo_ = timeCompo
  ELSE
    timeCompo_ = 1
  END IF
  IF( PRESENT( fieldType ) ) THEN
    IF( SIZE( fieldType ) .NE. tPhysicalVars ) &
      & CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Size of fieldType should be equal to the total number of physical &
      & variables.')
    fieldType_ = fieldType
  ELSE
    fieldType_ = FIELD_TYPE_NORMAL
  END IF
  !>
  ierr = param%set(key="BlockNodeField/spaceCompo", value=spaceCompo_)
  ierr = param%set(key="BlockNodeField/timeCompo", value=timeCompo_)
  ierr = param%set(key="BlockNodeField/fieldType", value=fieldType_)
  ierr = param%set(key="BlockNodeField/engine", value="NATIVE_SERIAL" )
END PROCEDURE setBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "Block_checkEssentialParam"
  INTEGER( I4B ) :: tPhysicalVars, ierr, ii
  !> main
  IF( .NOT. param%isPresent(key="BlockNodeField/engine") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/engine should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/fieldType") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/fieldType should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/spaceCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/spaceCompo should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/timeCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/timeCompo should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="BlockNodeField/tPhysicalVars") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'BlockNodeField/tPhysicalVars should be present in param')
  END IF
  ierr = param%get( key="BlockNodeField/tPhysicalVars", value=tPhysicalVars )
  DO ii = 1, tPhysicalVars
    IF( .NOT. param%isPresent(key="BlockNodeField/name" &
      & // TRIM(STR(ii, .TRUE.)))) THEN
      CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'BlockNodeField/name should be present in param')
    END IF
  END DO
END PROCEDURE Block_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Initiate1"
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
END PROCEDURE Block_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Initiate2
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Initiate2"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Initiate3
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Initiate3"
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
END PROCEDURE Block_Initiate3

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_DeallocateData
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_DeallocateData"
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
END PROCEDURE Block_DeallocateData

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Final
  CALL obj%DeallocateData()
END PROCEDURE Block_Final

END SUBMODULE ConstructorMethods