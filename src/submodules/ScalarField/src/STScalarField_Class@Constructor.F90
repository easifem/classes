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

SUBMODULE(STScalarField_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    setSTScalarFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setSTScalarFieldParam
  INTEGER( I4B ) :: ierr
  ierr = param%set(key="STScalarField/name", value=TRIM(name))
  ierr = param%set(key="STScalarField/timeCompo", value=timeCompo)
  IF( PRESENT( fieldType ) ) THEN
    ierr = param%set(key="STScalarField/fieldType", value=fieldType)
  ELSE
    ierr = param%set(key="STScalarField/fieldType", value=FIELD_TYPE_NORMAL)
  END IF
END PROCEDURE setSTScalarFieldParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_addSurrogate
  CALL e%addSurrogate(UserObj)
END PROCEDURE stsField_addSurrogate

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_checkEssentialParam"
  IF( .NOT. param%isPresent(key="STScalarField/name") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalarField/name should be present in param')
  END IF
  IF( .NOT. param%isPresent(key="STScalarField/timeCompo") ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalarField/timeCompo should be present in param')
  END IF
END PROCEDURE stsField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_Initiate1"
  INTEGER( I4B ) :: ierr, storageFMT, tNodes( 1 ), spaceCompo( 1 ), &
    & timeCompo( 1 )
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  CHARACTER( LEN=1 ) :: names_char( 1 )

  !> main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is already initiated')
  CALL obj%checkEssentialParam(param)
  ALLOCATE( CHARACTER( LEN = param%DataSizeInBytes( key="STScalarField/name" ) ) :: char_var )
  ierr = param%get( key="STScalarField/name", value=char_var )
  obj%name = char_var
  names_char( 1 )(1:1) = char_var( 1:1 )
  ierr = param%get( key="STScalarField/timeCompo", value=obj%timeCompo )
  IF( param%isPresent(key="STScalarField/fieldType") ) THEN
    ierr = param%get( key="STScalarField/fieldType", value=obj%fieldType )
  ELSE
    obj%fieldType = FIELD_TYPE_NORMAL
  END IF
  !> SET engine
  obj%engine="NATIVE_SERIAL"
  timeCompo = obj%timeCompo
  spaceCompo = 1
  storageFMT = FMT_NODES
  obj%domain => dom
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    tNodes = 1
    obj%tSize = obj%domain%getTotalNodes() * obj%timeCompo
  ELSE
    tNodes = obj%domain%getTotalNodes()
    obj%tSize = tNodes( 1 ) * obj%timeCompo
  END IF
  CALL initiate( obj=obj%dof, tNodes=tNodes, names=names_char, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT )
  CALL initiate( obj%realVec, obj%dof )
  obj%isInitiated = .TRUE.
  IF( ALLOCATED( char_var ) ) DEALLOCATE( char_var )
END PROCEDURE stsField_Initiate1

!----------------------------------------------------------------------------
!                                                           Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Initiate2
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_Initiate2"
  CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This method has not been implemented so far')
END PROCEDURE stsField_Initiate2

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_DeallocateData
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_DeallocateData"
  obj%tSize = 0_I4B
  obj%name = ''
  obj%timeCompo = 0_I4B
  obj%isInitiated = .FALSE.
  obj%fieldType = FIELD_TYPE_CONSTANT
  CALL DeallocateData( obj%realvec )
  CALL DeallocateData( obj%dof )
  NULLIFY( obj%domain )
END PROCEDURE stsField_DeallocateData

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Final
  CALL obj%DeallocateData()
END PROCEDURE stsField_Final

!----------------------------------------------------------------------------
!                                                                STScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Constructor1
  CALL ans%initiate( param, dom )
END PROCEDURE stsField_Constructor1

!----------------------------------------------------------------------------
!                                                        STScalarField_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Constructor_1
  ALLOCATE( ans )
  CALL ans%initiate( param, dom )
END PROCEDURE stsField_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor