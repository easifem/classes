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

SUBMODULE(STVectorField_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Display
  IF( LEN_TRIM( msg) .NE. 0 ) THEN
    CALL Display( "# "//TRIM( msg ), unitNo=unitNo )
  END IF
  IF( obj%isInitiated ) THEN
    CALL Display( "# isInitiated : TRUE", unitNo=unitNo)
  ELSE
    CALL Display( "# isInitiated : FALSE, Nothing to Display!", unitNo=unitNo)
    RETURN
  END IF
  CALL Display( obj%name, "# name : ", unitNo=unitNo)
  CALL Display( "# engine : NATIVE_SERIAL", unitNo=unitNo)
  CALL Display( obj%spaceCompo, "# space components : ", unitNo=unitNo )
  CALL Display( obj%timeCompo, "# time components : ", unitNo=unitNo )
  CALL Display( obj%tSize, "# tSize : ", unitNo=unitNo )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL Display( "# fieldType : CONSTANT", unitNo=unitNo )
  ELSE
    CALL Display( "# fieldType : NORMAL", unitNo=unitNo )
  END IF
  IF( ASSOCIATED( obj%domain )  ) THEN
    CALL Display( "# domain : ASSOCIATED", unitNo=unitNo )
  ELSE
    CALL Display( "# domain : NOT ASSOCIATED", unitNo=unitNo )
  END IF
  CALL Display( obj%realVec, obj%dof, msg="# realVec : ", unitNo=unitNo )
END PROCEDURE stvField_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_Import"
  TYPE( String ) :: strval, dsetname, name
  INTEGER( I4B ) :: fieldType, spaceCompo, timeCompo
  LOGICAL( LGT ) :: restart
  TYPE( ParameterList_ ) :: param
  ! main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Space time Vector field object is already initiated')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "IMPORTING SPACE TIME VECTOR FIELD")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file does not have read permission')
  END IF
  ! READ fieldType
  dsetname=trim(group)//"/fieldType"
  IF( hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
      SELECT CASE( TRIM(strval%chars()) )
      CASE( "NORMAL" )
        fieldType = FIELD_TYPE_NORMAL
      CASE( "CONSTANT" )
        fieldType = FIELD_TYPE_CONSTANT
      CASE( "CONSTANT_SPACE" )
        fieldType = FIELD_TYPE_CONSTANT_SPACE
      CASE( "CONSTANT_TIME" )
        fieldType = FIELD_TYPE_CONSTANT_TIME
      END SELECT
  ELSE
    fieldType = FIELD_TYPE_NORMAL
  END IF
  ! READ name
  dsetname=trim(group)//"/name"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset name should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=name)
  ! READ spaceCompo
  dsetname=trim(group)//"/spaceCompo"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset spaceCompo should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=spaceCompo)
  ! READ timeCompo
  dsetname=trim(group)//"/timeCompo"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset timeCompo should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=timeCompo)
  ! READ restart
  dsetname=trim(group)//"/restart"
  IF( hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%read(dsetname=dsetname%chars(),vals=restart)
  ELSE
    restart = .FALSE.
  END IF
  IF( .NOT. restart ) THEN
    CALL FPL_INIT(); CALL param%initiate()
    CALL setSTVectorFieldParam( param=param, name=trim(name%chars()), &
      & fieldType = fieldType, spaceCompo=spaceCompo, timeCompo=timeCompo )
    CALL obj%initiate( param=param, dom=dom )
    CALL param%deallocateData(); CALL FPL_FINALIZE()
  ELSE
    CALL e%raiseError(modName//'::'//myName// &
    & 'At present restart option is not available, we are working on it.' )
  END IF
END PROCEDURE stvField_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_Export"
  CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine has not been implemented')
END PROCEDURE stvField_Export

END SUBMODULE IOMethods