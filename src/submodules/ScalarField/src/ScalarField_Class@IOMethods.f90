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

SUBMODULE( ScalarField_Class ) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Display
  INTEGER( I4B ) :: I
  I = Input( option=unitNo, default=stdout )

  IF( LEN_TRIM( msg) .NE. 0 ) WRITE( I, "(A)") "# "//TRIM( msg )
  CALL Display( obj%name, "# name : ")
  IF( obj%isInitiated ) THEN
    WRITE( I, "(A)" ) "# isInitiated : TRUE"
  ELSE
    WRITE( I, "(A)" ) "# isInitiated : FALSE"
  END IF
  CALL Display( obj%tSize, "# tSize : " )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    WRITE( I, "(A)" ) "# fieldType : constant"
  ELSE
    WRITE( I, "(A)" ) "# fieldType : normal"
  END IF
  IF( ASSOCIATED( obj%domain )  ) THEN
    WRITE( I, "(A)" ) "# domain : associated"
  ELSE
    WRITE( I, "(A)" ) "# domain : not associated"
  END IF
  CALL Display( obj%realVec, obj%dof,  "# realVec : ", I )
END PROCEDURE sField_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_Import"
  TYPE( String ) :: strval, dsetname, name
  INTEGER( I4B ) :: fieldType
  LOGICAL( LGT ) :: restart
  TYPE( ParameterList_ ) :: param
  ! main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is already initiated')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "IMPORTING SCALAR FIELD")
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
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
      CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=strval)
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
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset name should be present')
  END IF
  CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=name)
  ! READ restart
  dsetname=trim(group)//"/restart"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=restart)
  ELSE
    restart = .FALSE.
  END IF
  IF( .NOT. restart ) THEN
    CALL FPL_INIT(); CALL param%initiate()
    CALL setScalarFieldParam( param=param, &
      & name=trim(name%chars()), &
      & fieldType = fieldType )
    CALL obj%initiate( param=param, dom=dom )
    CALL param%deallocateData(); CALL FPL_FINALIZE()
  ELSE
    CALL e%raiseError(modName//'::'//myName// &
    & 'At present restart option is not available, we are working on it.' )
  END IF
END PROCEDURE sField_Import

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_Export"
  CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine has not been implemented')
END PROCEDURE sField_Export


END SUBMODULE IOMethods