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
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Display
  CALL Display( "# "//TRIM( msg ), unitNo=unitNo )
  IF( obj%isInitiated ) THEN
    CALL Display( "# isInitiated : TRUE", unitNo=unitNo )
  ELSE
    CALL Display( "# isInitiated : FALSE, Nothing to Display!!", unitNo=unitNo )
    RETURN
  END IF
  CALL Display( "# engine : NATIVE_SERIAL", unitNo=unitNo )
  CALL Display( obj%name, "# name : ", unitNo=unitNo )
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
  CALL Display( obj%realVec, obj%dof,  "# realVec : ", unitNo=unitNo )
END PROCEDURE sField_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_Import"
  TYPE( String ) :: strval, dsetname, name, engine
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
  ! READ engine
  dsetname=trim(group)//"/engine"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset named engine should be present')
  END IF
  CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=engine)
  ! Construct the base
  CALL FPL_INIT(); CALL param%initiate()
  CALL setScalarFieldParam( param=param, &
    & name=trim(name%chars()), &
    & fieldType = fieldType )
  CALL obj%initiate( param=param, dom=dom )
  CALL param%deallocateData(); CALL FPL_FINALIZE()
  ! READ dof
  dsetname=trim(group)//"/dof"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL ImportDOF( obj=obj%dof, hdf5=hdf5, group=trim(dsetname%chars()))
  END IF
  ! READ realVec
  dsetname=trim(group)//"/realVec"
  IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL ImportRealVector( obj=obj%realVec, hdf5=hdf5, &
      & group=trim(dsetname%chars()) )
  END IF
END PROCEDURE sField_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_Export"
  TYPE( String ) :: strval, dsetname
  ! main program
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated initiated')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "EXPORTING SCALAR FIELD")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isWrite() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'HDF5 file does not have write permission')
  END IF
  ! WRITE fieldType
  dsetname=trim(group)//"/fieldType"
  strval = FIELD_TYPE_NAME( obj%fieldType )
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=strval)
  ! WRITE name
  dsetname=trim(group)//"/name"
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=obj%name)
  ! WRITE engine
  dsetname=trim(group)//"/engine"
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=obj%engine)
  ! WRITE dof
  dsetname=trim(group)//"/dof"
  CALL ExportDOF( obj=obj%dof, hdf5=hdf5, group=trim(dsetname%chars()))
  ! WRITE realVec
  dsetname=trim(group)//"/realVec"
  CALL ExportRealVector( obj=obj%realVec, hdf5=hdf5, &
    & group=trim(dsetname%chars()) )
END PROCEDURE sField_Export

END SUBMODULE IOMethods