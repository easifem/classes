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

SUBMODULE(VectorField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Display
  IF( LEN_TRIM( msg) .NE. 0 ) THEN
    CALL Display("# "//TRIM( msg ), unitNo=unitNo)
  END IF
  CALL Display( obj%name, "# name : ")
  CALL Display( "# engine : NATIVE_SERIAL")
  IF( obj%isInitiated ) THEN
    CALL Display( "# isInitiated : TRUE", unitNo=unitNo )
  ELSE
    CALL Display( "# isInitiated : FALSE, Nothing to Display!", &
      & unitNo=unitNo )
    RETURN
  END IF
  CALL Display( obj%spaceCompo, "# space components : ", unitNo=unitNo )
  CALL Display( obj%tSize, "# tSize : ", unitNo=unitNo )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL Display( "# fieldType : CONSTANT", unitNo=unitNo )
  ELSE
    CALL Display( "# fieldType : NORMAL", unitNo=unitNo )
  END IF
  IF( ASSOCIATED( obj%domain )  ) THEN
    CALL Display( "# domain : ASSOCIATED", unitNo=unitNo )
  ELSE
    CALL Display( "# domain : .NOT. ASSOCIATED", unitNo=unitNo )
  END IF
  CALL Display( obj%realVec, obj%dof, msg="# realVec : ", unitNo=unitNo )
END PROCEDURE vField_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_Import"
  TYPE( String ) :: strval, dsetname, name
  INTEGER( I4B ) :: fieldType, spaceCompo
  TYPE( ParameterList_ ) :: param
  ! main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is already initiated')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "IMPORTING VECTOR FIELD")
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
      obj%fieldType = FIELD_TYPE_NORMAL
    CASE( "CONSTANT" )
      obj%fieldType = FIELD_TYPE_CONSTANT
    CASE( "CONSTANT_SPACE" )
      obj%fieldType = FIELD_TYPE_CONSTANT_SPACE
    CASE( "CONSTANT_TIME" )
      obj%fieldType = FIELD_TYPE_CONSTANT_TIME
    END SELECT
  ELSE
    obj%fieldType = FIELD_TYPE_NORMAL
  END IF
  ! READ name
  dsetname=trim(group)//"/name"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset name should be present')
  END IF
  CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=obj%name)
  ! READ engine
  dsetname=trim(group)//"/engine"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset engine should be present')
  END IF
  CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=obj%engine)
  ! READ tSize
  dsetname=trim(group)//"/tSize"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset tSize should be present')
  END IF
  CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=obj%tSize)
  ! READ spaceCompo
  dsetname=trim(group)//"/spaceCompo"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset spaceCompo should be present')
  END IF
  CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=obj%spaceCompo)
  ! READ dof
  dsetname=trim(group)//"/dof"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset dof should be present')
  END IF
  CALL ImportDOF( obj=obj%dof, hdf5=hdf5, group=TRIM( dsetname%chars()) )
  ! READ realVec
  dsetname=trim(group)//"/realVec"
  IF( .NOT. hdf5%pathExists(trim(dsetname%chars())) ) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset realVec should be present')
  END IF
  CALL ImportRealVector( obj=obj%realVec, hdf5=hdf5, &
    & group=TRIM( dsetname%chars()) )
END PROCEDURE vField_Import

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_Export"
  TYPE( String ) :: dsetname, strval
  INTEGER( I4B ) :: fieldType, spaceCompo
  TYPE( ParameterList_ ) :: param
  ! main program
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated, initiate it first')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "EXPORTING VECTOR FIELD")
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
  SELECT CASE( obj%fieldType )
  CASE( FIELD_TYPE_NORMAL )
    strval = "NORMAL"
  CASE( FIELD_TYPE_CONSTANT )
    strval = "CONSTANT"
  CASE( FIELD_TYPE_CONSTANT_SPACE )
    strval = "CONSTANT_SPACE"
  CASE( FIELD_TYPE_CONSTANT_TIME )
    strval = "CONSTANT_TIME"
  END SELECT
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=strval)
  ! WRITE name
  dsetname=trim(group)//"/name"
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=obj%name)
  ! WRITE engine
  dsetname=trim(group)//"/engine"
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=obj%engine)
  ! WRITE tSize
  dsetname=trim(group)//"/tSize"
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=obj%tSize)
  ! WRITE spaceCompo
  dsetname=trim(group)//"/spaceCompo"
  CALL hdf5%write(dsetname=trim(dsetname%chars()),vals=obj%spaceCompo)
  ! WRITE dof
  dsetname=trim(group)//"/dof"
  CALL ExportDOF( obj=obj%dof, hdf5=hdf5, group=TRIM( dsetname%chars()) )
  ! WRITE realVec
  dsetname=trim(group)//"/realVec"
  CALL ExportRealVector( obj=obj%realVec, hdf5=hdf5, &
    & group=TRIM( dsetname%chars()) )
END PROCEDURE vField_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods