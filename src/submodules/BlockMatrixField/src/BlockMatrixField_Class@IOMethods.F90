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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(BlockMatrixField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Import
  ! CHARACTER( LEN = * ), PARAMETER :: myName="mField_Import"
  ! TYPE( String ) :: strval, dsetname, name, matrixProp
  ! INTEGER( I4B ) :: timeCompo, spaceCompo, fieldType
  ! LOGICAL( LGT ) :: restart
  ! TYPE( ParameterList_ ) :: param
  ! ! main program
  ! IF( obj%isInitiated ) &
  !   & CALL e%raiseError(modName//'::'//myName// " - "// &
  !   & 'Matrix field object is already initiated')
  ! !> print info
  ! CALL e%raiseInformation(modName//"::"//myName//" - "// &
  !   & "IMPORTING MATRIX FIELD")
  ! !> check
  ! IF( .NOT. hdf5%isOpen() ) THEN
  !   CALL e%raiseError(modName//'::'//myName// &
  !   & 'HDF5 file is not opened')
  ! END IF
  ! !> check
  ! IF( .NOT. hdf5%isRead() ) THEN
  !   CALL e%raiseError(modName//'::'//myName// &
  !   & 'HDF5 file does not have read permission')
  ! END IF
  ! ! READ fieldType
  ! dsetname=trim(group)//"/fieldType"
  ! IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
  !     CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=strval)
  !     SELECT CASE( TRIM(strval%chars()) )
  !     CASE( "NORMAL" )
  !       fieldType = FIELD_TYPE_NORMAL
  !     CASE( "CONSTANT" )
  !       fieldType = FIELD_TYPE_CONSTANT
  !     CASE( "CONSTANT_SPACE" )
  !       fieldType = FIELD_TYPE_CONSTANT_SPACE
  !     CASE( "CONSTANT_TIME" )
  !       fieldType = FIELD_TYPE_CONSTANT_TIME
  !     END SELECT
  ! ELSE
  !   fieldType = FIELD_TYPE_NORMAL
  ! END IF
  ! ! READ name
  ! dsetname=trim(group)//"/name"
  ! IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
  !   CALL e%raiseError(modName//'::'//myName// &
  !   & 'The dataset name should be present')
  ! END IF
  ! CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=name)
  ! ! READ matrixProp
  ! dsetname=trim(group)//"/matrixProp"
  ! IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
  !   CALL e%raiseError(modName//'::'//myName// &
  !   & 'The dataset matrixProp should be present')
  ! END IF
  ! CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=matrixProp)
  ! ! READ spaceCompo
  ! dsetname=trim(group)//"/spaceCompo"
  ! IF( .NOT. hdf5%pathExists(trim(dsetname%chars()))) THEN
  !   CALL e%raiseError(modName//'::'//myName// &
  !   & 'The dataset spaceCompo should be present')
  ! END IF
  ! CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=spaceCompo)
  ! ! READ timeCompo
  ! dsetname=trim(group)//"/timeCompo"
  ! IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
  !   CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=timeCompo)
  ! ELSE
  !   timeCompo = 1
  ! END IF
  ! ! READ restart
  ! dsetname=trim(group)//"/restart"
  ! IF( hdf5%pathExists(trim(dsetname%chars()))) THEN
  !   CALL hdf5%read(dsetname=trim(dsetname%chars()),vals=restart)
  ! ELSE
  !   restart = .FALSE.
  ! END IF
  ! IF( .NOT. restart ) THEN
  !   CALL FPL_INIT(); CALL param%initiate()
  !   CALL setMatrixFieldParam( param=param, &
  !     & name=trim(name%chars()), &
  !     & matrixProp=trim(matrixProp%chars()), &
  !     & spaceCompo=spaceCompo, &
  !     & timeCompo = timeCompo, &
  !     & fieldType = fieldType )
  !   CALL obj%initiate( param=param, dom=dom )
  !   CALL param%deallocateData(); CALL FPL_FINALIZE()
  ! ELSE
  !   CALL e%raiseError(modName//'::'//myName// &
  !   & 'At present restart option is not available, we are working on it.' )
  ! END IF
END PROCEDURE mField_Import

END SUBMODULE IOMethods