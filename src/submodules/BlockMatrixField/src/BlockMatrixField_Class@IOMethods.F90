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
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Import"
  TYPE( String ) :: strval, dsetname, name, matrixProp, engine
  INTEGER( I4B ), ALLOCATABLE :: timeCompo(:), spaceCompo(:)
  INTEGER( I4B ) :: fieldType, ii, tvar
  CHARACTER( LEN = 1 ), ALLOCATABLE :: physicalVarNames( : )

  TYPE( ParameterList_ ) :: param
  ! main program
  IF( obj%IsInitiated ) &
    & CALL e%RaiseError(modName//'::'//myName// " - "// &
    & 'The instance of BloclMatrixField_ is already initiated')
  !> print info
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
    & "Importing an Instance of BlockMatrixField_")
  !> check
  IF( .NOT. hdf5%IsOpen() ) THEN
    CALL e%RaiseError(modName//'::'//myName// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%IsRead() ) THEN
    CALL e%RaiseError(modName//'::'//myName// &
    & 'HDF5 file does not have read permission')
  END IF
  ! fieldType
  dsetname=TRIM(group)//"/fieldType"
  IF( hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
      CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()),vals=strval)
      SELECT CASE( TRIM(strval%Chars()) )
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
  ! name
  dsetname=TRIM(group)//"/name"
  IF( .NOT. hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
    CALL e%RaiseError(modName//'::'//myName// &
    & 'The dataset name should be present')
  ELSE
    CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()),vals=name)
  END IF
  ! engine
  dsetname=TRIM(group)//"/engine"
  IF( .NOT. hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
    engine="NATIVE_SERIAL"
  ELSE
    CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()),vals=engine)
  END IF
  ! matrixProp
  dsetname=TRIM(group)//"/matrixProp"
  IF( .NOT. hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
    CALL e%RaiseError(modName//'::'//myName// &
    & 'The dataset matrixProp should be present')
  ELSE
    CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()),vals=matrixProp)
  END IF
  ! spaceCompo
  dsetname=TRIM(group)//"/spaceCompo"
  IF( .NOT. hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
    CALL e%RaiseError(modName//'::'//myName// &
    & 'The dataset spaceCompo should be present')
  ELSE
    CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()),vals=spaceCompo)
  END IF
  ! timeCompo
  dsetname=TRIM(group)//"/timeCompo"
  IF( hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
    CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()),vals=timeCompo)
  ELSE
    timeCompo = spaceCompo
    timeCompo = 1
  END IF
  ! physicalVarNames
  ALLOCATE( physicalVarNames( SIZE(timeCompo )) )
  DO ii = 1, SIZE( physicalVarNames )
    dsetname=TRIM(group)//"/physicalVarName"//TOSTRING(ii)
    IF( .NOT. hdf5%PathExists(TRIM(dsetname%Chars()))) THEN
      CALL e%RaiseError(modName//'::'//myName// &
      & 'The dataset ' // dsetname%Chars() // ' should be present')
    ELSE
      CALL hdf5%Read(dsetname=TRIM(dsetname%Chars()), &
        & vals=physicalVarNames(ii))
    END IF
  END DO
  !>
  CALL FPL_INIT(); CALL param%Initiate()
  CALL SetBlockMatrixFieldParam( param=param, &
    & name=TRIM(name%Chars()), &
    & physicalVarNames = physicalVarNames, &
    & matrixProp=TRIM(matrixProp%Chars()), &
    & spaceCompo=spaceCompo, &
    & timeCompo = timeCompo, &
    & fieldType = fieldType )
  IF( PRESENT( dom ) ) THEN
    CALL obj%Initiate( param=param, dom=dom )
  ELSE IF( PRESENT( domains ) ) THEN
    CALL obj%Initiate( param=param, dom=domains )
  ELSE
    CALL e%RaiseError(modName//'::'//myName// &
      & 'Either dom or domains should be present')
  END IF
  CALL param%DeallocateData(); CALL FPL_FINALIZE()
END PROCEDURE mField_Import

END SUBMODULE IOMethods