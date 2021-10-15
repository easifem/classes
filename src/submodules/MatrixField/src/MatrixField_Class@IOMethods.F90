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

SUBMODULE(MatrixField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Display
  INTEGER( I4B ) :: ii
  !> main
  IF( .NOT. obj%isInitiated ) THEN
    CALL Display( "MatrixField is not initiated", unitNo=unitNo )
    RETURN
  END IF
  CALL Display( "#"//TRIM(msg), unitNo=unitNo )
  CALL Display( obj%name//'',  msg="# name : ", unitNo=unitNo )
  CALL Display( obj%fieldType, msg='# fieldType : ', unitNo=unitNo )
  CALL Display( obj%engine, msg='# engine : ', unitNo=unitNo )
  IF( ASSOCIATED( obj%domain ) ) THEN
    CALL Display( "# domain : ASSOCIATED", unitNo=unitNo )
  ELSE
    CALL Display( "# domain : NOT ASSOCIATED", unitNo=unitNo )
  END IF
  IF( ALLOCATED( obj%domains ) ) THEN
    CALL Display( "# domains : ALLOCATED [" &
      & // TOSTRING(SIZE(obj%domains)) &
      & // "]", unitNo=unitNo )
    DO ii = 1, SIZE( obj%domains )
      IF( ASSOCIATED(obj%domains(ii)%ptr) ) THEN
        CALL Display( "# domains(" // TOSTRING(ii) &
          & // ")%ptr : ASSOCIATED", unitNo=unitNo )
      ELSE
        CALL Display( "# domains(" // TOSTRING(ii)  &
          & // ")%ptr : NOT ASSOCIATED", unitNo=unitNo )
      END IF
    END DO
  ELSE
    CALL Display( "# domains : NOT ALLOCATED", unitNo=unitNo )
  END IF
  ! CALL Display( obj%mat, msg="# mat : ", unitNo=unitNo ))
END PROCEDURE mField_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Import"
  TYPE( String ) :: strval, dsetname, name, matrixProp, engine
  INTEGER( I4B ) :: timeCompo, spaceCompo, fieldType
  LOGICAL( LGT ) :: restart
  TYPE( ParameterList_ ) :: param
  ! main program
  IF( obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Matrix field object is already initiated')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing an Instance of MatrixField_")
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
  ! fieldType
  dsetname=TRIM(group)//"/fieldType"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
      CALL hdf5%read(dsetname=TRIM(dsetname%chars()),vals=strval)
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
  ! name
  dsetname=TRIM(group)//"/name"
  IF( .NOT. hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset name should be present')
  ELSE
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()),vals=name)
  END IF
  ! engine
  dsetname=TRIM(group)//"/engine"
  IF( .NOT. hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    engine="NATIVE_SERIAL"
  ELSE
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()),vals=engine)
  END IF
  ! matrixProp
  dsetname=TRIM(group)//"/matrixProp"
  IF( .NOT. hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset matrixProp should be present')
  ELSE
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()),vals=matrixProp)
  END IF
  ! spaceCompo
  dsetname=TRIM(group)//"/spaceCompo"
  IF( .NOT. hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName// &
    & 'The dataset spaceCompo should be present')
  END IF
  CALL hdf5%read(dsetname=TRIM(dsetname%chars()),vals=spaceCompo)
  ! timeCompo
  dsetname=TRIM(group)//"/timeCompo"
  IF( hdf5%pathExists(TRIM(dsetname%chars()))) THEN
    CALL hdf5%read(dsetname=TRIM(dsetname%chars()),vals=timeCompo)
  ELSE
    timeCompo = 1
  END IF
  CALL FPL_INIT(); CALL param%initiate()
  CALL setMatrixFieldParam( param=param, &
    & name=TRIM(name%chars()), &
    & matrixProp=TRIM(matrixProp%chars()), &
    & spaceCompo=spaceCompo, &
    & timeCompo = timeCompo, &
    & fieldType = fieldType )
  CALL obj%initiate( param=param, dom=dom )
  CALL param%deallocateData(); CALL FPL_FINALIZE()
END PROCEDURE mField_Import

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ImportPmat
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_ImportPmat"
END PROCEDURE mField_ImportPmat

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Export"
  TYPE( String ) :: dname
  TYPE( DOF_ ), POINTER :: dofobj
  INTEGER( I4B ) :: ii

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Instnace of MatrixField_ is not initiated')
  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Exporting Instance of MatrixField_")
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
  !> fieldType
  dname = TRIM( group ) // "/fieldType"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%fieldType )
  !> name
  dname = TRIM( group ) // "/name"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=TRIM(obj%name%chars()) )
  !> engine
  dname = TRIM( group ) // "/engine"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%engine)
  !> isPmatInitiated
  dname = TRIM( group ) // "/isPmatInitiated"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%isPmatInitiated )
  !> dofobj
  dofobj =>NULL()
  dofobj => getDOFPointer(obj%mat)
  dname = TRIM( group ) // "/tPhysicalVarNames"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=(.tNames. dofobj) )
  DO ii = 1, (.tNames. dofobj)
    dname= TRIM( group ) // "/physicalVarName"//TOSTRING(ii)
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=STRING(dofobj .Names. ii) )
  END DO
  dname = TRIM( group ) // "/spaceCompo"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=( .SpaceComponents. dofobj) )
  dname = TRIM( group ) // "/timeCompo"
  CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=( .TimeComponents. dofobj) )
  ! > mat
  CALL ExportCSRMatrix(obj=obj%mat, hdf5=hdf5, group=TRIM( group ) // "/mat")
  !> pmat
  CALL obj%ExportPmat( hdf5=hdf5, group=group )
  NULLIFY( dofobj )
END PROCEDURE mField_Export

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ExportPmat
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_ExportPmat"
  TYPE( String ) :: dname
  IF( obj%isPmatInitiated ) THEN
    !> pmat/pmatName
    dname = TRIM( group ) // "/pmat/pmatName"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%pmatName )
    !> pmat/nnz
    dname = TRIM( group ) // "/pmat/nnz"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%nnz )
    !> pmat/ncol
    dname = TRIM( group ) // "/pmat/ncol"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%ncol )
    !> pmat/nrow
    dname = TRIM( group ) // "/pmat/nrow"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%nrow )
    !> pmat/isInitiated
    dname = TRIM( group ) // "/pmat/isInitiated"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%isInitiated )
    !> pmat/lfil
    dname = TRIM( group ) // "/pmat/lfil"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%lfil )
    !> pmat/mbloc
    dname = TRIM( group ) // "/pmat/mbloc"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%mbloc )
    !> pmat/alpha
    dname = TRIM( group ) // "/pmat/alpha"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%alpha )
    !> pmat/droptol
    dname = TRIM( group ) // "/pmat/droptol"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%droptol )
    !> pmat/permtol
    dname = TRIM( group ) // "/pmat/permtol"
    CALL hdf5%write(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%permtol )
    !> pmat/A
    IF( ALLOCATED(obj%pmat%A) ) THEN
      dname = TRIM( group ) // "/pmat/A"
      CALL hdf5%write(dsetname=TRIM(dname%chars()), &
      & vals=obj%pmat%A )
    END IF
    !> pmat/JA
    IF( ALLOCATED(obj%pmat%JA) ) THEN
      dname = TRIM( group ) // "/pmat/JA"
      CALL hdf5%write(dsetname=TRIM(dname%chars()), &
      & vals=obj%pmat%JA )
    END IF
    !> pmat/IA
    IF( ALLOCATED(obj%pmat%IA) ) THEN
      dname = TRIM( group ) // "/pmat/IA"
      CALL hdf5%write(dsetname=TRIM(dname%chars()), &
      & vals=obj%pmat%IA )
    END IF
    !> pmat/JU
    IF( ALLOCATED(obj%pmat%JU) ) THEN
      dname = TRIM( group ) // "/pmat/JU"
      CALL hdf5%write(dsetname=TRIM(dname%chars()), &
      & vals=obj%pmat%JU )
    END IF
    !> pmat/IPERM
    IF( ALLOCATED(obj%pmat%IPERM) ) THEN
      dname = TRIM( group ) // "/pmat/IPERM"
      CALL hdf5%write(dsetname=TRIM(dname%chars()), &
      & vals=obj%pmat%IPERM )
    END IF
    !> pmat/LEVS
    IF( ALLOCATED(obj%pmat%LEVS) ) THEN
      dname = TRIM( group ) // "/pmat/LEVS"
      CALL hdf5%write(dsetname=TRIM(dname%chars()), &
      & vals=obj%pmat%LEVS )
    END IF
  END IF
END PROCEDURE mField_ExportPmat

!----------------------------------------------------------------------------
!                                                                      SPY
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_SPY
  CALL SPY( obj=obj%mat, filename=filename, ext=ext )
END PROCEDURE mField_SPY

END SUBMODULE IOMethods