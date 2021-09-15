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

SUBMODULE( XMLFile_Class ) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_addSurrogate
  call e%addSurrogate( UserObj )
END PROCEDURE xmlFile_addSurrogate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Initiate
  CHARACTER( LEN = * ), PARAMETER :: myName='xmlFile_Initiate'
  TYPE( String ) :: fpath, fname, fext, mode_in
  CHARACTER(LEN=LEN(filename)) :: tempchars
  LOGICAL( LGT ) :: exists
  !>
  IF( obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// &
      & ' - xmlFile '//obj%getFileName()// &
      & ' is already initialized!')
    RETURN
  ENDIF
  !>
  CALL getPath( chars=filename, path=tempchars )
  fpath=trim(tempchars)
  CALL getFileNameExt( chars=filename, ext=tempchars )
  fext=trim(tempchars)
  CALL getFileName( chars=filename, fname=tempchars )
  fname=trim(tempchars)
  CALL obj%setFilePath( fpath )
  CALL obj%setFileName( fname )
  CALL obj%setFileExt( fext )
  mode_in=mode
  mode_in = mode_in%upper()
  SELECT CASE( TRIM( mode_in%chars() ) )
  CASE( "READ" )
    INQUIRE(FILE=filename,EXIST=exists)
    IF(exists) THEN
      CALL obj%setWriteStat(.FALSE.)
      CALL obj%setReadStat(.TRUE.)
    ELSE
      CALL e%raiseError(modName//'::'//myName// &
        & ' - XML file '//filename//' is being opened with '// &
        & 'mode READ but does not exist.' )
    END IF
  CASE( "WRITE" )
    INQUIRE( FILE=filename, EXIST=exists )
    IF( exists ) THEN
      CALL obj%setWriteStat( .TRUE. )
      CALL obj%setReadStat( .FALSE. )
    ELSE
      CALL e%raiseError( modName//'::'//myName// &
        & ' - XML file '//filename//' is being opened with '// &
        & 'mode WRITE but does not exist.')
    END IF
  CASE( "OVERWRITE" )
    INQUIRE( FILE=filename, EXIST=exists )
    IF( exists ) THEN
      CALL obj%setWriteStat( .TRUE. )
      CALL obj%setOverwriteStat( .TRUE. )
      CALL obj%setReadStat( .TRUE. )
    ELSE
      CALL e%raiseError( modName // '::' // myName // &
        & ' - XML file ' // filename // ' is being opened with ' // &
        & 'mode OVERWRITE but does not exist.')
    END IF
  CASE( "NEW" )
    CALL obj%setWriteStat( .TRUE. )
    CALL obj%setReadStat( .TRUE. )
    CALL obj%setNewStat( .TRUE. )
    CALL obj%setOverwriteStat( .TRUE. )
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Unrecognized access mode.')
  END SELECT
  obj%fullname=trim(filename)
  ALLOCATE(obj%root)
  obj%isInitiated = .TRUE.
END PROCEDURE xmlFile_Initiate

!----------------------------------------------------------------------------
!                                                           DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_DeallocateData
  LOGICAL( LGT ) :: bool
  !>
  IF( ASSOCIATED(obj%root) ) THEN
    CALL obj%root%DeallocateData()
    DEALLOCATE(obj%root)
  END IF
  !>
  IF( obj%isInitiated ) THEN
    !Logical to close or delete the file.
    IF( PRESENT(delete) ) THEN
      bool=delete
    ELSE
      bool=.FALSE.
    END IF
    IF(bool) THEN
      CALL obj%delete()
    ELSE
      CALL obj%close()
    END IF
    !>
    obj%isInitiated=.FALSE.
    obj%newstat=.FALSE.
    obj%fullname=''
    obj%unitno=-1
    obj%overwriteStat = .FALSE.
    obj%version=1.0
    obj%encoding='UTF-8'
    CALL obj%DeallocateBaseData()
  END IF
END PROCEDURE xmlFile_DeallocateData

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Final
  CALL obj%DeallocateData()
END PROCEDURE xmlFile_Final

!----------------------------------------------------------------------------
!                                                                       Open
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Open
  CHARACTER(LEN=*),PARAMETER :: myName='xmlFile_Open'
  !>
  CHARACTER( LEN=7 ) :: statusvar
  CHARACTER( LEN=10 ) :: accessvar
  CHARACTER( LEN=11 ) :: formvar
  CHARACTER( LEN=9 ) :: actionvar
  INTEGER( I4B ) :: ierr
  !>
  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// &
      & ' - The xmlFile is not initiated.')
  END IF
  !>
  IF( obj%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
      ' - File is already open!')
  END IF
  !>
  !> STATUS clause value
  IF( .NOT. obj%newstat ) THEN
    statusvar='OLD'
  ELSE
    IF( obj%overwriteStat ) THEN
      statusvar='REPLACE'
    ELSE
      statusvar='NEW'
    END IF
  END IF
  !> FORM clause value
  IF(obj%isFormatted()) THEN
    formvar='FORMATTED'
  ELSE
    formvar='UNFORMATTED'
  ENDIF
  !ACCESS clause value
  accessvar='SEQUENTIAL'
  !ACTION clause value
  IF( obj%isRead() .AND. .NOT.obj%isWrite() ) THEN
    actionvar='READ'
  ELSE IF( .NOT. obj%isRead() .AND. obj%isWrite() ) THEN
    actionvar='WRITE'
  ELSE IF( obj%isRead() .AND. obj%isWrite() ) THEN
    actionvar='READWRITE'
  END IF
  OPEN( &
    & NEWUNIT=obj%unitNo,FILE=TRIM(obj%fullname%chars()), &
    & STATUS=statusvar, ACCESS=accessvar, &
    & FORM=formvar,ACTION=actionvar, ENCODING=obj%encoding, &
    & IOSTAT=ierr)
  !>
  IF(ierr == 0) THEN
    CALL obj%setOpenStat(.TRUE.)
  ELSE
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Trouble opening file!')
    CALL obj%setOpenStat(.FALSE.)
  ENDIF
END PROCEDURE xmlFile_Open

!----------------------------------------------------------------------------
!                                                                      Close
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Close
  CHARACTER( LEN = * ), PARAMETER :: myName='xmlFile_Close'
  INTEGER( I4B ) :: ierr
  !>
  IF( obj%isOpen() ) THEN
    CLOSE( obj%unitNo, IOSTAT=ierr )
    IF(ierr .EQ. 0) THEN
      CALL obj%setOpenStat(.FALSE.)
    ELSE
      CALL e%raiseError(modName//'::'//myName// &
        & ' - trouble closing file!')
    END IF
  END IF
END PROCEDURE xmlFile_Close

!----------------------------------------------------------------------------
!                                                                    Delete
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Delete
  CHARACTER( LEN=* ), PARAMETER :: myName='xmlFile_Delete'
  INTEGER( I4B ) :: ierr

  IF( obj%isOpen() ) THEN
    CLOSE( obj%unitNo, STATUS='DELETE', IOSTAT=ierr )
  ELSE
    CALL obj%open()
    CLOSE( obj%unitNo, STATUS='DELETE', IOSTAT=ierr )
  ENDIF
  IF(ierr == 0) THEN
    CALL obj%setOpenStat(.FALSE.)
  ELSE
    CALL e%raiseError(modName//'::'//myName// &
      & ' - trouble closing file!')
  ENDIF
END PROCEDURE xmlFile_Delete

END SUBMODULE ConstructorMethods