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

SUBMODULE( FortranFile_Class ) Methods
USE BaseMethod
IMPLICIT NONE
CHARACTER(LEN=maxStrLen) :: emesg, iomsg
INTEGER( I4B ) :: ioerr
CONTAINS

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE ff_addSurrogate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_initiate
  CHARACTER(LEN=*),PARAMETER :: myName='ff_initiate'
  CHARACTER(LEN=7) :: statusval
  CHARACTER(LEN=10) :: accessval
  CHARACTER(LEN=11) :: formval
  CHARACTER(LEN=9) :: actionval
  CHARACTER(LEN=3) :: padval
  TYPE(String) :: fpath,fname,fext, file_
  LOGICAL(LGT) :: ostat
  INTEGER(I4B) :: oldcnt, ierr

  !Initialize data
  statusval=''
  accessval=''
  formval=''
  actionval=''
  padval=''

  oldcnt=e%getCounter(EXCEPTION_ERROR)

  IF(obj%initstat) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Fortran file has already been initialized!')
  ELSE
    !Initialize the file
    file_ = string(trim(file))
    fpath = file_%basedir() // '/'
    fext = file_%extension()
    fname = file_%basename(extension=fext%chars())

    CALL obj%setFilePath(fpath)
    CALL obj%setFileName(fname)
    CALL obj%setFileExt(fext)

    IF(PRESENT(unit)) THEN
      IF(unit == stdout) THEN
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for optional input argument UNIT! Value is equal to '// &
            'default OUTPUT_UNIT.')
      ELSEIF(unit == stderr) THEN
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for optional input argument UNIT! Value is equal to '// &
            'default ERROR_UNIT.')
      ELSEIF(unit == stdin) THEN
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for optional input argument UNIT! Value is equal to '// &
            'default INPUT_UNIT.')
      ELSE
        INQUIRE(UNIT=unit,OPENED=ostat)
        IF(ostat) THEN
          CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
              'value for optional input argument UNIT! Unit is being used'// &
              ' by another file!')
        ELSE
          obj%unitno=unit
          obj%getNewUnit = .FALSE.
        ENDIF
      ENDIF
    ELSE
      obj%getNewUnit = .TRUE.
    ENDIF

    !STATUS clause for OPEN statement
    IF(PRESENT(status)) THEN
      SELECT CASE(status)
      CASE('OLD')
        !File already exists
        statusval='OLD'
      CASE('NEW')
        !File does not exist and will be created
        statusval='NEW'
        ierr  = system_mkdir(fpath//'', RWX_U)
      CASE('SCRATCH')
        !File is deleted after execution (treated as replace)
        statusval='REPLACE'
        ierr  = system_mkdir(fpath//'', RWX_U)
      CASE('REPLACE')
        !File may or may not exist, if it does it is replaced
        statusval='REPLACE'
        ierr  = system_mkdir(fpath//'', RWX_U)
      CASE('UNKNOWN')
        !Processor/Compiler dependent behavior
        statusval='REPLACE'
        ierr  = system_mkdir(fpath//'', RWX_U)
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//status//') for optional input argument STATUS!')
      ENDSELECT
    ELSE
      !Default value for status
      statusval='REPLACE'
      ierr  = system_mkdir(fpath//'', RWX_U)
    ENDIF

    !ACCESS clause for OPEN statement
    IF(PRESENT(access)) THEN
      SELECT CASE(access)
      CASE('SEQUENTIAL')
        !File is accessed sequentially
        accessval=access
      CASE('DIRECT')
        !File has direct access
        accessval=access
      CASE('STREAM')
        !File has streaming access !F2003, might have problems.
        accessval=access
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
          & 'value ('//access//') for optional input argument ACCESS!')
      ENDSELECT
    ELSE
      !Default value
      accessval='SEQUENTIAL'
    ENDIF

    !FORM clause for OPEN statement
    IF(PRESENT(form)) THEN
      SELECT CASE(form)
      CASE('FORMATTED')
        !File is a text file
        formval=form
      CASE('UNFORMATTED')
        !File a binary file
        formval=form
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
          & 'value ('//form//') for optional input argument FORM!')
      ENDSELECT
    ELSE
      !Default value
      formval='FORMATTED'
    ENDIF

    !POSITION clause for OPEN statement
    IF(PRESENT(position)) THEN
      SELECT CASE(position)
      CASE('REWIND')
        !File opens at beginning of file
        obj%posopt=position
      CASE('APPEND')
        !File opens at end of file
        obj%posopt=position
      CASE('ASIS')
        !File opens with file pointer as is
        obj%posopt=position
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
          & 'value ('//position//') for optional input argument POSITION!')
      ENDSELECT
    ELSE
      obj%posopt = 'ASIS'
    ENDIF

    !ACTION clause for OPEN statement
    IF(PRESENT(action)) THEN
      SELECT CASE(action)
      CASE('READ') !File opens with read access only
        actionval=action
      CASE('WRITE') !File opens with write access only
        actionval=action
      CASE('READWRITE') !File opens with read write access
        actionval=action
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//action//') for optional input argument ACTION!')
      ENDSELECT
    ELSE
      !Default value
      actionval='READWRITE'
    ENDIF

    IF(PRESENT(pad)) THEN
      SELECT CASE(pad)
      CASE('YES') !File is padded
        padval=pad
      CASE('NO') !File is not padded
        padval=pad
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
          & 'value ('//pad//') for optional input argument PAD!')
      ENDSELECT
    ELSE
      !Fortran default value
      padval='YES'
    ENDIF

    IF(PRESENT(recl)) THEN
      IF(recl < 1) THEN
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
          & 'value for input option RECL must be set to greater than 0!')
      ELSE
        obj%reclval=recl
      ENDIF
    ENDIF

    IF( PRESENT( comment ) ) THEN
      obj%comment = comment
    END IF

    IF( PRESENT( separator ) ) THEN
      obj%separator = separator
    END IF

    IF( PRESENT( delimiter ) ) THEN
      obj%delimiter = delimiter
    END IF

    IF(TRIM(statusval) /= 'OLD') THEN
      obj%newstat=.TRUE.
      obj%overwrite=(TRIM(statusval) == 'REPLACE')
    ENDIF
    obj%formatstat=(TRIM(formval) == 'FORMATTED')
    obj%padstat=(TRIM(padval) ==  'YES')
    IF(TRIM(accessval) == 'DIRECT' .OR. TRIM(accessval) == 'STREAM') THEN
      obj%accessstat=.TRUE.
      IF(obj%reclval < 1) CALL e%raiseError(modName//'::'// &
        & myName//' - Record length must be set to greater than 0 for '// &
        & 'direct access files!')
    ENDIF

    IF(TRIM(actionval) == 'READ') THEN
      CALL obj%setReadStat(.TRUE.)
      IF(obj%newstat) CALL e%raiseError(modName//'::'// &
        & myName//' - Cannot have a new file with a read only status!')
    ELSEIF(TRIM(actionval) == 'WRITE') THEN
      CALL obj%setWriteStat(.TRUE.)
    ELSEIF(TRIM(actionval) == 'READWRITE') THEN
      CALL obj%setReadStat(.TRUE.)
      CALL obj%setWriteStat(.TRUE.)
    ENDIF

    IF(oldcnt < e%getCounter(EXCEPTION_ERROR)) THEN
      CALL e%raiseError(modName//'::'//myName//' - Exceptions '// &
          'during file initialization! File not initialized!')
      !Reset all attributes if initialization failed.
      obj%unitno=-1
      obj%formatstat=.FALSE.
      obj%accessstat=.FALSE.
      obj%newstat=.FALSE.
      obj%overwrite=.FALSE.
      obj%reclval=-1
      obj%padstat=.FALSE.
      obj%posopt='ASIS  '
      CALL obj%setFilePath(string(''))
      CALL obj%setFileName(string(''))
      CALL obj%setFileExt(string(''))
      CALL obj%setEOFstat(.FALSE.)
      CALL obj%setOpenStat(.FALSE.)
      CALL obj%setReadStat(.FALSE.)
      CALL obj%setWriteStat(.FALSE.)
    ELSE
      obj%initstat=.TRUE.
    ENDIF
  ENDIF
END PROCEDURE ff_initiate

!----------------------------------------------------------------------------
!                                                                 clear
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_clear
  LOGICAL(LGT) :: bool

  !Close the file
  bool=.FALSE.
  IF(PRESENT(delete)) bool=delete
  IF(obj%initstat) THEN
    IF(bool) THEN
      CALL obj%delete()
    ELSE
      CALL obj%close()
    ENDIF
  ENDIF

  !Set FortranFileType attributes to defaults
  obj%initstat=.FALSE.
  obj%unitno=-1
  obj%formatstat=.FALSE.
  obj%accessstat=.FALSE.
  obj%newstat=.FALSE.
  obj%overwrite=.FALSE.
  obj%reclval=-1
  obj%padstat=.FALSE.
  obj%posopt='ASIS  '
  obj%comment = hash
  obj%separator = comma
  obj%delimiter = '\n'
  obj%getNewUnit = .FALSE.
  !Set BaseFileType attributes to default
  CALL aFile_DeallocateData(obj)
END PROCEDURE ff_clear

!----------------------------------------------------------------------------
!                                                                 open
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_open
  CHARACTER(LEN=*),PARAMETER :: myName='ff_open'
  CHARACTER(LEN=7) :: statusvar
  CHARACTER(LEN=10) :: accessvar
  CHARACTER(LEN=11) :: formvar
  CHARACTER(LEN=9) :: actionvar
  CHARACTER(LEN=3) :: padvar
  CHARACTER(LEN=256) :: iomsg, emesg
  INTEGER(I4B) :: reclval, ioerr
  TYPE( String ) :: path, filename, ext

  !Get the appropriate clause values for the OPEN statement
  IF(obj%initstat) THEN
    IF(obj%isOpen()) THEN
      WRITE(iomsg,'(a,i4,a)') 'Cannot open file (UNIT=', &
          obj%unitno,') File is already open!'
      CALL e%raiseError(modName//'::'//myName//' - '//TRIM(iomsg))
    ELSE
      path = obj%getFilePath()
      filename = obj%getFileName()
      ext = obj%getFileExt()
      !STATUS clause value
      IF(.NOT.obj%isNew()) THEN
        statusvar='OLD'
      ELSE
        IF(obj%overwrite) THEN
          statusvar='REPLACE'
        ELSE
          statusvar='NEW'
        ENDIF
      ENDIF
      !FORM clause value
      IF(obj%isFormatted()) THEN
        formvar='FORMATTED'
      ELSE
        formvar='UNFORMATTED'
      ENDIF
      !ACCESS clause value
      IF(obj%isDirect()) THEN
        accessvar='DIRECT'
        reclval=obj%reclval
      ELSE
        accessvar='SEQUENTIAL'
        reclval=0
      ENDIF
      !ACTION clause value
      IF(obj%isRead() .AND. .NOT.obj%isWrite()) THEN
        actionvar='READ'
      ELSEIF(.NOT.obj%isRead() .AND. obj%isWrite()) THEN
        actionvar='WRITE'
      ELSEIF(obj%isRead() .AND. obj%isWrite()) THEN
        actionvar='READWRITE'
      ENDIF
      !PAD clause value
      IF(obj%padstat) THEN
        padvar='YES'
      ELSE
        padvar='NO'
      ENDIF
      !The POSITION clause is illegal to use in the OPEN statement if
      !the file is DIRECT access.
      !The PAD clause is illegal to use in the OPEN statement if the file
      !is UNFORMATTED.
      IF(obj%isDirect()) THEN
        IF(obj%isFormatted()) THEN
          !Omit the POSITION clause, and include the PAD clause
          IF( obj%getNewUnit ) THEN
            OPEN(NEWUNIT=obj%unitno,STATUS=TRIM(statusvar),PAD=TRIM(padvar), &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),RECL=reclval, &
              & ACTION=TRIM(actionvar),FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()), &
              & IOSTAT=ioerr,IOMSG=iomsg)
          ELSE
            OPEN(UNIT=obj%unitno,STATUS=TRIM(statusvar),PAD=TRIM(padvar), &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),RECL=reclval, &
              & ACTION=TRIM(actionvar),FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()), &
              & IOSTAT=ioerr,IOMSG=iomsg)
          END IF
        ELSE
          !Omit the POSITION clause, and the PAD clause
          IF( obj%getNewUnit ) THEN
            OPEN(NEWUNIT=obj%unitno,STATUS=TRIM(statusvar),RECL=reclval, &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              & ACTION=TRIM(actionvar),FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()),IOMSG=iomsg)
          ELSE
            OPEN(UNIT=obj%unitno,STATUS=TRIM(statusvar),RECL=reclval, &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              & ACTION=TRIM(actionvar),FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()),IOMSG=iomsg)
          END IF
        ENDIF
      ELSE
        IF(obj%isFormatted()) THEN
          !Include the POSITION clause, and the PAD clause
          IF( obj%getNewUnit ) THEN
            OPEN(NEWUNIT=obj%unitno,STATUS=TRIM(statusvar),PAD=TRIM(padvar), &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              & POSITION=TRIM(obj%posopt),ACTION=TRIM(actionvar), &
              & FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()),IOMSG=iomsg)
          ELSE
            OPEN(UNIT=obj%unitno,STATUS=TRIM(statusvar),PAD=TRIM(padvar), &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              & POSITION=TRIM(obj%posopt),ACTION=TRIM(actionvar), &
              & FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()),IOMSG=iomsg)
          END IF
        ELSE
          !Include the POSITION clause, omit the PAD clause
          IF( obj%getNewUnit ) THEN
            OPEN(NEWUNIT=obj%unitno,STATUS=TRIM(statusvar), &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              & POSITION=TRIM(obj%posopt),ACTION=TRIM(actionvar), &
              & FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()),IOMSG=iomsg)
          ELSE
            OPEN(UNIT=obj%unitno,STATUS=TRIM(statusvar), &
              & ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              & POSITION=TRIM(obj%posopt),ACTION=TRIM(actionvar), &
              & FILE=TRIM(path%chars())// &
              & TRIM(filename%chars())//TRIM(ext%chars()),IOMSG=iomsg)
          ENDIF
        ENDIF
      ENDIF
      IF(ioerr .NE. 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error opening file "'// &
            & TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()) &
            & //'" (UNIT=',obj%unitno, ') IOSTAT=',ioerr
        CALL e%raiseError(modName//'::'//myName//' - '//TRIM(emesg) &
            //' IOMSG="'//TRIM(iomsg)//'"')
      ELSE
        CALL obj%setOpenStat(.TRUE.)
        CALL obj%setEOFStat(.FALSE.)
      ENDIF
    ENDIF
  ELSE
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Cannot open file! Object has not been initialized!')
  ENDIF
END PROCEDURE ff_open

!----------------------------------------------------------------------------
!                                                                 close
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_close
  CHARACTER(LEN=*),PARAMETER :: myName='FF_CLOSE'

  IF(obj%initstat) THEN
    IF(obj%isOpen()) THEN
      CLOSE(UNIT=obj%unitno,STATUS='KEEP',IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error closing file (UNIT=', &
          & obj%unitno,') IOSTAT=',ioerr
        CALL e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL obj%setOpenStat(.FALSE.)
      ENDIF
    ELSE
      WRITE(emesg,'(a,i4,a)') 'Cannot close file (UNIT=', &
        & obj%unitno,') File is not open!'
      CALL e%raiseDebug(modName//'::'//myName//' - '//emesg)
    ENDIF
  ELSE
    CALL e%raiseDebug(modName//'::'//myName//' - '// &
      & 'Cannot close file! File object has not been initialized!')
  ENDIF
END PROCEDURE ff_close

!----------------------------------------------------------------------------
!                                                                 delete
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_delete
  CHARACTER(LEN=*),PARAMETER :: myName='FF_DELETE'
  TYPE( String ) :: path, filename, ext

  IF(obj%initstat) THEN
    IF(obj%isOpen()) THEN
      CLOSE(UNIT=obj%unitno,STATUS='DELETE',IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
          & obj%unitno,') IOSTAT=',ioerr
        CALL e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL obj%setOpenStat(.FALSE.)
      ENDIF
    ELSE
      path = obj%getFilePath()
      filename = obj%getFileName()
      ext = obj%getFileExt()

      OPEN( UNIT=obj%unitno, &
        & FILE=TRIM(path%chars()) // &
        & TRIM(filename%chars()) // &
        & TRIM(ext%chars()), &
        & IOMSG=iomsg, &
        & IOSTAT=ioerr )

      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') &
          & 'Error deleting file (UNIT=', &
          & obj%unitno,') IOSTAT=',ioerr
        CALL e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
      CLOSE(UNIT=obj%unitno,STATUS='DELETE',IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
          & obj%unitno,') IOSTAT=',ioerr
        CALL e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL obj%setOpenStat(.FALSE.)
      ENDIF
    ENDIF
  ELSE
    CALL e%raiseDebug(modName//'::'//myName//' - '// &
      & 'Cannot delete file! File object has not been initialized!')
  ENDIF
END PROCEDURE ff_delete

!----------------------------------------------------------------------------
!                                                                 Backspace
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_backspace
  CHARACTER(LEN=*),PARAMETER :: myName='FF_BACKSPACE'
  INTEGER( I4B ) :: ioerr

  IF(obj%initstat) THEN
    IF(obj%isOpen()) THEN
      BACKSPACE(UNIT=obj%unitno,IOSTAT=ioerr, IOMSG=iomsg)
      IF(ioerr .NE. 0) THEN
        WRITE(emesg,'(a,i4,a,i4,a)') 'Error backspacing file (UNIT=', &
          & obj%unitno,') IOSTAT=',ioerr, ' IOMSG='//TRIM(iomsg)
        CALL e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        IF(obj%isEOF()) CALL obj%setEOFstat(.FALSE.)
      ENDIF
    ELSE
      WRITE(emesg,'(a,i4,a)') 'Cannot backspace file (UNIT=',obj%unitno, &
        & '). File not is not open!'
      CALL e%raiseDebug(modName//'::'//myName//' - '//emesg)
    ENDIF
  ELSE
    CALL e%raiseDebug(modName//'::'// myName//' - '// &
      & 'Cannot backspace file! File object has not been initialized!')
  ENDIF
END PROCEDURE ff_backspace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_rewind
  CHARACTER(LEN=*),PARAMETER :: myName='FF_REWIND'
  IF(obj%initstat) THEN
    IF(obj%isOpen()) THEN
      REWIND(UNIT=obj%unitno,IOSTAT=ioerr, IOMSG=iomsg )
      CALL obj%setEOFstat(.FALSE.)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4,a)') 'Error rewinding file (UNIT=', &
          & obj%unitno,') IOSTAT=',ioerr, ' IOMSG='//TRIM(iomsg)
        CALL e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL e%raiseInformation(modName//'::'//myName//' - '// &
          & 'REWIND of Fortran File unitNo='//TOSTRING(obj%unitNo)// &
          & ' [OK!]' )
      ENDIF
    ELSE
      WRITE(emesg,'(a,i4,a)') 'Cannot rewind file (UNIT=',obj%unitno, &
        & '). File not is not open!'
      CALL e%raiseError(modName//'::'//myName//' - '//emesg)
    ENDIF
  ELSE
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Cannot rewind file! File object has not been initialized!')
  ENDIF
END PROCEDURE ff_rewind

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_getUnitNo
  ans = obj%unitno
END PROCEDURE ff_getUnitNo

!----------------------------------------------------------------------------
!                                                                isFormatted
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isFormatted
  ans=obj%formatstat
END PROCEDURE ff_isFormatted

!----------------------------------------------------------------------------
!                                                                   isDirect
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isDirect
  ans=obj%accessstat
END PROCEDURE ff_isDirect

!----------------------------------------------------------------------------
!                                                                 getRecLen
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_getRecLen
  ans=obj%reclval
END PROCEDURE ff_getRecLen

!----------------------------------------------------------------------------
!                                                                   isPadded
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isPadded
  ans=obj%padstat
END PROCEDURE ff_isPadded

!----------------------------------------------------------------------------
!                                                                   isNew
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isNew
  ans=obj%newstat
END PROCEDURE ff_isNew

!----------------------------------------------------------------------------
!                                                               isOverwrite
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isOverwrite
  ans=obj%overwrite
END PROCEDURE ff_isOverwrite

!----------------------------------------------------------------------------
!                                                               isInit
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_isInit
  ans=obj%initstat
END PROCEDURE ff_isInit

!----------------------------------------------------------------------------
!                                                                 setStatus
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_setStatus
  CHARACTER(LEN=*),PARAMETER :: myName='setStatus_fortran_file'
  TYPE( String ) :: new_status

  IF(obj%initstat) THEN
    IF(.NOT.obj%isOpen()) THEN
      new_status=status
      new_status=new_status%upper()
      SELECT CASE(new_status%chars())
      CASE('OLD')
        !!File already exists
        obj%newstat=.FALSE.
        obj%overwrite=.FALSE.
      CASE('NEW')
        !!File does not exist and will be created
        obj%newstat=.TRUE.
        obj%overwrite=.FALSE.
      CASE('SCRATCH','REPLACE','UNKNOWN')
        obj%newstat=.TRUE.
        obj%overwrite=.TRUE.
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
          & 'value ('//status//') for input argument STATUS!')
      ENDSELECT
    ELSE
      CALL e%raiseError(modName//'::'//myName//' - File status '// &
        & 'Fortran file is not opened')
    ENDIF
  ELSE
    CALL e%raiseError(modName//'::'//myName//' - File status '// &
      & 'Fortran file is not initiated.')
  ENDIF
END PROCEDURE ff_setStatus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods