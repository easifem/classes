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

SUBMODULE(FortranFile_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
! CHARACTER(maxStrLen) :: emesg, iomsg
! INTEGER(I4B) :: ioerr
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_initiate
CHARACTER(*), PARAMETER :: myName = 'ff_initiate'
CHARACTER(7) :: statusval
CHARACTER(10) :: accessval
CHARACTER(11) :: formval
CHARACTER(9) :: actionval
CHARACTER(3) :: padval
TYPE(String) :: fpath, fname, fext, file_
LOGICAL(LGT) :: ostat
INTEGER(I4B) :: oldcnt, ierr

CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr
!
! Initialize data
!
statusval = ''
accessval = ''
formval = ''
actionval = ''
padval = ''
oldcnt = e%getCounter(EXCEPTION_ERROR)
!
! check
!
IF (obj%initstat) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'Fortran file has already been initialized!')
ELSE
  !
  !Initialize the file
  !
  file_ = TRIM(filename)
  IF (file_%SCAN(CHAR_SLASH) .EQ. 0_I4B) THEN
    fpath = "."//CHAR_SLASH
  ELSE
    fpath = file_%basedir(sep=CHAR_SLASH)//CHAR_SLASH
  END IF
  !
  fext = file_%extension()
  fname = file_%basename(extension=fext%chars(), sep=CHAR_SLASH)
  CALL obj%SetFilePath(fpath)
  CALL obj%SetFileName(fname)
  CALL obj%SetFileExt(fext)
  !
  IF (PRESENT(unit)) THEN
    !
    IF (unit == stdout) THEN
      !
      CALL e%RaiseError(modName//'::'//myName// &
        & ' - Illegal '// &
        & 'value for optional input argument UNIT! Value is equal to '// &
        & 'default OUTPUT_UNIT.')
      !
    ELSEIF (unit == stderr) THEN
      !
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value for optional input argument UNIT! Value is equal to '// &
        & 'default ERROR_UNIT.')
      !
    ELSEIF (unit == stdin) THEN
      !
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value for optional input argument UNIT! Value is equal to '// &
        & 'default INPUT_UNIT.')
      !
    ELSE
      !
      INQUIRE (UNIT=unit, OPENED=ostat)
      !
      IF (ostat) THEN
        !
        CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
          & 'value for optional input argument UNIT! Unit is being used'// &
          & ' by another file!')
        !
      ELSE
        !
        obj%unitno = unit
        obj%getNewUnit = .FALSE.
        !
      END IF
      !
    END IF
    !
  ELSE
    obj%getNewUnit = .TRUE.
  END IF
  !
  ! STATUS clause for OPEN statement
  !
  IF (PRESENT(status)) THEN
    statusval = UpperCase(status)
  ELSE
    statusval = 'REPLACE'
  END IF
  !
  IF (TRIM(statusval) .NE. 'OLD') THEN
    ierr = system_mkdir(fpath//'', RWX_U)
    IF (ierr .NE. 0_I4B .AND. ierr .NE. -1_I4B) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & 'error occured while creating the directory')
    END IF
  END IF
  !
  ! ACCESS clause for OPEN statement
  !
  IF (PRESENT(access)) THEN
    SELECT CASE (access)
    CASE ('SEQUENTIAL')
      !File is accessed sequentially
      accessval = access
    CASE ('DIRECT')
      !File has direct access
      accessval = access
    CASE ('STREAM')
      !File has streaming access !F2003, might have problems.
      accessval = access
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value ('//access//') for optional input argument ACCESS!')
    END SELECT
  ELSE
    !Default value
    accessval = 'SEQUENTIAL'
  END IF
  !
  ! FORM clause for OPEN statement
  !
  IF (PRESENT(form)) THEN
    SELECT CASE (form)
    CASE ('FORMATTED')
      !File is a text file
      formval = form
    CASE ('UNFORMATTED')
      !File a binary file
      formval = form
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value ('//form//') for optional input argument FORM!')
    END SELECT
  ELSE
    !Default value
    formval = 'FORMATTED'
  END IF
  !
  ! POSITION clause for OPEN statement
  !
  IF (PRESENT(position)) THEN
    SELECT CASE (position)
    CASE ('REWIND')
      !File opens at beginning of file
      obj%posopt = position
    CASE ('APPEND')
      !File opens at end of file
      obj%posopt = position
    CASE ('ASIS')
      !File opens with file pointer as is
      obj%posopt = position
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value ('//position//') for optional input argument POSITION!')
    END SELECT
  ELSE
    obj%posopt = 'ASIS'
  END IF
  !
  ! ACTION clause for OPEN statement
  !
  IF (PRESENT(action)) THEN
    SELECT CASE (action)
    CASE ('READ') !File opens with read access only
      actionval = action
    CASE ('WRITE') !File opens with write access only
      actionval = action
    CASE ('READWRITE') !File opens with read write access
      actionval = action
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value ('//action//') for optional input argument ACTION!')
    END SELECT
  ELSE
    !Default value
    actionval = 'READWRITE'
  END IF
  !
  ! padding
  !
  IF (PRESENT(pad)) THEN
    SELECT CASE (pad)
    CASE ('YES') !File is padded
      padval = pad
    CASE ('NO') !File is not padded
      padval = pad
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value ('//pad//') for optional input argument PAD!')
    END SELECT
  ELSE
    !Fortran default value
    padval = 'YES'
  END IF
  !
  ! record length
  !
  IF (PRESENT(recl)) THEN
    IF (recl < 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - Illegal '// &
        & 'value for input option RECL must be set to greater than 0!')
    ELSE
      obj%reclval = recl
    END IF
  END IF
  !
  ! comment
  !
  IF (PRESENT(comment)) THEN
    obj%comment = comment
  END IF
  !
  ! separator
  !
  IF (PRESENT(separator)) THEN
    obj%separator = separator
  END IF
  !
  ! delimiter
  !
  IF (PRESENT(delimiter)) THEN
    obj%delimiter = delimiter
  END IF
  !
  ! setStatus
  !
  CALL obj%SetStatus(statusval)
  !
  ! IF (TRIM(statusval) .NE. 'OLD') THEN
  !   obj%newstat = .TRUE.
  !   obj%overwrite = (TRIM(statusval) == 'REPLACE')
  ! END IF
  !
  obj%formatstat = (TRIM(formval) == 'FORMATTED')
  obj%padstat = (TRIM(padval) == 'YES')
  !
  IF (TRIM(accessval) == 'DIRECT' .OR. TRIM(accessval) == 'STREAM') THEN
    obj%accessstat = .TRUE.
    IF (obj%reclval < 1) CALL e%RaiseError(modName//'::'// &
      & myName//' - Record length must be set to greater than 0 for '// &
      & 'direct access files!')
  END IF
  !
  IF (TRIM(actionval) == 'READ') THEN
    CALL obj%SetReadStat(.TRUE.)
    IF (obj%newstat) CALL e%RaiseError(modName//'::'// &
      & myName//' - Cannot have a new file with a read only status!')
  ELSEIF (TRIM(actionval) == 'WRITE') THEN
    CALL obj%SetWriteStat(.TRUE.)
  ELSEIF (TRIM(actionval) == 'READWRITE') THEN
    CALL obj%SetReadStat(.TRUE.)
    CALL obj%SetWriteStat(.TRUE.)
  END IF
  !
  IF (oldcnt < e%getCounter(EXCEPTION_ERROR)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - Exceptions '// &
      & 'during file initialization! File not initialized!')
    !Reset all attributes if initialization failed.
    obj%unitno = -1
    obj%formatstat = .FALSE.
    obj%accessstat = .FALSE.
    obj%newstat = .FALSE.
    obj%overwrite = .FALSE.
    obj%reclval = -1
    obj%padstat = .FALSE.
    obj%posopt = 'ASIS  '
    CALL obj%SetFilePath(string(''))
    CALL obj%SetFileName(string(''))
    CALL obj%SetFileExt(string(''))
    CALL obj%SetEOFstat(.FALSE.)
    CALL obj%SetOpenStat(.FALSE.)
    CALL obj%SetReadStat(.FALSE.)
    CALL obj%SetWriteStat(.FALSE.)
  ELSE
    obj%initstat = .TRUE.
  END IF
END IF
END PROCEDURE ff_initiate

!----------------------------------------------------------------------------
!                                                                    clear
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_Deallocate
LOGICAL(LGT) :: bool

CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr

!Close the file
bool = .FALSE.
IF (PRESENT(delete)) bool = delete
IF (obj%initstat) THEN
  IF (bool) THEN
    CALL obj%delete()
  ELSE IF (obj%IsOpen()) THEN
    CALL obj%CLOSE()
  END IF
END IF
!Set FortranFileType attributes to defaults
obj%initstat = .FALSE.
obj%unitno = -1
obj%formatstat = .FALSE.
obj%accessstat = .FALSE.
obj%newstat = .FALSE.
obj%overwrite = .FALSE.
obj%reclval = -1
obj%padstat = .FALSE.
obj%posopt = 'ASIS  '
obj%comment = hash
obj%separator = " "
obj%delimiter = '\n'
obj%getNewUnit = .FALSE.
!Set BaseFileType attributes to default
CALL AbstractFileDeallocate(obj)
END PROCEDURE ff_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_Final
CALL obj%DEALLOCATE()
END PROCEDURE ff_Final

!----------------------------------------------------------------------------
!                                                                 open
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_open
CHARACTER(*), PARAMETER :: myName = 'ff_open'
CHARACTER(7) :: statusvar
CHARACTER(10) :: accessvar
CHARACTER(11) :: formvar
CHARACTER(9) :: actionvar
CHARACTER(3) :: padvar
INTEGER(I4B) :: reclval
TYPE(String) :: path, filename, ext
CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr

!Get the appropriate clause values for the OPEN statement
IF (obj%initstat) THEN
  IF (obj%IsOpen()) THEN
    WRITE (iomsg, '(a,i4,a)') 'Cannot open file (UNIT=', &
      obj%unitno, ') File is already open!'
    CALL e%RaiseError(modName//'::'//myName//' - '//TRIM(iomsg))
  ELSE
    path = obj%getFilePath()
    filename = obj%getFileName()
    ext = obj%getFileExt()
    !STATUS clause value
    IF (.NOT. obj%IsNew()) THEN
      statusvar = 'OLD'
    ELSE
      IF (obj%overwrite) THEN
        statusvar = 'REPLACE'
      ELSE
        statusvar = 'NEW'
      END IF
    END IF
    !FORM clause value
    IF (obj%IsFormatted()) THEN
      formvar = 'FORMATTED'
    ELSE
      formvar = 'UNFORMATTED'
    END IF
    !ACCESS clause value
    IF (obj%IsDirect()) THEN
      accessvar = 'DIRECT'
      reclval = obj%reclval
    ELSE
      accessvar = 'SEQUENTIAL'
      reclval = 0
    END IF
    !ACTION clause value
    IF (obj%IsRead() .AND. .NOT. obj%isWrite()) THEN
      actionvar = 'READ'
    ELSEIF (.NOT. obj%IsRead() .AND. obj%isWrite()) THEN
      actionvar = 'WRITE'
    ELSEIF (obj%IsRead() .AND. obj%isWrite()) THEN
      actionvar = 'READWRITE'
    END IF
    !PAD clause value
    IF (obj%padstat) THEN
      padvar = 'YES'
    ELSE
      padvar = 'NO'
    END IF
    !The POSITION clause is illegal to use in the OPEN statement if
    !the file is DIRECT access.
    !The PAD clause is illegal to use in the OPEN statement if the file
    !is UNFORMATTED.
    IF (obj%IsDirect()) THEN
      IF (obj%IsFormatted()) THEN
        !Omit the POSITION clause, and include the PAD clause
        IF (obj%getNewUnit) THEN
          OPEN ( &
            & NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), &
            & PAD=TRIM(padvar), &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), RECL=reclval, &
            & ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), &
            & IOSTAT=ioerr, IOMSG=iomsg)
        ELSE
          OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), PAD=TRIM(padvar), &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), RECL=reclval, &
            & ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), &
            & IOSTAT=ioerr, IOMSG=iomsg)
        END IF
      ELSE
        !Omit the POSITION clause, and the PAD clause
        IF (obj%getNewUnit) THEN
          OPEN (NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), RECL=reclval, &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
            & ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
        ELSE
          OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), RECL=reclval, &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
            & ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
        END IF
      END IF
    ELSE
      IF (obj%IsFormatted()) THEN
        !Include the POSITION clause, and the PAD clause
        IF (obj%getNewUnit) THEN
          OPEN ( &
            & NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), &
            & PAD=TRIM(padvar), &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
            & POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
            & FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
        ELSE
          OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), PAD=TRIM(padvar), &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
            & POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
            & FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
        END IF
      ELSE
        !Include the POSITION clause, omit the PAD clause
        IF (obj%getNewUnit) THEN
          OPEN (NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
            & POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
            & FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
        ELSE
          OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), &
            & ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
            & POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
            & FILE=TRIM(path%chars())// &
            & TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
        END IF
      END IF
    END IF
    IF (ioerr .NE. 0) THEN
      WRITE (emesg, '(a,i4,a,i4)') 'Error opening file "'// &
          & TRIM(path%chars())// &
          & TRIM(filename%chars())//TRIM(ext%chars()) &
          & //'" (UNIT=', obj%unitno, ') IOSTAT=', ioerr
      CALL e%RaiseError(modName//'::'//myName//' - '//TRIM(emesg) &
        & //' IOMSG="'//TRIM(iomsg)//'"')
    ELSE
      CALL obj%SetOpenStat(.TRUE.)
      CALL obj%SetEOFStat(.FALSE.)
    END IF
  END IF
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'Cannot open file! Object has not been initialized!')
END IF
END PROCEDURE ff_open

!----------------------------------------------------------------------------
!                                                                 close
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_close
CHARACTER(*), PARAMETER :: myName = 'ff_close'
CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr
!
IF (obj%initstat) THEN
  IF (obj%IsOpen()) THEN
    CLOSE (UNIT=obj%unitno, STATUS='KEEP', IOSTAT=ioerr)
    IF (ioerr /= 0) THEN
      WRITE (emesg, '(a,i4,a,i4)') 'Error closing file (UNIT=', &
        & obj%unitno, ') IOSTAT=', ioerr
      CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
    ELSE
      CALL obj%SetOpenStat(.FALSE.)
    END IF
  ELSE
    WRITE (emesg, '(a,i4,a)') 'Cannot close file (UNIT=', &
      & obj%unitno, ') File is not open!'
    CALL e%RaiseDebug(modName//'::'//myName//' - '//emesg)
  END IF
ELSE
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & 'Cannot close file! File object has not been initialized!')
END IF
END PROCEDURE ff_close

!----------------------------------------------------------------------------
!                                                                 delete
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_delete
CHARACTER(*), PARAMETER :: myName = 'ff_delete'
TYPE(String) :: path, filename, ext
CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr

IF (obj%initstat) THEN
  IF (obj%IsOpen()) THEN
    CLOSE (UNIT=obj%unitno, STATUS='DELETE', IOSTAT=ioerr)
    IF (ioerr /= 0) THEN
      WRITE (emesg, '(a,i4,a,i4)') 'Error deleting file (UNIT=', &
        & obj%unitno, ') IOSTAT=', ioerr
      CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
    ELSE
      CALL obj%SetOpenStat(.FALSE.)
    END IF
  ELSE
    path = obj%getFilePath()
    filename = obj%getFileName()
    ext = obj%getFileExt()

    OPEN (UNIT=obj%unitno, &
      & FILE=TRIM(path%chars())// &
      & TRIM(filename%chars())// &
      & TRIM(ext%chars()), &
      & IOMSG=iomsg, &
      & IOSTAT=ioerr)

    IF (ioerr /= 0) THEN
      WRITE (emesg, '(a,i4,a,i4)') &
        & 'Error deleting file (UNIT=', &
        & obj%unitno, ') IOSTAT=', ioerr
      CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
    END IF
    CLOSE (UNIT=obj%unitno, STATUS='DELETE', IOSTAT=ioerr)
    IF (ioerr /= 0) THEN
      WRITE (emesg, '(a,i4,a,i4)') 'Error deleting file (UNIT=', &
        & obj%unitno, ') IOSTAT=', ioerr
      CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
    ELSE
      CALL obj%SetOpenStat(.FALSE.)
    END IF
  END IF
ELSE
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & 'Cannot delete file! File object has not been initialized!')
END IF
END PROCEDURE ff_delete

!----------------------------------------------------------------------------
!                                                                 Backspace
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_backspace
CHARACTER(*), PARAMETER :: myName = 'ff_backspace'

CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr

IF (obj%initstat) THEN
  IF (obj%IsOpen()) THEN
    BACKSPACE (UNIT=obj%unitno, IOSTAT=ioerr, IOMSG=iomsg)
    IF (ioerr .NE. 0) THEN
      WRITE (emesg, '(a,i4,a,i4,a)') 'Error backspacing file (UNIT=', &
        & obj%unitno, ') IOSTAT=', ioerr, ' IOMSG='//TRIM(iomsg)
      CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
    ELSE
      IF (obj%IsEOF()) CALL obj%SetEOFstat(.FALSE.)
    END IF
  ELSE
    WRITE (emesg, '(a,i4,a)') 'Cannot backspace file (UNIT=', obj%unitno, &
      & '). File not is not open!'
    CALL e%RaiseDebug(modName//'::'//myName//' - '//emesg)
  END IF
ELSE
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & 'Cannot backspace file! File object has not been initialized!')
END IF
END PROCEDURE ff_backspace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_rewind
CHARACTER(*), PARAMETER :: myName = 'ff_rewind()'
CHARACTER(maxStrLen) :: emesg, iomsg
INTEGER(I4B) :: ioerr
LOGICAL(LGT) :: problem, isDarwin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

problem = .NOT. obj%initstat

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Cannot rewind file, file not initialized.')
  RETURN
END IF

problem = .NOT. (obj%IsOpen())
IF (problem) THEN
  WRITE (emesg, '(a,i4,a)') 'Cannot rewind file (UNIT=', obj%unitno, &
    & '). File not is not open!'
  CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
  RETURN
END IF

#ifdef Darwin_SYSTEM
isDarwin = .TRUE.
#else
isDarwin = .FALSE.
#endif

IF (.NOT. isDarwin) THEN
  REWIND (UNIT=obj%unitno, IOSTAT=ioerr, IOMSG=iomsg)
  CALL obj%SetEOFstat(.FALSE.)

  IF (ioerr .NE. 0) THEN
    WRITE (emesg, '(a,i4,a,i4,a)') 'Error rewinding file (UNIT=', &
      & obj%unitno, ') IOSTAT=', ioerr, ' IOMSG='//TRIM(iomsg)
    CALL e%RaiseError(modName//'::'//myName//' - '//emesg)
    RETURN
  END IF
END IF

IF (isDarwin) THEN
  CALL obj%CLOSE()
  CALL obj%OPEN()
  CALL obj%SetEOFstat(.FALSE.)
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[BUG] :: REWIND() function does not work with GNU Fortran. '// &
    & 'We are working on this issue. '// &
    & 'Currently, we are reopening the file for rewind.')
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE ff_rewind

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
