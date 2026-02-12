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
USE GlobalData, ONLY: stdout, stderr, stdin
USE Display_Method, ONLY: ToString
USE StringUtility, ONLY: UpperCase
USE System_Method, ONLY: System_Mkdir
USE System_Method, ONLY: RWX_U
USE InputUtility, ONLY: Input
USE AbstractFile_Class, ONLY: AbstractFileDeallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_Initiate()'
#endif

CHARACTER(7) :: statusval
CHARACTER(10) :: accessval
CHARACTER(11) :: formval
CHARACTER(9) :: actionval
CHARACTER(3) :: padval
TYPE(String) :: fpath, fname, fext, file_
LOGICAL(LGT) :: ostat, isok
INTEGER(I4B) :: oldcnt, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Initialize data
statusval = ''
accessval = ''
formval = ''
actionval = ''
padval = ''
oldcnt = e%GetCounter(EXCEPTION_ERROR)

#ifdef DEBUG_VER
isok = .NOT. obj%initstat
CALL AssertError1(isok, myName, &
                  "Fortran file has already bee initialized!")
#endif

!Initialize the file
file_ = TRIM(filename)
isok = file_%SCAN(fileopt%slash) .EQ. math%zero_i
IF (isok) THEN
  fpath = "."//fileopt%slash
ELSE
  fpath = file_%Basedir(sep=fileopt%slash)//fileopt%slash
END IF

fext = file_%Extension()
fname = file_%Basename(extension=fext%Chars(), sep=fileopt%slash)
CALL obj%SetFilePath(fpath)
CALL obj%SetFileName(fname)
CALL obj%SetFileExt(fext)

obj%getNewUnit = math%yes

isok = PRESENT(unit)
IF (isok) THEN
  isok = (unit .NE. stdout) &
         .AND. (unit .NE. stderr) &
         .AND. (unit .NE. stdin)

  CALL AssertError1(isok, myName, &
        'Illegal value for optional input argument unit! value cannot &
        & be equal to stdout, stderr, stdin. Found unit='//ToString(unit))

  INQUIRE (UNIT=unit, OPENED=ostat)

#ifdef DEBUG_VER
  isok = .NOT. ostat
  CALL AssertError1(isok, myName, &
              'value for optional input argument UNIT is being used &
              & by another file!')
#endif

  obj%unitno = unit
  obj%getNewUnit = math%no
END IF

! STATUS clause for OPEN statement
statusval = fileopt%replace
isok = PRESENT(status)
IF (isok) statusval = UpperCase(status)

isok = TRIM(statusval) .NE. fileopt%old
IF (isok) THEN
  ierr = System_Mkdir(fpath//'', RWX_U)

#ifdef DEBUG_VER
  isok = (ierr .EQ. math%zero_i) .OR. (ierr .EQ. math%minus_one_i)
  CALL AssertError1(isok, myName, &
                    "error occured while creating the directory.")
#endif
END IF

! ACCESS clause for OPEN statement
accessval = Input(option=access, default=fileopt%sequential)
accessval = UpperCase(accessval)

! FORM clause for OPEN statement
! formatted : file is a text file
! unformatted : file is a binary file
formval = Input(option=form, default=fileopt%formatted)
formval = UpperCase(formval)

! POSITION clause for OPEN statement
obj%posopt = Input(option=position, default=fileopt%asis)
obj%posopt = UpperCase(obj%posopt)

! ACTION clause for OPEN statement
actionval = Input(option=action, default=fileopt%readwrite)
actionval = UpperCase(actionval)

! padding value
padval = Input(option=pad, default=fileopt%yes)
padval = UpperCase(padval)

! record length
isok = PRESENT(recl)
IF (isok) obj%reclval = recl

! comment
isok = PRESENT(comment)
IF (isok) obj%comment = comment

! separator
isok = PRESENT(separator)
IF (isok) obj%separator = separator

! delimiter
isok = PRESENT(delimiter)
IF (isok) obj%delimiter = delimiter

! setStatus
CALL obj%SetStatus(statusval)

obj%formatstat = (TRIM(formval) == fileopt%formatted)
obj%padstat = (TRIM(padval) == fileopt%yes)

isok = TRIM(accessval) == 'DIRECT' .OR. TRIM(accessval) == 'STREAM'
IF (isok) THEN
  obj%accessstat = math%yes

#ifdef DEBUG_VER
  isok = obj%reclval .GT. 0
  CALL AssertError1(isok, myName, &
                    'RECL should be greater than 0 for direct access files')
#endif
END IF

SELECT CASE (TRIM(actionval))
CASE (fileopt%READ)
  CALL obj%SetReadStat(math%yes)

#ifdef DEBUG_VER
  isok = .NOT. obj%newstat
  CALL AssertError1(isok, myName, &
                    "Cannot have a new file with a read only status.")
#endif

CASE (fileopt%WRITE)
  CALL obj%SetWriteStat(math%yes)

CASE (fileopt%readwrite)
  CALL obj%SetReadStat(math%yes)
  CALL obj%SetWriteStat(math%yes)

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "no case found for actionval="//TRIM(actionval))
#endif
END SELECT

obj%initstat = math%yes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif
LOGICAL(LGT) :: isdelete, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isdelete = Input(option=delete, default=math%no)

isok = obj%initstat .AND. isdelete
IF (isok) CALL obj%Delete()

isok = obj%initstat .AND. (.NOT. isdelete) .AND. obj%IsOpen()
IF (isok) CALL obj%CLOSE()

!Set FortranFileType attributes to defaults
obj%initstat = math%no
obj%unitno = math%minus_one_i
obj%formatstat = math%no
obj%accessstat = math%no
obj%newstat = math%no
obj%overwrite = math%no
obj%reclval = math%minus_one_i
obj%padstat = math%no
obj%posopt = fileopt%asis
obj%comment = fileopt%hash
obj%separator = " "
obj%delimiter = '\n'
obj%getNewUnit = math%no
!Set BaseFileType attributes to default
CALL AbstractFileDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                 open
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Open
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_Open()'
#endif

CHARACTER(7) :: statusvar
CHARACTER(10) :: accessvar
CHARACTER(11) :: formvar
CHARACTER(9) :: actionvar
CHARACTER(3) :: padvar
INTEGER(I4B) :: reclval
TYPE(String) :: path, filename, ext
CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: ioerr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%initstat) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = obj%IsOpen()
IF (isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

!Get the appropriate clause values for the OPEN statement
path = obj%GetFilePath()
filename = obj%GetFileName()
ext = obj%GetFileExt()

!STATUS clause value
isok = obj%IsNew()
IF (.NOT. isok) THEN
  statusvar = fileopt%old
ELSE
  IF (obj%overwrite) THEN
    statusvar = fileopt%replace
  ELSE
    statusvar = fileopt%new
  END IF
END IF

!FORM clause value
isok = obj%IsFormatted()
IF (isok) THEN
  formvar = fileopt%formatted
ELSE
  formvar = fileopt%unformatted
END IF

!ACCESS clause value
isok = obj%IsDirect()
IF (isok) THEN
  accessvar = fileopt%direct
  reclval = obj%reclval
ELSE
  accessvar = fileopt%sequential
  reclval = 0
END IF

!ACTION clause value
IF (obj%IsRead() .AND. .NOT. obj%isWrite()) THEN
  actionvar = fileopt%READ
ELSEIF (.NOT. obj%IsRead() .AND. obj%isWrite()) THEN
  actionvar = fileopt%WRITE
ELSEIF (obj%IsRead() .AND. obj%isWrite()) THEN
  actionvar = fileopt%readwrite
END IF

!PAD clause value
IF (obj%padstat) THEN
  padvar = fileopt%yes
ELSE
  padvar = fileopt%no
END IF

!The POSITION clause is illegal to use in the OPEN statement if
!the file is DIRECT access.
!The PAD clause is illegal to use in the OPEN statement if the file
!is UNFORMATTED.

isok = obj%IsDirect() .AND. obj%IsFormatted() .AND. obj%getNewUnit
IF (isok) THEN
  OPEN ( &
    NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), &
    PAD=TRIM(padvar), ACCESS=TRIM(accessvar), FORM=TRIM(formvar), &
    RECL=reclval, ACTION=TRIM(actionvar), FILE=path%chars()// &
    filename%chars()//ext%chars(), IOSTAT=ioerr, IOMSG=iomsg)
END IF

isok = obj%IsDirect() .AND. obj%IsFormatted() .AND. (.NOT. obj%getNewUnit)
IF (isok) THEN
  OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), PAD=TRIM(padvar), &
        ACCESS=TRIM(accessvar), FORM=TRIM(formvar), RECL=reclval, &
        ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
        TRIM(filename%chars())//TRIM(ext%chars()), &
        IOSTAT=ioerr, IOMSG=iomsg)
END IF

isok = obj%IsDirect() .AND. (.NOT. obj%IsFormatted()) .AND. obj%getNewUnit
IF (isok) THEN
  OPEN (NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), RECL=reclval, &
        ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
        ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
        TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
END IF

isok = obj%IsDirect() &
       .AND. (.NOT. obj%IsFormatted()) &
       .AND. (.NOT. obj%getNewUnit)
IF (isok) THEN
  OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), RECL=reclval, &
        ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
        ACTION=TRIM(actionvar), FILE=TRIM(path%chars())// &
        TRIM(filename%chars())//TRIM(ext%chars()), IOMSG=iomsg)
END IF

isok = (.NOT. obj%IsDirect()) &
       .AND. (obj%IsFormatted()) &
       .AND. (obj%getNewUnit)
IF (isok) THEN
  OPEN ( &
    NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), &
    PAD=TRIM(padvar), ACCESS=TRIM(accessvar), FORM=TRIM(formvar), &
    IOSTAT=ioerr, POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
    FILE=path%chars()//filename%chars()//ext%chars(), IOMSG=iomsg)
END IF

isok = (.NOT. obj%IsDirect()) &
       .AND. (obj%IsFormatted()) &
       .AND. (.NOT. obj%getNewUnit)
IF (isok) THEN
  OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), PAD=TRIM(padvar), &
        ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
        POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
        FILE=path%chars()//filename%chars()//ext%chars(), IOMSG=iomsg)
END IF

isok = (.NOT. obj%IsDirect()) &
       .AND. (.NOT. obj%IsFormatted()) &
       .AND. (obj%getNewUnit)
IF (isok) THEN
  OPEN (NEWUNIT=obj%unitno, STATUS=TRIM(statusvar), &
        ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
        POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
        FILE=path%chars()//filename%chars()//ext%chars(), IOMSG=iomsg)
END IF

isok = (.NOT. obj%IsDirect()) &
       .AND. (.NOT. obj%IsFormatted()) &
       .AND. (.NOT. obj%getNewUnit)
IF (isok) THEN
  OPEN (UNIT=obj%unitno, STATUS=TRIM(statusvar), &
        ACCESS=TRIM(accessvar), FORM=TRIM(formvar), IOSTAT=ioerr, &
        POSITION=TRIM(obj%posopt), ACTION=TRIM(actionvar), &
        FILE=path%chars()//filename%chars()//ext%chars(), IOMSG=iomsg)
END IF

#ifdef DEBUG_VER
isok = ioerr .EQ. 0
CALL AssertError1(isok, myName, &
                  'Error opening file "'//path%Chars()//filename%chars()// &
                  ext%chars()//'" (UNIT='//ToString(obj%unitno)// &
                  ') IOSTAT='//ToString(ioerr))
#endif

CALL obj%SetOpenStat(math%yes)
CALL obj%SetEOFStat(math%no)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Open

!----------------------------------------------------------------------------
!                                                                       Close
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Close
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_Close()'
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ioerr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%initstat) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = obj%IsOpen()
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CLOSE (UNIT=obj%unitno, STATUS=fileopt%keep, IOSTAT=ioerr)

#ifdef DEBUG_VER
isok = ioerr .EQ. 0
CALL AssertError1(isok, myName, &
                  "Error closing file (unit="//ToString(obj%unitno)// &
                  ") iostat="//ToString(ioerr))
#endif

CALL obj%SetOpenStat(math%no)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Close

!----------------------------------------------------------------------------
!                                                                      Delete
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Delete
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_Delete()'
#endif

INTEGER(I4B) :: ioerr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%initstat) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = obj%IsOpen()
IF (isok) THEN
  CLOSE (UNIT=obj%unitno, STATUS=fileopt%delete, IOSTAT=ioerr)

#ifdef DEBUG_VER
  isok = ioerr .EQ. 0
  CALL AssertError1(isok, myName, &
                    "Error deleting file (unit="//ToString(obj%unitno)// &
                    ") iostat="//ToString(ioerr))
#endif

  CALL obj%SetOpenStat(math%no)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Delete

!----------------------------------------------------------------------------
!                                                                 Backspace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Backspace
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_Backspace()'
#endif

CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: ioerr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%initstat) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = obj%IsOpen()
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isok) THEN
  BACKSPACE (UNIT=obj%unitno, IOSTAT=ioerr, IOMSG=iomsg)

#ifdef DEBUG_VER
  isok = ioerr .EQ. 0
  CALL AssertError1(isok, myName, &
                    'Error backspacing file (UNIT='// &
                    ToString(obj%unitno)//') IOSTAT='// &
                    ToString(ioerr)//' IOMSG='//TRIM(iomsg))
#endif

  isok = obj%IsEOF()
  IF (isok) CALL obj%SetEOFstat(math%no)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Backspace

!----------------------------------------------------------------------------
!                                                                      Rewind
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Rewind
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_Rewind()'
#endif

CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: ioerr
LOGICAL(LGT) :: isok, isDarwin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (.NOT. obj%initstat) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = obj%IsOpen()
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef Darwin_SYSTEM
isDarwin = math%yes
#else
isDarwin = math%no
#endif

IF (isDarwin) THEN
  CALL obj%CLOSE()
  CALL obj%OPEN()
  CALL obj%SetEOFstat(math%no)

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'REWIND() function does not work with GNU Fortran. '// &
                    'We are working on this issue. '// &
                    'Currently, we are reopening the file for rewind.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (.NOT. isDarwin) THEN
  REWIND (UNIT=obj%unitno, IOSTAT=ioerr, IOMSG=iomsg)
  CALL obj%SetEOFstat(math%no)

#ifdef DEBUG_VER
  isok = ioerr .EQ. 0
  CALL AssertError1(isok, myName, &
                    'Error rewinding file (UNIT='// &
                    ToString(obj%unitno)//') IOSTAT='//ToString(ioerr)// &
                    ' IOMSG='//TRIM(iomsg))
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Rewind

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
