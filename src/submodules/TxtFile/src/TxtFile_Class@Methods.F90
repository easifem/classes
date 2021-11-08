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

SUBMODULE(TxtFile_Class) Methods
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR, IOSTAT_END
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_addSurrogate
CALL e%addSurrogate(UserObj)
END PROCEDURE txt_addSurrogate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_initiate
CHARACTER(LEN=*), PARAMETER :: myName = 'inp_initiate'
IF (PRESENT(access)) CALL e%raiseDebug(modName//'::'//myName// &
  & ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
IF (PRESENT(form)) CALL e%raiseDebug(modName//'::'//myName// &
  & ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
IF (PRESENT(pad)) CALL e%raiseDebug(modName//'::'//myName// &
  & ' - Optional input "PAD" is being ignored. Value is "YES".')
IF (PRESENT(position)) CALL e%raiseDebug(modName//'::'//myName// &
  & ' - Optional input "POSITION" is being ignored. Value is "REWIND".')
IF (PRESENT(recl)) CALL e%raiseDebug(modName//'::'//myName// &
  & ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')
!Initialize the input file
CALL InitiateFortranFile(obj=obj, unit=unit, filename=filename, &
  & status=status, access='SEQUENTIAL', form='FORMATTED', &
  & position='ASIS', action=action)
END PROCEDURE txt_initiate

!----------------------------------------------------------------------------
!                                                                 clear
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_clear
LOGICAL(LGT) :: bool
obj%echounit = -1
obj%echostat = .FALSE.
bool = .FALSE.
IF (PRESENT(Delete)) bool = Delete
CALL ClearFortranFile(obj, bool)
END PROCEDURE txt_clear

!----------------------------------------------------------------------------
!                                                                 readLine
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_readLine
CHARACTER(LEN=*), PARAMETER :: myName = 'txt_readLine'
CHARACTER(LEN=maxStrLen) :: buffer
CHARACTER(LEN=4) :: sioerr, sunit
INTEGER(I4B) :: buffer_size, eioerr, ioerr

ioerr = 0
IF (obj%isOpen() .AND. .NOT. obj%isEOF()) THEN
  DO WHILE (ioerr /= IOSTAT_EOR .AND. ioerr /= IOSTAT_END)
    !Repeatedly read chunks of current input file line into buffer
    READ (UNIT=obj%getUnitNo(), FMT='(a)', SIZE=buffer_size, ADVANCE='NO', &
      & IOSTAT=ioerr) buffer
    IF (ioerr == IOSTAT_END) THEN
      !End of file
      CALL obj%setEOFstat(.TRUE.)
    ELSEIF (ioerr == IOSTAT_EOR) THEN
      !Done reading line. Append last buffer to line.
      line = line//TRIM(buffer)
      IF (obj%echostat) THEN
        WRITE (UNIT=obj%echounit, FMT='(a)', IOSTAT=eioerr) TRIM(line%chars())
        IF (eioerr /= 0) THEN
          WRITE (sioerr, '(i4)') eioerr; sioerr = ADJUSTL(sioerr)
          WRITE (sunit, '(i4)') obj%echounit; sunit = ADJUSTL(sunit)
          CALL e%raiseError(modName//'::'//myName//" - "// &
            &' - Error echoing line to UNIT='//TRIM(sunit)//' (IOSTAT='//&
            & TRIM(sioerr)//')!')
        END IF
      END IF
      line = TRIM(line)
    ELSEIF (ioerr < IOSTAT_EOR) THEN
      !Error reading line from input file
      WRITE (sioerr, '(i4)') ioerr; sioerr = ADJUSTL(sioerr)
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Error reading one line from input file (IOSTAT='// &
        & TRIM(sioerr)//')!')
    ELSE
      !Still reading current line. Append buffer to line
      line = line//buffer
    END IF
  END DO
END IF
END PROCEDURE txt_readLine

!----------------------------------------------------------------------------
!                                                               setEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_setEchoStat
obj%echostat = bool
END PROCEDURE txt_setEchoStat

!----------------------------------------------------------------------------
!                                                               getEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getEchoStat
ans = obj%echostat
END PROCEDURE txt_getEchoStat

!----------------------------------------------------------------------------
!                                                               setEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_setEchoUnit
CHARACTER(LEN=*), PARAMETER :: myName = 'txt_setEchoUnit'
  IF( (0 .LT. unitno) .AND. (unitno .NE. stdout) .AND. (unitno .NE. stderr)) THEN
  obj%echounit = unitno
ELSE
  CALL e%raiseError('Incorrect input to '//modName//'::'// &
    & myName//' - Illegal value for unit number!')
END IF
END PROCEDURE txt_setEchoUnit

!----------------------------------------------------------------------------
!                                                               getEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getEchoUnit
ans = obj%echounit
END PROCEDURE txt_getEchoUnit

!----------------------------------------------------------------------------
!                                                    convertMarkdownToSource
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_convertMarkdownToSource
CHARACTER(LEN=*), PARAMETER :: myName = "txt_convertMarkdownToSource"
TYPE(String) :: aline
INTEGER(I4B) :: iostat
INTEGER(I4B) :: inUnit
INTEGER(I4B) :: outUnit
LOGICAL(LGT) :: insideCodeBlock
CHARACTER(len=100) :: iomsg
!
!> check is initiated
!> check isOpen
!> check read and write permission

!> check
IF (.NOT. obj%isInit() .OR. .NOT. obj%isOpen()) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'Markdown file is not initiated.')
!> check
IF (.NOT. outfile%isInit() .OR. .NOT. outfile%isOpen()) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'output file is not initiated')
!> check
IF (.NOT. obj%isRead()) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'Markdown file does not have read access')
!> check
IF (.NOT. outfile%isWrite()) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'Source file does not have write access')
!> program starts
inUnit = obj%getUnitNo()
outUnit = outfile%getUnitNo()
insideCodeBlock = .FALSE.
DO
  ! REVIEW In future This call will be replaced with
  ! obj%readLine(aline)
  CALL aline%read_line(unit=inUnit, iostat=iostat, iomsg=iomsg)
  IF (IS_IOSTAT_END(iostat)) THEN
    EXIT
  ELSE IF (iostat .GT. 0) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'Error while reading a line from Markdown file'// &
    & ' iostat='//tostring(iostat)//' iomsg='//TRIM(iomsg))
  END IF
  IF (aline%slice(1, 3) .EQ. '```') THEN
    aline = ""
    IF (insideCodeBlock) THEN
      insideCodeBlock = .FALSE.
    ELSE
      insideCodeBlock = .true.
    END IF
  END IF
  !
  IF (insideCodeBlock) THEN
    WRITE (outUnit, "(a)") aline%chars()
  END IF
END DO

END PROCEDURE txt_convertMarkdownToSource

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
