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

SUBMODULE( InputFile_Class ) Methods
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR, IOSTAT_END
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE inp_addSurrogate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_initiate
  CHARACTER(LEN=*),PARAMETER :: myName='inp_initiate'

  IF(PRESENT(status)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "STATUS" is being ignored. Value is "OLD".')
  IF(PRESENT(access)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
  IF(PRESENT(form)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
  IF(PRESENT(action)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "ACTION" is being ignored. Value is "READ".')
  IF(PRESENT(pad)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "PAD" is being ignored. Value is "YES".')
  IF(PRESENT(position)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "POSITION" is being ignored. Value is "REWIND".')
  IF(PRESENT(recl)) CALL e%raiseDebug(modName//'::'//myName// &
    & ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')

  !Initialize the input file
  CALL InitiateFortranFile(obj=obj, unit=unit, file=file, &
    &  status='OLD', access='SEQUENTIAL', form='FORMATTED', &
    & position = 'REWIND', action = 'READ')
END PROCEDURE inp_initiate

!----------------------------------------------------------------------------
!                                                                 Rewind
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_rewind
  obj%probe=''
  obj%lastprobe=''
  CALL RewindFortranFile(obj)
END PROCEDURE inp_rewind

!----------------------------------------------------------------------------
!                                                                 Backspace
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_backspace
  CALL BackspaceFortranFile(obj)
  obj%probe=obj%lastprobe
END PROCEDURE inp_backspace

!----------------------------------------------------------------------------
!                                                                 clear
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_clear
  LOGICAL( LGT ) :: bool
  obj%echounit=-1
  obj%echostat=.FALSE.
  obj%probe=''
  obj%lastprobe=''
  bool=.FALSE.
  IF(PRESENT(Delete)) bool=Delete
  CALL ClearFortranFile(obj,bool)
END PROCEDURE inp_clear

!----------------------------------------------------------------------------
!                                                                 readLine
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_readLine
  CHARACTER(LEN=*),PARAMETER :: myName='inp_readLine'
  CHARACTER(LEN=maxStrLen) :: buffer
  CHARACTER(LEN=4) :: sioerr,sunit
  INTEGER(I4B) :: buffer_size,eioerr, ioerr

  ioerr=0
  IF(obj%isOpen() .AND. .NOT.obj%isEOF()) THEN
    DO WHILE(ioerr /= IOSTAT_EOR .AND. ioerr /= IOSTAT_END)
      !Repeatedly read chunks of current input file line into buffer
      READ(UNIT=obj%getUnitNo(),FMT='(a)',SIZE=buffer_size,ADVANCE='NO', &
        & IOSTAT=ioerr) buffer
      IF(ioerr == IOSTAT_END) THEN
        !End of file
        CALL obj%setEOFstat(.TRUE.)
      ELSEIF(ioerr == IOSTAT_EOR) THEN
        !Done reading line. Append last buffer to line.
        line=line // TRIM(buffer)
        obj%lastprobe=obj%probe
        IF(obj%echostat) THEN
          WRITE(UNIT=obj%echounit,FMT='(a)',IOSTAT=eioerr) TRIM(line%chars())
          IF(eioerr /= 0) THEN
            WRITE(sioerr,'(i4)') eioerr; sioerr=ADJUSTL(sioerr)
            WRITE(sunit,'(i4)') obj%echounit; sunit=ADJUSTL(sunit)
            CALL e%raiseError(modName//'::'//myName// &
              &' - Error echoing line to UNIT='//TRIM(sunit) //' (IOSTAT='//&
              & TRIM(sioerr)//')!')
          ENDIF
        ENDIF
        line=TRIM(line)
      ELSEIF(ioerr < IOSTAT_EOR) THEN
        !Error reading line from input file
        WRITE(sioerr,'(i4)') ioerr; sioerr=ADJUSTL(sioerr)
        CALL e%raiseError(modName//'::'//myName// &
          & ' - Error reading one line from input file (IOSTAT='// &
          & TRIM(sioerr)//')!')
      ELSE
        !Still reading current line. Append buffer to line
        line=line//buffer
      ENDIF
    ENDDO
  ENDIF

  buffer = line%chars()
  IF(LEN(buffer) > 0) THEN
    obj%probe=buffer(1:1)
  ELSE
    obj%probe=''
    line=' '
  ENDIF
END PROCEDURE inp_readLine

!----------------------------------------------------------------------------
!                                                               setEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_setEchoStat
  obj%echostat=bool
END PROCEDURE inp_setEchoStat

!----------------------------------------------------------------------------
!                                                               getEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_getEchoStat
  ans=obj%echostat
END PROCEDURE inp_getEchoStat

!----------------------------------------------------------------------------
!                                                               setEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_setEchoUnit
  CHARACTER(LEN=*),PARAMETER :: myName='SETECHOUNIT_INP_FILE'
  IF( (0 .LT. unitno) .AND. (unitno .NE. stdout) .AND. (unitno .NE. stderr)) THEN
    obj%echounit=unitno
  ELSE
    CALL e%raiseError('Incorrect input to '//modName//'::'// &
      & myName//' - Illegal value for unit number!')
  ENDIF
END PROCEDURE inp_setEchoUnit

!----------------------------------------------------------------------------
!                                                               getEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_getEchoUnit
  ans = obj%echounit
END PROCEDURE inp_getEchoUnit

!----------------------------------------------------------------------------
!                                                                 getProbe
!----------------------------------------------------------------------------

MODULE PROCEDURE inp_getProbe
  ans = obj%probe
END PROCEDURE inp_getProbe
END SUBMODULE Methods
