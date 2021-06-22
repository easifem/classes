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

module test_m
use easifemBase
use easifemClasses
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type(inputFile_) :: obj
  type(FortranFile_) :: ffile
  TYPE(ExceptionHandler_), TARGET :: e
  TYPE(String) :: line

  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.FALSE.)
  CALL obj%e%addSurrogate(e)
  CALL Display(.NOT.(obj%getEchoStat()),'%getEchoStat()=')
  CALL Display(obj%getEchoUnit() == -1,'%getEchoUnit()=')
  CALL obj%setEchoStat(.TRUE.)
  CALL Display((obj%getEchoStat()),'%getEchoStat()=')
  CALL obj%setEchoUnit(0)
  CALL obj%setEchoUnit(25)
  CALL DISPLAY(obj%getEchoUnit() == 25,'%setEchoUnit(...)=')
  CALL ffile%initiate(UNIT=66,FILE='./test.inp',STATUS='REPLACE', &
    & ACTION='WRITE')
  CALL ffile%open()
  WRITE(ffile%getUnitNo(),'(a,i2)') 'sample oneline',1
  CALL ffile%clear()
  CALL ffile%initiate(UNIT=25,FILE='./test.out',STATUS='REPLACE', &
    & ACTION='WRITE')
  CALL ffile%open()
  CALL obj%initiate(UNIT=46,FILE='./test.inp')
  CALL obj%open()
  CALL obj%readline(line)
  CALL Display(line, 'line: ')
  CALL Display(line == 'sample oneline 1','%readline()=')
  CALL Display((obj%getProbe() == 's'),'%getProbe()=')
  CALL obj%rewind()
  CALL Display(LEN_TRIM(obj%getProbe()) == 0,'%rewind()=')
  CALL obj%readline(line)
  CALL obj%backspace()
  CALL Display(LEN_TRIM(obj%getProbe()) == 0,'%backspace()=')
  CALL ffile%clear()
  CALL obj%setEchoStat(.FALSE.)
  CALL obj%readline(line)
  CALL obj%clear(.TRUE.)
  CALL Display(obj%getEchoUnit() == -1,'%clear() (echo unit)')
  CALL Display(.NOT.(obj%getEchostat()),'%clear() (echo stat)')

end subroutine
end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main