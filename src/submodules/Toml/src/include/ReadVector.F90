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

CHARACTER(*), PARAMETER :: myName = "toml_get"
TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: tsize, stat0, iostat, ii
TYPE(String) :: filename
TYPE(TxtFile_) :: atxtfile
CHARACTER(512) :: iomsg
LOGICAL(LGT) :: isFound0, bool1, isok

isFound0 = .FALSE.

!----------------------------------------------------------------------------
! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------
array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=.FALSE.)

isok = ASSOCIATED(array)

IF (isok) THEN
  tsize = toml_len(array)
  CALL Reallocate(VALUE, tsize)
  isFound0 = .TRUE.
  DO ii = 1, tsize
    CALL toml_get(array, ii, VALUE(ii))
  END DO

  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isFound)) isFound = isFound0
  NULLIFY (array)
  RETURN
END IF

!----------------------------------------------------------------------------
! READ from a txt file
! try to read from the file
! the data is given in a txt file 
! the filename is given in the toml file
!----------------------------------------------------------------------------

CALL toml_get(table, key, filename%raw, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN
  CALL atxtfile%Initiate(filename=filename%Chars(), &
                          action="READ", status="OLD")
  CALL atxtfile%OPEN()
  CALL atxtfile%READ(val=VALUE, iostat=iostat, iomsg=iomsg)

  bool1 = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: Error while reading txtfile, errmsg= '// &
                      CHAR_LF//TRIM(iomsg))
    IF (PRESENT(isFound)) isFound = isFound0
    IF (PRESENT(stat)) stat = stat0
    filename = ""
    RETURN
  END IF

  isFound0 = .TRUE.
  CALL atxtfile%DEALLOCATE()

  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  filename = ""
  RETURN
END IF

!----------------------------------------------------------------------------
! READ a scalar value from toml
! In this case length of the vector is 1, this value is given in toml file
!----------------------------------------------------------------------------

CALL toml_get(table, key, temp, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN
  CALL Reallocate(VALUE, 1)
  VALUE(1) = temp
  isFound0 = .TRUE.
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  RETURN
END IF

isFound0 = .FALSE.
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0
