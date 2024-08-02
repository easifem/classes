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

      ! try to read from the array
      array => NULL()
      CALL toml_get(table, key, array, &
                    origin=origin, stat=stat0, requested=.FALSE.)

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

      ! try to read from the file
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
      END IF

      IF (PRESENT(isFound)) isFound = isFound0
      IF (PRESENT(stat)) stat = stat0
      filename = ""
