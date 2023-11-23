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
      TYPE(toml_array), POINTER :: array, row_array
      INTEGER(I4B) :: tcol, trow, stat0, iostat, ii, tcol_same, jj
      TYPE(String) :: filename
      TYPE(TxtFile_) :: atxtfile
      CHARACTER(512) :: iomsg
      LOGICAL(LGT) :: isFound0, bool1

      isFound0 = .FALSE.
      ! try to read from the array
      array => NULL()
      CALL toml_get(table, key, array,  &
        & origin=origin, stat=stat, requested=.FALSE.)

      IF (ASSOCIATED(array)) THEN
        trow = toml_len(array)
        DO ii = 1, trow
          row_array => NULL()
          CALL toml_get(array, ii, row_array)
          IF (.NOT. ASSOCIATED(row_array)) THEN
            CALL e%RaiseError(modName//'::'//myName//' - '// &
              & '[INTERNAL ERROR] :: While reading 2D array, '//  &
              & ' it is seems the '//tostring(ii)//'th row is empty '//  &
              & '(cannot read it).')
          END IF

          tcol = toml_len(row_array)
          IF (ii .EQ. 1) THEN
            tcol_same = tcol
            CALL Reallocate(VALUE, trow, tcol)
            isFound0 = .TRUE.
          ELSE
            IF (tcol .NE. tcol_same) THEN
              CALL e%RaiseError(modName//'::'//myName//' - '// &
                & '[INTERNAL ERROR] :: Staggered matrix is not allowed')
            END IF
          END IF

          DO jj = 1, tcol
            CALL toml_get(row_array, jj, VALUE(ii, jj))
          END DO
        END DO
      END IF

      ! try to read from the file
      CALL toml_get(table, key, filename%raw, origin=origin, stat=stat0)

      IF (PRESENT(stat)) stat = stat0

      IF (stat0 .EQ. toml_stat%success) THEN
        CALL atxtfile%Initiate(filename=filename%Chars(),  &
          & action="READ", status="OLD")
        CALL atxtfile%OPEN()
        CALL atxtfile%READ(val=VALUE, iostat=iostat, iomsg=iomsg)
        bool1 = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
        IF (bool1) THEN
          CALL e%RaiseError(modName//'::'//myName//' - '// &
            & '[INTERNAL ERROR] :: Error while reading txtfile, errmsg= '// &
            & CHAR_LF//TRIM(iomsg))
          IF (PRESENT(isFound)) isFound = isFound0
          RETURN
        END IF
        isFound0 = .TRUE.
        CALL atxtfile%DEALLOCATE()
      END IF

      IF (PRESENT(isFound)) isFound = isFound0

      filename = ""
      NULLIFY (array)
