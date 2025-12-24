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
INTEGER(I4B) :: ncol, nrow, stat0, iostat, ii, temp_ncol, temp_nrow, jj
TYPE(String) :: filename, ext, astr
TYPE(CSVFile_) :: acsvfile
TYPE(TxtFile_) :: atxtfile
CHARACTER(512) :: iomsg
LOGICAL(LGT) :: isFound0, bool1, isok
INTEGER(I4B), ALLOCATABLE :: tempintvec1(:), tempintvec2(:), &
                             tempintvec3(:), tempintvec4(:)

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
  nrow = toml_len(array)
  DO ii = 1, nrow
    row_array => NULL()
    CALL toml_get(array, ii, row_array)
    isok = ASSOCIATED(row_array)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: While reading 2D array, '// &
                     ' it is seems the '//tostring(ii)//'th row is empty '// &
                        '(cannot read it).')
    END IF

    temp_ncol = toml_len(row_array)
    IF (ii .EQ. 1) THEN
      ncol = temp_ncol
      CALL Reallocate(VALUE, nrow, ncol)
      isFound0 = .TRUE.
    ELSE
      IF (temp_ncol .NE. ncol) THEN
        CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: Staggered matrix is not allowed')
      END IF
    END IF

    DO jj = 1, ncol
      CALL toml_get(row_array, jj, VALUE(ii, jj))
    END DO
  END DO

  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  NULLIFY (array, row_array)
  RETURN
END IF

!----------------------------------------------------------------------------
! READ a scalar value from toml
! In this case shape of matrix is (1,1), this value is given in toml file
!----------------------------------------------------------------------------

CALL toml_get(table, key, temp, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN
  CALL Reallocate(VALUE, 1, 1)
  VALUE(1, 1) = temp
  isFound0 = .TRUE.
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  RETURN
END IF

!----------------------------------------------------------------------------
! READ from a txt file or csv file
! try to read from the file
! the data is given in a txt file
! the filename is given in the toml file
! the line beginning with # is treated as comment line and is ignored
! no header csv file is expected
!----------------------------------------------------------------------------

isFound0 = .FALSE.
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0

CALL toml_get(table, key, filename%raw, origin=origin, stat=stat0)

IF (stat0 .EQ. toml_stat%success) THEN
  ext = filename%extension()
  SELECT CASE (ext%chars())
  CASE (".csv")

    CALL acsvfile%Initiate(filename=filename%Chars(), &
                           action="READ", status="OLD", &
                           delimiter=",", comment="#")
    CALL acsvfile%OPEN()
    CALL acsvfile%READ()
    temp_ncol = acsvfile%Getncols()
    temp_nrow = acsvfile%Getnrows()

    CALL acsvfile%Get(icol=1, irow=1, val=astr)
    IF (.NOT. astr%is_integer()) THEN
      ! each column in csv is imported as a column of VALUE
      CALL Reallocate(VALUE, temp_nrow, temp_ncol)
      DO ii = 1, temp_ncol
        CALL acsvfile%Get(ii, val=tempvalvec)
        VALUE(:, ii) = tempvalvec
      END DO

      isFound0 = .TRUE.
      CALL acsvfile%DEALLOCATE()
      IF (PRESENT(isFound)) isFound = isFound0
      IF (PRESENT(stat)) stat = stat0
      filename = ""
      ext = ""
      RETURN
    END IF

    SELECT CASE (temp_ncol)
    CASE (3)
      ! first column is treated as row index of VALUE
      ! second column is treated as column index of VALUE
      ! third column is treated as value at this location
      CALL acsvfile%get(1, val=tempintvec1) ! row index
      CALL acsvfile%get(2, val=tempintvec2) ! column index
      CALL acsvfile%get(3, val=tempvalvec) ! value

      nrow = MAXVAL(tempintvec1)
      ncol = MAXVAL(tempintvec2)
      CALL Reallocate(VALUE, nrow, ncol)
      DO ii = 1, SIZE(tempintvec1)
        VALUE(tempintvec1(ii), tempintvec2(ii)) = tempvalvec(ii)
      END DO

      DEALLOCATE (tempintvec1, tempintvec2, tempvalvec)

    CASE (5)
      ! first to fourth column must be integers
      ! first and second integers are treated
      ! as start and end index of row in VALUE
      ! third and fourth integers are treated
      ! as start and end index of column in VALUE
      ! fifth is value for VALUE(start:end, start:end)
      CALL acsvfile%get(1, val=tempintvec1) ! start
      CALL acsvfile%get(2, val=tempintvec2) ! end
      CALL acsvfile%get(3, val=tempintvec3) ! start
      CALL acsvfile%get(4, val=tempintvec4) ! end
      CALL acsvfile%get(5, val=tempvalvec) ! value

      nrow = MAXVAL(tempintvec2)
      ncol = MAXVAL(tempintvec4)
      CALL reallocate(VALUE, nrow, ncol)

      DO ii = 1, SIZE(tempintvec1)
        VALUE(tempintvec1(ii):tempintvec2(ii), &
              tempintvec3(ii):tempintvec4(ii)) = tempvalvec(ii)
      END DO

      DEALLOCATE (tempintvec1, tempintvec2, tempintvec3, tempintvec4, &
                  tempvalvec)

    CASE default
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                       '[INTERNAL ERROR] :: Number of columns in csv file'// &
                        'should be 2, 3 or 5')
    END SELECT

    isFound0 = .TRUE.
    CALL acsvfile%DEALLOCATE()
    IF (PRESENT(isFound)) isFound = isFound0
    IF (PRESENT(stat)) stat = stat0
    filename = ""
    ext = ""
    RETURN

  CASE default
    CALL atxtfile%Initiate(filename=filename%Chars(), &
                           action="READ", status="OLD", &
                           comment="#")
    CALL atxtfile%OPEN()
    CALL atxtfile%READ(val=VALUE, iostat=iostat, iomsg=iomsg, &
                       ignoreComment=.TRUE.)
    bool1 = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
    IF (bool1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: Error while reading txtfile, errmsg= '// &
                        CHAR_LF//TRIM(iomsg))
      IF (PRESENT(isFound)) isFound = isFound0
      RETURN
    END IF
    isFound0 = .TRUE.
    CALL atxtfile%DEALLOCATE()

    IF (PRESENT(stat)) stat = stat0
    IF (PRESENT(isFound)) isFound = isFound0
    filename = ""
    ext = ""
  END SELECT
END IF
