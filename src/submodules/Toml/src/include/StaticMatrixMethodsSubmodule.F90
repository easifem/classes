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

! #define _SUBMODULE_NAME_ Int8StaticMatrixMethods
! #define _METHOD_NAME_ GetValue_Int8_r2_static
! #define _MY_NAME_ "GetValue_Int8_r2_static()"
! #define _DATA_TYPE_ INTEGER(INT8)

SUBMODULE(TomlUtility) _SUBMODULE_NAME_
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display
USE tomlf, ONLY: toml_error, &
                 toml_load, &
                 toml_parser_config, &
                 toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_context, &
                 toml_terminal, &
                 toml_load, &
                 toml_array, &
                 toml_stat
USE CSVFile_Class, ONLY: CSVFile_
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE _METHOD_NAME_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = _MY_NAME_
#endif

INTEGER(I4B) :: origin0, stat0
LOGICAL(LGT) :: isFound0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(origin)) origin0 = origin
IF (PRESENT(stat)) stat0 = stat

! the following call get value from the toml array
CALL GetMatrixValue1(table, key, VALUE, origin0, stat0, isFound0, nrow, &
                     ncol)

! if the above routine failed then we try to read the
! single value by using the following call
IF (.NOT. isFound0) &
  CALL GetMatrixValue2(table, key, VALUE, origin0, stat0, isFound0, nrow, &
                       ncol)

! if the above call fails then we try to read the
! value from the csv or text file
IF (.NOT. isFound0) &
  CALL GetMatrixValue3(table, key, VALUE, origin0, stat0, isFound0, nrow, &
                       ncol)

IF (PRESENT(origin)) origin = origin0
IF (PRESENT(stat)) stat = stat0
IF (PRESENT(isFound)) isFound = isFound0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE _METHOD_NAME_

!----------------------------------------------------------------------------
!                                                                  GetValue1
!----------------------------------------------------------------------------

! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
SUBROUTINE GetMatrixValue1(table, key, VALUE, origin, stat, isFound, &
                           nrow, ncol)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValue1()"
#endif

  TYPE(toml_array), POINTER :: array, row_array
  INTEGER(I4B) :: ii, temp_ncol, jj
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0; ncol = 0

  array => NULL()
  CALL toml_get( &
    table, key, array, origin=origin, stat=stat, requested=math%no)

  isok = ASSOCIATED(array)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code will be executed when array is associated.
  nrow = toml_len(array)
  DO ii = 1, nrow
    row_array => NULL()
    CALL toml_get(array, ii, row_array)

#ifdef DEBUG_VER
    isok = ASSOCIATED(row_array)
    CALL AssertError1( &
      isok, myName, &
      'While reading 2D array, it is seems the '//ToString(ii)// &
      'th row is empty (cannot read it).')
#endif

    temp_ncol = toml_len(row_array)

    IF (ii .EQ. 1) THEN
      ncol = temp_ncol
      isFound = math%yes
    ELSE

#ifdef DEBUG_VER
      CALL AssertError2(temp_ncol, ncol, myName, &
                        'Staggered matrix is not allowed')
#endif
    END IF

    DO jj = 1, ncol
      CALL toml_get(row_array, jj, VALUE(ii, jj))
    END DO

  END DO

  NULLIFY (array, row_array)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValue1

!----------------------------------------------------------------------------
!                                                            GetMatrixValue2
!----------------------------------------------------------------------------

! READ a scalar value from toml
! In this case shape of matrix is (1,1), this value is given in toml file
SUBROUTINE GetMatrixValue2(table, key, VALUE, origin, stat, isFound, nrow, &
                           ncol)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValue2()"
#endif

  _DATA_TYPE_ :: temp

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, key, temp, origin=origin, stat=stat)

  isFound = stat .EQ. toml_stat%success
  nrow = 0
  ncol = 0

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code is executed when isok is true
  nrow = 1
  ncol = 1
  VALUE(1, 1) = temp

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValue2

!----------------------------------------------------------------------------
!                                                            GetMatrixValue3
!----------------------------------------------------------------------------

! READ from a txt file or csv file
! try to read from the file
! the data is given in a txt file
! the filename is given in the toml file
! the line beginning with # is treated as comment line and is ignored
! no header csv file is expected
SUBROUTINE GetMatrixValue3(table, key, VALUE, origin, stat, isFound, &
                           nrow, ncol)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValue3()"
#endif

  CHARACTER(:), ALLOCATABLE :: astr
  LOGICAL(LGT) :: isok
  TYPE(String) :: filename, ext

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0
  ncol = 0

  CALL toml_get(table, key, astr, origin=origin, stat=stat)

  isok = stat .EQ. toml_stat%success
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  filename = astr
  astr = ""
  ext = filename%Extension()

  SELECT CASE (ext%chars())
  CASE (".csv")
    CALL GetMatrixValueFromCSV(VALUE, isFound, filename, nrow, ncol)
  CASE DEFAULT
    ! read the text file
    CALL GetMatrixValueFromTXT(VALUE, isFound, filename, nrow, ncol)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValue3

!----------------------------------------------------------------------------
!                                                           GetMatrixFromCSV
!----------------------------------------------------------------------------

! Read MatrixValue from CSV file
SUBROUTINE GetMatrixValueFromCSV(VALUE, isFound, filename, nrow, ncol)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(String), INTENT(INOUT) :: filename
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV()"
#endif
  TYPE(CSVFile_) :: afile
  INTEGER(I4B) :: fileRow, fileCol

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0; ncol = 0

  CALL afile%Initiate(filename=filename%Chars(), &
                      action="READ", status="OLD", &
                      delimiter=",", comment="#")
  CALL afile%OPEN()
  CALL afile%READ()
  fileCol = afile%Getncols()
  fileRow = afile%Getnrows()

  CALL GetMatrixValueFromCSV1(VALUE, isFound, afile, fileRow, fileCol, &
                              nrow, ncol)

  IF (.NOT. isFound) CALL GetMatrixValueFromCSV2(VALUE, isFound, afile, &
                                                 fileRow, fileCol, nrow, ncol)
  IF (.NOT. isFound) CALL GetMatrixValueFromCSV3(VALUE, isFound, afile, &
                                                 fileRow, fileCol, nrow, ncol)

  CALL afile%DEALLOCATE()

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for ncol, it should ne 2, 3, or 5")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValueFromCSV

!----------------------------------------------------------------------------
!                                                    GetMatrixValueFromCSV1
!----------------------------------------------------------------------------

SUBROUTINE GetMatrixValueFromCSV1(VALUE, isFound, afile, fileRow, fileCol, &
                                  nrow, ncol)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: fileRow, fileCol
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr
  INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0
  ncol = 0
  CALL afile%Get(icol=1, irow=1, val=astr)

  isok = astr%Is_Integer()

  IF (isok) THEN
    astr = ""
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code is executed when astr is not an integer

  ! Each column in csv is imported as a column of VALUE
  nrow = fileRow
  ncol = fileCol

  DO jj = 1, ncol
    DO ii = 1, nrow
      CALL afile%Get(irow=ii, icol=jj, val=VALUE(ii, jj))
    END DO
  END DO

  isFound = math%yes
  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValueFromCSV1

!----------------------------------------------------------------------------
!                                                     GetMatrixValueFromCSV2
!----------------------------------------------------------------------------

! first column is treated as row index of VALUE
! second column is treated as column index of VALUE
! third column is treated as value at this location
SUBROUTINE GetMatrixValueFromCSV2(VALUE, isFound, afile, fileRow, fileCol, &
                                  nrow, ncol)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: fileCol, fileRow
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV2()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, row_ind1, column_ind1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0; ncol = 0

  isok = fileCol .EQ. 3
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! first column is treated as row index of VALUE
  ! second column is treated as column index of VALUE
  ! third column is treated as value at this location
  DO ii = 1, fileRow
    CALL afile%Get(icol=1, irow=ii, val=row_ind1) ! row index
    CALL afile%Get(icol=2, irow=ii, val=column_ind1) ! column index
    CALL afile%Get(icol=3, irow=ii, val=VALUE(row_ind1, column_ind1)) ! value
    isok = row_ind1 .GT. nrow
    IF (isok) nrow = row_ind1

    isok = column_ind1 .GT. ncol
    IF (isok) ncol = column_ind1
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValueFromCSV2

!----------------------------------------------------------------------------
!                                                      GetMatrixValueFromCSV2
!----------------------------------------------------------------------------

! first to fourth column must be integers
! first and second integers are treated
! as start and end index of row in VALUE
! third and fourth integers are treated
! as start and end index of column in VALUE
! fifth is value for VALUE(start:end, start:end)
SUBROUTINE GetMatrixValueFromCSV3(VALUE, isFound, afile, fileRow, fileCol, &
                                  nrow, ncol)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: fileRow, fileCol
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV3()"
#endif
  _DATA_TYPE_ :: temp
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, row_ind1, row_ind2, column_ind1, &
                  column_ind2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0; ncol = 0

  isok = fileCol .EQ. 5
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! the following code is executed when fileCol is 5

  DO ii = 1, fileRow
    CALL afile%Get(icol=1, irow=ii, val=row_ind1) ! start
    CALL afile%Get(icol=2, irow=ii, val=row_ind2) ! end
    CALL afile%Get(icol=3, irow=ii, val=column_ind1) ! start
    CALL afile%Get(icol=4, irow=ii, val=column_ind2) ! end
    CALL afile%Get(icol=5, irow=ii, val=temp) ! value
    VALUE(row_ind1:row_ind2, column_ind1:column_ind2) = temp

    isok = row_ind2 .GT. nrow
    IF (isok) nrow = row_ind2

    isok = column_ind2 .GT. ncol
    IF (isok) ncol = column_ind2
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValueFromCSV3

!----------------------------------------------------------------------------
!                                                      GetMatrixValueFromTXT
!----------------------------------------------------------------------------

SUBROUTINE GetMatrixValueFromTXT(VALUE, isFound, filename, nrow, ncol)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(String), INTENT(INOUT) :: filename
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromTXT()"
#endif
  CHARACTER(1024) :: iomsg
  TYPE(String) :: astr
  TYPE(String), ALLOCATABLE :: tokens(:)
  TYPE(TxtFile_) :: afile
  INTEGER(I4B) :: iostat, fileRow, fileCol, ii, jj
  LOGICAL(LGT) :: isok
  _DATA_TYPE_ :: temp

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  nrow = 0; ncol = 0

  CALL afile%Initiate(filename=filename%Chars(), &
                      action="READ", status="OLD", &
                      comment="#")

  CALL afile%OPEN()
  fileRow = afile%GetTotalRecords()

  DO ii = 1, fileRow
    nrow = nrow + 1
    CALL afile%READ(val=astr, iostat=iostat, iomsg=iomsg)

    IF (.NOT. afile%isValidRecord( &
        aline=astr, ignoreComment=math%yes, commentSymbol="#")) THEN
      nrow = nrow - 1
      CYCLE
    END IF

#ifdef DEBUG_VER
    isok = (iostat .EQ. 0) .OR. afile%IsEOF()
    CALL AssertError1(isok, myName, &
                   "error while reading txtfile, error message "//TRIM(iomsg))
#endif

    CALL astr%split(tokens=tokens, sep=afile%separator)

    fileCol = SIZE(tokens)
    IF (nrow .EQ. 1) ncol = fileCol

#ifdef DEBUG_VER
    CALL AssertError2(fileCol, ncol, myName, &
                      "Staggered matrix not allowed, a=fileCol, b=ncol")
#endif

    DO jj = 1, ncol
      VALUE(nrow, jj) = tokens(jj)%to_number(temp)
    END DO
  END DO

  isFound = math%yes

  CALL afile%DEALLOCATE()
  astr = ""
  isok = ALLOCATED(tokens)
  IF (isok) DEALLOCATE (tokens)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValueFromTXT

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE _SUBMODULE_NAME_

! #undef _SUBMODULE_NAME_
! #undef _METHOD_NAME_
! #undef _MY_NAME_
! #undef _DATA_TYPE_
