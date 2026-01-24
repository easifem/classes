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
! #define _SUBMODULE_NAME_ Int8MatrixMethods
! #define _METHOD_NAME_ GetValue_Int8_r2
! #define _MY_NAME_ "GetValue_Int8_r2()"
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
CALL GetMatrixValue1(table, key, VALUE, origin0, stat0, isFound0)

! if the above routine failed then we try to read the
! single value by using the following call
IF (.NOT. isFound0) &
  CALL GetMatrixValue2(table, key, VALUE, origin0, stat0, isFound0)

! if the above call fails then we try to read the
! value from the csv or text file
IF (.NOT. isFound0) &
  CALL GetMatrixValue3(table, key, VALUE, origin0, stat0, isFound0)

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
SUBROUTINE GetMatrixValue1(table, key, VALUE, origin, stat, isFound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValue1()"
#endif

  TYPE(toml_array), POINTER :: array, row_array
  INTEGER(I4B) :: ncol, nrow, ii, temp_ncol, jj
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no

  array => NULL()
  CALL toml_get(table, key, array, origin=origin, stat=stat, &
                requested=math%no)

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
      CALL Reallocate(VALUE, nrow, ncol)
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
SUBROUTINE GetMatrixValue2(table, key, VALUE, origin, stat, isFound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound

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

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code is executed when isok is true
  CALL Reallocate(VALUE, 1, 1)
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
SUBROUTINE GetMatrixValue3(table, key, VALUE, origin, stat, isFound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound

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
    CALL GetMatrixValueFromCSV(VALUE, isFound, filename)
  CASE DEFAULT
    ! read the text file
    CALL GetMatrixValueFromTXT(VALUE, isFound, filename)
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
SUBROUTINE GetMatrixValueFromCSV(VALUE, isFound, filename)
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(String), INTENT(INOUT) :: filename

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV()"
#endif
  TYPE(CSVFile_) :: afile
  INTEGER(I4B) :: nrow, ncol

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no

  CALL afile%Initiate(filename=filename%Chars(), &
                      action="READ", status="OLD", &
                      delimiter=",", comment="#")
  CALL afile%OPEN()
  CALL afile%READ()
  ncol = afile%Getncols()
  nrow = afile%Getnrows()

  CALL GetMatrixValueFromCSV1(VALUE, isFound, afile, nrow, ncol)

  IF (.NOT. isFound) CALL GetMatrixValueFromCSV2(VALUE, isFound, afile, ncol)
  IF (.NOT. isFound) CALL GetMatrixValueFromCSV3(VALUE, isFound, afile, ncol)

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

SUBROUTINE GetMatrixValueFromCSV1(VALUE, isFound, afile, nrow, ncol)
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: nrow, ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr
  INTEGER(I4B) :: ii
  _DATA_TYPE_, ALLOCATABLE :: tempvalvec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  CALL afile%Get(icol=1, irow=1, val=astr)

  isok = astr%Is_Integer()

  IF (isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code is executed when astr is not an integer

  ! Each column in csv is imported as a column of VALUE
  CALL Reallocate(VALUE, nrow, ncol)

  DO ii = 1, ncol
    CALL afile%Get(ii, val=tempvalvec)
    VALUE(1:nrow, ii) = tempvalvec(1:nrow)
  END DO

  isFound = math%yes

  IF (ALLOCATED(tempvalvec)) DEALLOCATE (tempvalvec)

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
SUBROUTINE GetMatrixValueFromCSV2(VALUE, isFound, afile, ncol)
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV2()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, tsize
  _DATA_TYPE_, ALLOCATABLE :: tempvalvec(:)
  INTEGER(I4B), ALLOCATABLE :: tempintvec1(:), tempintvec2(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no

  isok = ncol .EQ. 3
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
  CALL afile%Get(1, val=tempintvec1) ! row index
  CALL afile%Get(2, val=tempintvec2) ! column index
  CALL afile%Get(3, val=tempvalvec) ! value

  ii = MAXVAL(tempintvec1)
  jj = MAXVAL(tempintvec2)
  tsize = SIZE(tempintvec1)

  CALL Reallocate(VALUE, ii, jj)

  DO ii = 1, tsize
    VALUE(tempintvec1(ii), tempintvec2(ii)) = tempvalvec(ii)
  END DO

  DEALLOCATE (tempintvec1, tempintvec2, tempvalvec)

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
SUBROUTINE GetMatrixValueFromCSV3(VALUE, isFound, afile, ncol)
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: ncol

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromCSV3()"
#endif
  _DATA_TYPE_, ALLOCATABLE :: tempvalvec(:)

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, tsize
  INTEGER(I4B), ALLOCATABLE :: tempintvec1(:), tempintvec2(:), &
                               tempintvec3(:), tempintvec4(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isok = ncol .EQ. 5

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! the following code is executed when ncol is 5

  CALL afile%Get(1, val=tempintvec1) ! start
  CALL afile%Get(2, val=tempintvec2) ! end
  CALL afile%Get(3, val=tempintvec3) ! start
  CALL afile%Get(4, val=tempintvec4) ! end
  CALL afile%Get(5, val=tempvalvec) ! value

  ii = MAXVAL(tempintvec2)
  jj = MAXVAL(tempintvec4)
  tsize = SIZE(tempintvec1)
  CALL Reallocate(VALUE, ii, jj)

  DO ii = 1, SIZE(tempintvec1)
    VALUE(tempintvec1(ii):tempintvec2(ii), &
          tempintvec3(ii):tempintvec4(ii)) = tempvalvec(ii)
  END DO

  DEALLOCATE (tempintvec1, tempintvec2, tempintvec3, tempintvec4, &
              tempvalvec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMatrixValueFromCSV3

!----------------------------------------------------------------------------
!                                                      GetMatrixValueFromTXT
!----------------------------------------------------------------------------

SUBROUTINE GetMatrixValueFromTXT(VALUE, isFound, filename)
  _DATA_TYPE_, ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  TYPE(String), INTENT(INOUT) :: filename

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMatrixValueFromTXT()"
#endif
  CHARACTER(1024) :: iomsg
  TYPE(TxtFile_) :: afile
  INTEGER(I4B) :: iostat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no

  CALL afile%Initiate(filename=filename%Chars(), &
                      action="READ", status="OLD", &
                      comment="#")

  CALL afile%OPEN()
  CALL afile%READ(val=VALUE, iostat=iostat, iomsg=iomsg, &
                  ignoreComment=math%yes)

#ifdef DEBUG_VER
  isok = (iostat .EQ. 0) .OR. afile%IsEOF()
  CALL AssertError1(isok, myName, &
                   "error while reading txtfile, error message "//TRIM(iomsg))
#endif

  isFound = math%yes

  CALL afile%DEALLOCATE()

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
