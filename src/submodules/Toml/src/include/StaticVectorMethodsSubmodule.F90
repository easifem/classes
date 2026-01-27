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
! #define _SUBMODULE_NAME_ Int8StaticVectorMethods
! #define _METHOD_NAME_ GetValue_Int8_r1_static
! #define _MY_NAME_ "GetValue_Int8_r1_static()"
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
LOGICAL(LGT) :: isFound0, isScalar0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(origin)) origin0 = origin
IF (PRESENT(stat)) stat0 = stat

! the following call get value from the toml array
CALL GetVectorValue1(table, key, VALUE, origin0, stat0, isFound0, &
                     isScalar0, tsize)

! if the above routine failed then we try to read the
! single value by using the following call
IF (.NOT. isFound0) &
  CALL GetVectorValue2(table, key, VALUE, origin0, stat0, isFound0, &
                       isScalar0, tsize)

! if the above call fails then we try to read the
! value from the csv or text file
IF (.NOT. isFound0) &
  CALL GetVectorValue3(table, key, VALUE, origin0, stat0, isFound0, &
                       isScalar0, tsize)

IF (PRESENT(origin)) origin = origin0
IF (PRESENT(stat)) stat = stat0
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(isScalar)) isScalar = isScalar0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE _METHOD_NAME_

!----------------------------------------------------------------------------
!                                                          GetMetaDataFromCSV
!----------------------------------------------------------------------------

SUBROUTINE GetMetaDataFromCSV(afile, version, nrow, ncol)
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(OUT) :: version
  INTEGER(I4B), INTENT(OUT) :: nrow
  INTEGER(I4B), INTENT(OUT) :: ncol

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetMetaDataFromCSV()"
  CHARACTER(*), PARAMETER :: three_dashes = "---", &
                             version_char = "version", &
                             nrow_char = "nrow", &
                             ncol_char = "ncol"
  CHARACTER(3) :: dashes
#endif

  CHARACTER(*), PARAMETER :: sep = ",", colon = ":"
  TYPE(String) :: astr
  TYPE(String), ALLOCATABLE :: tokens(:), two_tokens(:)
  INTEGER(I4B) :: iostat
  CHARACTER(1024) :: iomsg
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading first line of csv file metadata...')
#endif

! skip line 1
  CALL afile%ReadLine(val=astr, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
  isok = .NOT. afile%IsEOF()
  CALL AssertError1(isok, myName, &
                    "It seems the csvfile has reached end of file")
#endif

#ifdef DEBUG_VER
  isok = iostat .LE. 0
  CALL AssertError1(isok, myName, &
                    "error while reading csvfile, iomsg: "//TRIM(iomsg))
#endif

#ifdef DEBUG_VER
  dashes(1:3) = astr%slice(2, 4)
  isok = dashes .EQ. three_dashes
  CALL AssertError1(isok, myName, &
    "first line in csvfile should have three dashes ---, but it has "//dashes)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading second line of csv file metadata...')
#endif

! read line 2, and process it
  CALL afile%ReadLine(val=astr, iostat=iostat, iomsg=iomsg, &
                      ignoreComment=math%no, ignoreBlank=math%no)

#ifdef DEBUG_VER
  dashes(1:1) = astr%slice(1, 1)
  isok = dashes(1:1) .EQ. "#"
  CALL AssertError1(isok, myName, &
                    "Second line should be a comment starting with #")
#endif

#ifdef DEBUG_VER
  ii = astr%SCAN(set=version_char)
  isok = ii .NE. math%zero_i
  CALL AssertError1(isok, myName, version_char//" not found in line 2")
#endif

#ifdef DEBUG_VER
  ii = astr%SCAN(set=nrow_char)
  isok = ii .NE. math%zero_i
  CALL AssertError1(isok, myName, nrow_char//" not found in line 2")
#endif

#ifdef DEBUG_VER
  ii = astr%SCAN(set=ncol_char)
  isok = ii .NE. math%zero_i
  CALL AssertError1(isok, myName, ncol_char//" not found in line 2")
#endif

  CALL astr%split(tokens=tokens, sep=sep)
  tsize = SIZE(tokens)

#ifdef DEBUG_VER
  isok = tsize .GE. 3
  CALL AssertError1(isok, myName, &
                    "version, nrow, ncol should be present in line 3")
#endif

! handle token 1
  CALL tokens(1)%split(tokens=two_tokens, sep=colon)

#ifdef DEBUG_VER
  astr = two_tokens(1)%slice(2, two_tokens(1)%LEN())
  two_tokens(1) = astr%ADJUSTL()
  astr = two_tokens(1)%TRIM()

  isok = astr//"" == version_char
  CALL AssertError1(isok, myName, &
                    "first of tokens(1) should be version, but it is "//astr)
#endif

#ifdef DEBUG_VER
  isok = two_tokens(2)%is_integer()
  CALL AssertError1(isok, myName, &
     "second of token(1) should be integer number, but it is "//two_tokens(2))
#endif

  version = two_tokens(2)%to_number(ii)

! handle token 2
  CALL tokens(2)%split(tokens=two_tokens, sep=colon)

#ifdef DEBUG_VER
  astr = two_tokens(1)%ADJUSTL()
  two_tokens(1) = astr%TRIM()
  isok = two_tokens(1)//"" == nrow_char
  CALL AssertError1(isok, myName, &
                "first of tokens(2) should be nrow, but it is"//two_tokens(1))
#endif

#ifdef DEBUG_VER
  isok = two_tokens(2)%is_integer()
  CALL AssertError1(isok, myName, &
     "second of token(2) should be integer number, but it is "//two_tokens(2))
#endif

  nrow = two_tokens(2)%to_number(ii)

! handle token 3
  CALL tokens(3)%split(tokens=two_tokens, sep=colon)

#ifdef DEBUG_VER
  astr = two_tokens(1)%ADJUSTL()
  two_tokens(1) = astr%TRIM()
  isok = two_tokens(1)//"" == ncol_char
  CALL AssertError1(isok, myName, &
                "first of tokens(3) should be ncol, but it is"//two_tokens(1))
#endif

#ifdef DEBUG_VER
  isok = two_tokens(2)%is_integer()
  CALL AssertError1(isok, myName, &
    "second of tokens(3) should be integer number, but it is "//two_tokens(2))
#endif

  ncol = two_tokens(2)%to_number(ii)

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading third line of csv file metadata')
#endif

! skip line 3
  CALL afile%ReadLine(val=astr, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
  isok = .NOT. afile%IsEOF()
  CALL AssertError1(isok, myName, &
                    "It seems the csvfile has reached end of file")
#endif

#ifdef DEBUG_VER
  isok = iostat .LE. 0
  CALL AssertError1(isok, myName, &
                    "error while reading csvfile, iomsg: "//TRIM(iomsg))
#endif

#ifdef DEBUG_VER
  dashes(1:3) = astr%slice(2, 4)
  isok = dashes .EQ. three_dashes
  CALL AssertError1(isok, myName, &
    "first line in csvfile should have three dashes ---, but it has "//dashes)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading second line of csv file metadata...')
#endif

  DEALLOCATE (tokens, two_tokens)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetMetaDataFromCSV

!----------------------------------------------------------------------------
!                                                            GetVectorValue1
!----------------------------------------------------------------------------

! READ from TOML array
! try to read from the toml array
! the data is given in toml file itself as toml array
SUBROUTINE GetVectorValue1(table, key, VALUE, origin, stat, isFound, &
                           isScalar, tsize)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue1()"
#endif

  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: ii
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

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
  tsize = toml_len(array)
  isFound = math%yes

  DO ii = 1, tsize
    CALL toml_get(array, ii, VALUE(ii))
  END DO

  NULLIFY (array)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValue1

!----------------------------------------------------------------------------
!                                                            GetMatrixValue2
!----------------------------------------------------------------------------

! READ a scalar value from toml
! In this case shape of matrix is (1,1), this value is given in toml file
SUBROUTINE GetVectorValue2(table, key, VALUE, origin, stat, isFound, &
                           isScalar, tsize)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue2()"
#endif

  _DATA_TYPE_ :: temp

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, key, temp, origin=origin, stat=stat)

  isFound = stat .EQ. toml_stat%success
  isScalar = math%no
  tsize = 0

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! The following code is executed when isok is true
  VALUE(1) = temp
  isScalar = math%yes
  tsize = 1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValue2

!----------------------------------------------------------------------------
!                                                            GetMatrixValue3
!----------------------------------------------------------------------------

! READ from a txt file or csv file
! try to read from the file
! the data is given in a txt file
! the filename is given in the toml file
! the line beginning with # is treated as comment line and is ignored
! no header csv file is expected
SUBROUTINE GetVectorValue3(table, key, VALUE, origin, stat, isFound, &
                           isScalar, tsize)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  INTEGER(I4B), INTENT(OUT) :: tsize

! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue3()"
#endif

  CHARACTER(:), ALLOCATABLE :: astr
  LOGICAL(LGT) :: isok
  TYPE(String) :: filename, ext

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

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
    CALL GetVectorValueFromCSV(VALUE, isFound, isScalar, filename, tsize)
  CASE DEFAULT
    ! read the text file
    CALL GetVectorValueFromTXT(VALUE, isFound, isScalar, filename, tsize)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValue3

!----------------------------------------------------------------------------
!                                                           GetMatrixFromCSV
!----------------------------------------------------------------------------

! Read MatrixValue from CSV file
! first three rows are comments which contains the meta data
! the first row is just dashes for pretrtifying
! the second row is version: 1, nrow: xx, ncol: xx
! the third row is just dashes for prettifying
SUBROUTINE GetVectorValueFromCSV(VALUE, isFound, isScalar, filename, tsize)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  TYPE(String), INTENT(INOUT) :: filename
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValueFromCSV()"
#endif
  TYPE(CSVFile_) :: afile
  INTEGER(I4B) :: nrow, ncol, version, fileRow, fileCol

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

  CALL afile%Initiate(filename=filename%Chars(), &
                      action="READ", status="OLD", &
                      delimiter=",", comment="#")
  CALL afile%OPEN()

  ! read the meta data of the file
  CALL GetMetaDataFromCSV(afile, version, fileRow, fileCol)

  CALL afile%READ()

  ncol = afile%Getncols()
  nrow = afile%Getnrows()

#ifdef DEBUG_VER
  CALL AssertError2(fileCol, ncol, myName, &
                    "error reading csvfile, a=fileCol, b=ncol")
#endif

  SELECT CASE (version)

  CASE (1)
#ifdef DEBUG_VER
    CALL AssertError2(fileCol, math%one_i, myName, &
                      "error reading version 1, a=fileCol, b=1")
#endif
    ! read version 1
    CALL GetVectorValueFromCSV1(VALUE, isFound, isScalar, afile, nrow, ncol, &
                                tsize)

  CASE (2)
#ifdef DEBUG_VER
    CALL AssertError2(fileCol, math%two_i, myName, &
                      "error reading version 2, a=fileCol, b=2")
#endif
    CALL GetVectorValueFromCSV2(VALUE, isFound, isScalar, afile, nrow, &
                                ncol, tsize)

  CASE (3)
#ifdef DEBUG_VER
    CALL AssertError2(fileCol, math%three_i, myName, &
                      "error reading version 3, a=fileCol, b=3")
#endif
    CALL GetVectorValueFromCSV3(VALUE, isFound, isScalar, afile, nrow, ncol, &
                                tsize)

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for ncol, it should equal 1, 2, or 3")
#endif

  END SELECT

  CALL afile%DEALLOCATE()

#ifdef DEBUG_VER
  CALL AssertError1(isFound, myName, &
                    "some error has occured while reading csvfile.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValueFromCSV

!----------------------------------------------------------------------------
!                                                    GetVectorValueFromCSV1
!----------------------------------------------------------------------------

! Here we are reading version 1
SUBROUTINE GetVectorValueFromCSV1(VALUE, isFound, isScalar, afile, &
                                  nrow, ncol, tsize)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: nrow, ncol
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValueFromCSV1()"
#endif
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

  ! The following code is executed when ncol is equal to 1
  ! here we are getting the first column
  tsize = nrow
  DO ii = 1, tsize
    CALL afile%Get(icol=1, irow=ii, val=VALUE(ii))
  END DO

  isFound = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValueFromCSV1

!----------------------------------------------------------------------------
!                                                     GetVectorValueFromCSV2
!----------------------------------------------------------------------------

! here we read version 2
! first column is treated as row index of VALUE
! second column is treated as column index of VALUE
! third column is treated as value at this location
SUBROUTINE GetVectorValueFromCSV2(VALUE, isFound, isScalar, afile, nrow, &
                                  ncol, tsize)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: nrow, ncol
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValueFromCSV2()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, ind1
  _DATA_TYPE_ :: temp

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

  ! The following code is executed when number of columns are 2
  ! First column is treated as index of VALUE
  ! Second column is treated as value
  DO ii = 1, nrow
    CALL afile%Get(icol=1, irow=ii, val=ind1) ! index
    CALL afile%Get(icol=2, irow=ii, val=temp) ! value

    VALUE(ind1) = temp

    isok = ind1 .GT. tsize
    IF (isok) tsize = ind1
  END DO

  isFound = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValueFromCSV2

!----------------------------------------------------------------------------
!                                                      GetVectorValueFromCSV2
!----------------------------------------------------------------------------

! Here we read version 3
! Version 3 has three columns
! First and Second column must be integers
!     first and second integers are treated
!     as start and end index of VALUE
! Third column is value for VALUE(start:end)
SUBROUTINE GetVectorValueFromCSV3(VALUE, isFound, isScalar, afile, &
                                  nrow, ncol, tsize)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  TYPE(CSVFile_), INTENT(INOUT) :: afile
  INTEGER(I4B), INTENT(IN) :: nrow, ncol
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValueFromCSV3()"
#endif
  _DATA_TYPE_ :: temp

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, ind1, ind2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

  ! the following code is executed when ncol is 3
  DO ii = 1, nrow
    CALL afile%Get(icol=1, irow=ii, val=ind1) ! start
    CALL afile%Get(icol=2, irow=ii, val=ind2) ! end
    CALL afile%Get(icol=3, irow=ii, val=temp) ! value
    VALUE(ind1:ind2) = temp

    isok = ind2 .GT. tsize
    IF (isok) tsize = ind2
  END DO

  isFound = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValueFromCSV3

!----------------------------------------------------------------------------
!                                                      GetMatrixValueFromTXT
!----------------------------------------------------------------------------

SUBROUTINE GetVectorValueFromTXT(VALUE, isFound, isScalar, filename, tsize)
  _DATA_TYPE_, INTENT(INOUT) :: VALUE(:)
  LOGICAL(LGT), INTENT(INOUT) :: isFound
  LOGICAL(LGT), INTENT(INOUT) :: isScalar
  TYPE(String), INTENT(INOUT) :: filename
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValueFromTXT()"
#endif
  CHARACTER(1024) :: iomsg
  TYPE(TxtFile_) :: afile
  INTEGER(I4B) :: iostat, ii
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isFound = math%no
  isScalar = math%no
  tsize = 0

  CALL afile%Initiate(filename=filename%Chars(), &
                      action="READ", status="OLD", &
                      comment="#")

  CALL afile%OPEN()

  tsize = afile%GetTotalRecords(ignoreComment=math%yes)

  DO ii = 1, tsize
    CALL afile%READ(val=VALUE(ii), iostat=iostat, iomsg=iomsg, &
                    ignoreComment=math%yes)

#ifdef DEBUG_VER
    isok = (iostat .EQ. 0) .OR. (afile%IsEOF())
    CALL AssertError1(isok, myName, &
                      'Error while reading txtfile, errmsg= '//TRIM(iomsg))
#endif

  END DO

  isFound = math%yes

  CALL afile%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetVectorValueFromTXT

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE _SUBMODULE_NAME_

! #undef _SUBMODULE_NAME_
! #undef _METHOD_NAME_
! #undef _MY_NAME_
! #undef _DATA_TYPE_
