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

!----------------------------------------------------------------------------
!                                                              Include Start
!----------------------------------------------------------------------------

TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: tsize, stat0, iostat, ii, ncol
TYPE(String) :: filename, ext
TYPE(TxtFile_) :: atxtfile
TYPE(CSVFile_) :: acsvfile
CHARACTER(512) :: iomsg
LOGICAL(LGT) :: isFound0, isok
INTEGER(I4B), ALLOCATABLE :: tempintvec1(:), tempintvec2(:)
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isFound0 = math%no
IF (PRESENT(isScalar)) isScalar = math%no

!----------------------------------------------------------------------------
! READ from TOML array (1D)
! try to read from the toml array
! the data is given in toml file itself as toml array
!----------------------------------------------------------------------------
array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat0, &
              requested=math%no)

isok = ASSOCIATED(array)

IF (isok) THEN
  tsize = toml_len(array)

  CALL Reallocate(VALUE, tsize)

  isFound0 = math%yes

  DO ii = 1, tsize
    CALL toml_get(array, ii, VALUE(ii))
  END DO

  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isFound)) isFound = isFound0
  NULLIFY (array)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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

  isFound0 = math%yes
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  IF (PRESENT(isScalar)) isScalar = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isFound0 = math%no
IF (PRESENT(isFound)) isFound = isFound0
IF (PRESENT(stat)) stat = stat0

!----------------------------------------------------------------------------
! READ from a txt file or csv file
! try to read from the file
! the data is given in a txt file
! the filename is given in the toml file
! the line beginning with # is ignored
! no header csv file is expected
!----------------------------------------------------------------------------

CALL toml_get(table, key, astr, origin=origin, stat=stat0)

isok = (stat0 .EQ. toml_stat%success)

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

SELECT CASE (ext%Chars())

CASE (".csv")
  CALL acsvfile%Initiate(filename=filename%Chars(), &
                         action="READ", status="OLD", &
                         delimiter=",", comment="#")
  CALL acsvfile%OPEN()
  ! CALL acsvfile%SetHeaderIndx(1)
  CALL acsvfile%READ()
  ncol = acsvfile%Getncols()
  SELECT CASE (ncol)
  CASE (1)
    ! a column values is imported as VALUE
    CALL acsvfile%Get(1, val=VALUE)

  CASE (2)
    ! first column is treated as index of VALUE
    ! second column is treated as value at that index
    CALL acsvfile%Get(1, val=tempintvec1) ! index
    CALL acsvfile%Get(2, val=tempvalvec) ! value

    tsize = MAXVAL(tempintvec1)
    CALL Reallocate(VALUE, tsize)
    tsize = SIZE(tempintvec1)
    DO ii = 1, tsize
      VALUE(tempintvec1(ii)) = tempvalvec(ii)
    END DO

  CASE (3)
    ! first and second columns must be integers
    ! which determine the start and end index of VALUE
    ! third column is value for this range
    CALL acsvfile%Get(1, val=tempintvec1) ! start
    CALL acsvfile%Get(2, val=tempintvec2) ! end
    CALL acsvfile%Get(3, val=tempvalvec) ! value

    tsize = MAXVAL(tempintvec2)
    CALL Reallocate(VALUE, tsize)

    tsize = SIZE(tempintvec1)
    DO ii = 1, tsize
      VALUE(tempintvec1(ii):tempintvec2(ii)) = tempvalvec(ii)
    END DO

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      'Number of columns in csv file should be 1, 2 or 3')
#endif

  END SELECT

  isFound0 = math%yes

  CALL acsvfile%DEALLOCATE()
  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  filename = ""
  ext = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN

CASE DEFAULT

  CALL atxtfile%Initiate(filename=filename%Chars(), &
                         action="READ", status="OLD", &
                         comment="#")
  CALL atxtfile%OPEN()
  CALL atxtfile%READ(val=VALUE, iostat=iostat, iomsg=iomsg, &
                     ignoreComment=math%yes)

#ifdef DEBUG_VER
  isok = (iostat .EQ. 0) .OR. (atxtfile%IsEOF())
  CALL AssertError1(isok, myName, &
                    'Error while reading txtfile, errmsg= '//TRIM(iomsg))
#endif

  isFound0 = math%yes
  CALL atxtfile%DEALLOCATE()

  IF (PRESENT(isFound)) isFound = isFound0
  IF (PRESENT(stat)) stat = stat0
  filename = ""
  ext = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

!----------------------------------------------------------------------------
!                                                                Include end
!----------------------------------------------------------------------------
