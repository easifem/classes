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

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Utility module for [[CommandLineArg_]] class
!
!# Introduction
!
! TODO Merge this module with the [[String_Class]] module

MODULE CommandLineArg_Utils
USE PENF
IMPLICIT NONE
PRIVATE
PUBLIC :: count
PUBLIC :: replace
PUBLIC :: replace_all
PUBLIC :: tokenize
PUBLIC :: unique
PUBLIC :: upper_case
PUBLIC :: wstrip

! Overload intrinsic function count for counting substring occurences into
! strings.

INTERFACE count
  MODULE PROCEDURE count_substring
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Contains
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                  count
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION count_substring(string, substring) RESULT(No)
  !! Count the number of occurences of a substring into a string.
  CHARACTER(*), INTENT(in) :: string
  !! String.
  CHARACTER(*), INTENT(in) :: substring
  !! Substring.
  INTEGER(I4P) :: No
  !! Number of occurrences.
  INTEGER(I4P) :: c1
  !! Counters.
  INTEGER(I4P) :: c2
  !! Counters.
  No = 0
  IF (LEN(substring) > LEN(string)) RETURN
  c1 = 1
  DO
    c2 = INDEX(string=string(c1:), substring=substring)
    IF (c2 == 0) RETURN
    No = No + 1
    c1 = c1 + c2 + LEN(substring)
  END DO
END FUNCTION count_substring

!----------------------------------------------------------------------------
!                                                                  replace
!----------------------------------------------------------------------------

PURE FUNCTION replace(string, substring, restring) RESULT(newstring)
  !! Replace substring (only first occurrence) into a string.
  CHARACTER(len=*), INTENT(in) :: string
  !! to be modified.
  CHARACTER(len=*), INTENT(in) :: substring
  !! Substring to be replaced.
  CHARACTER(len=*), INTENT(in) :: restring
  !! String to be inserted.
  CHARACTER(len=:), ALLOCATABLE :: newstring
  !! New modified string.
  INTEGER(I4P) :: pos
  !! Position from which replace the substring.

  pos = INDEX(string=string, substring=substring)
  newstring = string
  IF (pos > 0) THEN
    IF (pos == 1) THEN
      newstring = restring//string(LEN(substring) + 1:)
    ELSE
      newstring = string(1:pos - 1)//restring//string(pos + LEN(substring):)
    END IF
  END IF
END FUNCTION replace

!----------------------------------------------------------------------------
!                                                                  replace
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Replace all occurences of substring into a string
!
!# Introduction
!
!@note
!Leading and trailing white spaces are stripped out.
!@endnote

PURE FUNCTION replace_all(string, substring, restring) RESULT(newstring)
  CHARACTER(len=*), INTENT(in) :: string
  !! String to be modified.
  CHARACTER(len=*), INTENT(in) :: substring
  !! Substring to be replaced.
  CHARACTER(len=*), INTENT(in) :: restring
  !! String to be inserted.
  CHARACTER(len=:), ALLOCATABLE :: newstring
  !! New modified string.

  newstring = wstrip(string)
  DO
    IF (INDEX(newstring, substring) > 0) THEN
      newstring = replace(string=newstring, substring=substring, &
        & restring=restring)
    ELSE
      EXIT
    END IF
  END DO
END FUNCTION replace_all

!----------------------------------------------------------------------------
!                                                                  tokenize
!----------------------------------------------------------------------------

! Tokenize a string in order to parse it.
!
! @note The dummy array containing tokens must allocatable and its character
! elements must have the same length of the input
! string. If the length of the delimiter is higher than the input string
! one then
! the output tokens array is allocated with
! only one element set to input string.

PURE SUBROUTINE tokenize(strin, delimiter, toks, Nt)
  CHARACTER(len=*), INTENT(in) :: strin
  !! String to be tokenized.
  CHARACTER(len=*), INTENT(in) :: delimiter
  !! Delimiter of tokens.
  CHARACTER(len=LEN(strin)), INTENT(out), ALLOCATABLE :: toks(:)
  !! Tokens.
  INTEGER(I4P), INTENT(out), OPTIONAL :: Nt
  !! Number of tokens.
  CHARACTER(len=LEN(strin)) :: strsub
  !! Temporary string.
  INTEGER(I4P) :: dlen
  !! Delimiter length.
  INTEGER(I4P) :: c
  !! Counter.
  INTEGER(I4P) :: n
  !! Counter.
  INTEGER(I4P) :: t
  !! Counter.

  ! initialization
  IF (ALLOCATED(toks)) DEALLOCATE (toks)
  strsub = strin
  dlen = LEN(delimiter)
  IF (dlen > LEN(strin)) THEN
    ALLOCATE (toks(1:1)); toks(1) = strin; IF (PRESENT(Nt)) Nt = 1; RETURN
  END IF
  ! compute the number of tokens
  n = 1
  DO c = 1, LEN(strsub) - dlen ! loop over string characters
    IF (strsub(c:c + dlen - 1) == delimiter) n = n + 1
  END DO
  ALLOCATE (toks(1:n))
  ! tokenization
  DO t = 1, n ! loop over tokens
    c = INDEX(strsub, delimiter)
    IF (c > 0) THEN
      toks(t) = strsub(1:c - 1)
      strsub = strsub(c + dlen:)
    ELSE
      toks(t) = strsub
    END IF
  END DO
  IF (PRESENT(Nt)) Nt = n
END SUBROUTINE tokenize

!----------------------------------------------------------------------------
!                                                                  unique
!----------------------------------------------------------------------------

! Reduce to one (unique) multiple (sequential) occurrences of a
! characters substring into a string.
!
! For example the string ' ab-cre-cre-ab' is reduce to 'ab-cre-ab'
! if the substring is '-cre'.
! @note Eventual multiple trailing white space are not reduced to
! one occurrence.

ELEMENTAL FUNCTION unique(string, substring) RESULT(uniq)
  CHARACTER(len=*), INTENT(in) :: string
  !! String to be parsed.
  CHARACTER(len=*), INTENT(in) :: substring
  !! Substring which multiple occurences must be reduced to one.
  CHARACTER(len=LEN(string)) :: uniq
  !! String parsed.
  INTEGER(I4P) :: Lsub
  !! Lenght of substring.
  INTEGER(I4P) :: c1
  !! Counter.
  INTEGER(I4P) :: c2
  !! Counter.

  uniq = string
  Lsub = LEN(substring)
  IF (Lsub > LEN(string)) RETURN
  c1 = 1
  Loop1: DO
    IF (c1 >= len_TRIM(uniq)) EXIT Loop1
    IF (uniq(c1:c1+Lsub-1)==substring.AND.uniq(c1+Lsub:c1+2*Lsub-1)==substring) THEN
      c2 = c1 + Lsub
      Loop2: DO
        IF (c2 >= len_TRIM(uniq)) EXIT Loop2
        IF (uniq(c2:c2 + Lsub - 1) == substring) THEN
          c2 = c2 + Lsub
        ELSE
          EXIT Loop2
        END IF
      END DO Loop2
      uniq = uniq(1:c1)//uniq(c2:)
    ELSE
      c1 = c1 + Lsub
    END IF
  END DO Loop1
END FUNCTION unique

!----------------------------------------------------------------------------
!                                                                  upper_case
!----------------------------------------------------------------------------

! Convert the lower case characters of a string to upper case one.

ELEMENTAL FUNCTION upper_case(string)
  CHARACTER(len=*), INTENT(in) :: string
!! String to be converted.
  CHARACTER(len=LEN(string)) :: upper_case
!! Converted string.
  INTEGER :: n1
!! Characters counter.
  INTEGER :: n2
!! Characters counter.
  CHARACTER(len=26), PARAMETER :: &
    & upper_alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!! Upper case alphabet.
  CHARACTER(len=26), PARAMETER :: &
    & lower_alphabet = 'abcdefghijklmnopqrstuvwxyz'
!! Lower case alphabet.

  upper_case = string
  DO n1 = 1, LEN(string)
    n2 = INDEX(lower_alphabet, string(n1:n1))
    IF (n2 > 0) upper_case(n1:n1) = upper_alphabet(n2:n2)
  END DO
END FUNCTION upper_case

!----------------------------------------------------------------------------
!                                                                  wstrip
!----------------------------------------------------------------------------
! Strip out leading and trailing white spaces from a string.

PURE FUNCTION wstrip(string) RESULT(newstring)
  CHARACTER(len=*), INTENT(in) :: string
  !! String to be modified.
  CHARACTER(len=:), ALLOCATABLE :: newstring
  !! New modified string.

  ALLOCATE (newstring, source=TRIM(ADJUSTL(string)))
END FUNCTION wstrip
endmodule CommandLineArg_Utils
