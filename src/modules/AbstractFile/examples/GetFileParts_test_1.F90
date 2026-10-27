!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Get the path, name, and extension of a file
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! Initiate splits the filename into path, name, and extension.
! GetFilePath, GetFileName, GetFileExt, and GetFileParts return them.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./GetFileParts_test_1.txt"
  TYPE(FortranFile_) :: obj
  TYPE(String) :: path, fname, ext

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename)

  path = obj%GetFilePath()
  CALL OK(path%Chars() .EQ. "./", "GetFilePath")

  fname = obj%GetFileName()
  CALL OK(fname%Chars() .EQ. "GetFileParts_test_1", "GetFileName")

  ext = obj%GetFileExt()
  CALL OK(ext%Chars() .EQ. ".txt", "GetFileExt")

  CALL obj%GetFileParts(path=path, filename=fname, ext=ext)
  CALL OK(path%Chars() .EQ. "./", "GetFileParts: path")
  CALL OK(fname%Chars() .EQ. "GetFileParts_test_1", &
          "GetFileParts: fileName")
  CALL OK(ext%Chars() .EQ. ".txt", "GetFileParts: ext")

  CALL obj%DEALLOCATE()
END PROGRAM main
