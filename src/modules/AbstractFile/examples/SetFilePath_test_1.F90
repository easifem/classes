!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Set the path, name, and extension of a closed file
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! SetFilePath, SetFileName, and SetFileExt change the parts of the file
! name while the file is not open.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./SetFilePath_test_1.txt"
  TYPE(FortranFile_) :: obj
  TYPE(String) :: path, fname, ext

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename)

  path = "./output/"
  CALL obj%SetFilePath(path)
  fname = "result"
  CALL obj%SetFileName(fname)
  ext = ".dat"
  CALL obj%SetFileExt(ext)

  CALL obj%GetFileParts(path=path, filename=fname, ext=ext)
  CALL OK(path%Chars() .EQ. "./output/", "SetFilePath")
  CALL OK(fname%Chars() .EQ. "result", "SetFileName")
  CALL OK(ext%Chars() .EQ. ".dat", "SetFileExt")

  CALL obj%DEALLOCATE()
END PROGRAM main
