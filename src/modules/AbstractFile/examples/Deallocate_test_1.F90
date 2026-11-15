!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Reset the data of an abstract file
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! AbstractFileDeallocate resets the path, name, extension, and status
! flags. Extended types call it from their own Deallocate.

PROGRAM main
  USE AbstractFile_Class, ONLY: AbstractFileDeallocate
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./Deallocate_test_1.txt"
  TYPE(FortranFile_) :: obj
  TYPE(String) :: path, fname, ext

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename)
  CALL AbstractFileDeallocate(obj)

  CALL obj%GetFileParts(path=path, filename=fname, ext=ext)
  CALL OK(path%Chars() .EQ. "", "Deallocate: path")
  CALL OK(fname%Chars() .EQ. "", "Deallocate: fileName")
  CALL OK(ext%Chars() .EQ. "", "Deallocate: ext")
  CALL OK(.NOT. obj%IsOpen(), "Deallocate: IsOpen")
  CALL OK(.NOT. obj%IsRead(), "Deallocate: IsRead")
  CALL OK(.NOT. obj%IsWrite(), "Deallocate: IsWrite")

  CALL obj%DEALLOCATE()
END PROGRAM main
