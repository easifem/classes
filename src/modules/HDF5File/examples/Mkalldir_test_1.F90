!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Create a group and all its parents

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Mkalldir_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL obj%Mkalldir("/grp4/grp4a/grp4a1")
  CALL OK(obj%IsGroup("/grp4"), "/grp4")
  CALL OK(obj%IsGroup("/grp4/grp4a"), "/grp4/grp4a")
  CALL OK(obj%IsGroup("/grp4/grp4a/grp4a1"), "/grp4/grp4a/grp4a1")

! existing groups are left as they are
  CALL obj%Mkalldir("/grp4/grp4a/grp4a2")
  CALL OK(obj%Ngrp("/grp4/grp4a") .EQ. 2, "Ngrp('/grp4/grp4a')")

  CALL obj%DEALLOCATE()
END PROGRAM main
