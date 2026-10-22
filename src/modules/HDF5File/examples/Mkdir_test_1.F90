!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Create groups in a HDF5 file
!
! Mkdir creates a group. Missing parent groups are created too.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Mkdir_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL obj%Mkdir("/grp1")
  CALL OK(obj%IsGroup("/grp1"), "Mkdir /grp1")

  CALL obj%Mkdir("/grp2/grp2a")
  CALL OK(obj%IsGroup("/grp2"), "parent group /grp2")
  CALL OK(obj%IsGroup("/grp2/grp2a"), "Mkdir /grp2/grp2a")
  CALL OK(obj%Ngrp("/") .EQ. 2, "Ngrp('/')")
  CALL OK(obj%Ngrp("/grp2") .EQ. 1, "Ngrp('/grp2')")

  CALL obj%DEALLOCATE()
END PROGRAM main
