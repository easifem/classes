!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the number of objects in a group
!
! Ngrp counts all objects in the group: groups and datasets.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Ngrp_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL OK(obj%Ngrp("/") .EQ. 0, "Ngrp of an empty file")
  CALL obj%Mkdir("/g1")
  CALL obj%Mkdir("/g2")
  CALL obj%WRITE(dsetname="/d", vals=1.0_DFP)
  CALL obj%WRITE(dsetname="/g1/d", vals=1.0_DFP)
  CALL OK(obj%Ngrp("/") .EQ. 3, "Ngrp('/')")
  CALL OK(obj%Ngrp("/g1") .EQ. 1, "Ngrp('/g1')")
  CALL OK(obj%Ngrp("/g2") .EQ. 0, "Ngrp('/g2')")

  CALL obj%DEALLOCATE()
END PROGRAM main
