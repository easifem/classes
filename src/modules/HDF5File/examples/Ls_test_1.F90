!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: List the contents of a group
!
! The names are returned in increasing alphabetical order. If the
! path is not a group, objs is returned unallocated.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Ls_test_1.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String), ALLOCATABLE :: objs(:)
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL obj%Mkdir("/b")
  CALL obj%Mkdir("/a")
  CALL obj%WRITE(dsetname="/c", vals=1.0_DFP)

  CALL obj%Ls(path="/", objs=objs)
  isok = SIZE(objs) .EQ. 3
  IF (isok) THEN
    isok = (objs(1)%chars() .EQ. "a") .AND. &
      (objs(2)%chars() .EQ. "b") .AND. &
      (objs(3)%chars() .EQ. "c")
  END IF
  CALL OK(isok, "Ls('/')")

  CALL obj%Ls(path="/c", objs=objs)
  CALL OK(.NOT. ALLOCATED(objs), "Ls of a dataset")

  CALL obj%DEALLOCATE()
END PROGRAM main
