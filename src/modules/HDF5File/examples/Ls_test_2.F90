!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: List a group with many members
!
! Ls returns the names in increasing (byte) order, whatever the order of
! creation. Here 300 datasets are created in decreasing order of their
! names, and the list must be in increasing order.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Ls_test_2.h5"
  INTEGER(I4B), PARAMETER :: n = 300
  TYPE(HDF5File_) :: obj
  TYPE(String), ALLOCATABLE :: objs(:)
  CHARACTER(16) :: dname
  INTEGER(I4B) :: k
  LOGICAL :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  DO k = n, 1, -1
    WRITE(dname, '(A,I4.4)') "/g/x", k
    CALL obj%WRITE(dsetname=TRIM(dname), vals=REAL(k, DFP))
  END DO

  CALL obj%Ls(path="/g", objs=objs)
  CALL OK(SIZE(objs) .EQ. n, "Ls: number of names")

  isok = .TRUE.
  DO k = 1, n
    WRITE(dname, '(A,I4.4)') "x", k
    isok = isok .AND. (objs(k)%chars() .EQ. TRIM(dname))
  END DO
  CALL OK(isok, "Ls: names in increasing order")

  CALL obj%DEALLOCATE()
END PROGRAM main
