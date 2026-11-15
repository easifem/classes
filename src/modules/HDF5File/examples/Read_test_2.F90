!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read reallocates String arrays to the dataset shape
!
! vals is reallocated when any of its dimensions differs from the
! dataset.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Read_test_2.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: m(2, 3)
  TYPE(String), ALLOCATABLE :: m1(:, :)
  LOGICAL(LGT) :: isok
  INTEGER :: i, j

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  DO j = 1, 3
    DO i = 1, 2
      m(i, j) = "x"
    END DO
  END DO
  m(2, 3) = "y"

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

! only the second dimension differs from the dataset
  ALLOCATE(m1(2, 5))
  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/m", vals=m1)
  CALL obj%DEALLOCATE()

  CALL OK(ALL(SHAPE(m1) .EQ. [2, 3]), "reallocated")
  isok = (m1(1, 1)%chars() .EQ. "x") .AND. (m1(2, 3)%chars() .EQ. "y")
  CALL OK(isok, "values")
END PROGRAM main
