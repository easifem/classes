!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write and read rank-3 String data
!
! A rank-3 String array is written with Write and read back with Read
! (allocates the array) and with Read_ (returns the extent). The strings
! are stored with the length of the longest one; empty strings are
! allowed.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_9.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: t(2, 3, 2), t2(2, 3, 2)
  TYPE(String), ALLOCATABLE :: t1(:, :, :)
  INTEGER(I4B) :: i, j, k, d1, d2, d3
  CHARACTER(16) :: buf
  LOGICAL :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  DO k = 1, 2
    DO j = 1, 3
      DO i = 1, 2
        WRITE(buf, '(A,I0,A,I0,A,I0)') "t", i, "_", j, "_", k
        t(i, j, k) = TRIM(buf)
      END DO
    END DO
  END DO
  t(2, 3, 2) = ""

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/t", vals=t)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/t", vals=t1)
  CALL obj%READ_(dsetname="/t", vals=t2, dim1=d1, dim2=d2, dim3=d3)
  CALL obj%DEALLOCATE()

  CALL OK(ALL(SHAPE(t1) .EQ. [2, 3, 2]), "Read: shape")
  isok = .TRUE.
  DO k = 1, 2
    DO j = 1, 3
      DO i = 1, 2
        isok = isok .AND. &
          (TRIM(t1(i, j, k)%chars()) .EQ. t(i, j, k)%chars())
      END DO
    END DO
  END DO
  CALL OK(isok, "Read: values")
  CALL OK(t1(2, 3, 2)%LEN_TRIM() .EQ. 0, "Read: empty string")

  CALL OK(d1 .EQ. 2 .AND. d2 .EQ. 3 .AND. d3 .EQ. 2, "Read_: extent")
  CALL OK(TRIM(t2(1, 2, 2)%chars()) .EQ. "t1_2_2", "Read_: value")
END PROGRAM main
