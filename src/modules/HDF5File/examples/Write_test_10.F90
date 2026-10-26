!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write and read large logical and String arrays
!
! The character copy of a logical or String array is allocatable, so
! arrays whose copy is larger than the stack (8 MB on macOS) can be
! written. Here 16 million logical values (16 MB copy) and one million
! strings of 12 characters (12 MB copy) are written and read back.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_10.h5"
  INTEGER(I4B), PARAMETER :: nb = 16000000, ns = 1000000
  TYPE(HDF5File_) :: obj
  LOGICAL(LGT), ALLOCATABLE :: b(:), b1(:)
  TYPE(String), ALLOCATABLE :: s(:), s1(:)
  INTEGER(I4B) :: i
  CHARACTER(12) :: buf

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(b(nb), s(ns))
  DO i = 1, nb
    b(i) = MOD(i, 3) .EQ. 0
  END DO
  DO i = 1, ns
    WRITE(buf, '(A,I0)') "name", i
    s(i) = TRIM(buf)
  END DO

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/b", vals=b)
  CALL obj%WRITE(dsetname="/s", vals=s)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/b", vals=b1)
  CALL obj%READ(dsetname="/s", vals=s1)
  CALL obj%DEALLOCATE()

  CALL OK(SIZE(b1) .EQ. nb, "logical: size")
  CALL OK(ALL(b1 .EQV. b), "logical: values")
  CALL OK(SIZE(s1) .EQ. ns, "String: size")
  CALL OK(TRIM(s1(ns)%chars()) .EQ. s(ns)%chars(), "String: last value")
END PROGRAM main
