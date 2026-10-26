!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Measure the file size and the time of Write and Read
!
! Write and read nfield Real64 fields of n x n values, without and with
! compression (zlibOpt=4), and 2000 scalar datasets in a group of depth 1
! and of depth 5 (cost of the path checks). The file sizes and
! the times are printed as comments ("#"), so the numbers can be compared
! before and after a change of HDF5File_. The checks only test that the
! data is read back unchanged.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: INT64
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  INTEGER(I4B), PARAMETER :: n = 1000, nfield = 4, nsmall = 2000
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: m(:, :), m1(:, :)
  REAL(DFP) :: x
  INTEGER(I4B) :: i, j, k, fsize
  INTEGER(INT64) :: t0, t1, rate
  CHARACTER(16) :: dname
  LOGICAL :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(m(n, n))
  DO j = 1, n
    DO i = 1, n
      m(i, j) = SIN(REAL(i, DFP) / n) * COS(REAL(j, DFP) / n)
    END DO
  END DO

  CALL Fields("Storage_test_1_raw.h5", -1)
  CALL Fields("Storage_test_1_zlib.h5", 4)

  ! many small datasets in a group of depth 1 and of depth 5
  CALL Scalars("Storage_test_1_small.h5", "/small/x")
  CALL Scalars("Storage_test_1_deep.h5", "/a/b/c/d/x")

CONTAINS

  SUBROUTINE Scalars(filename, prefix)
    CHARACTER(*), INTENT(IN) :: filename, prefix
    INTEGER(INT64) :: tw, tr

    CALL SYSTEM_CLOCK(t0, rate)
    CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=-1)
    CALL obj%OPEN()
    DO k = 1, nsmall
      WRITE(dname, '(A,I0)') prefix, k
      CALL obj%WRITE(dsetname=TRIM(dname), vals=REAL(k, DFP))
    END DO
    CALL obj%DEALLOCATE()
    CALL SYSTEM_CLOCK(t1)
    tw = t1 - t0

    CALL SYSTEM_CLOCK(t0)
    CALL obj%Initiate(filename=filename, mode="READ")
    CALL obj%OPEN()
    isok = .TRUE.
    DO k = 1, nsmall
      WRITE(dname, '(A,I0)') prefix, k
      CALL obj%READ(dsetname=TRIM(dname), vals=x)
      isok = isok .AND. (ABS(x - REAL(k, DFP)) .LT. tol)
    END DO
    CALL obj%DEALLOCATE()
    CALL SYSTEM_CLOCK(t1)
    tr = t1 - t0

    INQUIRE(FILE=filename, SIZE=fsize)
    WRITE(*, '(A,I0,A,A,A,I0,A,F8.3,A,F8.3,A)') "# ", nsmall, &
      " scalars ", prefix, "N: ", fsize, " bytes, write ", &
      REAL(tw) / REAL(rate), " s, read ", REAL(tr) / REAL(rate), " s"
    CALL OK(isok, prefix // "N: values")
  END SUBROUTINE Scalars

  SUBROUTINE Fields(filename, zlib)
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER(I4B), INTENT(IN) :: zlib
    INTEGER(INT64) :: tw, tr

    CALL SYSTEM_CLOCK(t0, rate)
    CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=zlib)
    CALL obj%OPEN()
    DO k = 1, nfield
      WRITE(dname, '(A,I0)') "/field/f", k
      CALL obj%WRITE(dsetname=TRIM(dname), vals=m)
    END DO
    CALL obj%DEALLOCATE()
    CALL SYSTEM_CLOCK(t1)
    tw = t1 - t0

    CALL SYSTEM_CLOCK(t0)
    CALL obj%Initiate(filename=filename, mode="READ")
    CALL obj%OPEN()
    isok = .TRUE.
    DO k = 1, nfield
      WRITE(dname, '(A,I0)') "/field/f", k
      CALL obj%READ(dsetname=TRIM(dname), vals=m1)
      isok = isok .AND. ALL(ABS(m1 - m) .LT. tol)
    END DO
    CALL obj%DEALLOCATE()
    CALL SYSTEM_CLOCK(t1)
    tr = t1 - t0

    INQUIRE(FILE=filename, SIZE=fsize)
    WRITE(*, '(A,I0,A,I0,A,F8.3,A,F8.3,A)') "# zlib=", zlib, ": ", &
      fsize, " bytes, write ", REAL(tw) / REAL(rate), " s, read ", &
      REAL(tr) / REAL(rate), " s"
    CALL OK(isok, filename // ": values")
  END SUBROUTINE Fields

END PROGRAM main
