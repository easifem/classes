!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write and read logical data
!
! Logical values are stored as the characters 'T' and 'F'.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_4.h5"
  TYPE(HDF5File_) :: obj
  LOGICAL(LGT) :: s, v(3), m(2, 2)
  LOGICAL(LGT) :: s1
  LOGICAL(LGT), ALLOCATABLE :: v1(:), m1(:, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  s = .TRUE.
  v = [.TRUE., .FALSE., .TRUE.]
  m = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/s", vals=s)
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/s", vals=s1)
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL obj%READ(dsetname="/m", vals=m1)
  CALL obj%DEALLOCATE()

  CALL OK(s1 .EQV. s, "rank 0")
  CALL OK(ALL(v1 .EQV. v), "rank 1")
  CALL OK(ALL(m1 .EQV. m), "rank 2")
END PROGRAM main
