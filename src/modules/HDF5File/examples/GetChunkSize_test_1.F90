!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the chunk size of a dataset
!
! Only compressed datasets are chunked. A dataset is compressed when
! the file uses compression and it has more than 1 MB of data along
! a dimension.
! cdims is unallocated for a dataset which is not chunked.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: LGT
  USE HDF5, ONLY: HSIZE_T
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "GetChunkSize_test_1.h5"
  INTEGER, PARAMETER :: n = 200000
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: big(:)
  INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(big(n))
  big = 1.0_DFP

  CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=5)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/small", vals=[1.0_DFP, 2.0_DFP])
  CALL obj%WRITE(dsetname="/big", vals=big)

  CALL obj%GetChunkSize(path="/small", cdims=cdims)
  CALL OK(.NOT. ALLOCATED(cdims), "small dataset is not chunked")

  CALL obj%GetChunkSize(path="/big", cdims=cdims)
  isok = ALLOCATED(cdims)
  IF (isok) isok = (SIZE(cdims) .EQ. 1) .AND. (cdims(1) .LT. n)
  CALL OK(isok, "big dataset is chunked")

  CALL obj%DEALLOCATE()
END PROGRAM main
