!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: The methods of HDF5File_ do not leave open HDF5 objects
!
! After Write, Read, GetChunkSize, IsCompressed, GetDataShape,
! WriteAttribute and ReadAttribute, no dataset, group or attribute of
! the file is open. The open objects are counted with
! H5Fget_obj_count; only the file itself is open.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5, ONLY: HSIZE_T
  USE HDF5, ONLY: SIZE_T
  USE HDF5, ONLY: H5F_OBJ_ALL_F
  USE HDF5, ONLY: H5FGET_OBJ_COUNT_F
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "OpenHandles_test_1.h5"
  INTEGER(I4B), PARAMETER :: n = 200000
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: big(:), v1(:)
  INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
  INTEGER(I4B), ALLOCATABLE :: dshape(:)
  TYPE(String) :: s, s1
  LOGICAL :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(big(n))
  big = 1.0_DFP
  s = "hello"

  CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=4)
  CALL obj%OPEN()

  CALL obj%WRITE(dsetname="/g/big", vals=big)
  CALL obj%WRITE(dsetname="/g/s", vals=s)
  CALL OK(OnlyFileOpen(), "after Write")

  CALL obj%READ(dsetname="/g/big", vals=v1)
  CALL obj%READ(dsetname="/g/s", vals=s1)
  CALL OK(OnlyFileOpen(), "after Read")

  CALL obj%GetChunkSize(path="/g/big", cdims=cdims)
  CALL OK(OnlyFileOpen(), "after GetChunkSize")

  isok = obj%IsCompressed(path="/g/big")
  CALL OK(OnlyFileOpen(), "after IsCompressed")

  dshape = obj%GetDataShape(dsetname="/g/big")
  CALL OK(OnlyFileOpen(), "after GetDataShape")

  CALL obj%WriteAttribute(obj_name="/g/big", attr_name="unit", &
                          attr_val=s)
  CALL obj%ReadAttribute(obj_name="/g/big", attr_name="unit", &
                         attr_val=s1)
  CALL OK(OnlyFileOpen(), "after WriteAttribute and ReadAttribute")

  CALL obj%DEALLOCATE()

CONTAINS

  FUNCTION OnlyFileOpen() RESULT(ans)
    LOGICAL :: ans
    INTEGER(SIZE_T) :: nobj
    INTEGER :: ierr
    CALL H5FGET_OBJ_COUNT_F(obj%file_id, H5F_OBJ_ALL_F, nobj, ierr)
    ans = (ierr .EQ. 0) .AND. (nobj .EQ. 1)
    IF (.NOT. ans) WRITE(*, '(A,I0)') "# open objects: ", nobj
  END FUNCTION OnlyFileOpen

END PROGRAM main
