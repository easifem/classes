!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Storage format of String arrays
!
! String arrays (datasets and attributes) are stored as fixed-length
! strings of the longest length, padded with NUL characters
! (H5T_STR_NULLPAD). Read removes the padding, so the values come back
! with their own length; h5py shows them without padding too.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5, ONLY: HID_T
  USE HDF5, ONLY: H5T_STR_NULLPAD_F
  USE HDF5, ONLY: H5DCLOSE_F
  USE HDF5, ONLY: H5DGET_TYPE_F
  USE HDF5, ONLY: H5DOPEN_F
  USE HDF5, ONLY: H5TCLOSE_F
  USE HDF5, ONLY: H5TGET_STRPAD_F
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "StringPadding_test_1.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: v(3), m(2, 2)
  TYPE(String), ALLOCATABLE :: v1(:), m1(:, :), a1(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  v(1) = "ab"
  v(2) = "abcd"
  v(3) = ""
  m(1, 1) = "x"
  m(2, 1) = "xyz"
  m(1, 2) = "xy"
  m(2, 2) = "xyzw"

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%WriteAttribute(obj_name="/v", attr_name="names", attr_val=v)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL OK(StrPad("/v") .EQ. H5T_STR_NULLPAD_F, "padding is NULLPAD")

  CALL obj%READ(dsetname="/v", vals=v1)
  CALL OK(v1(1)%LEN() .EQ. 2 .AND. v1(1)%chars() .EQ. "ab", &
          "rank 1: no padding in a short value")
  CALL OK(v1(2)%chars() .EQ. "abcd", "rank 1: longest value")
  CALL OK(v1(3)%LEN() .EQ. 0, "rank 1: empty value")

  CALL obj%READ(dsetname="/m", vals=m1)
  CALL OK(m1(1, 1)%LEN() .EQ. 1 .AND. m1(1, 2)%LEN() .EQ. 2, &
          "rank 2: no padding")

  CALL obj%ReadAttribute(obj_name="/v", attr_name="names", attr_val=a1)
  CALL OK(a1(1)%LEN() .EQ. 2 .AND. a1(3)%LEN() .EQ. 0, &
          "attribute: no padding")
  CALL obj%DEALLOCATE()

CONTAINS

  FUNCTION StrPad(path) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path
    INTEGER :: ans
    INTEGER(HID_T) :: dset_id, dtype
    INTEGER :: ierr
    CALL H5DOPEN_F(obj%file_id, path, dset_id, ierr)
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL H5TGET_STRPAD_F(dtype, ans, ierr)
    CALL H5TCLOSE_F(dtype, ierr)
    CALL H5DCLOSE_F(dset_id, ierr)
  END FUNCTION StrPad

END PROGRAM main
