!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Storage format of logical data
!
! Logical datasets and attributes are stored as the HDF5 enum
! {FALSE=0, TRUE=1} on an 8-bit integer (h5py reads it as bool), and
! GetDataType returns 'BOL'. Read also accepts the characters 'T'/'F' of
! older files: a character dataset and a String attribute are read as
! logical values. Read of rank 3 allocates the array.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: LGT
  USE HDF5, ONLY: HID_T
  USE HDF5, ONLY: H5T_ENUM_F
  USE HDF5, ONLY: H5DCLOSE_F
  USE HDF5, ONLY: H5DGET_TYPE_F
  USE HDF5, ONLY: H5DOPEN_F
  USE HDF5, ONLY: H5TCLOSE_F
  USE HDF5, ONLY: H5TGET_CLASS_F
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "LogicalFormat_test_1.h5"
  TYPE(HDF5File_) :: obj
  LOGICAL(LGT) :: b0, t(2, 2, 2), flag
  LOGICAL(LGT), ALLOCATABLE :: b1(:), t1(:, :, :), old(:), flags(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  t = .FALSE.
  t(1, 2, 1) = .TRUE.
  t(2, 2, 2) = .TRUE.

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/b0", vals=.TRUE.)
  CALL obj%WRITE(dsetname="/b1", vals=[.TRUE., .FALSE., .TRUE.])
  CALL obj%WRITE(dsetname="/t", vals=t)
  ! older files: characters 'T'/'F'
  CALL obj%WRITE(dsetname="/old", vals="TFTT")
  CALL obj%WriteAttribute(obj_name="/b1", attr_name="isValid", &
                          attr_val=.TRUE.)
  CALL obj%WriteAttribute(obj_name="/b1", attr_name="flags", &
                          attr_val=[.FALSE., .TRUE.])
  CALL obj%WriteAttribute(obj_name="/b1", attr_name="oldFlag", &
                          attr_val="T")
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL OK(TypeClass("/b1") .EQ. H5T_ENUM_F, "stored as an HDF5 enum")
  CALL OK(obj%GetDataType("/b1") .EQ. "BOL", "GetDataType: BOL")

  CALL obj%READ(dsetname="/b0", vals=b0)
  CALL obj%READ(dsetname="/b1", vals=b1)
  CALL obj%READ(dsetname="/t", vals=t1)
  CALL OK(b0, "scalar")
  CALL OK(ALL(b1 .EQV. [.TRUE., .FALSE., .TRUE.]), "rank 1")
  CALL OK(ALL(SHAPE(t1) .EQ. [2, 2, 2]), "rank 3: shape")
  CALL OK(ALL(t1 .EQV. t), "rank 3: values")

  CALL obj%READ(dsetname="/old", vals=old)
  CALL OK(ALL(old .EQV. [.TRUE., .FALSE., .TRUE., .TRUE.]), &
          "characters 'T'/'F' of older files")

  CALL obj%ReadAttribute(obj_name="/b1", attr_name="isValid", &
                         attr_val=flag)
  CALL OK(flag, "logical attribute")
  CALL obj%ReadAttribute(obj_name="/b1", attr_name="flags", &
                         attr_val=flags)
  CALL OK(ALL(flags .EQV. [.FALSE., .TRUE.]), "logical vector attribute")
  CALL obj%ReadAttribute(obj_name="/b1", attr_name="oldFlag", &
                         attr_val=flag)
  CALL OK(flag, "String attribute 'T' of older files")
  CALL obj%DEALLOCATE()

CONTAINS

  FUNCTION TypeClass(path) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path
    INTEGER :: ans
    INTEGER(HID_T) :: dset_id, dtype
    INTEGER :: ierr
    CALL H5DOPEN_F(obj%file_id, path, dset_id, ierr)
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL H5TGET_CLASS_F(dtype, ans, ierr)
    CALL H5TCLOSE_F(dtype, ierr)
    CALL H5DCLOSE_F(dset_id, ierr)
  END FUNCTION TypeClass

END PROGRAM main
