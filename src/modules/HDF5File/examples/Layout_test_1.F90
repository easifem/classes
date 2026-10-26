!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Layout of datasets and format version of a new file
!
! With the default settings a new file uses the format version bounds
! (HDF5 1.10, latest). A dataset with less than compactBytes (4 KiB) of
! data, a scalar for example, uses the compact layout. A dataset between
! compactBytes and chunkPolicy%minBytes (64 KiB) is contiguous, and a
! larger one is chunked. libverLow=0 keeps the default bounds of the HDF5
! library (1.8 format in HDF5 2.x, so older than 1.10), and
! compactBytes=0 turns the compact layout off.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: INT64
  USE HDF5, ONLY: HID_T
  USE HDF5, ONLY: H5D_CHUNKED_F
  USE HDF5, ONLY: H5D_COMPACT_F
  USE HDF5, ONLY: H5D_CONTIGUOUS_F
  USE HDF5, ONLY: H5F_LIBVER_V110_F
  USE HDF5, ONLY: H5DCLOSE_F
  USE HDF5, ONLY: H5DGET_CREATE_PLIST_F
  USE HDF5, ONLY: H5DOPEN_F
  USE HDF5, ONLY: H5FGET_ACCESS_PLIST_F
  USE HDF5, ONLY: H5PCLOSE_F
  USE HDF5, ONLY: H5PGET_LAYOUT_F
  USE HDF5, ONLY: H5PGET_LIBVER_BOUNDS_F
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Layout_test_1.h5"
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: v1(100), v2(2000), v3(20000)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  v1 = 1.0_DFP
  v2 = 2.0_DFP
  v3 = 3.0_DFP

  ! default settings
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/s", vals=1.0_DFP)
  CALL obj%WRITE(dsetname="/v1", vals=v1)
  CALL obj%WRITE(dsetname="/v2", vals=v2)
  CALL obj%WRITE(dsetname="/v3", vals=v3)
  CALL OK(LibverLow() .EQ. H5F_LIBVER_V110_F, "default: format 1.10")
  CALL OK(Layout("/s") .EQ. H5D_COMPACT_F, "scalar is compact")
  CALL OK(Layout("/v1") .EQ. H5D_COMPACT_F, "800 B is compact")
  CALL OK(Layout("/v2") .EQ. H5D_CONTIGUOUS_F, "16 KB is contiguous")
  CALL OK(Layout("/v3") .EQ. H5D_CHUNKED_F, "160 KB is chunked")
  CALL obj%DEALLOCATE()

  ! data is read back from the new format
  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL OK(ReadBack(), "values are read back")
  CALL obj%DEALLOCATE()

  ! default format of the HDF5 library, no compact layout
  CALL obj%Initiate(filename=filename, mode="NEW", LibverLow=0, &
                    compactBytes=0_INT64)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/s", vals=1.0_DFP)
  CALL OK(LibverLow() .LT. H5F_LIBVER_V110_F, &
          "libverLow=0: library default, older than 1.10")
  CALL OK(Layout("/s") .EQ. H5D_CONTIGUOUS_F, &
          "compactBytes=0: scalar is contiguous")
  CALL obj%DEALLOCATE()

CONTAINS

  FUNCTION Layout(path) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path
    INTEGER :: ans
    INTEGER(HID_T) :: dset_id, dcpl
    INTEGER :: ierr
    CALL H5DOPEN_F(obj%file_id, path, dset_id, ierr)
    CALL H5DGET_CREATE_PLIST_F(dset_id, dcpl, ierr)
    CALL H5PGET_LAYOUT_F(dcpl, ans, ierr)
    CALL H5PCLOSE_F(dcpl, ierr)
    CALL H5DCLOSE_F(dset_id, ierr)
  END FUNCTION Layout

  FUNCTION LibverLow() RESULT(ans)
    INTEGER :: ans
    INTEGER(HID_T) :: fapl
    INTEGER :: high, ierr
    CALL H5FGET_ACCESS_PLIST_F(obj%file_id, fapl, ierr)
    CALL H5PGET_LIBVER_BOUNDS_F(fapl, ans, high, ierr)
    CALL H5PCLOSE_F(fapl, ierr)
  END FUNCTION LibverLow

  FUNCTION ReadBack() RESULT(ans)
    LOGICAL :: ans
    REAL(DFP) :: s
    REAL(DFP), ALLOCATABLE :: w(:)
    CALL obj%READ(dsetname="/s", vals=s)
    ans = ABS(s - 1.0_DFP) .LT. 1.0E-12_DFP
    CALL obj%READ(dsetname="/v1", vals=w)
    ans = ans .AND. ALL(ABS(w - v1) .LT. 1.0E-12_DFP)
    CALL obj%READ(dsetname="/v2", vals=w)
    ans = ans .AND. ALL(ABS(w - v2) .LT. 1.0E-12_DFP)
    CALL obj%READ(dsetname="/v3", vals=w)
    ans = ans .AND. ALL(ABS(w - v3) .LT. 1.0E-12_DFP)
  END FUNCTION ReadBack

END PROGRAM main
