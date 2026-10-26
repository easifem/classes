!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! update:
!   - 2026-10-26: overwrite writes in place, the shape must not change
! summary: Overwrite a dataset in place
!
! In mode OVERWRITE a dataset can be written again with data of the same
! shape; the values are replaced in place. This holds for a small dataset
! and for a large chunked and compressed one. HDF5 converts the type, for
! example integers written into a real dataset. The attributes of the
! dataset are kept. Data of another shape is not written (DEBUG mode
! raises an error); use Append for data that grows.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Overwrite_test_1.h5"
  INTEGER(I4B), PARAMETER :: n = 400
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: v1(:), big(:, :), big1(:, :)
  REAL(DFP) :: scale

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(big(n, n))
  big = 1.0_DFP

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=[1.0_DFP, 2.0_DFP, 3.0_DFP])
  CALL obj%WriteAttribute(obj_name="/v", attr_name="scale", &
                          attr_val=0.5_DFP)
  CALL obj%WRITE(dsetname="/big", vals=big)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="OVERWRITE")
  CALL obj%OPEN()

  ! small dataset, same shape
  CALL obj%WRITE(dsetname="/v", vals=[4.0_DFP, 5.0_DFP, 6.0_DFP])
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL OK(SIZE(v1) .EQ. 3, "same shape: size")
  CALL OK(ALL(ABS(v1 - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
          "same shape: values")

  ! the attributes of the dataset are kept
  CALL OK(obj%AttributeExists("/v", "scale"), "attribute is kept")
  CALL obj%ReadAttribute(obj_name="/v", attr_name="scale", &
                         attr_val=scale)
  CALL OK(ABS(scale - 0.5_DFP) .LT. tol, "attribute value is kept")

  ! integers written into a real dataset are converted
  CALL obj%WRITE(dsetname="/v", vals=[7_I4B, 8_I4B, 9_I4B])
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL OK(ALL(ABS(v1 - [7.0_DFP, 8.0_DFP, 9.0_DFP]) .LT. tol), &
          "integers into a real dataset")

  ! large chunked and compressed dataset, same shape
  CALL OK(obj%IsCompressed(path="/big"), "large dataset is compressed")
  big = 2.0_DFP
  CALL obj%WRITE(dsetname="/big", vals=big)
  CALL obj%READ(dsetname="/big", vals=big1)
  CALL OK(ALL(SHAPE(big1) .EQ. [n, n]), "compressed: shape")
  CALL OK(ALL(ABS(big1 - 2.0_DFP) .LT. tol), "compressed: values")

  CALL obj%DEALLOCATE()
END PROGRAM main
