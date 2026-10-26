!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write and read logical and rank-1 integer and real attributes
!
! Logical attributes are stored as "T" or "F". Rank-1 attributes are
! read into allocatable arrays; HDF5 converts an integer attribute read
! into a real array.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_4.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(HDF5File_) :: obj
  INTEGER(I4B), ALLOCATABLE :: ivec(:)
  REAL(DFP), ALLOCATABLE :: rvec(:)
  LOGICAL(LGT) :: flag

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%Mkdir("/mesh")
  CALL obj%WriteAttribute(obj_name="/mesh", attr_name="isActive", &
                          attr_val=.TRUE.)
  CALL obj%WriteAttribute(obj_name="/mesh", attr_name="isBoundary", &
                          attr_val=.FALSE.)
  CALL obj%WriteAttribute(obj_name="/mesh", attr_name="orders", &
                          attr_val=[1_I4B, 2_I4B, 3_I4B])
  CALL obj%WriteAttribute(obj_name="/mesh", attr_name="boundingBox", &
                          attr_val=[0.0_DFP, 1.0_DFP, 0.0_DFP, 2.0_DFP])
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%ReadAttribute(obj_name="/mesh", attr_name="isActive", &
                         attr_val=flag)
  CALL OK(flag, "logical attribute: true")
  CALL obj%ReadAttribute(obj_name="/mesh", attr_name="isBoundary", &
                         attr_val=flag)
  CALL OK(.NOT. flag, "logical attribute: false")
  CALL obj%ReadAttribute(obj_name="/mesh", attr_name="orders", &
                         attr_val=ivec)
  CALL OK(SIZE(ivec) .EQ. 3, "integer vector: size")
  CALL OK(ALL(ivec .EQ. [1, 2, 3]), "integer vector: values")
  CALL obj%ReadAttribute(obj_name="/mesh", attr_name="boundingBox", &
                         attr_val=rvec)
  CALL OK(SIZE(rvec) .EQ. 4, "real vector: size")
  CALL OK(ALL(ABS(rvec - [0.0_DFP, 1.0_DFP, 0.0_DFP, 2.0_DFP]) .LT. tol), &
          "real vector: values")
  ! an integer vector read as a real vector is converted by HDF5
  CALL obj%ReadAttribute(obj_name="/mesh", attr_name="orders", &
                         attr_val=rvec)
  CALL OK(ALL(ABS(rvec - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
          "integer vector read as real")
  CALL obj%DEALLOCATE()
END PROGRAM main
