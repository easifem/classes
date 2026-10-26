!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write and read vector attributes
!
! Rank-1 String and logical attributes, and Real32 and Int64
! attributes. A String vector is stored with fixed
! length strings; the padding is removed when it is read.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: INT64
  USE GlobalData, ONLY: LGT
  USE GlobalData, ONLY: REAL32
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_5.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: names(3)
  TYPE(String), ALLOCATABLE :: names1(:)
  LOGICAL(LGT), ALLOCATABLE :: flags(:)
  REAL(REAL32) :: r32
  REAL(REAL32), ALLOCATABLE :: r32v(:)
  INTEGER(INT64), ALLOCATABLE :: i64v(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  names(1) = "steel"
  names(2) = "concrete"
  names(3) = ""

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%Mkdir("/model")
  CALL obj%WriteAttribute(obj_name="/model", attr_name="materials", &
                          attr_val=names)
  CALL obj%WriteAttribute(obj_name="/model", attr_name="isFixed", &
                          attr_val=[.TRUE., .FALSE., .TRUE.])
  CALL obj%WriteAttribute(obj_name="/model", attr_name="tol", &
                          attr_val=1.0E-3_REAL32)
  CALL obj%WriteAttribute(obj_name="/model", attr_name="weights", &
                          attr_val=[0.25_REAL32, 0.75_REAL32])
  CALL obj%WriteAttribute(obj_name="/model", attr_name="bigIds", &
                          attr_val=[3000000000_INT64, 4000000000_INT64])
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%ReadAttribute(obj_name="/model", attr_name="materials", &
                         attr_val=names1)
  CALL OK(SIZE(names1) .EQ. 3, "String vector: size")
  CALL OK(names1(1)%chars() .EQ. "steel" .AND. &
          names1(2)%chars() .EQ. "concrete" .AND. &
          names1(3)%LEN() .EQ. 0, "String vector: values")
  CALL obj%ReadAttribute(obj_name="/model", attr_name="isFixed", &
                         attr_val=flags)
  CALL OK(SIZE(flags) .EQ. 3, "logical vector: size")
  CALL OK(flags(1) .AND. (.NOT. flags(2)) .AND. flags(3), &
          "logical vector: values")
  CALL obj%ReadAttribute(obj_name="/model", attr_name="tol", attr_val=r32)
  CALL OK(ABS(r32 - 1.0E-3_REAL32) .LT. 1.0E-9_REAL32, "Real32 scalar")
  CALL obj%ReadAttribute(obj_name="/model", attr_name="weights", &
                         attr_val=r32v)
  CALL OK(ALL(ABS(r32v - [0.25_REAL32, 0.75_REAL32]) .LT. 1.0E-6_REAL32), &
          "Real32 vector")
  CALL obj%ReadAttribute(obj_name="/model", attr_name="bigIds", &
                         attr_val=i64v)
  CALL OK(ALL(i64v .EQ. [3000000000_INT64, 4000000000_INT64]), &
          "Int64 vector")
  CALL obj%DEALLOCATE()
END PROGRAM main
