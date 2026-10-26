!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! update:
!   - 2026-10-26: rank-1 integer, real and Real32 attributes
! summary: Read attributes of a dataset
!
! A String attribute can be read in a String or in a character
! variable. Rank-1 attributes are read into allocatable arrays, which
! are reallocated to the size of the attribute (also to size 0). HDF5
! converts an integer attribute read into a real array.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: REAL32
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "ReadAttribute_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  REAL(REAL32), PARAMETER :: tol32 = 1.0E-6_REAL32
  TYPE(HDF5File_) :: obj
  TYPE(String) :: strval
  CHARACTER(LEN=5) :: cval
  INTEGER(I4B) :: ival
  REAL(DFP) :: rval
  INTEGER(I4B) :: empty(0)
  INTEGER(I4B), ALLOCATABLE :: ivec(:)
  REAL(DFP), ALLOCATABLE :: rvec(:)
  REAL(REAL32), ALLOCATABLE :: svec(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  strval = "meter"
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/length", vals=2.0_DFP)
  CALL obj%WriteAttribute("/length", "unit", strval)
  CALL obj%WriteAttribute("/length", "count", 3_I4B)
  CALL obj%WriteAttribute("/length", "scale", 0.5_DFP)
  CALL obj%WriteAttribute("/length", "ids", [4_I4B, 5_I4B, 6_I4B])
  CALL obj%WriteAttribute("/length", "noIds", empty)
  CALL obj%WriteAttribute("/length", "limits", [-1.5_DFP, 1.5_DFP])
  CALL obj%WriteAttribute("/length", "weights", &
                          [0.1_REAL32, 0.2_REAL32, 0.3_REAL32, 0.4_REAL32])
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%ReadAttribute(obj_name="/length", attr_name="unit", &
                         attr_val=strval)
  CALL obj%ReadAttribute(obj_name="/length", attr_name="unit", &
                         attr_val=cval)
  CALL obj%ReadAttribute(obj_name="/length", attr_name="count", &
                         attr_val=ival)
  CALL obj%ReadAttribute(obj_name="/length", attr_name="scale", &
                         attr_val=rval)

  CALL OK(strval%chars() .EQ. "meter", "read in String")
  CALL OK(cval .EQ. "meter", "read in character")
  CALL OK(ival .EQ. 3, "integer attribute")
  CALL OK(ABS(rval - 0.5_DFP) .LT. tol, "real attribute")

  ! rank-1 integer (_i1); ivec is allocated with another size first
  ALLOCATE(ivec(10))
  CALL obj%ReadAttribute(obj_name="/length", attr_name="ids", &
                         attr_val=ivec)
  CALL OK(SIZE(ivec) .EQ. 3, "integer vector: size")
  CALL OK(ALL(ivec .EQ. [4, 5, 6]), "integer vector: values")
  CALL obj%ReadAttribute(obj_name="/length", attr_name="noIds", &
                         attr_val=ivec)
  CALL OK(SIZE(ivec) .EQ. 0, "empty integer vector")

  ! rank-1 real (_d1)
  CALL obj%ReadAttribute(obj_name="/length", attr_name="limits", &
                         attr_val=rvec)
  CALL OK(SIZE(rvec) .EQ. 2, "real vector: size")
  CALL OK(ALL(ABS(rvec - [-1.5_DFP, 1.5_DFP]) .LT. tol), &
          "real vector: values")
  ! an integer vector read into a real vector is converted by HDF5
  CALL obj%ReadAttribute(obj_name="/length", attr_name="ids", &
                         attr_val=rvec)
  CALL OK(ALL(ABS(rvec - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
          "integer vector read as real vector")

  ! rank-1 Real32 (_s1)
  CALL obj%ReadAttribute(obj_name="/length", attr_name="weights", &
                         attr_val=svec)
  CALL OK(SIZE(svec) .EQ. 4, "Real32 vector: size")
  CALL OK(ALL(ABS(svec - [0.1_REAL32, 0.2_REAL32, 0.3_REAL32, &
                          0.4_REAL32]) .LT. tol32), "Real32 vector: values")
  CALL obj%DEALLOCATE()
END PROGRAM main
