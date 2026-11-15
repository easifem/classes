!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the data type of a dataset
!
! GetDataType returns the class and the precision of a dataset: 'R32',
! 'R64' for reals, 'I08', 'I16', 'I32', 'I64' for integers, and 'STR' for
! strings (logical datasets are stored as characters, so also 'STR').
!
! Changes:
!   - 2026-10-26: the code gives the class and the precision

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: REAL32
USE HDF5File_Class, ONLY: HDF5File_
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK

IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "GetDataType_test_1.h5"
TYPE(HDF5File_) :: obj
TYPE(String) :: strval

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

strval = "hello"
CALL obj%Initiate(filename=filename, mode="NEW")
CALL obj%OPEN()
CALL obj%WRITE(dsetname="/real", vals=1.0_DFP)
CALL obj%WRITE(dsetname="/real32", vals=1.0_REAL32)
CALL obj%WRITE(dsetname="/int", vals=1_I4B)
CALL obj%Append("/types", [5_INT8, 9_INT8])
CALL obj%WRITE(dsetname="/str", vals=strval)

CALL OK(obj%GetDataType("/real") .EQ. "R64", "Real64")
CALL OK(obj%GetDataType("/real32") .EQ. "R32", "Real32")
CALL OK(obj%GetDataType("/int") .EQ. "I32", "Int32")
CALL OK(obj%GetDataType("/types") .EQ. "I08", "8-bit integer")
CALL OK(obj%GetDataType("/str") .EQ. "STR", "string")

CALL obj%DEALLOCATE()
END PROGRAM main
