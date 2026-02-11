! This example shows the usage of `Initiate` methods.

PROGRAM main
USE FortranModuleFile_Class, ONLY: FortranModuleFile_
USE String_Class, ONLY: String
USE GlobalData, ONLY: DFP, LGT, I4B
USE Display_Method, ONLY: Display
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType, ONLY: math => TypeMathOpt

TYPE(FortranModuleFile_) :: obj
! TYPE(String) :: aline
CHARACTER(LEN=*), PARAMETER :: filename = "./fortranFiles/module2.F90"

! set verbosity
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! Initiate an instance of obj and then
CALL obj%Initiate( &
  filename=filename, status='OLD', action='READ', comment='!')

! open the fortran module file
CALL obj%OPEN()

CALL obj%ReadModuleFile()

! deallocate the fortran module files
CALL obj%DEALLOCATE()
END PROGRAM main
