!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate OneDimFEDOF object by import from toml

PROGRAM main
USE OneDimFEDOF_Class, ONLY: OneDimFEDOF_
USE OneDimDomain_Class, ONLY: OneDimDomain_
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: I4B, LGT, DFP
USE Test_Method, ONLY: OK, IS
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE

TYPE(OneDimFEDOF_) :: obj
TYPE(OneDimDomain_) :: dom
CHARACTER(*), PARAMETER :: filename = "./toml/ImportFromToml_test_1.toml"
CHARACTER(*), PARAMETER :: tomlNameForDom = "domain"
CHARACTER(*), PARAMETER :: tomlNameForFedof = "fedof"

CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL dom%ImportFromToml(tomlName=tomlNameForDom, filename=filename)
CALL obj%ImportFromToml(tomlName=tomlNameForFedof, filename=filename, &
                        mesh=dom)
! CALL dom%Display("dom: ")
CALL obj%Display("obj: ")
END PROGRAM main
