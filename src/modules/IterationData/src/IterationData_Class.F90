! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

MODULE IterationData_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE GlobalData, ONLY: NormL2
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeConvergenceOpt
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE
PRIVATE

PUBLIC :: IterationData_
PUBLIC :: IterationDataPointer_
PUBLIC :: TypeIterationData

!----------------------------------------------------------------------------
!                                                             IterationData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Iteration data

TYPE :: IterationData_
  LOGICAL(LGT) :: isInit = math%no
  !! Status if iteration data is initiated or not
  LOGICAL(LGT) :: converged = math%no
  !! Status of convergence
  LOGICAL(LGT) :: storeHistory = math%no
  !! store residual and solution error history
  INTEGER(I4B) :: maxIter = 100_DFP
  !! Maximum number of iterations allowed
  INTEGER(I4B) :: iterationNumber = math%zero_i
  !! Iteration number
  INTEGER(I4B) :: convergenceType = TypeConvergenceOpt%relative
  !! Type of convergence
  INTEGER(I4B) :: convergenceIn = TypeConvergenceOpt%res
   !! Check Convergence in solution and/or residual
  INTEGER(I4B) :: normType = NormL2
  !! Error norm type
  REAL(DFP) :: residualError0 = math%zero
  !! Initial Residual error
  REAL(DFP) :: residualError = math%zero
  !! Current residual error
  REAL(DFP) :: residualRelTolerance = 1.0E-5_DFP
   !! Tolerance for checking relative convergence in residual
  REAL(DFP) :: residualAbsTolerance = 1.0E-5_DFP
   !! Tolerance for checking absolute convergence in residual
  REAL(DFP) :: solutionError0 = math%zero
  !! Initial solution error
  REAL(DFP) :: solutionError = math%zero
  !! Current solution error
  REAL(DFP) :: solutionRelTolerance = 1.0E-5_DFP
  !! Tolerance for checking relative convergence in solution
  REAL(DFP) :: solutionAbsTolerance = 1.0E-5_DFP
  !! Tolerance for checking abolute convergence in solution
  REAL(DFP) :: timeAtStart = math%zero
  !! Starting time
  REAL(DFP) :: timeAtEnd = math%zero
  !! Present time
  TYPE(String) :: name
  !! name of the iteration scheme
  REAL(DFP), ALLOCATABLE :: residualHistory(:)
  !! History of residual data
  REAL(DFP), ALLOCATABLE :: solutionHistory(:)
  !! History of solution data

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate iteration data
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Initiate iteration data
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Initiate iteration data
  PROCEDURE, PUBLIC, PASS(obj) :: IsConverged => obj_IsConverged
  !! Initiate iteration data
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxIter => obj_GetMaxIter
  !! Get maximum number of iteration.
  PROCEDURE, PUBLIC, PASS(obj) :: GetNormType => obj_GetNormType
  !! Get normType
  PROCEDURE, PUBLIC, PASS(obj) :: GetIterationNumber => &
    obj_GetIterationNumber
  !! Get iteration number
  PROCEDURE, PUBLIC, PASS(obj) :: SetResidualError0 => obj_SetResidualError0
  !! Set residualError0
  PROCEDURE, PUBLIC, PASS(obj) :: SetResidualError => obj_SetResidualError
  !! Set residualError
  PROCEDURE, PUBLIC, PASS(obj) :: SetSolutionError0 => obj_SetSolutionError0
  !! Set SolutionError0
  PROCEDURE, PUBLIC, PASS(obj) :: SetSolutionError => obj_SetSolutionError
  !! Set SolutionError
  PROCEDURE, PUBLIC, PASS(obj) :: SetIterationNumber => &
    obj_SetIterationNumber
  !! Set iteration number
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
END TYPE IterationData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(IterationData_), PARAMETER :: TypeIterationData = IterationData_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: IterationDataPointer_
  CLASS(IterationData_), POINTER :: ptr => NULL()
END TYPE IterationDataPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Initiate iteration data
!
!# Initiate
!
! Initiate iteration data.

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, maxIter, iterationNumber, residualError0, residualError, &
    residualRelTolerance, residualAbsTolerance, solutionError0, &
    solutionError, solutionRelTolerance, solutionAbsTolerance, &
    convergenceType, convergenceIn, normType, converged, &
    timeAtStart, timeAtEnd)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iterationNumber
    REAL(DFP), OPTIONAL, INTENT(IN) :: residualError0
    REAL(DFP), OPTIONAL, INTENT(IN) :: residualError
    REAL(DFP), OPTIONAL, INTENT(IN) :: residualRelTolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: residualAbsTolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: solutionError0
    REAL(DFP), OPTIONAL, INTENT(IN) :: solutionError
    REAL(DFP), OPTIONAL, INTENT(IN) :: solutionRelTolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: solutionAbsTolerance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: normType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: converged
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeAtStart
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeAtEnd
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-07-13
! summary: Deallocate the IterationData_
!
!# Deallocate
!
! Deallocate iteration data.

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(IterationData_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-07-13
! summary: Display the IterationData_
!
!# Display
!
! Display the iteration data.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                        IsConverged@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Returns true if the convergence critria is satisfied
!
!# IsConverged
!
!  Returns true if the convergence criteria is satisfied.

INTERFACE
  MODULE FUNCTION obj_IsConverged(obj) RESULT(ans)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsConverged
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetMaxIter@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Returns maxIter, maximum number of iteration
!
!# GetMaxIter
!
!  Returns maxIter, maximum number of iteration.

INTERFACE
  MODULE FUNCTION obj_GetMaxIter(obj) RESULT(ans)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxIter
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNormType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Returns normType stored in obj
!
!# GetNormType
!
!  Returns NormType stored in obj

INTERFACE
  MODULE FUNCTION obj_GetNormType(obj) RESULT(ans)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNormType
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetIterationNumber@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Returns iteration number stored in obj
!
!# GetIterationNumber
!
!  Returns iteration number stored in obj.

INTERFACE
  MODULE FUNCTION obj_GetIterationNumber(obj) RESULT(ans)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIterationNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetResidualError0@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Set residualError0
!
!# SetResidualError0
!
! Set residualError0

INTERFACE
  MODULE SUBROUTINE obj_SetResidualError0(obj, VALUE)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetResidualError0
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetResidualError@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Set residualError
!
!# SetResidualError
!
! Set residualError

INTERFACE
  MODULE SUBROUTINE obj_SetResidualError(obj, VALUE)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetResidualError
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetSolutionError0@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Set SolutionError0
!
!# SetSolutionError0
!
! Set SolutionError0

INTERFACE
  MODULE SUBROUTINE obj_SetSolutionError0(obj, VALUE)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetSolutionError0
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetSolutionError@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Set SolutionError
!
!# SetSolutionError
!
! Set SolutionError

INTERFACE
  MODULE SUBROUTINE obj_SetSolutionError(obj, VALUE)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetSolutionError
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetIterationNumber@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Set iteration number
!
!# SetIterationNumber
!
! Set iterationNumber.

INTERFACE
  MODULE SUBROUTINE obj_SetIterationNumber(obj, VALUE)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetIterationNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Import from toml
!
!# ImportFromToml
!
! Import capillary model from toml.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate capillary model from the toml file
!
!# ImportFromToml
!
! Import capillary model from toml file.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(IterationData_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IterationData_Class
