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
!

MODULE AbstractCapillaryModel_Class
USE BaseType, ONLY: math => TypeMathOpt
USE GlobalData, ONLY: DFP, I4B, LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractCapillaryModel_
PUBLIC :: AbstractCapillaryModelPointer_

INTEGER(I4B), PARAMETER :: MAX_CAPILLARY_PARAMETERS = 20_I4B
INTEGER(I4B), PARAMETER :: MAX_STRING_LEN = 256_I4B

!----------------------------------------------------------------------------
!                                                            CapillaryModel_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Capillary model
!
!# AbstractCapillaryModel_
!
! Abstract capillary models.

TYPE, ABSTRACT :: AbstractCapillaryModel_
  INTEGER(I4B) :: totalParameters = math%zero_i
  !! total number of parameters
  REAL(DFP) :: params(MAX_CAPILLARY_PARAMETERS)
  !! model parameters
  CHARACTER(MAX_STRING_LEN) :: name = ""
  !! name of capillary model
  CHARACTER(MAX_STRING_LEN) :: paramNames(MAX_CAPILLARY_PARAMETERS)
  !! name of parameters
CONTAINS

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate capillary model
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    DEALLOCATE => obj_Deallocate
  !! Deallocate the capillary model
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of capillary model
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalParameters => obj_GetTotalParameters
  !! Get totalParameters from abstract capaillary model
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetParameters => obj_GetParameters
  !! Get parameters of abstract capillary model
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import abstract capillary model from toml
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import abstract capillary model from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE AbstractCapillaryModel_

!----------------------------------------------------------------------------
!                                                     CapillaryModelPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractCapillaryModelPointer_
  CLASS(AbstractCapillaryModel_), POINTER :: ptr => NULL()
END TYPE AbstractCapillaryModelPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Initiate capillary model
!
!# Initiate
!
! Initiate capillary model.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, modelName, params, totalParameters)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: modelName
    !! It is the name of the material
    REAL(DFP), INTENT(IN) :: params(:)
    !! model parameters
    INTEGER(I4B), INTENT(IN) :: totalParameters
    !! total number of parameters
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Deallocate capillary model
!
!# Deallocate
!
! Deallocate capillary model.

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Display capillary model
!
!# Display
!
! Display capillary model.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalParameters@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-10
! summary: Get total parameters
!
!# GetTotalParameters
!
! Get totalParameters of Abstract capillary model.

INTERFACE
  MODULE FUNCTION obj_GetTotalParameters(obj) RESULT(ans)
    CLASS(AbstractCapillaryModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalParameters
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetParameters@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-10
! summary: Get parameters from AbstractCapillaryModel
!
!# GetParameters
!
! Get parameters of abstract capillary model.

INTERFACE
  MODULE SUBROUTINE obj_GetParameters(obj, ans, tsize)
    CLASS(AbstractCapillaryModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetParameters
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ImportFromToml
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
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
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
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

END MODULE AbstractCapillaryModel_Class
