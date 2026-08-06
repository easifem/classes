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
USE BaseType, ONLY: FEVariable_
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
  PROCEDURE, PUBLIC, PASS(obj) :: GetValue1 => obj_GetValue1
  !! Get saturation
  PROCEDURE, PUBLIC, PASS(obj) :: GetValue2 => obj_GetValue2
  !! Get saturation
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetValue3 => &
    obj_GetValue3
  !! Get saturation
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetValue4 => &
    obj_GetValue4
  !! Get saturation
  GENERIC, PUBLIC :: GetValue => GetValue1, GetValue2, &
    GetValue3, GetValue4
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
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get Sw and Krw from input parameters and suction
!
!# GetValue
!
! Get Sw and Krw from input parameters and suction.

INTERFACE
  MODULE SUBROUTINE obj_GetValue1(obj, params, suction, isSuction, sw, &
                                  krw)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    REAL(DFP), INTENT(IN) :: suction
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    REAL(DFP), INTENT(OUT) :: sw
    !! Saturation
    REAL(DFP), INTENT(OUT) :: krw
    !! Relative permeability of water
  END SUBROUTINE obj_GetValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get a vector of Sw and Krw from parameters and suctions
!
!# GetValue
!
! Get a vector of Sw and Krw from parameters and suctions.

INTERFACE
  MODULE SUBROUTINE obj_GetValue2(obj, params, suction, isSuction, &
                                  sw, krw)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    REAL(DFP), INTENT(IN) :: suction(:)
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    REAL(DFP), INTENT(INOUT) :: sw(:)
    !! degree of saturation
    REAL(DFP), INTENT(INOUT) :: krw(:)
    !! relative permeability
  END SUBROUTINE obj_GetValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get FEVariable of Sw and Krw from parameters and suctions
!
!# GetValue
!
! Get FEVariable of Sw and Krw from parameters and suctions.
! Sw and Krw is also returned as FEVariable.

INTERFACE
  MODULE SUBROUTINE obj_GetValue3(obj, params, suction, isSuction, sw, krw)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    TYPE(FEVariable_), INTENT(IN) :: suction
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    TYPE(FEVariable_), INTENT(INOUT) :: sw
    !! degree of saturation
    TYPE(FEVariable_), INTENT(INOUT) :: krw
    !! relative permeability of water
  END SUBROUTINE obj_GetValue3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get FEVariable of Sw and Krw from parameters and saturations
!
!# GetValue
!
! Get FEVariable of Sw and Krw from parameters and saturations.

INTERFACE
  MODULE SUBROUTINE obj_GetValue4(obj, params, suction, isSuction, sw, krw)
    CLASS(AbstractCapillaryModel_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: params
    TYPE(FEVariable_), INTENT(IN) :: suction
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    TYPE(FEVariable_), INTENT(INOUT) :: sw
    !! Degree of saturation of water
    TYPE(FEVariable_), INTENT(INOUT) :: krw
    !! Relative permeability of water
  END SUBROUTINE obj_GetValue4
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
