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

MODULE GardnerModel_Class
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP, I4B, LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE AbstractCapillaryModel_Class, ONLY: AbstractCapillaryModel_
IMPLICIT NONE

PRIVATE
PUBLIC :: GardnerModel_
PUBLIC :: GardnerModelPointer_

!----------------------------------------------------------------------------
!                                                            CapillaryModel_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Capillary model
!
!# GardnerModel_
!
! Van Genuchten-Mualem capillary models.
! Saturation is computed using the van Genuchten model and
! Relative permeability is computed using the Mualem model.
!
! The following parameters are defined in the order: pe, pg, smin, smax

TYPE, EXTENDS(AbstractCapillaryModel_) :: GardnerModel_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetValue1 => obj_GetValue1
  !! Get saturation
  PROCEDURE, PUBLIC, PASS(obj) :: GetValue2 => obj_GetValue2
  !! Get saturation
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import abstract capillary model from toml
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import abstract capillary model from toml
END TYPE GardnerModel_

!----------------------------------------------------------------------------
!                                                     CapillaryModelPointer_
!----------------------------------------------------------------------------

TYPE :: GardnerModelPointer_
  CLASS(GardnerModel_), POINTER :: ptr => NULL()
END TYPE GardnerModelPointer_

!----------------------------------------------------------------------------
!                                                            GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get sw and krw from input parameters and suction
!
!# GetValue
!
! Get sw and krw from input parameters and suction.

INTERFACE
  MODULE SUBROUTINE obj_GetValue1(obj, params, suction, isSuction, sw, krw)
    CLASS(GardnerModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    REAL(DFP), INTENT(IN) :: suction
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    REAL(DFP), INTENT(OUT) :: sw
    !! Degree of saturation of water
    REAL(DFP), INTENT(OUT) :: krw
    !! Relative permeability of water
  END SUBROUTINE obj_GetValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get a vector of sw and krw from parameters and suctions
!
!# GetSaturation
!
! Get a vector of sw and krw from parameters and suctions.

INTERFACE
  MODULE SUBROUTINE obj_GetValue2(obj, params, suction, isSuction, sw, &
                                  krw)
    CLASS(GardnerModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    REAL(DFP), INTENT(IN) :: suction(:)
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    REAL(DFP), INTENT(INOUT) :: sw(:)
    !! degree of saturation of water
    REAL(DFP), INTENT(INOUT) :: krw(:)
    !! relative permeability of water
  END SUBROUTINE obj_GetValue2
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
    CLASS(GardnerModel_), INTENT(INOUT) :: obj
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
    CLASS(GardnerModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

END MODULE GardnerModel_Class

