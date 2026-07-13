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

MODULE VanGenuchtenModel_Class
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP, I4B, LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE AbstractCapillaryModel_Class, ONLY: AbstractCapillaryModel_
IMPLICIT NONE

PRIVATE
PUBLIC :: VanGenuchtenModel_
PUBLIC :: VanGenuchtenModelPointer_

!----------------------------------------------------------------------------
!                                                            CapillaryModel_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-09
! summary: Capillary model
!
!# VanGenuchtenModel_
!
! Van Genuchten capillary models. The following parameters are defined
! in that order: ng, mg, pg, smin, smax

TYPE, EXTENDS(AbstractCapillaryModel_) :: VanGenuchtenModel_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetSaturation1 => obj_GetSaturation1
  !! Get saturation
  PROCEDURE, PUBLIC, PASS(obj) :: GetSaturation2 => obj_GetSaturation2
  !! Get saturation
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import abstract capillary model from toml
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import abstract capillary model from toml
END TYPE VanGenuchtenModel_

!----------------------------------------------------------------------------
!                                                     CapillaryModelPointer_
!----------------------------------------------------------------------------

TYPE :: VanGenuchtenModelPointer_
  CLASS(VanGenuchtenModel_), POINTER :: ptr => NULL()
END TYPE VanGenuchtenModelPointer_

!----------------------------------------------------------------------------
!                                                     GetSaturation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get saturation from input parameters and suction
!
!# GetSaturation
!
! Get saturation from input parameters and suction.

INTERFACE
  MODULE SUBROUTINE obj_GetSaturation1(obj, params, suction, isSuction, ans)
    CLASS(VanGenuchtenModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    REAL(DFP), INTENT(IN) :: suction
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE obj_GetSaturation1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetSaturation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-13
! summary: Get a vector of saturations from parameters and saturations
!
!# GetSaturation
!
! Get a vector of saturations from parameters and saturations.

INTERFACE
  MODULE SUBROUTINE obj_GetSaturation2(obj, params, suction, isSuction, ans)
    CLASS(VanGenuchtenModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: params(:)
    REAL(DFP), INTENT(IN) :: suction(:)
    LOGICAL(LGT), INTENT(IN) :: isSuction
    !! if it is true then suction is suction, otherwise suction is
    !! fluid pressure, and suction is given by -suction.
    REAL(DFP), INTENT(INOUT) :: ans(:)
  END SUBROUTINE obj_GetSaturation2
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
    CLASS(VanGenuchtenModel_), INTENT(INOUT) :: obj
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
    CLASS(VanGenuchtenModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

END MODULE VanGenuchtenModel_Class

