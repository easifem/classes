! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Abstract class for Material behavior

MODULE AbstractSolidMechanicsModel_Class
USE GlobalData, ONLY: I4B, LGT, DFP
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE FPL, ONLY: ParameterList_
USE AbstractMaterialModel_Class, ONLY: AbstractMaterialModel_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE

PRIVATE

PUBLIC :: AbstractSolidMechanicsModel_
PUBLIC :: AbstractSolidMechanicsModelDeallocate

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractSolidMechanicsModel_Class"
#endif

!----------------------------------------------------------------------------
!                                              AbstractSolidMechanicsModel_
!----------------------------------------------------------------------------

TYPE, ABSTRACT, EXTENDS(AbstractMaterialModel_) :: &
  AbstractSolidMechanicsModel_
  PRIVATE
  LOGICAL(LGT) :: isPStress = .FALSE.
  !! PlaneStress
  LOGICAL(LGT) :: isPStrain = .FALSE.
  !! PlaneStrain
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! IO:
  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! IO:
  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticParam => obj_GetElasticParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetC => obj_GetC
  PROCEDURE, PUBLIC, PASS(obj) :: GetInvC => obj_GetInvC
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => &
    obj_GetElasticityType
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: isPlaneStrain => obj_isPlaneStrain
  PROCEDURE, PUBLIC, PASS(obj) :: isPlaneStress => obj_isPlaneStress

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetPlaneStress => obj_SetPlaneStress
  PROCEDURE, PUBLIC, PASS(obj) :: SetPlaneStrain => obj_SetPlaneStrain
END TYPE AbstractSolidMechanicsModel_

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Check the essential parameter

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the model

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Deallocate data

INTERFACE AbstractSolidMechanicsModelDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractSolidMechanicsModelDeallocate

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Initiate the model by import

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Export the model

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Displays the content of model

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElasticParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-30
! summary:  Get elastic parameter

INTERFACE
  MODULE SUBROUTINE obj_GetElasticParam(obj, poissonRatio, &
     & shearModulus, lambda, youngsModulus, stiffnessPower, C, invC)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(:, :)
  END SUBROUTINE obj_GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the elastic tangent matrix

INTERFACE
  MODULE SUBROUTINE obj_GetC(obj, C)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
  END SUBROUTINE obj_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get inverse of elastic tangent matrix

INTERFACE
  MODULE SUBROUTINE obj_GetInvC(obj, InvC)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
  END SUBROUTINE obj_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get elasticity type

INTERFACE
  MODULE FUNCTION obj_GetElasticityType(obj) RESULT(Ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isPlaneStress@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns true if the problem is plane stress

INTERFACE
  MODULE FUNCTION obj_isPlaneStress(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isPlaneStress
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isPlaneStrain@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns true if the problem is plane strain

INTERFACE
  MODULE FUNCTION obj_isPlaneStrain(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isPlaneStrain
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetPlaneStress@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetPlaneStress(obj, VALUE)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetPlaneStress
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetPlaneStrain@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetPlaneStrain(obj, VALUE)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetPlaneStrain
END INTERFACE

END MODULE AbstractSolidMechanicsModel_Class
