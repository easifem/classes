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
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE String_Class
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractMaterialModel_Class
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractSolidMechanicsModel_Class"
PUBLIC :: AbstractSolidMechanicsModel_
PUBLIC :: AbstractSolidMechanicsModelDeallocate

!----------------------------------------------------------------------------
!                                              AbstractSolidMechanicsModel_
!----------------------------------------------------------------------------

TYPE, ABSTRACT, EXTENDS(AbstractMaterialModel_) :: &
  & AbstractSolidMechanicsModel_
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
    & lem_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => lem_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => lem_Deallocate

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => lem_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => lem_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => lem_Display
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => lem_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => lem_ImportFromToml2

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticParam => lem_GetElasticParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetC => lem_GetC
  PROCEDURE, PUBLIC, PASS(obj) :: GetInvC => lem_GetInvC
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => &
    & lem_GetElasticityType
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => lem_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: isPlaneStrain => lem_isPlaneStrain
  PROCEDURE, PUBLIC, PASS(obj) :: isPlaneStress => lem_isPlaneStress

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetPlaneStress => lem_SetPlaneStress
  PROCEDURE, PUBLIC, PASS(obj) :: SetPlaneStrain => lem_SetPlaneStrain
END TYPE AbstractSolidMechanicsModel_

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Check the essential parameter

INTERFACE
  MODULE SUBROUTINE lem_CheckEssentialParam(obj, param)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE lem_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the model

INTERFACE
  MODULE SUBROUTINE lem_Initiate(obj, param)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE lem_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Deallocate data

INTERFACE AbstractSolidMechanicsModelDeallocate
  MODULE SUBROUTINE lem_Deallocate(obj)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  END SUBROUTINE lem_Deallocate
END INTERFACE AbstractSolidMechanicsModelDeallocate

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Initiate the model by import

INTERFACE
  MODULE SUBROUTINE lem_Import(obj, hdf5, group)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE lem_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Export the model

INTERFACE
  MODULE SUBROUTINE lem_Export(obj, hdf5, group)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE lem_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Displays the content of model

INTERFACE
  MODULE SUBROUTINE lem_Display(obj, msg, unitNo)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE lem_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE lem_ImportFromToml1(obj, table)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE lem_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE lem_ImportFromToml2(obj, tomlName, afile, filename,  &
    & printToml)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE lem_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElasticParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-30
! summary:  Get elastic parameter

INTERFACE
  MODULE SUBROUTINE lem_GetElasticParam(obj, poissonRatio, &
     & shearModulus, lambda, youngsModulus, stiffnessPower, C, invC)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(:, :)
  END SUBROUTINE lem_GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the elastic tangent matrix

INTERFACE
  MODULE SUBROUTINE lem_GetC(obj, C)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
  END SUBROUTINE lem_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get inverse of elastic tangent matrix

INTERFACE
  MODULE SUBROUTINE lem_GetInvC(obj, InvC)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
  END SUBROUTINE lem_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get elasticity type

INTERFACE
  MODULE FUNCTION lem_GetElasticityType(obj) RESULT(Ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION lem_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION lem_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION lem_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isPlaneStress@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns true if the problem is plane stress

INTERFACE
  MODULE FUNCTION lem_isPlaneStress(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION lem_isPlaneStress
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isPlaneStrain@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns true if the problem is plane strain

INTERFACE
  MODULE FUNCTION lem_isPlaneStrain(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION lem_isPlaneStrain
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetPlaneStress@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_SetPlaneStress(obj, VALUE)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE lem_SetPlaneStress
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetPlaneStrain@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_SetPlaneStrain(obj, VALUE)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE lem_SetPlaneStrain
END INTERFACE

END MODULE AbstractSolidMechanicsModel_Class
