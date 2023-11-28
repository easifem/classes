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
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractSolidMechanicsModel_Class"
PUBLIC :: AbstractSolidMechanicsModel_
PUBLIC :: AbstractSolidMechanicsModelDeallocate

INTEGER(I4B), PARAMETER, PUBLIC :: IsoLinearElasticModel = 1
INTEGER(I4B), PARAMETER, PUBLIC :: AnisoLinearElasticModel = 2
INTEGER(I4B), PARAMETER, PUBLIC :: OrthoLinearElasticModel = 3
INTEGER(I4B), PARAMETER, PUBLIC :: TransLinearElasticModel = 4

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
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & lem_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => lem_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => lem_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => lem_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => lem_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => lem_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticParam => lem_GetElasticParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetC => lem_GetC
  PROCEDURE, PUBLIC, PASS(obj) :: GetInvC => lem_GetInvC
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => &
    & lem_GetElasticityType
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => lem_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: isPlaneStrain => lem_isPlaneStrain
  PROCEDURE, PUBLIC, PASS(obj) :: isPlaneStress => lem_isPlaneStress
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
! summary: This routine initiates the the Linear elastic model

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
! summary:         Deallocate data

INTERFACE
  MODULE SUBROUTINE lem_Deallocate(obj)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  END SUBROUTINE lem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Initiate the linear elastic model by import

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
! summary: Export the linear elastic model

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
! summary: Displays the content of linear elastic model

INTERFACE
  MODULE SUBROUTINE lem_Display(obj, msg, unitNo)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE lem_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElasticParam@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_GetElasticParam(obj, PoissonRatio, &
     & ShearModulus, lambda, YoungsModulus, stiffnessPower)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: PoissonRatio
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: ShearModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: YoungsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
  END SUBROUTINE lem_GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_GetC(obj, C)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
  END SUBROUTINE lem_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_GetInvC(obj, InvC)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: InvC(:, :)
  END SUBROUTINE lem_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION lem_GetElasticityType(obj) RESULT(Ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION lem_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION lem_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION lem_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE AbstractSolidMechanicsModelDeallocate(obj)
  CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  CALL AbstractMaterialModelDeallocate(obj)
  obj%isPStress = .FALSE.
  obj%isPStrain = .FALSE.
END SUBROUTINE AbstractSolidMechanicsModelDeallocate

!----------------------------------------------------------------------------
!                                                         isPlaneStress
!----------------------------------------------------------------------------

FUNCTION lem_isPlaneStress(obj) RESULT(ans)
  CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = obj%isPStress
END FUNCTION lem_isPlaneStress

!----------------------------------------------------------------------------
!                                                         isPlaneStrain
!----------------------------------------------------------------------------

FUNCTION lem_isPlaneStrain(obj) RESULT(ans)
  CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = obj%isPStrain
END FUNCTION lem_isPlaneStrain

!----------------------------------------------------------------------------
!                                                         SetPlaneStress
!----------------------------------------------------------------------------

SUBROUTINE lem_SetPlaneStress(obj, VALUE)
  CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  LOGICAL(LGT), INTENT(IN) :: VALUE
  obj%isPStress = VALUE
END SUBROUTINE lem_SetPlaneStress

!----------------------------------------------------------------------------
!                                                         SetPlaneStrain
!----------------------------------------------------------------------------

SUBROUTINE lem_SetPlaneStrain(obj, VALUE)
  CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  LOGICAL(LGT), INTENT(IN) :: VALUE
  obj%isPStrain = VALUE
END SUBROUTINE lem_SetPlaneStrain

END MODULE AbstractSolidMechanicsModel_Class
