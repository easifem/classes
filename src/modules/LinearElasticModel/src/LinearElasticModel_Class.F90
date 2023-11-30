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

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Data type of linear elastic model

MODULE LinearElasticModel_Class
USE GlobalData
USE String_Class
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractSolidMechanicsModel_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "LinearElasticModel_Class"
CHARACTER(*), PARAMETER :: myprefix = "LinearElasticModel"
PUBLIC :: LinearElasticModel_
PUBLIC :: TypeLinearElasticModel
PUBLIC :: LinearElasticModelPointer_
PUBLIC :: SetLinearElasticModelParam
PUBLIC :: GetElasticParam
PUBLIC :: Get_PlaneStress_C_InvC
PUBLIC :: Get_PlaneStrain_C_InvC
PUBLIC :: Get_3D_C_InvC
PUBLIC :: TypeElasticity
PUBLIC :: ElasticityType_char
PUBLIC :: ElasticityType_tonumber

INTEGER(I4B), PARAMETER, PUBLIC :: IsoLinearElasticModel = 1
INTEGER(I4B), PARAMETER, PUBLIC :: AnisoLinearElasticModel = 2
INTEGER(I4B), PARAMETER, PUBLIC :: OrthoLinearElasticModel = 3
INTEGER(I4B), PARAMETER, PUBLIC :: TransLinearElasticModel = 4

!----------------------------------------------------------------------------
!                                                           ElasticityType_
!----------------------------------------------------------------------------

TYPE :: ElasticityType_
  INTEGER(I4B) :: Isotropic = IsoLinearElasticModel
  INTEGER(I4B) :: Anisotropic = AnisoLinearElasticModel
  INTEGER(I4B) :: Orthotropic = OrthoLinearElasticModel
  INTEGER(I4B) :: TransIsotropic = TransLinearElasticModel
  CHARACTER(3) :: Isotropic_char = "ISO"
  CHARACTER(5) :: Anisotropic_char = "ANISO"
  CHARACTER(5) :: Orthotropic_char = "ORTHO"
  CHARACTER(5) :: TransIsotropic_chars = "TRANS"
END TYPE ElasticityType_

TYPE(ElasticityType_), PARAMETER :: TypeElasticity = ElasticityType_()

!----------------------------------------------------------------------------
!                                                       LinearElasticModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Datatype for modeling Linear elastic behavior of solids

TYPE, EXTENDS(AbstractSolidMechanicsModel_) :: LinearElasticModel_
  PRIVATE
  INTEGER(I4B) :: elasticityType = 0
  REAL(DFP) :: nu = 0.0_DFP
  REAL(DFP) :: G = 0.0_DFP
  REAL(DFP) :: E = 0.0_DFP
  REAL(DFP) :: lambda = 0.0_DFP
  REAL(DFP) :: C(6, 6) = 0.0_DFP
  REAL(DFP) :: invC(6, 6) = 0.0_DFP
  REAL(DFP) :: stiffnessPower = 0.0_DFP

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & lem_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => lem_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => lem_Deallocate
  FINAL :: lem_FINAL

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => lem_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => lem_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => lem_Display

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticParam => lem_GetElasticParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetC => lem_GetC
  PROCEDURE, PUBLIC, PASS(obj) :: GetInvC => lem_GetInvC
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => lem_GetElasticityType
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => lem_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => lem_GetParam

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => lem_SetParam
END TYPE LinearElasticModel_

TYPE(LinearElasticModel_), PARAMETER :: TypeLinearElasticModel = &
  & LinearElasticModel_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: LinearElasticModelPointer_
  CLASS(LinearElasticModel_), POINTER :: ptr => NULL()
END TYPE LinearElasticModelPointer_

!----------------------------------------------------------------------------
!                               ElasticityType_tonumber@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns the elasticity number

INTERFACE
  MODULE FUNCTION ElasticityType_tonumber(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION ElasticityType_tonumber
END INTERFACE

!----------------------------------------------------------------------------
!                               ElasticityType_tonumber@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns the elasticity number

INTERFACE
  MODULE FUNCTION ElasticityType_char(num) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: num
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElasticityType_char
END INTERFACE

!----------------------------------------------------------------------------
!                             SetLinearElasticModelParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Set the essential parameter

INTERFACE
  MODULE SUBROUTINE SetLinearElasticModelParam(param, elasticityType, &
    & isPlaneStrain, isPlaneStress, poissonRatio, youngsModulus, &
    & shearModulus, lambda, C, invC, stiffnessPower)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    INTEGER(I4B), INTENT(IN) :: elasticityType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStress
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStrain
    REAL(DFP), OPTIONAL, INTENT(IN) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: stiffnessPower
  END SUBROUTINE SetLinearElasticModelParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Check the essential parameter

INTERFACE
  MODULE SUBROUTINE lem_CheckEssentialParam(obj, param)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
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
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE lem_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE lem_Deallocate(obj)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  END SUBROUTINE lem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE lem_Final(obj)
    TYPE(LinearElasticModel_), INTENT(INOUT) :: obj
  END SUBROUTINE lem_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Initiate the linear elastic model by import

INTERFACE
  MODULE SUBROUTINE lem_Import(obj, hdf5, group)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
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
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
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
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE lem_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetElasticParameter@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This returns the elastic parameter

INTERFACE
  MODULE SUBROUTINE GetElasticParam(lam, G, EE, nu, shearModulus, &
    & youngsModulus, poissonRatio, lambda)
    REAL(DFP), INTENT(OUT) :: lam
    REAL(DFP), INTENT(OUT) :: G
    REAL(DFP), INTENT(OUT) :: EE
    REAL(DFP), INTENT(OUT) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  END SUBROUTINE GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          Get_PlaneStress_C_InvC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Get_PlaneStress_C_InvC(C, invC, youngsModulus, nu)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    REAL(DFP), INTENT(IN) :: youngsModulus
    REAL(DFP), INTENT(IN) :: nu
  END SUBROUTINE Get_PlaneStress_C_InvC
END INTERFACE

!----------------------------------------------------------------------------
!                                          Get_PlaneStrain_C_InvC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Get_PlaneStrain_C_InvC(C, invC, youngsModulus, nu)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    REAL(DFP), INTENT(IN) :: youngsModulus
    REAL(DFP), INTENT(IN) :: nu
  END SUBROUTINE Get_PlaneStrain_C_InvC
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Get_3D_C_InvC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Get_3D_C_InvC(C, invC, youngsModulus, nu)
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    REAL(DFP), INTENT(IN) :: youngsModulus
    REAL(DFP), INTENT(IN) :: nu
  END SUBROUTINE Get_3D_C_InvC
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElasticParam@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_GetElasticParam(obj, poissonRatio, &
     & shearModulus, lambda, youngsModulus, stiffnessPower)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
  END SUBROUTINE lem_GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_GetC(obj, C)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
  END SUBROUTINE lem_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE lem_GetInvC(obj, InvC)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: InvC(:, :)
  END SUBROUTINE lem_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION lem_GetElasticityType(obj) RESULT(Ans)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION lem_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION lem_GetPrefix(obj) RESULT(ans)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION lem_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Set param

INTERFACE
  MODULE SUBROUTINE lem_SetParam(obj, elasticityType,  &
  & nu, G, youngsModulus, lambda, C, invC, stiffnessPower)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elasticityType
    REAL(DFP), OPTIONAL, INTENT(IN) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: G
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: stiffnessPower
  END SUBROUTINE lem_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get param

INTERFACE
  MODULE SUBROUTINE lem_GetParam(obj, elasticityType,  &
    & nu, G, youngsModulus, lambda, C, invC, stiffnessPower)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: elasticityType
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: nu
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: G
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(6, 6)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
  END SUBROUTINE lem_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LinearElasticModel_Class
