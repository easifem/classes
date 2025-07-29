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
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE FPL, ONLY: ParameterList_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE AbstractSolidMechanicsModel_Class, ONLY: AbstractSolidMechanicsModel_
USE LinearElasticModelUtility, ONLY: GetShearModulus, &
                                     GetYoungsModulus
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "LinearElasticModel_Class"
CHARACTER(*), PARAMETER :: myPrefix = "LinearElasticModel"
CHARACTER(*), PUBLIC, PARAMETER :: LinearElasticModel_Prefix = myPrefix

PUBLIC :: LinearElasticModel_
PUBLIC :: TypeLinearElasticModel
PUBLIC :: LinearElasticModelPointer_
PUBLIC :: LinearElasticModelInitiate
PUBLIC :: SetLinearElasticModelParam
PUBLIC :: GetElasticParam
PUBLIC :: Get_PlaneStress_C_InvC
PUBLIC :: Get_PlaneStrain_C_InvC
PUBLIC :: Get_3D_C_InvC
PUBLIC :: TypeElasticityOpt
PUBLIC :: ElasticityType_char
PUBLIC :: ElasticityType_tonumber
PUBLIC :: GetYoungsModulus
PUBLIC :: GetShearModulus

INTEGER(I4B), PARAMETER, PUBLIC :: IsoLinearElasticModel = 1
INTEGER(I4B), PARAMETER, PUBLIC :: AnisoLinearElasticModel = 2
INTEGER(I4B), PARAMETER, PUBLIC :: OrthoLinearElasticModel = 3
INTEGER(I4B), PARAMETER, PUBLIC :: TransLinearElasticModel = 4
INTEGER(I4B), PARAMETER :: SIZE_C_PLANE_STRESS = 3
INTEGER(I4B), PARAMETER :: SIZE_C_PLANE_STRAIN = 3

!----------------------------------------------------------------------------
!                                                           ElasticityOpt_
!----------------------------------------------------------------------------

TYPE :: ElasticityOpt_
  INTEGER(I4B) :: Isotropic = IsoLinearElasticModel
  INTEGER(I4B) :: Anisotropic = AnisoLinearElasticModel
  INTEGER(I4B) :: Orthotropic = OrthoLinearElasticModel
  INTEGER(I4B) :: TransIsotropic = TransLinearElasticModel
  CHARACTER(3) :: Isotropic_char = "ISO"
  CHARACTER(5) :: Anisotropic_char = "ANISO"
  CHARACTER(5) :: Orthotropic_char = "ORTHO"
  CHARACTER(5) :: TransIsotropic_chars = "TRANS"
END TYPE ElasticityOpt_

TYPE(ElasticityOpt_), PARAMETER :: TypeElasticityOpt = ElasticityOpt_()

!----------------------------------------------------------------------------
!                                                       LinearElasticModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Datatype for modeling Linear elastic behavior of solids

TYPE, EXTENDS(AbstractSolidMechanicsModel_) :: LinearElasticModel_
  PRIVATE
  INTEGER(I4B) :: elasticityType = 0
  INTEGER(I4B) :: nc = 6
  !! actual size of C
  !! in case of plane-stress and plane-strain, nc is 4
  !! otherwise C is 6
  REAL(DFP) :: nu = 0.0_DFP
  !! poissonRatio
  REAL(DFP) :: G = 0.0_DFP
  !! shearModulus
  REAL(DFP) :: E = 0.0_DFP
  !! youngsModulus
  REAL(DFP) :: lambda = 0.0_DFP
  !! lame parameter
  REAL(DFP) :: C(6, 6) = 0.0_DFP
  !! elastic tensor
  REAL(DFP) :: invC(6, 6) = 0.0_DFP
  !! inverse of elastic tensor
  REAL(DFP) :: stiffnessPower = 0.0_DFP

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_FINAL

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticParam => obj_GetElasticParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetC => obj_GetC
  PROCEDURE, PUBLIC, PASS(obj) :: GetInvC => obj_GetInvC
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => obj_GetElasticityType
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetDataSize => obj_GetDataSize
  !! Get size of data
  PROCEDURE, PUBLIC, PASS(obj) :: GetData => obj_GetData
  !! Get the data

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  PROCEDURE, PUBLIC, PASS(obj) :: SetData => obj_SetData
  !! Set data
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateData => obj_UpdateData
  !! Get updated data
END TYPE LinearElasticModel_

TYPE(LinearElasticModel_), PARAMETER :: TypeLinearElasticModel = &
                                        LinearElasticModel_()

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
                                               isPlaneStrain, isPlaneStress, &
                                               poissonRatio, youngsModulus, &
                                               shearModulus, lambda, C, &
                                               invC, stiffnessPower)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    INTEGER(I4B), INTENT(IN) :: elasticityType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStress
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStrain
    REAL(DFP), OPTIONAL, INTENT(IN) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(:, :)
    !! In the case of plane-stress and plane-strain
    !! c should be at least 3-by-3. Otherwise, it should
    !! 6-by-6
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(:, :)
    !! In the case of plane-stress and plane-strain
    !! invC should be at least 3-by-3. Otherwise, it should
    !! 6-by-6
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
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the the Linear elastic model

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the the Linear elastic model

INTERFACE
  MODULE SUBROUTINE LinearElasticModelInitiate(obj, elasticityType, &
                                               isPlaneStrain, isPlaneStress, &
                                               poissonRatio, youngsModulus, &
                                               shearModulus, lambda, C, &
                                               invC, stiffnessPower)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: elasticityType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStress
    !! Is Plane stress
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStrain
    !! Is plane strain
    REAL(DFP), OPTIONAL, INTENT(IN) :: poissonRatio
    !! Poisson ratio
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    !! Yongs modulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: shearModulus
    !! Shear modulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Lame parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(:, :)
    !! In the case of plane-stress and plane-strain
    !! c should be at least 3-by-3. Otherwise, it should
    !! 6-by-6
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(:, :)
    !! In the case of plane-stress and plane-strain
    !! invC should be at least 3-by-3. Otherwise, it should
    !! 6-by-6
    REAL(DFP), OPTIONAL, INTENT(IN) :: stiffnessPower
  END SUBROUTINE LinearElasticModelInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(LinearElasticModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Initiate the linear elastic model by import

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Export the linear elastic model

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Displays the content of linear elastic model

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
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
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
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

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-01
! summary:  This routine returns C and invC from E and nu

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

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-01
! summary:  This routine returns C and invC from E and nu

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

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-01
! summary:  This routine returns C and invC from E and nu

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
  MODULE SUBROUTINE obj_GetElasticParam(obj, poissonRatio, &
     & shearModulus, lambda, youngsModulus, stiffnessPower, C, invC)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
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
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get param

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, elasticityType,  &
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
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetC(obj, C)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
  END SUBROUTINE obj_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetInvC(obj, InvC)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: InvC(:, :)
  END SUBROUTINE obj_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetElasticityType(obj) RESULT(Ans)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetDataSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the size of data needed by obj

INTERFACE
  MODULE FUNCTION obj_GetDataSize(obj) RESULT(ans)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetDataSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model

INTERFACE
  MODULE SUBROUTINE obj_GetData(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE obj_GetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for Isotropic elasticity
!
!# Introduction
!  This routine returns the data for Isotropic linear elasticity
! Data(1) contains the lambda
! Data(2) contains the G
!

INTERFACE
  MODULE SUBROUTINE LinearElasticModelGetData_Iso(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE LinearElasticModelGetData_Iso
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for anisotropic elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelGetData_Aniso(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE LinearElasticModelGetData_Aniso
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for Orthotropic elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelGetData_Ortho(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE LinearElasticModelGetData_Ortho
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for Trans Isotropic elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelGetData_Trans(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE LinearElasticModelGetData_Trans
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Set param

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, elasticityType,  &
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
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data

INTERFACE
  MODULE SUBROUTINE obj_SetData(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE obj_SetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Isotropic linear elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelSetData_Iso(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE LinearElasticModelSetData_Iso
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Aniso linear elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelSetData_Aniso(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE LinearElasticModelSetData_Aniso
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Orthotropic linear elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelSetData_Ortho(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE LinearElasticModelSetData_Ortho
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Set data for Transverse Isotropic linear elasticity

INTERFACE
  MODULE SUBROUTINE LinearElasticModelSetData_Trans(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: DATA(:)
  END SUBROUTINE LinearElasticModelSetData_Trans
END INTERFACE

!----------------------------------------------------------------------------
!                                                     UpdateData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Update data

INTERFACE
  MODULE SUBROUTINE obj_UpdateData(obj, DATA)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
  END SUBROUTINE obj_UpdateData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LinearElasticModel_Class
