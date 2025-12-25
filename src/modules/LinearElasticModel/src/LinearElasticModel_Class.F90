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
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE AbstractSolidMechanicsModel_Class, ONLY: AbstractSolidMechanicsModel_
USE LinearElasticModelUtility, ONLY: GetShearModulus
USE LinearElasticModelUtility, ONLY: GetYoungsModulus
USE LinearElasticModelUtility, ONLY: GetElasticParam
USE LinearElasticModelUtility, ONLY: Get_PlaneStress_C_InvC
USE LinearElasticModelUtility, ONLY: Get_PlaneStrain_C_InvC
USE LinearElasticModelUtility, ONLY: Get_3D_C_InvC
USE ElasticityOpt_Class, ONLY: ElasticityOpt_, TypeElasticityOpt

IMPLICIT NONE

PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "LinearElasticModel_Class"
#endif

PUBLIC :: LinearElasticModel_
PUBLIC :: TypeLinearElasticModel
PUBLIC :: LinearElasticModelPointer_
PUBLIC :: LinearElasticModelInitiate
! TypeElasticityOpt is defined in ElasticityOpt_Class
PUBLIC :: TypeElasticityOpt
! Following methods are coming from LinearElasticModelUtility module
PUBLIC :: Get_PlaneStress_C_InvC
PUBLIC :: Get_PlaneStrain_C_InvC
PUBLIC :: Get_3D_C_InvC
PUBLIC :: GetYoungsModulus
PUBLIC :: GetShearModulus
PUBLIC :: GetElasticParam

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
  !! Often used in moving mesh algorithm based on Elasticity

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_FINAL

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
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => obj_GetElasticityType
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
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the the Linear elastic model

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, elasticityType, isPlaneStrain, isPlaneStress, poissonRatio, &
    youngsModulus, shearModulus, lambda, C, invC, stiffnessPower)
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
  END SUBROUTINE obj_Initiate
END INTERFACE

INTERFACE LinearElasticModelInitiate
  MODULE PROCEDURE obj_Initiate
END INTERFACE LinearElasticModelInitiate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
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
!                                                    Final@ConstructorMethods
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
!                                                            Import@IOMethods
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
!                                                            Export@IOMethods
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
!                                                           Display@IOMethods
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
!                                                    ImportFromToml@IOMethods
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
!                                                  GetElasticParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-25
! summary: Get the elasticity parameters

INTERFACE
  MODULE SUBROUTINE obj_GetElasticParam( &
    obj, poissonRatio, shearModulus, lambda, youngsModulus, stiffnessPower, &
    C, invC, nrowC, ncolC, nrowInvC, ncolInvC)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: poissonRatio
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: shearModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrowC, ncolC, nrowInvC, ncolInvC
  END SUBROUTINE obj_GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get param

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, elasticityType, nu, G, youngsModulus, lambda, C, invC, &
    stiffnessPower, nc)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elasticityType
    REAL(DFP), OPTIONAL, INTENT(OUT) :: nu
    REAL(DFP), OPTIONAL, INTENT(OUT) :: G
    REAL(DFP), OPTIONAL, INTENT(OUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(:, :)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: stiffnessPower
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nc
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetC(obj, C, nrow, ncol)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetInvC(obj, invC, nrow, ncol)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElasticityType@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetElasticityType(obj) RESULT(Ans)
    CLASS(LinearElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetDataSize@GetMethods
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
!                                                          GetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model

INTERFACE
  MODULE SUBROUTINE obj_GetData(obj, DATA, tsize)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Set param

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, elasticityType, nu, G, youngsModulus, lambda, C, invC, &
    stiffnessPower, nc)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elasticityType
    REAL(DFP), OPTIONAL, INTENT(IN) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: G
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: stiffnessPower
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nc
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetData@SetMethods
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
!                                                       UpdateData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Update data

INTERFACE
  MODULE SUBROUTINE obj_UpdateData(obj, DATA, tsize)
    CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: DATA(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_UpdateData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LinearElasticModel_Class
