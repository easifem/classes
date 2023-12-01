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
! date: 4 Oct 2021
! summary: Data type of linear elastic model

MODULE LinearPoroElasticModel_Class
USE GlobalData
USE String_Class
USE BaSetype
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractPoroMechanicsModel_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "LinearPoroElasticModel_Class"
CHARACTER(*), PARAMETER :: myPrefix = "LinearPoroElasticModel"
CHARACTER(*), PARAMETER, PUBLIC :: LinearPoroElasticModel_Prefix  &
  & = myPrefix
PUBLIC :: LinearPoroElasticModel_
PUBLIC :: TypeLinearPoroElasticModel
PUBLIC :: LinearPoroElasticModelPointer_
PUBLIC :: SetLinearPoroElasticModelParam

!----------------------------------------------------------------------------
!                                                   LinearPoroElasticModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Datatype for modeling Linear elastic behavior of [[PorousMaterial_]]

TYPE, EXTENDS(AbstractPoroMechanicsModel_) :: LinearPoroElasticModel_
  PRIVATE
  INTEGER(I4B) :: elasticityType = 0
  REAL(DFP) :: nu = 0.0_DFP
  REAL(DFP) :: G = 0.0_DFP
  REAL(DFP) :: E = 0.0_DFP
  REAL(DFP) :: lambda = 0.0_DFP
  REAL(DFP) :: C(6, 6) = 0.0_DFP
  REAL(DFP) :: invC(6, 6) = 0.0_DFP
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & lpem_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => lpem_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => lpem_Deallocate
  FINAL :: lpem_FINAL
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => lpem_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => lpem_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => lpem_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticParam => lpem_GetElasticParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetC => lpem_GetC
  PROCEDURE, PUBLIC, PASS(obj) :: GetInvC => lpem_GetInvC
  PROCEDURE, PUBLIC, PASS(obj) :: GetElasticityType => &
    & lpem_GetElasticityType
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam =>  &
    & lpem_GetParam
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam =>  &
    & lpem_SetParam
END TYPE LinearPoroElasticModel_

TYPE(LinearPoroElasticModel_), PARAMETER :: TypeLinearPoroElasticModel  &
  & = LinearPoroElasticModel_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: LinearPoroElasticModelPointer_
  CLASS(LinearPoroElasticModel_), POINTER :: ptr => NULL()
END TYPE LinearPoroElasticModelPointer_

!----------------------------------------------------------------------------
!                         SetLinearPoroElasticModelParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Check the essential parameter

INTERFACE
  MODULE SUBROUTINE SetLinearPoroElasticModelParam(param, elasticityType, &
    & isPlaneStrain, isPlaneStress, PoissonRatio, YoungsModulus, &
    & ShearModulus, lambda, C, invC)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    INTEGER(I4B), INTENT(IN) :: elasticityType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStress
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPlaneStrain
    REAL(DFP), OPTIONAL, INTENT(IN) :: PoissonRatio
    REAL(DFP), OPTIONAL, INTENT(IN) :: YoungsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: ShearModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(6, 6)
  END SUBROUTINE SetLinearPoroElasticModelParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Check the essential parameter

INTERFACE
  MODULE SUBROUTINE lpem_CheckEssentialParam(obj, param)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE lpem_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: This routine initiates the the Linear elastic model

INTERFACE
  MODULE SUBROUTINE lpem_Initiate(obj, param)
    CLASS(LinearPoroElasticModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE lpem_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary:         Deallocate data

INTERFACE
  MODULE SUBROUTINE lpem_Deallocate(obj)
    CLASS(LinearPoroElasticModel_), INTENT(INOUT) :: obj
  END SUBROUTINE lpem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary:         Deallocate data

INTERFACE
  MODULE SUBROUTINE lpem_Final(obj)
    TYPE(LinearPoroElasticModel_), INTENT(INOUT) :: obj
  END SUBROUTINE lpem_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Initiate the linear elastic model by import

INTERFACE
  MODULE SUBROUTINE lpem_Import(obj, hdf5, group)
    CLASS(LinearPoroElasticModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE lpem_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Export the linear elastic model

INTERFACE
  MODULE SUBROUTINE lpem_Export(obj, hdf5, group)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE lpem_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Displays the content of linear elastic model

INTERFACE
  MODULE SUBROUTINE lpem_Display(obj, msg, unitNo)
    CLASS(LinearPoroElasticModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE lpem_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElasticParam@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE lpem_GetElasticParam(obj, PoissonRatio, &
    & ShearModulus, lambda, YoungsModulus, stiffnessPower, C, invC)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: PoissonRatio
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: ShearModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: YoungsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: stiffnessPower
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(:, :)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(:, :)
  END SUBROUTINE lpem_GetElasticParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE lpem_GetC(obj, C)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
  END SUBROUTINE lpem_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE lpem_GetInvC(obj, InvC)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: InvC(:, :)
  END SUBROUTINE lpem_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION lpem_GetElasticityType(obj) RESULT(Ans)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION lpem_GetElasticityType
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Set param

INTERFACE
  MODULE SUBROUTINE lpem_SetParam(obj, elasticityType,  &
  & nu, G, youngsModulus, lambda, C, invC)
    CLASS(LinearPoroElasticModel_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elasticityType
    REAL(DFP), OPTIONAL, INTENT(IN) :: nu
    REAL(DFP), OPTIONAL, INTENT(IN) :: G
    REAL(DFP), OPTIONAL, INTENT(IN) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    REAL(DFP), OPTIONAL, INTENT(IN) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(IN) :: invC(6, 6)
  END SUBROUTINE lpem_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get param

INTERFACE
  MODULE SUBROUTINE lpem_GetParam(obj, elasticityType,  &
    & nu, G, youngsModulus, lambda, C, invC)
    CLASS(LinearPoroElasticModel_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: elasticityType
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: nu
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: G
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: youngsModulus
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: C(6, 6)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: invC(6, 6)
  END SUBROUTINE lpem_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LinearPoroElasticModel_Class
