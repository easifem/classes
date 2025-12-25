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
PUBLIC :: AbstractSolidMechanicsModelPointer_
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
  !! Is Plane Stress
  LOGICAL(LGT) :: isPStrain = .FALSE.
  !! Is Plane Strain

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! GET:
  ! @GetMethods
  PROCEDURE(obj_GetElasticParam), DEFERRED, PUBLIC, PASS(obj) :: &
    GetElasticParam
  !! Implemented by child classes
  PROCEDURE(obj_GetC), DEFERRED, PUBLIC, PASS(obj) :: GetC
  !! Implemented by child classes
  PROCEDURE(obj_GetInvC), DEFERRED, PUBLIC, PASS(obj) :: GetInvC
  !! Implemented by child classes
  PROCEDURE(obj_GetElasticityType), DEFERRED, PUBLIC, PASS(obj) :: &
    GetElasticityType
  !! Implemented by child classes

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: isPlaneStrain => &
    obj_isPlaneStrain
  !! Get the PStrain
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: isPlaneStress => &
    obj_isPlaneStress
  !! Get the PStress

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetPlaneStress => &
    obj_SetPlaneStress
  !! Set PStress
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetPlaneStrain => &
    obj_SetPlaneStrain
  !! Set PStrain
END TYPE AbstractSolidMechanicsModel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractSolidMechanicsModelPointer_
  CLASS(AbstractSolidMechanicsModel_), POINTER :: ptr => NULL()
END TYPE AbstractSolidMechanicsModelPointer_

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Deallocate data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractSolidMechanicsModel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE AbstractSolidMechanicsModelDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE AbstractSolidMechanicsModelDeallocate

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
!                                                GetElasticParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-30
! summary:  Get elastic parameter

ABSTRACT INTERFACE
  SUBROUTINE obj_GetElasticParam( &
    obj, poissonRatio, shearModulus, lambda, youngsModulus, stiffnessPower, &
    C, invC, nrowC, ncolC, nrowInvC, ncolInvC)
    IMPORT :: AbstractSolidMechanicsModel_, DFP, I4B
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
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
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the elastic tangent matrix

ABSTRACT INTERFACE
  SUBROUTINE obj_GetC(obj, C, nrow, ncol)
    IMPORT :: AbstractSolidMechanicsModel_, DFP, I4B
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: C(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetC
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get inverse of elastic tangent matrix

ABSTRACT INTERFACE
  SUBROUTINE obj_GetInvC(obj, InvC, nrow, ncol)
    IMPORT :: AbstractSolidMechanicsModel_, DFP, I4B
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: invC(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetInvC
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElasticityType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get elasticity type

ABSTRACT INTERFACE
  FUNCTION obj_GetElasticityType(obj) RESULT(Ans)
    IMPORT :: AbstractSolidMechanicsModel_, I4B
    CLASS(AbstractSolidMechanicsModel_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElasticityType
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
