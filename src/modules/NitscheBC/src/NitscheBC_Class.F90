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

MODULE NitscheBC_Class
USE GlobalData
USE BaseType
USE String_Class
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class
USE Domain_Class
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
USE DirichletBC_Class
USE NeumannBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "NitscheBC_CLASS"

!----------------------------------------------------------------------------
!                                                               NitscheBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-02-14
! summary: This is Nitsche boundary condition

TYPE, EXTENDS(NeumannBC_) :: NitscheBC_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & bc_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => bc_Initiate
  FINAL :: bc_Final
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => bc_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => bc_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => bc_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Set => bc_Set
END TYPE NitscheBC_

PUBLIC :: NitscheBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NitscheBCPointer_
  CLASS(NitscheBC_), POINTER :: ptr => NULL()
END TYPE NitscheBCPointer_

PUBLIC :: NitscheBCPointer_

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-02-14
! summary: Check essential parameters

INTERFACE
  MODULE SUBROUTINE bc_checkEssentialParam(obj, param)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                      setDirichletParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE setNitscheBCParam(param, name, idof, nodalValueType, &
    & useFunction)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: nodalValueType
    !! Space
    !! Time
    !! SpaceTime
    !! Constant
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: useFunction
  END SUBROUTINE setNitscheBCParam
END INTERFACE

PUBLIC :: setNitscheBCParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Final(obj)
    TYPE(NitscheBC_), INTENT(INOUT) :: obj
  END SUBROUTINE bc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Import(obj, hdf5, group, dom)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Export(obj, hdf5, group)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE bc_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Display(obj, msg, unitNo)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE bc_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Set(obj, ConstantNodalValue, SpaceNodalValue, &
    & TimeNodalValue, SpaceTimeNodalValue, SpaceFunction, TimeFunction, &
    & SpaceTimeFunction)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: ConstantNodalValue
    REAL(DFP), OPTIONAL, INTENT(IN) :: SpaceNodalValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: TimeNodalValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: SpaceTimeNodalValue(:, :)
    PROCEDURE(iface_SpaceTimeFunction), POINTER, OPTIONAL, INTENT(IN) :: &
      & SpaceTimeFunction
    PROCEDURE(iface_SpaceFunction), POINTER, OPTIONAL, INTENT(IN) :: &
      & SpaceFunction
    PROCEDURE(iface_TimeFunction), POINTER, OPTIONAL, INTENT(IN) :: &
      & TimeFunction
  END SUBROUTINE bc_Set
END INTERFACE

END MODULE NitscheBC_Class
