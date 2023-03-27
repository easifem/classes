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

MODULE NeumannBC_Class
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE Domain_Class, ONLY: Domain_
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "NeumannBC_CLASS"
CHARACTER(*), PARAMETER :: myprefix = "NeumannBC"

!----------------------------------------------------------------------------
!                                                               NeumannBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS(DirichletBC_) :: NeumannBC_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & bc_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => bc_Initiate
  FINAL :: bc_Final
END TYPE NeumannBC_

PUBLIC :: NeumannBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NeumannBCPointer_
  CLASS(NeumannBC_), POINTER :: ptr => NULL()
END TYPE NeumannBCPointer_

PUBLIC :: NeumannBCPointer_

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Feb 2022
! summary: Check essential parameters

INTERFACE
  MODULE SUBROUTINE bc_checkEssentialParam(obj, param)
    CLASS(NeumannBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                      setDirichletParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE setNeumannBCParam(param, name, idof, nodalValueType, &
    & useFunction, isNormal, isTangent)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: nodalValueType
    !! Space
    !! Time
    !! SpaceTime
    !! Constant
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: useFunction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNormal
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTangent
  END SUBROUTINE setNeumannBCParam
END INTERFACE

PUBLIC :: setNeumannBCParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    CLASS(NeumannBC_), INTENT(INOUT) :: obj
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
    TYPE(NeumannBC_), INTENT(INOUT) :: obj
  END SUBROUTINE bc_Final
END INTERFACE

END MODULE NeumannBC_Class
