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

MODULE AbstractBC_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class
USE Domain_Class
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "DirichletBC_Class"

!----------------------------------------------------------------------------
!                                                                AbstractBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, ABSTRACT :: AbstractBC_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  TYPE(String) :: name
  INTEGER(I4B) :: idof = 0
  INTEGER(I4B) :: nodalValueType = -1
    !! Constant, Space, Time, SpaceTime
  LOGICAL(LGT) :: useFunction = .FALSE.
    !! True if the boundary condition is analytical
  REAL(DFP), ALLOCATABLE :: nodalValue(:, :)
    !! nodal values are kept here,
    !! nodalValues( :, its ) denotes nodal values at
    !! time step its
  PROCEDURE(iface_SpaceTimeFunction), POINTER, NOPASS :: &
    & SpaceTimeFunction => NULL()
    !! SpaceTime Functions
  PROCEDURE(iface_SpaceFunction), POINTER, NOPASS :: &
    & SpaceFunction => NULL()
    !! Space Function
  PROCEDURE(iface_TimeFunction), POINTER, NOPASS :: &
    & TimeFunction => NULL()
    !! Time Function
  TYPE(MeshSelection_) :: boundary
    !! Boundary
  CLASS(Domain_), POINTER :: dom => NULL()
    !! Domain
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => bc_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: getMeshID => bc_getMeshID
  PROCEDURE, PUBLIC, PASS(obj) :: Get => bc_Get
  PROCEDURE, PUBLIC, PASS(obj) :: getDOFNo => bc_getDOFNo
  PROCEDURE(bc_checkEssentialParam), DEFERRED, PUBLIC, PASS(obj) ::  &
    & checkEssentialParam
  PROCEDURE(bc_Initiate), DEFERRED, PUBLIC, PASS(obj) :: Initiate
  PROCEDURE(bc_Import), DEFERRED, PUBLIC, PASS(obj) :: Import
  PROCEDURE(bc_Export), DEFERRED, PUBLIC, PASS(obj) :: Export
  PROCEDURE(bc_Display), DEFERRED, PUBLIC, PASS(obj) :: Display
END TYPE AbstractBC_

PUBLIC :: AbstractBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractBCPointer_
  CLASS(AbstractBC_), POINTER :: ptr => NULL()
END TYPE AbstractBCPointer_

PUBLIC :: AbstractBCPointer_
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE bc_checkEssentialParam(obj, param)
    IMPORT
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    IMPORT
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE bc_Import(obj, hdf5, group, dom)
    IMPORT
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Import
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE bc_Export(obj, hdf5, group)
    IMPORT
    CLASS(AbstractBC_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE bc_Export
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE bc_Display(obj, msg, unitNo)
    IMPORT
    CLASS(AbstractBC_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE bc_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Deallocate(obj)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
  END SUBROUTINE bc_Deallocate
END INTERFACE

INTERFACE AbstractBCDeallocate
  MODULE PROCEDURE bc_Deallocate
END INTERFACE AbstractBCDeallocate

PUBLIC :: AbstractBCDeallocate

!----------------------------------------------------------------------------
!                                                          GetMeshID@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION bc_getMeshID(obj, dim) RESULT(Ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION bc_getMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Get(obj, nodeNum, nodalValue, times)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nodeNum(:)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: nodalValue(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE bc_Get
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getDOFNo@getMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_getDOFNo(obj) RESULT(Ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION bc_getDOFNo
END INTERFACE

END MODULE AbstractBC_Class
