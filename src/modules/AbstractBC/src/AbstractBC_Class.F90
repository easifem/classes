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
CHARACTER(*), PARAMETER :: modName = "AbstractBC_Class"

!----------------------------------------------------------------------------
!                                                                AbstractBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, ABSTRACT :: AbstractBC_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !!
  TYPE(String) :: name
  !! name of boundary condition
  INTEGER(I4B) :: idof = 0
  !! degree of freedom number
  INTEGER(I4B) :: nodalValueType = -1
  !! Constant
  !! Space
  !! Time
  !! SpaceTime
  LOGICAL(LGT) :: useFunction = .FALSE.
  !! True if the boundary condition is analytical
  LOGICAL(LGT) :: isNormal = .FALSE.
  !! True if the boundary condition is normal to the boundary
  LOGICAL(LGT) :: isTangent = .FALSE.
  !! True if the boundary condition is tangent to the boundary
  LOGICAL(LGT) :: useExternal = .FALSE.
  !! if true then nodal values are used externally
  !! depending upon the context.
  !! Basically we do not use the nodal value stored in the
  !! instance of AbstractBC_
  REAL(DFP), ALLOCATABLE :: nodalValue(:, :)
  !! nodal values are kept here,
  !! nodalValues( :, its ) denotes nodal values at time step its
  PROCEDURE(iface_SpaceTimeFunction), POINTER, NOPASS :: &
    & spaceTimeFunction => NULL()
  !! SpaceTime Functions
  PROCEDURE(iface_SpaceFunction), POINTER, NOPASS :: &
    & spaceFunction => NULL()
  !! Space Function
  PROCEDURE(iface_TimeFunction), POINTER, NOPASS :: &
    & timeFunction => NULL()
  !! Time Function
  TYPE(MeshSelection_) :: boundary
  !! Boundary
  CLASS(Domain_), POINTER :: dom => NULL()
  !! Domain
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => bc_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID => bc_GetMeshID
  PROCEDURE, PUBLIC, PASS(obj) :: Get => bc_Get
  PROCEDURE, PUBLIC, PASS(obj) :: GetFromFunction => bc_GetFromFunction
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFNo => bc_GetDOFNo
  PROCEDURE, PUBLIC, PASS(obj) ::  &
    & CheckEssentialParam => bc_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => bc_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => bc_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => bc_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => bc_Display
  PROCEDURE, PUBLIC, PASS(obj) :: isUseFunction => bc_isUseFunction
  !! Returns true if the useFunction is true
  PROCEDURE, PUBLIC, PASS(obj) :: Set => bc_Set
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuery => bc_GetQuery
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
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_CheckEssentialParam(obj, param)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bc_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Deallocate data

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
!                           AbstractBCcheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE AbstractBCcheckEssentialParam(obj, param, prefix)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE AbstractBCcheckEssentialParam
END INTERFACE

PUBLIC :: AbstractBCcheckEssentialParam

!----------------------------------------------------------------------------
!                                   SetAbstractBCParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE setAbstractBCParam(param, prefix, &
    & name, idof, nodalValueType, useFunction, isNormal, isTangent, &
    & useExternal)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    !! name of boundary condition
    !! default is AbstractBC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    !! degree of freedom number
    !! default is 0
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodalValueType
    !! Space
    !! Time
    !! SpaceTime
    !! Constant
    !! default is -1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: useFunction
    !! use fucntion
    !! default is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNormal
    !! default is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTangent
    !! default is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: useExternal
    !! default is false
  END SUBROUTINE setAbstractBCParam
END INTERFACE

PUBLIC :: setAbstractBCParam

!----------------------------------------------------------------------------
!                                    AbstractBCInitiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE AbstractBCInitiate(obj, param, prefix, boundary, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE AbstractBCInitiate
END INTERFACE

PUBLIC :: AbstractBCInitiate

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Import(obj, hdf5, group, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Export in hdf5 file

INTERFACE
  MODULE SUBROUTINE bc_Export(obj, hdf5, group)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE bc_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE bc_Display(obj, msg, unitNo)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE bc_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMeshID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE FUNCTION bc_GetMeshID(obj, dim) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION bc_GetMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Get the node number and nodal value

INTERFACE
  MODULE SUBROUTINE bc_Get(obj, nodeNum, nodalValue, times)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nodeNum(:)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: nodalValue(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE bc_Get
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetFromFunction@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_GetFromFunction(obj, nodeNum, nodalValue, times)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodeNum(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodalValue(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE bc_GetFromFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetDOFNo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Get degree of freedom number

INTERFACE
  MODULE PURE FUNCTION bc_GetDOFNo(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetDOFNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isUseFunction@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_isUseFunction(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION bc_isUseFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Set(obj, ConstantNodalValue, SpaceNodalValue, &
    & TimeNodalValue, SpaceTimeNodalValue, SpaceFunction, TimeFunction, &
    & SpaceTimeFunction)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                        GetQuery@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE bc_GetQuery(obj, isInitiated, &
    & isSelectionByBox, isSelectionByMeshID, isSelectionByElemNum, &
    & isSelectionByNodeNum, idof, isTangent, isNormal, useFunction, &
    & nodalValueType, useExternal)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByBox
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByMeshID
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByNodeNum
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isTangent
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isNormal
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: useFunction
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: idof
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nodalValueType
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: useExternal
  END SUBROUTINE bc_GetQuery
END INTERFACE

END MODULE AbstractBC_Class
