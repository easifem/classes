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

MODULE AbstractBC_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class
USE Domain_Class
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE tomlf, ONLY: toml_table
USE TxtFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractBC_Class"
CHARACTER(*), PARAMETER :: default_name = "AbstractBC"
INTEGER(I4B), PARAMETER :: default_idof = 0_I4B
INTEGER(I4B), PARAMETER :: default_nodalValueType = -1_I4B
CHARACTER(*) , PARAMETER :: default_nodalValueType_char = "NONE"
LOGICAL(LGT), PARAMETER :: default_useFunction = .FALSE.
LOGICAL(LGT), PARAMETER :: default_isNormal = .FALSE.
LOGICAL(LGT), PARAMETER :: default_isTangent = .FALSE.
LOGICAL(LGT), PARAMETER :: default_useExternal = .FALSE.

PUBLIC :: AbstractBC_
PUBLIC :: AbstractBCPointer_
PUBLIC :: AbstractBCDeallocate
PUBLIC :: AbstractBCcheckEssentialParam
PUBLIC :: SetAbstractBCParam
PUBLIC :: AbstractBCInitiate
PUBLIC :: AbstractBCImportFromToml
PUBLIC :: AbstractBCImportParamFromToml

!----------------------------------------------------------------------------
!                                                                AbstractBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, ABSTRACT :: AbstractBC_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  TYPE(String) :: name
  !! name of boundary condition
  INTEGER(I4B) :: idof = default_idof
  !! degree of freedom number
  INTEGER(I4B) :: nodalValueType = default_nodalValueType
  !! Constant
  !! Space
  !! Time
  !! SpaceTime
  LOGICAL(LGT) :: useFunction = default_useFunction
  !! True if the boundary condition is analytical
  LOGICAL(LGT) :: isNormal = default_isNormal
  !! True if the boundary condition is normal to the boundary
  LOGICAL(LGT) :: isTangent = default_isTangent
  !! True if the boundary condition is tangent to the boundary
  LOGICAL(LGT) :: useExternal = default_useExternal
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

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => bc_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam =>  &
    & bc_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => bc_Initiate

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => bc_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => bc_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => bc_Display
  PROCEDURE, PASS(obj) :: ImportFromToml1 => bc_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => bc_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    & ImportFromToml2
  !! Import abstract kernel from toml
  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml =>  &
    & bc_ImportParamFromToml

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Set => bc_Set

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID => bc_GetMeshID
  PROCEDURE, PUBLIC, PASS(obj) :: Get1 => bc_Get
  PROCEDURE, PUBLIC, PASS(obj) :: Get2 => bc_GetFEVar
  GENERIC, PUBLIC :: Get => Get1, Get2
  PROCEDURE, PUBLIC, PASS(obj) :: GetFromFunction => bc_GetFromFunction
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFNo => bc_GetDOFNo
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuery => bc_GetQuery
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => bc_GetQuery
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => bc_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: isUseFunction => bc_isUseFunction
  !! Returns true if the useFunction is true
END TYPE AbstractBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractBCPointer_
  CLASS(AbstractBC_), POINTER :: ptr => NULL()
END TYPE AbstractBCPointer_

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Check essential parameters

INTERFACE AbstractBCcheckEssentialParam
  MODULE SUBROUTINE bc_CheckEssentialParam(obj, param, prefix)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE bc_CheckEssentialParam
END INTERFACE AbstractBCcheckEssentialParam

!----------------------------------------------------------------------------
!                                   SetAbstractBCParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary: Set abstract boundary condition parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractBCParam(param, prefix, &
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
  END SUBROUTINE SetAbstractBCParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Initiate abstract boundary condition

INTERFACE AbstractBCInitiate
  MODULE SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Initiate
END INTERFACE AbstractBCInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Deallocate data

INTERFACE AbstractBCDeallocate
  MODULE SUBROUTINE bc_Deallocate(obj)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
  END SUBROUTINE bc_Deallocate
END INTERFACE AbstractBCDeallocate

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Import AbstractBC from HDF5File

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
!                                              ImportParamFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param by reading the toml table

INTERFACE AbstractBCImportParamFromToml
  MODULE SUBROUTINE bc_ImportParamFromToml(obj, param, table)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE bc_ImportParamFromToml
END INTERFACE AbstractBCImportParamFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE AbstractBCImportFromToml
  MODULE SUBROUTINE bc_ImportFromToml1(obj, table, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_ImportFromToml1
END INTERFACE AbstractBCImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractBCImportFromToml
  MODULE SUBROUTINE bc_ImportFromToml2(obj, dom, tomlName, afile,  &
    & filename, printToml)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE bc_ImportFromToml2
END INTERFACE AbstractBCImportFromToml

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
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Get the node number and nodal value

INTERFACE
  MODULE SUBROUTINE bc_GetFEVar(obj, fevar, globalNode,  &
  & spaceQuadPoints, timeQuadPoints, atime, timeVec)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceQuadPoints(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeQuadPoints(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: atime
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)
  END SUBROUTINE bc_GetFEVar
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
  MODULE PURE FUNCTION bc_IsUseFunction(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION bc_IsUseFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Set(obj, constantNodalValue, spaceNodalValue, &
    & timeNodalValue, spaceTimeNodalValue, spaceFunction, timeFunction, &
    & spaceTimeFunction)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: constantNodalValue
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceNodalValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeNodalValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceTimeNodalValue(:, :)
    PROCEDURE(iface_SpaceTimeFunction), POINTER, OPTIONAL, INTENT(IN) :: &
      & spaceTimeFunction
    PROCEDURE(iface_SpaceFunction), POINTER, OPTIONAL, INTENT(IN) :: &
      & spaceFunction
    PROCEDURE(iface_TimeFunction), POINTER, OPTIONAL, INTENT(IN) :: &
      & timeFunction
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

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION bc_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION bc_GetPrefix
END INTERFACE

END MODULE AbstractBC_Class
