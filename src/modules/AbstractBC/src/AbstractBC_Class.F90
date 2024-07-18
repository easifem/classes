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
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: TypeFEVariableOpt, &
                    FEVariable_
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE HDF5File_Class, ONLY: HDF5File_
USE UserFunction_Class, ONLY: UserFunction_
USE FPL, ONLY: ParameterList_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractBC_Class"
CHARACTER(*), PARAMETER :: default_name = "AbstractBC"
INTEGER(I4B), PARAMETER :: default_idof = 1_I4B
INTEGER(I4B), PARAMETER :: default_nodalValueType = TypeFEVariableOpt%constant
CHARACTER(*), PARAMETER :: default_nodalValueType_char = "NONE"
LOGICAL(LGT), PARAMETER :: default_isUserFunction = .FALSE.
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
  PRIVATE

  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  TYPE(String) :: name
  !! name of boundary condition
  INTEGER(I4B) :: idof = default_idof
  !! degree of freedom number
  INTEGER(I4B) :: nodalValueType = default_nodalValueType
  !! Constant, Space, SpaceTime, Time
  INTEGER(I4B) :: nrow = 0
  !! number of rows in nodalValue
  INTEGER(I4B) :: ncol = 0
  !! number of columns in nodalvalue
  LOGICAL(LGT) :: isNormal = default_isNormal
  !! True if the boundary condition is normal to the boundary
  LOGICAL(LGT) :: isTangent = default_isTangent
  !! True if the boundary condition is tangent to the boundary
  LOGICAL(LGT) :: useExternal = default_useExternal
  !! if true then nodal values are used externally
  !! depending upon the context.
  !! Basically we do not use the nodal value stored in the
  !! instance of AbstractBC_
  LOGICAL(LGT) :: isUserFunction = default_isUserFunction
  !! True if userFunction is set
  REAL(DFP), ALLOCATABLE :: nodalValue(:, :)
  !! nodal values are kept here,
  !! nodalValues( :, its ) denotes nodal values at time step its
  CLASS(UserFunction_), POINTER :: func => NULL()
  !! User function
  TYPE(MeshSelection_) :: boundary
  !! Boundary
  CLASS(AbstractDomain_), POINTER :: dom => NULL()
  !! Domain

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate memory occupied by AbstractBC
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameter
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate an instance of AbstractBC

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display content of AbstractBC
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Initiate from toml
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Initiate from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import abstract kernel from toml
  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml => &
    obj_ImportParamFromToml

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID => obj_GetMeshID
  !! Get MeshID

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshIDPointer => obj_GetMeshIDPointer
  !! Get mesh id pointer

  PROCEDURE, PUBLIC, PASS(obj) :: Get1 => obj_Get1
  !! Get value of boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: Get2 => obj_Get2
  !! Get value of boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: Get3 => obj_Get3
  !! Get value of boundary condition in FEVariable_

  GENERIC, PUBLIC :: Get => Get1, Get2, Get3
  !! Generic method to get the boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalNodeNum => obj_GetTotalNodeNum
  !! Get total node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetFromUserFunction => &
    obj_GetFromUserFunction
  !! Get value from userfunction
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFNo => obj_GetDOFNo
  !! Get degree of freedom number
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: IsUseFunction => obj_IsUseFunction
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
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param, prefix)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE AbstractBCcheckEssentialParam

!----------------------------------------------------------------------------
!                                   SetAbstractBCParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary: Set abstract boundary condition parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractBCParam(param, prefix, &
                            name, idof, nodalValueType, isNormal, isTangent, &
                                       useExternal, isUserFunction)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    !! name of boundary condition
    !! default is AbstractBC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    !! degree of freedom number
    !! default is 0
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodalValueType
    !! Space, Time, SpaceTime, Constant
    !! default is -1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUserFunction
    !! set true when userfucntion is used; default is false
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
  MODULE SUBROUTINE obj_Initiate(obj, param, boundary, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractBCInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Deallocate data

INTERFACE AbstractBCDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractBCDeallocate

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Import AbstractBC from HDF5File

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Export in hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                              ImportParamFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param by reading the toml table

INTERFACE AbstractBCImportParamFromToml
  MODULE SUBROUTINE obj_ImportParamFromToml(obj, param, table)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportParamFromToml
END INTERFACE AbstractBCImportParamFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE AbstractBCImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE AbstractBCImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractBCImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, dom, tomlName, afile, &
                                        filename, printToml)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE AbstractBCImportFromToml

!----------------------------------------------------------------------------
!                                                        Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMeshID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE FUNCTION obj_GetMeshID(obj, dim) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMeshIDPointer@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetMeshIDPointer(obj, dim, ans, tsize)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), POINTER, INTENT(OUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetMeshIDPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, nodeNum, nodalValue, nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! nrow = size of nodeNum
    !! ncol = 1 or size of times
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, nodeNum, tsize)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Get the dirichlet boundary condition in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, fevar, globalNode, &
                             spaceQuadPoints, timeQuadPoints, atime, timeVec)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceQuadPoints(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeQuadPoints(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: atime
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeNum(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodeNum
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetFromFunction_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  This function returns the nodalValue from userfunction
!
!# Introduction
!
! the shape and value of nodal value depends upon the obj%nodalValuetype
!
! constant: shape of nodalvalue is tnodes, 1
! in this case we get the value from function and set
! all values of nodalvalue(:, 1)
!
! space:
! ncol = 1
!
! time
! ncol = ttimes
!
! spacetime
! ncol = ttimes

INTERFACE
  MODULE SUBROUTINE obj_GetFromUserFunction(obj, nodeNum, nodalValue, &
                                            nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodeNum(:)
    !! node number should be allocated
    !! size of nodenum is nrow
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! nodal values
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time value
    !! time values are needed when userfunction is time or space-time
    INTEGER(I4B) :: nrow, ncol
    !! nrow is size of nodenum and size of number of rows in nodalvalue
    !! ncol is colsize of nodalvalue
  END SUBROUTINE obj_GetFromUserFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetDOFNo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Get degree of freedom number

INTERFACE
  MODULE PURE FUNCTION obj_GetDOFNo(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetDOFNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetQuery@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get field values of abstract boundary condition

INTERFACE
  MODULE PURE SUBROUTINE obj_GetParam(obj, isInitiated, &
                isSelectionByBox, isSelectionByMeshID, isSelectionByElemNum, &
               isSelectionByNodeNum, idof, isTangent, isNormal, useFunction, &
                                  nodalValueType, useExternal, isUserFunction)
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
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isUserFunction
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsUseFunction@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Returns true if useFunction is true

INTERFACE
  MODULE PURE FUNCTION obj_IsUseFunction(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsUseFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Set fields of abstract boundary condition

INTERFACE
  MODULE SUBROUTINE obj_Set(obj, constantNodalValue, spaceNodalValue, &
                            timeNodalValue, spaceTimeNodalValue, userFunction)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: constantNodalValue
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceNodalValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: timeNodalValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceTimeNodalValue(:, :)
    TYPE(UserFunction_), TARGET, OPTIONAL, INTENT(IN) :: userFunction
  END SUBROUTINE obj_Set
END INTERFACE

END MODULE AbstractBC_Class
