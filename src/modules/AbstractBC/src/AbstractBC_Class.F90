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
USE FEDOF_Class, ONLY: FEDOF_

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

  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is true if the object is initiated
  LOGICAL(LGT) :: isNormal = default_isNormal
  !! True if the boundary condition is normal to the boundary
  LOGICAL(LGT) :: isTangent = default_isTangent
  !! True if the boundary condition is tangent to the boundary
  LOGICAL(LGT) :: isUseExternal = default_useExternal
  !! if true then nodal values are used externally
  !! depending upon the context.
  !! Basically we do not use the nodal value stored in the
  !! instance of AbstractBC_
  LOGICAL(LGT) :: isUserFunction = default_isUserFunction
  !! True if userFunction is set
  LOGICAL(LGT) :: isElemToFace = .FALSE.
  !! When elemToFace is set then isElemToFace is true
  LOGICAL(LGT) :: isElemToEdge = .FALSE.
  !! When elemToEdge is set then isElemToEdge is true

  TYPE(String) :: name
  !! name of boundary condition

  INTEGER(I4B) :: idof = default_idof
  !! degree of freedom number
  INTEGER(I4B) :: nodalValueType = default_nodalValueType
  !! Constant, Space, SpaceTime, Time
  INTEGER(I4B) :: nrow = 0
  !! number of rows in nodalValue
  !! constantNodalValue nrow = 1
  !! spaceNodalValue nrow = size of nodenum
  !! timeNodalValue nrow = size of timenodalvalue
  !! spaceTimeNodalValue nrow = size of nodenum
  INTEGER(I4B) :: ncol = 0
  !! number of columns in nodalvalue
  !! constantNodalValue ncol = 1
  !! spaceNodalValue ncol = 1
  !! timeNodalValue ncol = 1
  !! spaceTimeNodalValue ncol = size of times
  INTEGER(I4B) :: tElemToFace = 0
  !! number of col in elemToFace
  INTEGER(I4B) :: tElemToEdge = 0
  !! number of col in elemToEdge

  INTEGER(I4B), ALLOCATABLE :: nodenum(:)
  !! node numbers, where dirichlet boundary condition will be imposed
  !! info: to be used soon

  INTEGER(I4B), ALLOCATABLE :: elemToFace(:, :)
  !! each col contains the following data:
  !! localCellNum, localFaceNum
  !! two col are ordered with respect to localCellNum
  !! For example, if a cell has two or more faces where boundary condition
  !! is applied, then this data will be stored in two consequtive rows:
  !! col1:       localCellNum1, localFace1
  !! col2:       localCellNum1, localFace2
  !!
  !! if the value of localFace is zero, then it means
  !! boundary condition is not applied on that face

  INTEGER(I4B), ALLOCATABLE :: elemToEdge(:, :)
  !! It is used for 3D mesh
  !! each cols contains the following data:
  !! localCellNum, localEdgeNum
  !! two cols are ordered with respect to localCellNum
  !! For example, if a cell has two or more faces where boundary condition
  !! is applied, then this data will be stored in two consequtive rows:
  !! col1:       localCellNum1, localEdge1
  !! col2:       localCellNum1, localEdge2
  !!
  !! if the value of localEdge is zero, then it means
  !! boundary condition is not applied on that edge

  REAL(DFP), ALLOCATABLE :: nodalValue(:, :)
  !! nodal values are kept here,
  !! nodalValues( :, its ) denotes nodal values at time step its
  !! nodalValue is used when useFunction and useExternal is false

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

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate memory occupied by AbstractBC
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameter
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate an instance of AbstractBC
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate an instance of AbstractBC with arguments
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2

  ! IO:
  ! @IOMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from HDF5File
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to HDF5File
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display content of AbstractBC

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Initiate from toml
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Initiate from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import abstract kernel from toml

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ImportParamFromToml => &
    obj_ImportParamFromToml
  !! Import parameter from toml file

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Set the boundary condition value

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetElemToLocalBoundary => &
    obj_SetElemToLocalBoundary

  ! GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! Returns isInit

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetMeshID => &
    obj_GetMeshID
  !! Get MeshID

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetMeshIDPointer => &
    obj_GetMeshIDPointer
  !! Get mesh id pointer

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetH1Lagrange1 => &
    obj_GetH1Lagrange1
  !! Get nodenum and nodal value for H1 and Lagrange polynomial
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetH1Lagrange2 => &
    obj_GetH1Lagrange2
  !! Get the node number for H1 and Lagrange polynomials
  GENERIC, PUBLIC :: GetH1Lagrange => GetH1Lagrange1, GetH1Lagrange2

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetH1Hierarchical1 => &
    obj_GetH1Hierarchical1
  !! Get node number and nodal value for H1 Hierarchical polynomials
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetH1Hierarchical2 => &
    obj_GetH1Hierarchical2
  !! Get the node number for H1 and Lagrange polynomials
  GENERIC, PUBLIC :: GetH1Hierarchical => GetH1Hierarchical1, &
    GetH1Hierarchical2

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Get1 => obj_Get1
  !! Get the nodal value of boundary condition
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Get2 => obj_Get2
  !! Get node numbers where boundary condition is applied
  GENERIC, PUBLIC :: Get => Get1, Get2
  !! Generic method to get the boundary condition

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalNodeNum => &
    obj_GetTotalNodeNum
  !! Get total node number

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalNodeNumH1Lagrange => obj_GetTotalNodeNumH1Lagrange
  !! Get total node number for H1 Lagrange element

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalNodeNumH1Hierarchical => obj_GetTotalNodeNumH1Hierarchical

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetDOFNo => obj_GetDOFNo
  !! Get degree of freedom number
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters of AbstractBC
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsUseFunction => &
    obj_IsUseFunction
  !! Returns true if the useFunction is true
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get the prefix of boundary condition, it should be
  !! overridden in the derived class
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
  MODULE SUBROUTINE SetAbstractBCParam(param, prefix, name, idof, &
                                       nodalValueType, isNormal, isTangent, &
                                       isUseExternal, isUserFunction)
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUseExternal
    !! default is false
  END SUBROUTINE SetAbstractBCParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Initiate abstract boundary condition

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, boundary, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE AbstractBCInitiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE AbstractBCInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary:  Initiate AbstractBC with arguments

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, boundary, dom, name, idof, &
                                  nodalValueType, isNormal, isTangent, &
                                  isUseExternal, isUserFunction)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! Domain
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUseExternal
    !! default is false
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE AbstractBCInitiate
  MODULE PROCEDURE obj_Initiate2
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

INTERFACE
  MODULE SUBROUTINE obj_ImportParamFromToml(obj, param, table)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportParamFromToml
END INTERFACE

INTERFACE AbstractBCImportParamFromToml
  MODULE PROCEDURE obj_ImportParamFromToml
END INTERFACE AbstractBCImportParamFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE AbstractBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE AbstractBCImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, dom, tomlName, afile, &
                                        filename, printToml)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE AbstractBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
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
!                                                     IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary: Returns isInit

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
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

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Get the nodenum and nodalValue
!
!# Introduction
!
! This method calls GetH1Lagrange or GetH1Hierarchical methods

INTERFACE
MODULE SUBROUTINE obj_Get1(obj, fedof, nodeNum, nodalValue, nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! Degree of freedom
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! Nodal values of boundary value
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

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Get the nodeNum
!
!# Introduction
!
! This method returns the nodeNum and tsize
! It calls GetH1Lagrange or GetH1Hierarchical methods

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, fedof, nodeNum, tsize)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! finite element degree of freedom
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-02-12
! summary:  Get the nodenum and nodalvalue

INTERFACE
  MODULE SUBROUTINE obj_GetH1Lagrange1(obj, fedof, nodeNum, &
                                       nodalValue, nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! Degree of freedom
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! nrow = Size of nodeNum
    !! ncol = 1 or Size of times
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj
  END SUBROUTINE obj_GetH1Lagrange1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetH1Lagrange@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-26
! summary:  Get number of nodes from abstract boundary conditions

INTERFACE
  MODULE SUBROUTINE obj_GetH1Lagrange2(obj, fedof, nodeNum, tsize)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! Degree of freedom object
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetH1Lagrange2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetH1Hierarchical@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-26
! summary:  Get number of node and nodal values for H1 and Hierarchical

INTERFACE
  MODULE SUBROUTINE obj_GetH1Hierarchical1(obj, fedof, nodeNum, &
                                           nodalValue, nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! Degree of freedom
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! nrow = Size of nodeNum
    !! ncol = 1 or Size of times
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj
  END SUBROUTINE obj_GetH1Hierarchical1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetH1Lagrange@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-26
! summary: Get number of nodes from abstract boundary conditions

INTERFACE
  MODULE SUBROUTINE obj_GetH1Hierarchical2(obj, fedof, nodeNum, tsize)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !!
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! degreee of freedom object
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetH1Hierarchical2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeNum(obj, fedof) RESULT(ans)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(IN) :: fedof
    !! FEDOF
    INTEGER(I4B) :: ans
    !! ans
  END FUNCTION obj_GetTotalNodeNum
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalNodeNum@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-27
! summary:  Get total node number
!
!# Introduction
!
! This method returns the total number of nodes for H1Lagrange element
!
! This routine calls GetTotalNodeNum on boundary

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeNumH1Lagrange(obj, fedof) RESULT(ans)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(IN) :: fedof
    !! FEDOF
    INTEGER(I4B) :: ans
    !! ans
  END FUNCTION obj_GetTotalNodeNumH1Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalNodeNum@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-27
! summary: Get total node number for Hierarchical element
!
!# Introduction
!
! This method returns the total number of nodes for Hierarchical element
!
! It performs the follownig steps:
!
! 1. It calls GetTotalNodeNum on boundary (This is vertex nodes)
! 2. Then it calls SetElemToLocalBoundary
! 3. Start a loop for tElemToFace, get the localCellNum and localFaceNum from
!    elemToFace, and call fedof%GetTotalFaceDOF
! 4. Start a loop for tElemToEdge, get the localCellNum and localEdgeNum from
!    elemToEdge,

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeNumH1Hierarchical(obj, fedof) RESULT(ans)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(IN) :: fedof
    !! FEDOF
    INTEGER(I4B) :: ans
    !! ans
  END FUNCTION obj_GetTotalNodeNumH1Hierarchical
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
                                nodalValueType, isUseExternal, isUserFunction)
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
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isUseExternal
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
    !! constant nodal value
    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceNodalValue(:)
    !! space nodal value
    !! size should be same as the number of boundary nodes

    REAL(DFP), OPTIONAL, INTENT(IN) :: timeNodalValue(:)
    !! time nodal value
    !! size is total number of time nodes

    REAL(DFP), OPTIONAL, INTENT(IN) :: spaceTimeNodalValue(:, :)
    !! space time nodal value
    !! rowsize is total number of boundary nodes
    !! colsize is total number of time nodes

    TYPE(UserFunction_), TARGET, OPTIONAL, INTENT(IN) :: userFunction
    !! user function
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                           SetElemToLocalBoundary@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-19
! summary:  Set the element to local boundary data

INTERFACE
  MODULE SUBROUTINE obj_SetElemToLocalBoundary(obj)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetElemToLocalBoundary
END INTERFACE

END MODULE AbstractBC_Class
