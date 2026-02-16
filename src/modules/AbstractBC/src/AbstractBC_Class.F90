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
!> author: Vikas Sharma, Ph. D.
! date: 2026-02-15
! summary: This module defines abstract data type for boundary conditions
!
!# AbstractBC
!
! This module defines AbstractBC class, an abstract class for handling
! boundary condition.
!
! `AbstractBC` class handles the boundary condition in finite element
!  methods. Currently, it has following subclasses.
!
! - [DirichletBC](../DirichletBC)
! - [NeumannBC](../NeumannBC)
! - [NitscheBC](../NitscheBC)

MODULE AbstractBC_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE HDF5File_Class, ONLY: HDF5File_
USE UserFunction_Class, ONLY: UserFunction_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE FEDOF_Class, ONLY: FEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractBC_Class"
#endif

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
PUBLIC :: TypeAbstractBCOpt
PUBLIC :: AbstractBCDeallocate
PUBLIC :: AbstractBCInitiate
PUBLIC :: AbstractBCImportFromToml

!----------------------------------------------------------------------------
!                                                              AbstractBCOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-11
! summary: Options for AbstractBC

TYPE :: AbstractBCOpt_
  CHARACTER(10) :: name = "AbstractBC"
  INTEGER(I4B) :: idof = math%one_i
  INTEGER(I4B) :: nodalValueType = TypeFEVariableOpt%constant
  CHARACTER(4) :: nodalValueType_char = "NONE"
  LOGICAL(LGT) :: isUserFunction = math%no
  LOGICAL(LGT) :: isNormal = math%no
  LOGICAL(LGT) :: isTangent = math%no
  LOGICAL(LGT) :: isUseExternal = math%no
END TYPE AbstractBCOpt_

!----------------------------------------------------------------------------
!                                                           TypeAbstractBCOpt
!----------------------------------------------------------------------------

TYPE(AbstractBCOpt_), PARAMETER :: TypeAbstractBCOpt = AbstractBCOpt_()

!----------------------------------------------------------------------------
!                                                                AbstractBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions
!
!# AbstractBC_
!
! This is an abstract data type for boundary condition.

TYPE, ABSTRACT :: AbstractBC_
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is true if the object is initiated
  LOGICAL(LGT) :: isNormal = TypeAbstractBCOpt%isNormal
  !! True if the boundary condition is normal to the boundary
  LOGICAL(LGT) :: isTangent = TypeAbstractBCOpt%isTangent
  !! True if the boundary condition is tangent to the boundary
  LOGICAL(LGT) :: isUseExternal = TypeAbstractBCOpt%isUseExternal
  !! if true then nodal values are used externally
  !! depending upon the context.
  !! Basically we do not use the nodal value stored in the
  !! instance of AbstractBC_
  LOGICAL(LGT) :: isUserFunction = default_isUserFunction
  !! True if userFunction is set
  LOGICAL(LGT) :: isElemToFace = math%no
  !! When elemToFace is set then isElemToFace is true
  LOGICAL(LGT) :: isElemToEdge = math%no
  !! When elemToEdge is set then isElemToEdge is true
  TYPE(String) :: name
  !! name of boundary condition
  INTEGER(I4B) :: idof = TypeAbstractBCOpt%idof
  !! degree of freedom number
  INTEGER(I4B) :: nodalValueType = TypeAbstractBCOpt%nodalValueType
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
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate memory occupied by AbstractBC
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate an instance of AbstractBC with arguments

  ! IO:
  ! @HDFMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from HDF5File
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to HDF5File

  ! IO:
  ! @IOMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display content of AbstractBC

  ! IO:
  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Initiate from toml
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Initiate from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import abstract kernel from toml
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ImportConstBCFromToml => &
    obj_ImportConstBCFromToml
  !! Import constant boundary condition from toml

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Set the boundary condition value
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    SetElemToLocalBoundary => obj_SetElemToLocalBoundary

  ! GET:
  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! Returns isInit
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsElemToFaceInitiated => &
    obj_IsElemToFaceInitiated
  !! Returns isElemToFace
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsElemToEdgeInitiated => &
    obj_IsElemToEdgeInitiated
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalElemToFace => &
    obj_GetTotalElemToFace
  !! Get total elemToFace
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalElemToEdge => &
    obj_GetTotalElemToEdge
  !! Get total elemToEdge
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetElemToFace => &
    obj_GetElemToFace
  !! Get elemToFace
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetElemToEdge => &
    obj_GetElemToEdge
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetMeshID => &
    obj_GetMeshID
  !! Get MeshID
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetMeshIDPointer => &
    obj_GetMeshIDPointer
  !! Get mesh id pointer
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalNodeNum => &
    obj_GetTotalNodeNum
  !! Get total node number
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetDOFNo => obj_GetDOFNo
  !! Get degree of freedom number
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters of AbstractBC
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsUseFunction => &
    obj_IsUseFunction
  !! Returns true if the useFunction is true

  ! GET:
  ! @GetValueMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get the node number and nodal value of the boundary conditions
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get the node number and nodal value of the boundary conditions
  GENERIC, PUBLIC :: Get => Get1, Get2

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetNodeNumber => obj_GetNodeNumber
  !! Get the degree of freedom number on boundary

  !! @NBCMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetNBCValue => obj_GetNBCValue
  !! Get the Neumann boundary condition value at a given nodes
END TYPE AbstractBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-15
! summary: A data type to contain the pointer to AbstractBC

TYPE :: AbstractBCPointer_
  CLASS(AbstractBC_), POINTER :: ptr => NULL()
END TYPE AbstractBCPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary:  Initiate AbstractBC with arguments
!
!# Initiate
!
! This method contstructs an instance of AbstractBC.
!

INTERFACE AbstractBCInitiate
  MODULE SUBROUTINE obj_Initiate( &
    obj, boundary, dom, name, idof, nodalValueType, isNormal, isTangent, &
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
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractBCInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Deallocate data stored in AbstractBC.
!
!# Deallocate
!
! Deallocate the data stored in AbstractBC.

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
!
!# Import
!
! Import the data for AbstractBC from hdf file.

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5File which will be used for importing data
    CHARACTER(*), INTENT(IN) :: group
    !! group where the data is kept
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! pointer to AbstractDomain_
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
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate an instance from toml table
!
!# ImportFromToml
!
! This method initiates an instance of AbstractBC from toml table.
!
! ``` toml
! [bc]
! name = "DirichletBC"
! # Name of the boundary condition
! idof = 1
! # idof denotes the degree of freedom where
! # we apply the contraint,
! # For x component idof is 1
! # For y component idof is 2
! # For z component idof is 3
! nodalValueType = "Constant" # Time, SpaceTime, Space
! # Other option: Constant, Time, Space,  SpaceTime
! # nodalValuetype denotes the type of boundary condition
! # It can take following values
! # Constant: It means the boundary condition is constant in
! #           space and time.
! # Space:    It means the boundary condition is variable in
! #           space but constant in time.
! # Time:     It means the boundary condition is variable in
! #           in time but constant in Space
! # SpaceTime:It means the boundary condition is variable in
! #           both space and time.
! value = 20.0
! # Value of boundary condition
! # The shape of value depends upon the nodalValueType as explained below.
! # For nodalValueType="Constant", value should be a scalar real value
! # For nodalValueType="Space", value should be a vector of real values
! # value=[1.0, 2.0, 3.0, 4.0]
! # For nodalValueType="Time", value should  be a vector of real values
! # value=[1.0, 2.0, 3.0, 4.0]
! # For nodalValueType="SpaceTime", value should be two dimensional array of
! # real values
! # value=[[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0]]
! isUserFunction = false
! isNormal = false
! isTangent = false
! useExternal = false
! [bc.function]
! name = "boundaryFunction"
! # name of the function
! returnType = "Scalar"
! # returnType must be "Scalar"
! numReturns = 1
! # number of returns must be 1
! argType = "Space"
! # argumen type of function, It can take following values
! # "Constant", when the user function is constant
! # "Space", when the user function is space dependent only.
! # "Time", when the user function is time dependent only.
! # "SpaceTime", when the user function is space-time dependent.
! numArgs = 3
! # number of arguments
! # number of arguments should be 0, when argType is "Constant"
! # number of arguments should be 1, when argType is "Time"
! # number of arguments should be 3, when argType is "Space"
! # number of arguments should be 4, when argType is "SpaceTime"
! value = 1.0
! # if lua script and luaFunctionName are absent then this
! # variable must be given.
! # When this is given, then argType must be Constant, and
! # numArgs should be 0.
! luaScript = "./hello.lua"
! # name of the lua script which contains the functions
! luaFunctionName = "hello"
! # which function in the lua script to be used for boundary condition
! [bc.boundary]
! isSelectionByMeshID = false
! # set isSelectionByMeshID to true when mesh selection is by meshID
! # if this variable is set to true then we should provide following
! # table
! [bc.boundary.meshID]
! point = [1, 2, 3]
! # id of mesh of points
! # you can also give the filename
! line = [1, 2, 3, 4]
! # id of mesh of lines
! # you can also give the filename
! surface = [1, 2, 3]
! # id of mesh of surfaces
! # you can also give the filename "filename.txt"
! volume = [1, 2, 3]
! # id of mesh of volumes
! # you can also give the filename "filename.txt"
! isSelectionByElemNum = false
! # set isSelectionByElemNum to true when mesh selection is by
! # specifying element number
! # if this variable is set to true then we should provide following
! # table
! [bc.boundary.elemNum]
! point = [1, 2, 3]
! # element number for points
! line = [1, 2, 3]
! # element number of lines
! surface = [1, 2, 3]
! # element number of surfaces
! volume = [1, 2, 3]
! # element number of volume
! isSelectionByBox = false
! # set isSelectionByBox to true when mesh selection is by
! # specifying the bounding boxes
! # if this variable is set to true then we should provide following
! # table
! [bc.boundary.box]
! point = [
!  {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 0.0,zmax = 1.0},
!  {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 2.0,zmax = 3.0},
! ]
! # boxes for mesh of points
! line = [
!   {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 0.0,zmax = 1.0},
!   {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 2.0,zmax = 3.0 },
! ]
! # boxes for mesh of lines
! surface = [
!   {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 0.0,zmax = 1.0},
!   {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 2.0,zmax = 3.0 },
! ]
! # boxes for mesh of surfaces
! volume = [
!   {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 0.0,zmax = 1.0},
!   {xmin = 0.0,xmax = 1.0,ymin = 0.0,ymax = 1.0,zmin = 2.0,zmax = 3.0 },
! ]
! # boxes for mesh of volumes
! isSelectionByNodeNum = false
! # set isSelectionByNodeNum to true when mesh selection is by
! # by specifying the node number.
! # if this variable is set to true then we should provide following
! # table
! [bc.boundary.nodeNum]
! point = [1, 2, 3]
! line = [1, 2, 3]
! surface = [1, 2, 3]
! volume = [1, 2, 3]
! ```

INTERFACE AbstractBCImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE AbstractBCImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate an instance from the toml file
!
!# ImportFromToml
!
! This methods initiates an instance of AbstractBC from toml file.

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
!                                            ImportConstBCFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Import constant boundary condition from toml table
!
!# AbstractBCImportConstBCFromToml
!
! This method is designed for ConstDirichletBC and ConstNeumannBC.
! These child classes require lesser data from the user.
! Also, they simplifies and optimizes the application of such BCs in FEM.

INTERFACE AbstractBCImportConstBCFromToml
  MODULE SUBROUTINE obj_ImportConstBCFromToml(obj, table, dom)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportConstBCFromToml
END INTERFACE AbstractBCImportConstBCFromToml

!----------------------------------------------------------------------------
!                                                        Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Display the content of AbstractBC
!
!# Display
!
! This method displays the content of AbstractBC.

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
!
!# IsInitiated
!
! This method returns IsInitiated.

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                            IsElemToEdgeInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary: Returns isElemToEdge
!
!# IsElemToEdgeInitiated
!
! Returns the status of isElemToEdge.

INTERFACE
  MODULE FUNCTION obj_IsElemToEdgeInitiated(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsElemToEdgeInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                            IsElemToFaceInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary: Returns isElemToFace
!
!# IsElemToFaceInitiated
!
! Returns the status of isElemToFace.

INTERFACE
  MODULE FUNCTION obj_IsElemToFaceInitiated(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsElemToFaceInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalElemToEdge@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary: Returns tElemToEdge
!
!# GetTotalElemToEdge
!
! This method returns tElemToEdge, total element to edge.

INTERFACE
  MODULE FUNCTION obj_GetTotalElemToEdge(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElemToEdge
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalElemToFace@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-26
! summary: Returns tElemToFace
!
!# GetTotalElemToFace
!
! Returns tElemToFace, total element to face.

INTERFACE
  MODULE FUNCTION obj_GetTotalElemToFace(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElemToFace
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemToFace@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-08
! summary:  Get data from ElemToFace
!
!# GetElemToFace
!
! Get data from elemToFace.

INTERFACE
  MODULE SUBROUTINE obj_GetElemToFace( &
    obj, indx, localCellNumber, localFaceNumber)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    !! Abstract boundary condition.
    INTEGER(I4B), INTENT(IN) :: indx
    !! indx
    INTEGER(I4B), INTENT(OUT) :: localCellNumber
    !! local cell number
    INTEGER(I4B), INTENT(OUT) :: localFaceNumber
    !! local face number
  END SUBROUTINE obj_GetElemToFace
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemToEdge@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-08
! summary:  Get data from ElemToEdge
!
!# GetElemToEdge
!
! Get data from ElemToEdge.

INTERFACE
  MODULE SUBROUTINE obj_GetElemToEdge( &
    obj, indx, localCellNumber, localEdgeNumber)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B), INTENT(OUT) :: localCellNumber
    !! local cell number
    INTEGER(I4B), INTENT(OUT) :: localEdgeNumber
    !! local edge number
  END SUBROUTINE obj_GetElemToEdge
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMeshID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-02-16
! summary: This routine returns MeshID
!
!# GetMeshID
!
! Get the mesh id.

INTERFACE
  MODULE FUNCTION obj_GetMeshID(obj, dim) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMeshIDPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-16
! summary: Get the pointer to MeshID
!
!# GetMeshIDPointer
!
! Get the pointer to MeshID.

INTERFACE
  MODULE SUBROUTINE obj_GetMeshIDPointer(obj, dim, ans, tsize)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), POINTER, INTENT(OUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetMeshIDPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Get the nodenum and nodalValue
!
!# Get
!
! This method calls GetH1Lagrange or GetH1Hierarchical methods

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, fedof, geofedof, nodeNum, nodalValue, &
                             nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    !! Degree of freedom for variable and geometry
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! Nodal values of boundary value
    !! nrow = size of nodeNum
    !! ncol = 1 or size of times
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in nodalValue
    REAL(DFP), OPTIONAL, INTENT(IN) :: times
    !! times vector is only used when usefunction is true in obj
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Get the nodenum and nodalValue
!
!# Get
!
! This method calls GetH1Lagrange or GetH1Hierarchical methods

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, fedof, geofedof, timefedof, nodeNum, &
                             nodalValue, nrow, ncol, times)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    !! Degree of freedom for variable and geometry
    CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
    !! Time degrees of freedom
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! Nodal values of boundary value
    !! nrow = size of nodeNum
    !! ncol = 1 or size of times
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in nodalValue
    REAL(DFP), INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj
    !! times length should be 2
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
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
!                                                       GetDOFNo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-12
! summary: Get degree of freedom number
!
!# GetDOFNo
!
! Get degree of freedom number.

INTERFACE
  MODULE PURE FUNCTION obj_GetDOFNo(obj) RESULT(ans)
    CLASS(AbstractBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetDOFNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get field values of abstract boundary condition
!
!# GetParam
!
! Get field values of abstract boundary condition.

INTERFACE
  MODULE PURE SUBROUTINE obj_GetParam( &
    obj, isInitiated, isSelectionByBox, isSelectionByMeshID, &
    isSelectionByElemNum, isSelectionByNodeNum, idof, isTangent, isNormal, &
    useFunction, nodalValueType, isUseExternal, isUserFunction, &
    isElemToFace, isElemToEdge)
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
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isElemToFace
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isElemToEdge
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsUseFunction@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Returns true if useFunction is true
!
!# IsUseFunction
!
! Returns the status of useFunction.

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
!
!# Set
!
! This method set fields of abstract boundary condition.

INTERFACE
  MODULE SUBROUTINE obj_Set( &
    obj, constantNodalValue, spaceNodalValue, &
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
! date: 2024-07-19
! summary: Set the element to local boundary data
!
!# GetElemToLocalBoundary
!
!This method sets the element to face and element to edge data.
!In particular it set the following data in an instance of `AbstractBC_`:
!
!- `elemToFace`
!- `elemToEdge`
!- `tElemToFace`
!- `tElemToEdge`
!
!## Purpose
!
!This procedure sets up the mapping data between boundary elements and
!their corresponding local cell elements in the domain.
!It identifies which cells are connected to boundary faces or edges,
!and stores this information in the `elemToFace` and `elemToEdge` arrays.
!
!## Description
!
!The method establishes two key mappings required for applying
!boundary conditions:
!
!1. **Element to Face Mapping**: For each boundary face element,
!identifies the corresponding cell element and local face number
!within that cell.
!
!2. **Element to Edge Mapping**: For 3D problems, identifies
!the mapping between boundary edge elements and the corresponding
!cell elements and local edge numbers.
!
!These mappings are essential for enforcing boundary conditions in
!finite element analysis, especially for hierarchical elements where
!conditions need to be applied to specific faces or edges of elements.

!## Implementation Details
!
!The procedure:
!
!1. Calls `set_elem_to_faces` to establish the element-to-face connectivity
!2. Calls `set_elem_to_edges` to establish the element-to-edge connectivity
!(for 3D problems)
!
! The implementation populates:
!
!- `obj%elemToFace`: A 2×N array where each column contains
![localCellNumber, localFaceNumber]
!- `obj%elemToEdge`: A 2×N array where each column contains
![localCellNumber, localEdgeNumber]
!
!The results are stored in the `AbstractBC_` object and can be
!accessed when applying boundary conditions.
!
!## Notes
!
!- This method is particularly important for hierarchical finite elements
!where boundary conditions must be applied to specific faces or edges.
!- For face elements, the method works in domains of dimension 2 or higher.
!- For edge elements, the method only works in 3D domains.
!- Once called, the object's `isElemToFace` and/or `isElemToEdge` flags
!are set to `.TRUE.`.

INTERFACE
  MODULE SUBROUTINE obj_SetElemToLocalBoundary(obj)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetElemToLocalBoundary
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNBCValue@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Get the nodenum and nodalValue
!
!# GetNBCValue
!
! This method calls GetH1Lagrange or GetH1Hierarchical methods

INTERFACE
  MODULE SUBROUTINE obj_GetNBCValue( &
    obj, mesh, fedof, geofedof, indx, nodeNum, nodalValue, nrow, ncol, &
    cellCon, tCellCon, geoCellCon, tGeoCellCon, geoFacetCon, tGeoFacetCon, &
    quad, facetQuad, elemsd, facetElemsd, geoElemsd, geoFacetElemsd, &
    xij, xij_i, xij_j, times)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    !! Mesh
    CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    !! Degree of freedom for variable and geometry
    INTEGER(I4B), INTENT(IN) :: indx
    !! Index of elemT
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%GetTotalNodeNum
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! Nodal values of boundary value
    !! nrow = size of nodeNum
    !! ncol = 1 or size of times
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in nodalValue
    INTEGER(I4B), INTENT(INOUT) :: cellCon(:), geoCellCon(:), geoFacetCon(:)
    !! cell connectivity for variable and geometry
    INTEGER(I4B), INTENT(OUT) :: tCellCon, tGeoCellCon, tGeoFacetCon
    !! total data written in cellCon and geoCellCon
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! quadrature for element and facet
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, facetElemsd, geoElemsd, &
                                           geoFacetElemsd
    !! Element shape data on cell and facet
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! xij for element
    INTEGER(I4B), INTENT(OUT) :: xij_i, xij_j
    !! size of data written in xij
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj
  END SUBROUTINE obj_GetNBCValue
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeNumber@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Get the nodeNum
!
!# GetNodeNumber
!
! This method returns the nodeNum and tsize
! It calls GetH1Lagrange or GetH1Hierarchical methods

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNumber( &
    obj, fedof, nodeNum, tsize, iNodeOnNode, iNodeOnFace, iNodeOnEdge)
    CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! finite element degree of freedom
    INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Total size of data written in nodeNum(:)
    INTEGER(I4B), INTENT(OUT) :: iNodeOnNode
    !! starting point of nodes on nodes
    INTEGER(I4B), INTENT(OUT) :: iNodeOnFace
    !! starting point of nodes on face
    INTEGER(I4B), INTENT(OUT) :: iNodeOnEdge
    !! starting point of nodes on edge
  END SUBROUTINE obj_GetNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractBC_Class
