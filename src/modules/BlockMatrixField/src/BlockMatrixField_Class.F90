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
! date: 15 July 2021
! summary: This module defines [[BlockMatrixField_]] class
!
!# Introduction
!
! - This module defines [[BlockMatrixField_]] class
! - It is designed for handling the block tangent matrix in FEM
!
!@note
! [[BlockMatrixField_]] uses `NATIVE_SERIAL` engine for handling the
! global tangent matrices.
!@endnote

MODULE BlockMatrixField_Class
USE GlobalData
USE BaSetype
USE FPL, ONLY: ParameterList_
USE FPL_Method
USE HDF5File_Class
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
USE MatrixField_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "BlockMatrixField_Class"
CHARACTER(*), PARAMETER :: myPrefix = "BlockMatrixField"
PUBLIC :: BlockMatrixField_
PUBLIC :: TypeBlockMatrixField
PUBLIC :: SetBlockMatrixFieldParam
PUBLIC :: SetBlockMatrixFieldPrecondParam
PUBLIC :: BlockMatrixFieldInitiate1
PUBLIC :: BlockMatrixFieldInitiate3

!----------------------------------------------------------------------------
!                                                          BlockMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.

TYPE, EXTENDS(MatrixField_) :: BlockMatrixField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & obj_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate from the parameter list
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate for block matrices
  FINAL :: obj_Final
  !! Finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from hdf5 file
  !
  ! @SetMethods
  !
  PROCEDURE, PASS(obj) :: Set1 => obj_Set1
  PROCEDURE, PASS(obj) :: Set2 => obj_Set2
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
  PROCEDURE, PASS(obj) :: Set5 => obj_Set5
  PROCEDURE, PASS(obj) :: Set6 => obj_Set6
  PROCEDURE, PASS(obj) :: Set7 => obj_Set7
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
  !
  ! @SetColumnMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn1 => obj_SetColumn1
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn2 => obj_SetColumn2
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn3 => obj_SetColumn3
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn4 => obj_SetColumn4
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn5 => obj_SetColumn5
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn6 => obj_SetColumn6
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn7 => obj_SetColumn7
  !
  ! @SetRowMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow1 => obj_SetRow1
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow2 => obj_SetRow2
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow3 => obj_SetRow3
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow4 => obj_SetRow4
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow5 => obj_SetRow5
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow6 => obj_SetRow6
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow7 => obj_SetRow7
  !
  ! @GetRowMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow1 => obj_GetRow1
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow2 => obj_GetRow2
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow3 => obj_GetRow3
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow4 => obj_GetRow4
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow5 => obj_GetRow5
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow6 => obj_GetRow6
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow7 => obj_GetRow7
  !
  ! @GetColumnMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn1 => obj_GetColumn1
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn2 => obj_GetColumn2
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn3 => obj_GetColumn3
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn4 => obj_GetColumn4
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn5 => obj_GetColumn5
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn6 => obj_GetColumn6
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn7 => obj_GetColumn7
END TYPE BlockMatrixField_

TYPE(BlockMatrixField_), PARAMETER :: TypeBlockMatrixField = &
  & BlockMatrixField_(domains=NULL())

!----------------------------------------------------------------------------
!                                SetBlockMatrixFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for creating [[BlockMatrixField_]]
!
!# Introduction
!
! - This routine Sets the essential parameter for initiating
! an instance of [[BlockMatrixField_]]
! - The results will be returned in `param`
! - After Getting `param` user can call [[BlockMatrixField_:Initiate]] method
!
!@note
! The size of `physicalVarNames`, `spaceCompo`, and `timeCompo` should be
! the same.
!@endnote

INTERFACE
  MODULE SUBROUTINE SetBlockMatrixFieldParam(param, name, matrixProp, &
    & physicalVarNames, spaceCompo, timeCompo, engine, fieldType, &
    & comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create [[BlockMatrixField_]] will be stored in this
    CHARACTER(*), INTENT(IN) :: name
    !! Name of the matrix field
    CHARACTER(*), INTENT(IN) :: matrixProp
    !! Matrix property, "SYM" or "UNSYM"
    CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
    !! Name of physical variables
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! Number of space-components in each physicalVarNames, see [[DOF_]]
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! Number of time-components in each physicalVarNames, see [[DOF_]]
    CHARACTER(*), INTENT(IN) :: engine
    !! engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL <-- DEFAULT
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetBlockMatrixFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                       SetBlockMatrixFieldPrecondParam@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for precondition of BlockMatrixField_

INTERFACE
  MODULE SUBROUTINE SetBlockMatrixFieldPrecondParam(param, name, engine, &
    & lfil, mbloc, droptol, permtol, alpha, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create precondition of [[BlockMatrixField_]]
    INTEGER(I4B), INTENT(IN) :: name
    !! Name of precondition
    CHARACTER(*), INTENT(IN) :: engine
    !! engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: lfil
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: mbloc
    REAL(DFP), OPTIONAL, INTENT(IN) :: droptol
    !! Droptoleranace
    REAL(DFP), OPTIONAL, INTENT(IN) :: permtol
    !! permutation tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetBlockMatrixFieldPrecondParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
!
! This routine check the essential parameters required to the initiate the
! [[BlockMatrixField_]] data type.

INTERFACE
  MODULE SUBROUTINE obj_checkEssentialParam(obj, param)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates an instance of [[BlockMatrixField_]].
! The options/arguments to initiate the matrix field are
! contained inside param, which is an instance of [[ParameterList_]].
! In addition, [[Domain_]] `dom` is tarGet to the pointer
! [[AbstractField_:domain]] and [[AbstractField_::domains]]
!
! - `param` contains both essential and optional parameters which are used in
! constructing the matrix field
! - `dom` is a pointer to a domain
!
! ESSENTIAL PARAMETERS are
!
! - `name` This is name of field (char)
! - `matrixProp`, UNSYM, SYM (char)
!
! OPTIONAL PARAMETERS
!
! - `spaceCompo`, INT, default is 1
! - `timeCompo`, INT, default is 1
! - `fieldType`, INT, default is FIELD_TYPE_NORMAL

INTERFACE BlockMatrixFieldInitiate1
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE BlockMatrixFieldInitiate1

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field

INTERFACE BlockMatrixFieldInitiate3
  MODULE SUBROUTINE obj_Initiate3(obj, param, dom)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE obj_Initiate3
END INTERFACE BlockMatrixFieldInitiate3

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine is not callable for BlockMatrixField

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, VALUE, storageFMT, scale,  &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! If globalNode is present then this routine is not callable
!

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine is not callable for block matrix field

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, iNodeNum, jNodeNum, idof, &
    & jdof, VALUE, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! See [[CSRMatrix_Method:Set6]]
! See [[CSRMatrix_Method:Add6]]

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, iNodeNum, jNodeNum, ivar, &
    & jvar, VALUE, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! See [[CSRMatrix_Method:Set6]]
! See [[CSRMatrix_Method:Add6]]

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, iNodeNum, jNodeNum, ivar, &
    & jvar, idof, jdof, VALUE, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & idof, jdof, VALUE, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo(:)
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo(:)
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo(:)
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine is not callable for BlockMatrixField

INTERFACE
  MODULE SUBROUTINE obj_SetRow1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetRow2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetRow3(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetRow4(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetRow5(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetRow6(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetRow7(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine is not callable for BlockMatrixField_

INTERFACE
  MODULE SUBROUTINE obj_SetColumn1(obj, globalNode, idof, scalarVal, &
    & vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetColumn2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetColumn3(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set
! to this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetColumn4(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetColumn5(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetColumn6(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE obj_SetColumn7(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine is not callable for BlockMatrixField_

INTERFACE
  MODULE SUBROUTINE obj_GetRow1(obj, globalNode, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE obj_GetRow2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE obj_GetRow3(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE obj_GetRow4(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE obj_GetRow5(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE obj_GetRow6(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE obj_GetRow7(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine is not callable for BlockMatrixField_

INTERFACE
  MODULE SUBROUTINE obj_GetColumn1(obj, globalNode, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

INTERFACE
  MODULE SUBROUTINE obj_GetColumn2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

INTERFACE
  MODULE SUBROUTINE obj_GetColumn3(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

INTERFACE
  MODULE SUBROUTINE obj_GetColumn4(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

INTERFACE
  MODULE SUBROUTINE obj_GetColumn5(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

INTERFACE
  MODULE SUBROUTINE obj_GetColumn6(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

INTERFACE
  MODULE SUBROUTINE obj_GetColumn7(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockMatrixField_Class
