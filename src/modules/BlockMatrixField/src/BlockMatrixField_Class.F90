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
!
!@todo
! Add getting-started manual
!@endtodo

MODULE BlockMatrixField_Class
USE GlobalData
USE BaseType
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

!----------------------------------------------------------------------------
!                                                          BlockMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.
!
!{!pages/BlockMatrixField_.md!}

TYPE, EXTENDS(MatrixField_) :: BlockMatrixField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & bmField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => mField_Initiate1
      !! Initiate from the parameter list
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => mField_Initiate2
      !! Initiate by copying other object
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => mField_Initiate3
      !! Initiate for block matrices
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => mField_Deallocate
      !! Deallocate the field
  FINAL :: mField_Final
      !! Finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => mField_Import
      !! Import from hdf5 file
  PROCEDURE, PASS(obj) :: set1 => mField_set1
  PROCEDURE, PASS(obj) :: set2 => mField_set2
  PROCEDURE, PASS(obj) :: set3 => mField_set3
  PROCEDURE, PASS(obj) :: set4 => mField_set4
  PROCEDURE, PASS(obj) :: set5 => mField_set5
  PROCEDURE, PASS(obj) :: set6 => mField_set6
  PROCEDURE, PASS(obj) :: set7 => mField_set7
  PROCEDURE, PASS(obj) :: set8 => mField_set8
  PROCEDURE, PASS(obj) :: set9 => mField_set9
  PROCEDURE, PASS(obj) :: set10 => mField_set10
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: getRow1 => mField_getRow1
  PROCEDURE, PUBLIC, PASS(obj) :: getRow2 => mField_getRow2
  PROCEDURE, PUBLIC, PASS(obj) :: getRow3 => mField_getRow3
  PROCEDURE, PUBLIC, PASS(obj) :: getRow4 => mField_getRow4
  PROCEDURE, PUBLIC, PASS(obj) :: getRow5 => mField_getRow5
  PROCEDURE, PUBLIC, PASS(obj) :: getRow6 => mField_getRow6
  PROCEDURE, PUBLIC, PASS(obj) :: getRow7 => mField_getRow7
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn1 => mField_getColumn1
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn2 => mField_getColumn2
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn3 => mField_getColumn3
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn4 => mField_getColumn4
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn5 => mField_getColumn5
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn6 => mField_getColumn6
  PROCEDURE, PUBLIC, PASS(obj) :: getColumn7 => mField_getColumn7
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: setRow1 => mField_setRow1
  PROCEDURE, PUBLIC, PASS(obj) :: setRow2 => mField_setRow2
  PROCEDURE, PUBLIC, PASS(obj) :: setRow3 => mField_setRow3
  PROCEDURE, PUBLIC, PASS(obj) :: setRow4 => mField_setRow4
  PROCEDURE, PUBLIC, PASS(obj) :: setRow5 => mField_setRow5
  PROCEDURE, PUBLIC, PASS(obj) :: setRow6 => mField_setRow6
  PROCEDURE, PUBLIC, PASS(obj) :: setRow7 => mField_setRow7
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn1 => mField_setColumn1
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn2 => mField_setColumn2
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn3 => mField_setColumn3
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn4 => mField_setColumn4
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn5 => mField_setColumn5
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn6 => mField_setColumn6
  PROCEDURE, PUBLIC, PASS(obj) :: setColumn7 => mField_setColumn7
    !!
END TYPE BlockMatrixField_

PUBLIC :: BlockMatrixField_

TYPE(BlockMatrixField_), PARAMETER, PUBLIC :: TypeBlockMatrixField = &
  & BlockMatrixField_(domains=NULL())

!----------------------------------------------------------------------------
!                                setBlockMatrixFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine sets the parameter for creating [[BlockMatrixField_]]
!
!# Introduction
!
! - This routine sets the essential parameter for initiating
! an instance of [[BlockMatrixField_]]
! - The results will be returned in `param`
! - After getting `param` user can call [[BlockMatrixField_:Initiate]] method
!
!@note
! The size of `physicalVarNames`, `spaceCompo`, and `timeCompo` should be
! the same.
!@endnote

INTERFACE
  MODULE SUBROUTINE setBlockMatrixFieldParam(param, name, matrixProp, &
    & physicalVarNames, spaceCompo, timeCompo, fieldType)
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL <-- DEFAULT
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
  END SUBROUTINE setBlockMatrixFieldParam
END INTERFACE

PUBLIC :: setBlockMatrixFieldParam

!----------------------------------------------------------------------------
!                           SetBlockMatrixFieldPrecondParam@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for precondition of BlockMatrixField_

INTERFACE
  MODULE SUBROUTINE SetBlockMatrixFieldPrecondParam(param, name, &
    & lfil, mbloc, droptol, permtol, alpha)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create precondition of [[BlockMatrixField_]]
    INTEGER(I4B), INTENT(IN) :: name
    !! Name of precondition
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: lfil
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: mbloc
    REAL(DFP), OPTIONAL, INTENT(IN) :: droptol
    !! Droptoleranace
    REAL(DFP), OPTIONAL, INTENT(IN) :: permtol
    !! permutation tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  END SUBROUTINE SetBlockMatrixFieldPrecondParam
END INTERFACE

PUBLIC :: SetBlockMatrixFieldPrecondParam

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
  MODULE SUBROUTINE bmField_checkEssentialParam(obj, param)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bmField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine deallocates the data stored inside the matrix

INTERFACE
  MODULE SUBROUTINE mField_Deallocate(obj)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE mField_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE mField_Final(obj)
    TYPE(BlockMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE mField_Final
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
! In addition, [[Domain_]] `dom` is target to the pointer
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

INTERFACE
  MODULE SUBROUTINE mField_Initiate1(obj, param, dom)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE mField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates the `obj` [[BlockMatrixField_]] by copying contents
! from `obj2`, an instance of chid class of [[AbstractField_]].
! In this way we try to minimize the computation effort.
!
!@note
! If `copyFull, copyStructure, usePointer` are absent then this subroutine,
! copies the value of the matrix from obj2 to obj.
!@endnote
!
!@note
! However, in [[BlockMatrixField_:mat]], it will not allocate space for
! [[CSRSparsity_]] field of
! [[CSRMatrix_]], that is [[CSRMatrix_:CSR]] field of
! [[BlockMatrixField_:mat]].
! Instead, it will use the obj2%mat%csr as the target for the pointer
! obj%mat%csr.
! In this way, there is no need to create multiple sparsity patterns
! for the same domain.
!@endnote
!
!@todo
! At present, the routine works for `copyFull=.TRUE., copyStructure=.TRUE.,
! usePointer=.TRUE.`, which equivalent to the default behavior.
! Add functionality for other options too.
!@endtodo

INTERFACE
  MODULE SUBROUTINE mField_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE mField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field

INTERFACE
  MODULE SUBROUTINE mField_Initiate3(obj, param, dom)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE mField_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE mField_Import(obj, hdf5, group, dom, domains)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE mField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine is not callable for BlockMatrixField

INTERFACE
  MODULE SUBROUTINE mField_set1(obj, globalNode, VALUE, storageFMT, scale,  &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
!
! If globalNode is present then this routine is not callable
!

INTERFACE
  MODULE SUBROUTINE mField_set2(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine is not callable for block matrix field

INTERFACE
  MODULE SUBROUTINE mField_set3(obj, iNodeNum, jNodeNum, idof, &
    & jdof, VALUE, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
!
! See [[CSRMatrix_Method:Set6]]
! See [[CSRMatrix_Method:Add6]]

INTERFACE
  MODULE SUBROUTINE mField_set4(obj, iNodeNum, jNodeNum, ivar, &
    & jvar, VALUE, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
!
! See [[CSRMatrix_Method:Set6]]
! See [[CSRMatrix_Method:Add6]]

INTERFACE
  MODULE SUBROUTINE mField_set5(obj, iNodeNum, jNodeNum, ivar, &
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
  END SUBROUTINE mField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine sets a scalar value `value` to a single entry of the matrix.
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
  MODULE SUBROUTINE mField_set6(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE mField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine sets a scalar value `value` to a single entry of the matrix.
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
  MODULE SUBROUTINE mField_set7(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE mField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine sets a scalar value `value` to a single entry of the matrix.
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
  MODULE SUBROUTINE mField_set8(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE mField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine sets a scalar value `value` to a single entry of the matrix.
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
  MODULE SUBROUTINE mField_set9(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE mField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine sets a scalar value `value` to a single entry of the matrix.
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
  MODULE SUBROUTINE mField_set10(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE mField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow3(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow4(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow5(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow6(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setRow7(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn1(obj, globalNode, idof, scalarVal, &
    & vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn3(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set
! to this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn4(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn5(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn6(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_setColumn7(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(BlockMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_setColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow1(obj, globalNode, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_getRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_getRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow3(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE mField_getRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow4(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE mField_getRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow5(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE mField_getRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow6(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE mField_getRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
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
  MODULE SUBROUTINE mField_getRow7(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE mField_getRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn1(obj, globalNode, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_getColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(BlockMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_getColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn3(obj, globalNode, ivar, spaceCompo, &
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
  END SUBROUTINE mField_getColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn4(obj, globalNode, ivar, spaceCompo, &
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
  END SUBROUTINE mField_getColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn5(obj, globalNode, ivar, spaceCompo, &
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
  END SUBROUTINE mField_getColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn6(obj, globalNode, ivar, spaceCompo, &
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
  END SUBROUTINE mField_getColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
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
  MODULE SUBROUTINE mField_getColumn7(obj, globalNode, ivar, spaceCompo, &
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
  END SUBROUTINE mField_getColumn7
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockMatrixField_Class
