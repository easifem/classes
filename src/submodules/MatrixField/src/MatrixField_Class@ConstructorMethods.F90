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
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) ConstructorMethods
USE AbstractField_Class, ONLY: AbstractFieldInitiate
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldDeallocate
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldSetFEDOF
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: TypePrecondOpt
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE CSRMatrix_Method, ONLY: ASSIGNMENT(=)
USE CSRMatrix_Method, ONLY: CSRMatrix_Deallocate => DEALLOCATE
USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: DOF_Deallocate => DEALLOCATE
USE DOF_Method, ONLY: DOF_Initiate => Initiate
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE FEDOF_Class, ONLY: FEDOFSetSparsity
USE InputUtility, ONLY: INPUT
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE String_Class, ONLY: String
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MatrixField_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

CHARACTER(*), PARAMETER :: matrixProp = "UNSYM"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%IsInitiated()) THEN
  CALL obj%DEALLOCATE()
END IF

#ifdef DEBUG_VER
isok = obj2%IsInitiated()
CALL AssertError1(isok, myName, "obj2 is not initiated")
#endif

CALL AbstractFieldInitiate(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, &
                           usePointer=usePointer)

SELECT TYPE (obj2)
CLASS IS (AbstractNodeField_)
  ! Get nrow and ncol from obj2
  nrow = obj2%SIZE()
  ncol = nrow

  ! Initiate CSRMatrix
  CALL CSRMatrix_Initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=obj2%dof, &
                          jdof=obj2%dof, matrixProp=matrixProp)

  isok = obj%local_n .EQ. 0
  IF (isok) obj%local_n = nrow

  isok = obj%global_n .EQ. 0
  IF (isok) obj%global_n = nrow

  ! square matrix: the columns use the FEDOF of the rows
  CALL AbstractMatrixFieldSetFEDOF(obj=obj, rowFedof=obj2%fedof, &
                                   rowFedofs=obj2%fedofs, &
                                   rowGeoFedof=obj2%geofedof, &
                                   rowGeoFedofs=obj2%geofedofs, &
                                   colFedofs=obj2%fedofs, &
                                   colGeoFedof=obj2%geofedof, &
                                   colGeoFedofs=obj2%geofedofs)

  CALL obj%rowFedof%SetSparsity(mat=obj%mat)

CLASS IS (MatrixField_)
  obj%mat = obj2%mat
  obj%submat = obj2%submat
  obj%isPmatInitiated = obj2%isPMatInitiated
  obj%isRectangle = obj2%isRectangle

  CALL AbstractMatrixFieldSetFEDOF( &
    obj=obj, rowFedof=obj2%rowFedof, rowFedofs=obj2%rowFedofs, &
    rowGeoFedof=obj2%rowGeoFedof, rowGeoFedofs=obj2%rowGeoFedofs, &
    colFedof=obj2%colFedof, colFedofs=obj2%colFedofs, &
    colGeoFedof=obj2%colGeoFedof, colGeoFedofs=obj2%colGeoFedofs)

  CALL MatrixFieldPreconditionCopy(obj=obj%pmat, obj2=obj2%pmat)

  tsize = SafeSize(obj2%dbcPtrs)
  CALL Reallocate(obj%dbcPtrs, tsize)
  DO ii = 1, tsize
    obj%dbcPtrs(ii) = obj2%dbcPtrs(ii)
  END DO

  tsize = SafeSize(obj2%subIndices)
  CALL Reallocate(obj%subIndices, tsize)
  DO ii = 1, tsize
    obj%subIndices(ii) = obj2%subIndices(ii)
  END DO

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
USE BlockNodeField_Class, ONLY: BlockNodeField_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif
CHARACTER(*), PARAMETER :: matrixProp = "UNSYM"
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%IsInitiated()) THEN
  CALL obj%DEALLOCATE()
END IF

#ifdef DEBUG_VER
isok = rowObj%IsInitiated()
CALL AssertError1(isok, myName, "rowObj is not initiated")
#endif

#ifdef DEBUG_VER
isok = colObj%IsInitiated()
CALL AssertError1(isok, myName, "colObj is not initiated")
#endif

#ifdef DEBUG_VER
isok = rowObj%GetEngineName() .EQ. colObj%GetEngineName()
CALL AssertError1(isok, myName, &
                  "rowObj and colObj do not engine name")
#endif

#ifdef DEBUG_VER
SELECT TYPE (rowObj)
CLASS IS (BlockNodeField_)
  CALL AssertError1(math%no, myName, &
                    "This method should not be called for blockNodeField_")
END SELECT
#endif

#ifdef DEBUG_VER
SELECT TYPE (colObj)
CLASS IS (BlockNodeField_)
  CALL AssertError1(math%no, myName, &
                    "This method should not be called for blockNodeField_")
END SELECT
#endif

CALL AbstractFieldInitiate(obj=obj, obj2=colObj, copyFull=math%yes)

nrow = rowObj%SIZE()
ncol = colObj%SIZE()

CALL CSRMatrix_Initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=rowObj%dof, &
                        jdof=colObj%dof, matrixProp=matrixProp)

isok = obj%local_n .EQ. 0
IF (isok) obj%local_n = nrow

isok = obj%global_n .EQ. 0
IF (isok) obj%global_n = nrow

! set fedofs
CALL AbstractMatrixFieldSetFEDOF(obj=obj, rowFedof=rowObj%fedof, &
                                 rowGeoFedof=rowObj%geofedof, &
                                 colFedof=colObj%fedof, &
                                 colGeoFedof=colObj%geofedof)

! Set sparsity
CALL obj%rowFedof%SetSparsity(mat=obj%mat, col_fedof=obj%colFedof, &
                              ivar=math%one_i, &
                              jvar=math%one_i)

! Set final sparsity
CALL CSRMatrix_SetSparsity(obj=obj%mat)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                               MatrixFieldPreconditionCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldPreconditionCopy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "MatrixFieldPreconditionCopy()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInitiated = obj2%isInitiated
obj%PmatName = obj2%PmatName
obj%nnz = obj2%nnz
obj%ncol = obj2%ncol
obj%nrow = obj2%nrow
obj%lfil = obj2%lfil
obj%mbloc = obj2%mbloc
obj%alpha = obj2%alpha
obj%droptol = obj2%droptol
obj%permtol = obj2%permtol

tsize = SafeSize(obj2%A)
CALL Reallocate(obj%A, tsize)
DO ii = 1, tsize
  obj%A(ii) = obj2%A(ii)
END DO

tsize = SafeSize(obj2%JA)
CALL Reallocate(obj%JA, tsize)
DO ii = 1, tsize
  obj%JA(ii) = obj2%JA(ii)
END DO

tsize = SafeSize(obj2%IA)
CALL Reallocate(obj%IA, tsize)
DO ii = 1, tsize
  obj%IA(ii) = obj2%IA(ii)
END DO

tsize = SafeSize(obj2%JU)
CALL Reallocate(obj%JU, tsize)
DO ii = 1, tsize
  obj%JU(ii) = obj2%JU(ii)
END DO

tsize = SafeSize(obj2%IPERM)
CALL Reallocate(obj%IPERM, tsize)
DO ii = 1, tsize
  obj%IPERM(ii) = obj2%IPERM(ii)
END DO

tsize = SafeSize(obj2%LEVS)
CALL Reallocate(obj%LEVS, tsize)
DO ii = 1, tsize
  obj%LEVS(ii) = obj2%LEVS(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE MatrixFieldPreconditionCopy

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMatrixFieldDeallocate(obj)
CALL CSRMatrix_Deallocate(obj%mat)
CALL CSRMatrix_Deallocate(obj%submat)
CALL Pmat_Deallocate(obj%pmat)
obj%isRectangle = .FALSE.
obj%tdbcptrs = 0
obj%tsubindices = 0
IF (ALLOCATED(obj%dbcPtrs)) DEALLOCATE (obj%dbcPtrs)
IF (ALLOCATED(obj%subIndices)) DEALLOCATE (obj%subIndices)
obj%isSubmatInit = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Pmat_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Pmat_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%PmatName = 0
IF (ALLOCATED(obj%A)) DEALLOCATE (obj%A)
IF (ALLOCATED(obj%JA)) DEALLOCATE (obj%JA)
IF (ALLOCATED(obj%IA)) DEALLOCATE (obj%IA)
IF (ALLOCATED(obj%JU)) DEALLOCATE (obj%JU)
IF (ALLOCATED(obj%IPERM)) DEALLOCATE (obj%IPERM)
IF (ALLOCATED(obj%LEVS)) DEALLOCATE (obj%LEVS)
obj%nnz = 0
obj%ncol = 0
obj%nrow = 0
obj%isInitiated = .FALSE.
obj%lfil = 0
obj%mbloc = 0
obj%alpha = 0
obj%droptol = 0
obj%permtol = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Pmat_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_ptr_vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_ptr_vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate_ptr_vector

!----------------------------------------------------------------------------
!                                                               SafeAllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MatrixFieldAllocate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MatrixFieldAllocate1()"
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj)

IF (.NOT. isok) THEN
  ALLOCATE (obj(newsize))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

tsize = SIZE(obj)

isok = tsize .LT. newsize
IF (isok) THEN
  CALL MatrixFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MatrixFieldAllocate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
