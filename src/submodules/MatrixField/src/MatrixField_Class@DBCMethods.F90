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

SUBMODULE(MatrixField_Class) DBCMethods
USE ReallocateUtility, ONLY: Reallocate
USE CSRMatrix_Method, ONLY: CSRMatrix_GetSubMatrix => GetSubMatrix
USE CSRMatrix_Method, ONLY: CSRMatrix_Matvec => Matvec
USE CSRMatrix_Method, ONLY: CSRMatrix_ApplyDBC => ApplyDBC
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
#endif

LOGICAL(LGT) :: isok, isDBCPtrsPresent
INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT), PARAMETER :: isExpand = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isDBCPtrsPresent = PRESENT(dbcPtrs)

! If dbcPtrs are present
IF (isDBCPtrsPresent) THEN
  obj%tdbcptrs = SIZE(dbcPtrs)
  CALL Reallocate( &
    obj%dbcPtrs, obj%tdbcptrs, isExpand=isExpand, expandFactor=expandFactor)

  obj%dbcPtrs(1:obj%tdbcptrs) = dbcPtrs(1:obj%tdbcptrs)

  CALL CSRMatrix_GetSubMatrix( &
    obj=obj%mat, cols=obj%dbcptrs(1:obj%tdbcptrs), submat=obj%submat, &
    subIndices=obj%subIndices)

  obj%isSubmatInit = math%yes

  obj%tsubindices = SIZE(obj%subindices)

  isok = obj%tdbcptrs .GT. 0
  IF (isok) &
    CALL CSRMatrix_ApplyDBC(obj=obj%mat, dbcptrs=obj%dbcptrs(1:obj%tdbcptrs))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
isok = ALLOCATED(obj%subindices)
CALL AssertError1(isok, myname, &
                  "MatrxiField_::obj%subindices not allocated")
#endif

isok = obj%tsubindices .GT. 0
IF (isok) &
  CALL CSRMatrix_GetSubMatrix( &
  obj=obj%mat, subIndices=obj%subindices(1:obj%tsubindices), &
  submat=obj%submat)

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dbcptrs)
CALL AssertError1(isok, myname, &
                  "MatrxiField_::obj%dbcptrs not allocated")
#endif

isok = obj%tdbcptrs .GT. 0
IF (isok) &
  CALL CSRMatrix_ApplyDBC(obj=obj%mat, dbcPtrs=obj%dbcptrs(1:obj%tdbcptrs))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC1

!----------------------------------------------------------------------------
!                                                             GetDBCSubMat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCSubMat
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCSubMat()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCSubMat

!----------------------------------------------------------------------------
!                                                       ApplyDBCToRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBCToRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBCToRHS()"
#endif

REAL(DFP), POINTER :: xvec(:)
REAL(DFP), POINTER :: yvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xvec => x%GetPointer()
yvec => y%GetPointer()

CALL CSRMatrix_Matvec( &
  obj=obj%submat, y=yvec, x=xvec, isTranspose=isTranspose, &
  addContribution=addContribution, scale=scale)

NULLIFY (xvec, yvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBCToRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods
