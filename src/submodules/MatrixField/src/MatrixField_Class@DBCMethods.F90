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

SUBMODULE(MatrixField_Class) DBCMethods
USE ReallocateUtility, ONLY: Reallocate
USE CSRMatrix_Method, ONLY: GetSubMatrix, Matvec, ApplyDBC
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC3()"
#endif

LOGICAL(LGT) :: case1, isok
INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT), PARAMETER :: isExpand = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

case1 = PRESENT(dbcPtrs)

IF (case1) THEN
  obj%tdbcptrs = SIZE(dbcptrs)
  CALL Reallocate(obj%dbcptrs, obj%tdbcptrs, isExpand=isExpand, &
                  expandFactor=expandFactor)

  obj%dbcPtrs(1:obj%tdbcptrs) = dbcPtrs(1:obj%tdbcptrs)

  CALL GetSubMatrix(obj=obj%mat, cols=obj%dbcptrs(1:obj%tdbcptrs), &
                    submat=obj%submat, subIndices=obj%subindices)

  obj%tsubindices = SIZE(obj%subindices)

  IF (obj%tdbcptrs .GT. 0) THEN
    CALL ApplyDBC(obj=obj%mat, dbcptrs=obj%dbcptrs(1:obj%tdbcptrs))
  END IF

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

IF (obj%tsubindices .GT. 0) THEN
  CALL GetSubMatrix(obj=obj%mat, &
                    subIndices=obj%subindices(1:obj%tsubindices), &
                    submat=obj%submat)
END IF

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dbcptrs)
CALL AssertError1(isok, myname, &
                  "MatrxiField_::obj%dbcptrs not allocated")
#endif

IF (obj%tdbcptrs .GT. 0) THEN
  CALL ApplyDBC(obj=obj%mat, dbcPtrs=obj%dbcptrs(1:obj%tdbcptrs))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ApplyDirichletBC3

!----------------------------------------------------------------------------
!                                                             GetDBCSubMat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCSubMat
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCSubMat()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetDirichletBCSubMat

!----------------------------------------------------------------------------
!                                                       ApplyDBCToRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBCToRHS
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBCToRHS()"
REAL(DFP), POINTER :: xvec(:)
REAL(DFP), POINTER :: yvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xvec => x%GetPointer()
yvec => y%GetPointer()

CALL Matvec(obj=obj%submat, y=yvec, x=xvec, isTranspose=isTranspose, &
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
