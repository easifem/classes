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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ApplyDBC
CHARACTER(*), PARAMETER :: myName = "mField_ApplyDBC()"
LOGICAL(LGT) :: case1
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

case1 = PRESENT(dbcPtrs)

IF (case1) THEN
  tsize = SIZE(dbcPtrs)
  CALL Reallocate(obj%dbcPtrs, tsize)
  obj%dbcPtrs = dbcPtrs
  CALL GetSubMatrix(obj=obj%mat, cols=obj%dbcPtrs, submat=obj%submat,  &
    & subIndices=obj%subIndices)
ELSE
  CALL GetSubMatrix(obj=obj%mat, subIndices=obj%subIndices, submat=obj%submat)
END IF

CALL ApplyDBC(obj=obj%mat, dbcPtrs=dbcPtrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE mField_ApplyDBC

!----------------------------------------------------------------------------
!                                                             GetDBCSubMat
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_GetDBCSubMat
CHARACTER(*), PARAMETER :: myName = "mField_GetDBCSubMat()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE mField_GetDBCSubMat

!----------------------------------------------------------------------------
!                                                       ApplyDBCToRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ApplyDBCToRHS
CHARACTER(*), PARAMETER :: myName = "mField_ApplyDBCToRHS()"
REAL(DFP), POINTER :: xvec(:)
REAL(DFP), POINTER :: yvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

xvec => x%GetPointer()
yvec => y%GetPointer()

CALL Matvec( &
  & obj=obj%submat, &
  & y=yvec, &
  & x=xvec, &
  & isTranspose=isTranspose, &
  & addContribution=addContribution, &
  & scale=scale)

NULLIFY (xvec, yvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE mField_ApplyDBCToRHS

END SUBMODULE DBCMethods
