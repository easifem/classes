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

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This modules is a factory for linear solvers

SUBMODULE(FieldFactory) NodeFieldFactoryMethods
USE FPL, ONLY: ParameterList_
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         NodeFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE NodeFieldFactory
CHARACTER(*), PARAMETER :: myName = "NodeFieldFactory"

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")

  IF (TRIM(datatype) .EQ. "SCALAR") THEN
    ALLOCATE (ScalarField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "ST_SCALAR") THEN
    ALLOCATE (STScalarField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "VECTOR") THEN
    ALLOCATE (VectorField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "ST_VECTOR") THEN
    ALLOCATE (STVectorField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "BLOCK") THEN
    ALLOCATE (BlockNodeField_ :: ans)
  END IF

CASE ("NATIVE_OMP")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'PETSC engine is not available currently!!')

CASE ("LIS_OMP")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!!')

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for given engine')

END SELECT
END PROCEDURE NodeFieldFactory

!----------------------------------------------------------------------------
!                                                     BlockNodeFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE BlockNodeFieldFactory
CHARACTER(*), PARAMETER :: myName = "BlockNodeFieldFactory"

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (BlockNodeField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for given engine')

END SELECT
END PROCEDURE BlockNodeFieldFactory

!----------------------------------------------------------------------------
!                                                         ScalarFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldFactory
CHARACTER(*), PARAMETER :: myName = "ScalarFieldFactory"

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (ScalarField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for given engine')

END SELECT
END PROCEDURE ScalarFieldFactory

!----------------------------------------------------------------------------
!                                                         VectorFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorFieldFactory
CHARACTER(*), PARAMETER :: myName = "VectorFieldFactory"

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (VectorField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for given engine')

END SELECT
END PROCEDURE VectorFieldFactory

!----------------------------------------------------------------------------
!                                                      STVectorFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorFieldFactory
CHARACTER(*), PARAMETER :: myName = "STVectorFieldFactory"

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (STVectorField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for given engine')

END SELECT

END PROCEDURE STVectorFieldFactory

!----------------------------------------------------------------------------
!                                                         STScalarFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarFieldFactory
CHARACTER(*), PARAMETER :: myName = "STScalarFieldFactory"

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (STScalarField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_OMP engine is not available, currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for given engine')

END SELECT
END PROCEDURE STScalarFieldFactory

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorField_Initiate1
CHARACTER(*), PARAMETER :: myName = "VectorFieldIntiate1"
INTEGER(I4B) :: tsize, ii
TYPE(ParameterList_) :: param

CALL param%Initiate()

tsize = SIZE(obj)

IF (SIZE(names) .LT. tsize) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: The size of names should be atleast the size of obj')
END IF

DO ii = 1, tsize
  IF (ASSOCIATED(obj(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[ALLOCATION ERROR] :: obj('//tostring(ii)//  &
      & ") is already associated. We don't allocate like this"//  &
      & " as it may cause memory leak.")
  END IF

  obj(ii)%ptr => VectorFieldFactory(engine)

  CALL SetVectorFieldParam( &
    & param=param,  &
    & name=names(ii)%Chars(), &
    & spaceCompo=spaceCompo,  &
    & fieldType=fieldType,  &
    & engine=engine)

  CALL obj(ii)%ptr%Initiate(param=param, dom=dom)
END DO

CALL param%DEALLOCATE()

END PROCEDURE VectorField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorField_Initiate2
CHARACTER(*), PARAMETER :: myName = "VectorFieldIntiate2"
INTEGER(I4B) :: tsize, ii, nn(6)
TYPE(ParameterList_) :: param

CALL param%Initiate()

tsize = SIZE(obj)

nn = [ &
  & tsize, SIZE(names), SIZE(spaceCompo), SIZE(fieldType), SIZE(engine),  &
  & SIZE(dom) &
]

CALL Assert( &
  & nn=nn,  &
  & msg="[ARG ERROR] :: The size of obj, names, spaceCompo, fileType, "// &
  & "engine, dom should be the same",  &
  & file=__FILE__, line=__LINE__, routine=myName)

DO ii = 1, tsize
  IF (ASSOCIATED(obj(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[ALLOCATION ERROR] :: VectorField_::obj('//tostring(ii)//  &
      & ") is already associated. We don't allocate like this"//  &
      & ", as it may cause memory leak.")
  END IF

  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[POINTER ERROR] :: Domain_::dom('//tostring(ii)//  &
      & ") is not associated. It will lead to segmentation fault.")
  END IF

  obj(ii)%ptr => VectorFieldFactory(engine(ii)%Chars())

  CALL SetVectorFieldParam( &
    & param=param,  &
    & name=names(ii)%Chars(), &
    & spaceCompo=spaceCompo(ii),  &
    & fieldType=fieldType(ii),  &
    & engine=engine(ii)%Chars())

  CALL obj(ii)%ptr%Initiate(param=param, dom=dom(ii)%ptr)
END DO

CALL param%DEALLOCATE()

END PROCEDURE VectorField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarField_Initiate1
CHARACTER(*), PARAMETER :: myName = "ScalarFieldIntiate1"
INTEGER(I4B) :: tsize, ii
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL param%Initiate()

tsize = SIZE(obj)

IF (SIZE(names) .LT. tsize) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: The size of names should be atleast the size of obj')
END IF

DO ii = 1, tsize
  IF (ASSOCIATED(obj(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[ALLOCATION ERROR] :: obj('//tostring(ii)//  &
      & ") is already associated. We don't allocate like this"//  &
      & " as it may cause memory leak.")
  END IF

  obj(ii)%ptr => ScalarFieldFactory(engine)

  CALL SetScalarFieldParam( &
    & param=param,  &
    & name=names(ii)%Chars(), &
    & fieldType=fieldType,  &
    & engine=engine)

  CALL obj(ii)%ptr%Initiate(param=param, dom=dom)
END DO

CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE ScalarField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarField_Initiate2
CHARACTER(*), PARAMETER :: myName = "ScalarFieldIntiate2"
INTEGER(I4B) :: tsize, ii, nn(5)
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL param%Initiate()

tsize = SIZE(obj)

nn = [ &
  & tsize, SIZE(names), SIZE(fieldType), SIZE(engine),  &
  & SIZE(dom) &
]

CALL Assert( &
  & nn=nn,  &
  & msg="[ARG ERROR] :: The size of obj, names, fileType, "// &
  & "engine, dom should be the same",  &
  & file=__FILE__, line=__LINE__, routine=myName)

DO ii = 1, tsize
  IF (ASSOCIATED(obj(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[ALLOCATION ERROR] :: ScalarField_::obj('//tostring(ii)//  &
      & ") is already associated. We don't allocate like this"//  &
      & ", as it may cause memory leak.")
  END IF

  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[POINTER ERROR] :: Domain_::dom('//tostring(ii)//  &
      & ") is not associated. It will lead to segmentation fault.")
  END IF

  obj(ii)%ptr => ScalarFieldFactory(engine(ii)%Chars())

  CALL SetScalarFieldParam( &
    & param=param,  &
    & name=names(ii)%Chars(), &
    & fieldType=fieldType(ii),  &
    & engine=engine(ii)%Chars())

  CALL obj(ii)%ptr%Initiate(param=param, dom=dom(ii)%ptr)
END DO

CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE ScalarField_Initiate2

END SUBMODULE NodeFieldFactoryMethods
