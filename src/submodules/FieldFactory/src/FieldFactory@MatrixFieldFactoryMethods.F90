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

SUBMODULE(FieldFactory) MatrixFieldFactoryMethods
USE StringUtility, ONLY: UpperCase
USE AssertUtility, ONLY: Assert
USE Display_Method, ONLY: ToString
USE MatrixField_Class, ONLY: MatrixField_
USE MatrixFieldLis_Class, ONLY: MatrixFieldLis_
USE BlockMatrixField_Class, ONLY: BlockMatrixField_
USE BlockMatrixFieldLis_Class, ONLY: BlockMatrixFieldLis_
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         MatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMatrixFieldFactory
CHARACTER(*), PARAMETER :: myName = "AbstractMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: case0

case0 = UpperCase(engine)//":"//UpperCase(name)

SELECT CASE (case0)
CASE ("NATIVE_SERIAL:MATRIX")
  ALLOCATE (MatrixField_ :: ans)

CASE ("NATIVE_SERIAL:BLOCKMATRIX")
  ALLOCATE (BlockMatrixField_ :: ans)

CASE ("LIS_OMP:MATRIX")
  ALLOCATE (MatrixFieldLis_ :: ans)

CASE ("LIS_OMP:BLOCKMATRIX")
  ALLOCATE (BlockMatrixFieldLis_ :: ans)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[NO CASE FOUND] :: No case found for given engine '// &
                    "following values are acceptable = "// &
                    "[NATIVE_SERIAL:MATRIX, NATIVE_SERIAL:BLOCKMATRIX, "// &
                    " LIS_OMP:MATRIX, LIS_OMP:BLOCKMATRIX]"// &
                    " but found  = "//case0)

  ALLOCATE (MatrixField_ :: ans)
  RETURN
END SELECT
END PROCEDURE AbstractMatrixFieldFactory

!----------------------------------------------------------------------------
!                                                         MatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldFactory
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "MatrixFieldFactory()"
#endif

CHARACTER(:), ALLOCATABLE :: case0

case0 = UpperCase(engine)

SELECT CASE (case0)
CASE ("NATIVE_SERIAL")
  ALLOCATE (MatrixField_ :: ans)

CASE ("LIS_OMP")
  ALLOCATE (MatrixFieldLis_ :: ans)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(math%no, myName, "No case found for given engine name")
#endif
END SELECT

case0 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE MatrixFieldFactory

!----------------------------------------------------------------------------
!                                                   BlockMatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE BlockMatrixFieldFactory
CHARACTER(*), PARAMETER :: myName = "BlockMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: case0

case0 = UpperCase(engine)

SELECT CASE (case0)
CASE ("NATIVE_SERIAL")
  ALLOCATE (BlockMatrixField_ :: ans)

CASE ("LIS_OMP")
  ALLOCATE (BlockMatrixFieldLis_ :: ans)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] :: No case found for given engine '//  &
    & "following values are acceptable = "//  &
    & "[NATIVE_SERIAL, LIS_OMP]"// &
    & " but found  = "//TRIM(case0))
  ALLOCATE (BlockMatrixField_ :: ans)
  RETURN
END SELECT
END PROCEDURE BlockMatrixFieldFactory

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixField_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "MatrixFieldIntiate1()"
#endif

! INTEGER(I4B) :: tsize, ii
! TYPE(ParameterList_) :: param
! LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL param%Initiate()
!
! tsize = SIZE(names)
! isok = SIZE(obj) .GE. tsize
!
! CALL AssertError1(isok, myname, &
!                   "Size of obj is not enough it is less than size of names")
!
! DO ii = 1, tsize
!   problem = ASSOCIATED(obj(ii)%ptr)
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[ALLOCATION ERROR] :: obj('//tostring(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       " as it may cause memory leak.")
!   END IF
!
!   obj(ii)%ptr => MatrixFieldFactory(engine)
!
!   CALL SetMatrixFieldParam( &
!     param=param, name=names(ii)%Chars(), matrixProp=matrixProps, &
!     spaceCompo=spaceCompo, timeCompo=timeCompo, fieldType=fieldType, &
!     engine=engine)
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof, geofedof=geofedof)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE MatrixField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixField_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "MatrixFieldIntiate2()"
#endif

! INTEGER(I4B) :: tsize, ii, nn(8)
! TYPE(ParameterList_) :: param
! LOGICAL(LGT) :: isok, problem

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

! CALL param%Initiate()
!
! tsize = SIZE(names)
!
! #ifdef DEBUG_VER
!
! nn = [tsize, SIZE(names), SIZE(spaceCompo), SIZE(fieldType), &
!       SIZE(engine), SIZE(fedof), SIZE(timeCompo), SIZE(matrixProps)]
!
! CALL Assert(nn=nn, &
!       msg="[ARG ERROR] :: The size of obj, names, spaceCompo, fieldType, "// &
!             "timeCompo, engine, fedof, matProps should be the same", &
!             file=__FILE__, line=__LINE__, routine=myName)
!
! isok = SIZE(obj) .GE. tsize
! CALL AssertError1(isok, myname, &
!                   "Size of obj is not enough it is less than size of names")
!
! #endif
!
! DO ii = 1, tsize
!   problem = ASSOCIATED(obj(ii)%ptr)
!
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                   '[ALLOCATION ERROR] :: MatrixField_::obj('//tostring(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       ", as it may cause memory leak.")
!     RETURN
!   END IF
!
!   isok = ASSOCIATED(fedof(ii)%ptr)
!
!   IF (.NOT. isok) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[POINTER ERROR] :: FEDOF_::fedof('//tostring(ii)// &
!                    ") is not associated. It will lead to segmentation fault.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => MatrixFieldFactory(engine(ii)%Chars())
!
!   CALL SetMatrixFieldParam( &
!     param=param, name=names(ii)%Chars(), matrixProp=matrixProps(ii)%Chars(), &
!     spaceCompo=spaceCompo(ii), timeCompo=timeCompo(ii), &
!     fieldType=fieldType(ii), engine=engine(ii)%Chars())
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof(ii)%ptr, &
!                             geofedof=geofedof(ii)%ptr)
! END DO
!
! CALL param%DEALLOCATE()

END PROCEDURE MatrixField_Initiate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MatrixFieldFactoryMethods
