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

USE Display_Method, ONLY: ToString

USE AssertUtility, ONLY: Assert

USE ScalarField_Class, ONLY: SetScalarFieldParam

USE STScalarField_Class, ONLY: SetSTScalarFieldParam

USE STVectorField_Class, ONLY: SetSTVectorFieldParam

USE VectorField_Class, ONLY: SetVectorFieldParam

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         NodeFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE NodeFieldFactory
CHARACTER(*), PARAMETER :: myName = "NodeFieldFactory()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => NULL()

SELECT CASE (engine)

CASE ("NATIVE_SERIAL")

  SELECT CASE (datatype)
  CASE ("SCALAR")
    ALLOCATE (ScalarField_ :: ans)
  CASE ("ST_SCALAR")
    ALLOCATE (STScalarField_ :: ans)
  CASE ("VECTOR")
    ALLOCATE (VectorField_ :: ans)
  CASE ("ST_VECTOR")
    ALLOCATE (STVectorField_ :: ans)
  CASE ("BLOCK")
    ALLOCATE (BlockNodeField_ :: ans)
  CASE default
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: No case found for given datatype')
    RETURN
  END SELECT

CASE ("NATIVE_OMP")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: PETSC engine is not available currently!!')

CASE ("LIS_OMP")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")

  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_MPI engine is not available currently!!')

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for given engine')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE NodeFieldFactory

!----------------------------------------------------------------------------
!                                                     BlockNodeFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE BlockNodeFieldFactory
CHARACTER(*), PARAMETER :: myName = "BlockNodeFieldFactory()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (BlockNodeField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for given engine')

  RETURN

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE BlockNodeFieldFactory

!----------------------------------------------------------------------------
!                                                         ScalarFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldFactory
CHARACTER(*), PARAMETER :: myName = "ScalarFieldFactory()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (ScalarField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
        '[INTERNAL ERRROR] :: NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for given engine')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE ScalarFieldFactory

!----------------------------------------------------------------------------
!                                                         VectorFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorFieldFactory
CHARACTER(*), PARAMETER :: myName = "VectorFieldFactory()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (VectorField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'No case found for given engine')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE VectorFieldFactory

!----------------------------------------------------------------------------
!                                                      STVectorFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorFieldFactory
CHARACTER(*), PARAMETER :: myName = "STVectorFieldFactory()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (STVectorField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_OMP engine is not available currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for given engine')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE STVectorFieldFactory

!----------------------------------------------------------------------------
!                                                         STScalarFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarFieldFactory
CHARACTER(*), PARAMETER :: myName = "STScalarFieldFactory"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

SELECT CASE (TRIM(engine))

CASE ("NATIVE_SERIAL")
  ALLOCATE (STScalarField_ :: ans)

CASE ("NATIVE_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'NATIVE_OMP engine is not available, currently!!')

CASE ("NATIVE_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'NATIVE_MPI engine is not available currently!!')

CASE ("PETSC")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'PETSC engine is not available currently!!')

CASE ("LIS_OMP")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'LIS_OMP engine is not available currently!!')

CASE ("LIS_MPI")
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'LIS_MPI engine is not available currently!!')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'No case found for given engine')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE STScalarFieldFactory

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorField_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VectorField_Initiate1()"
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
! CALL AssertError1(isok, myname, &
!                   "Size of obj is not enough it is less than size of names")
!
! DO ii = 1, tsize
!
!   problem = ASSOCIATED(obj(ii)%ptr)
!
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[ALLOCATION ERROR] :: obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       " as it may cause memory leak.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => VectorFieldFactory(engine)
!
!   CALL SetVectorFieldParam( &
!     param=param, name=names(ii)%Chars(), spaceCompo=spaceCompo, &
!     fieldType=fieldType, engine=engine)
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof, geofedof=geofedof)
!
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VectorField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorField_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VectorField_Initiate2()"
#endif

! INTEGER(I4B) :: tsize, ii, nn(6)
! TYPE(ParameterList_) :: param

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
! tsize = SIZE(obj)
!
! nn = [ &
!   & tsize, SIZE(names), SIZE(spaceCompo), SIZE(fieldType), SIZE(engine),  &
!   & SIZE(fedof) &
! ]
!
! CALL Assert(nn=nn, &
!       msg="[ARG ERROR] :: The size of obj, names, spaceCompo, fieldType, "// &
!             "engine, fedof should be the same", &
!             file=__FILE__, line=__LINE__, routine=myName)
!
! DO ii = 1, tsize
!   IF (ASSOCIATED(obj(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                   '[ALLOCATION ERROR] :: VectorField_::obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       ", as it may cause memory leak.")
!   END IF
!
!   IF (.NOT. ASSOCIATED(fedof(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[POINTER ERROR] :: FEDOF_::fedof('//ToString(ii)// &
!                    ") is not associated. It will lead to segmentation fault.")
!   END IF
!
!   obj(ii)%ptr => VectorFieldFactory(engine(ii)%Chars())
!
!   CALL SetVectorFieldParam(param=param, name=names(ii)%Chars(), &
!                            spaceCompo=spaceCompo(ii), &
!                            fieldType=fieldType(ii), &
!                            engine=engine(ii)%chars())
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof(ii)%ptr, &
!                             geofedof=geofedof(ii)%ptr)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VectorField_Initiate2

!----------------------------------------------------------------------------
!                                                                 initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorField_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STVectorField_Initiate1()"
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
! CALL AssertError1(isok, myname, &
!                   "Size of obj is not enough it is less than size of names")
!
! DO ii = 1, tsize
!
!   problem = ASSOCIATED(obj(ii)%ptr)
!
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[ALLOCATION ERROR] :: obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       " as it may cause memory leak.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => STVectorFieldFactory(engine)
!
!   CALL SetSTVectorFieldParam( &
!     param=param, name=names(ii)%chars(), spaceCompo=spaceCompo, &
!     timeCompo=timeCompo, fieldType=fieldType, engine=engine)
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof, geofedof=geofedof)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE STVectorField_Initiate1

!----------------------------------------------------------------------------
!                                                                 initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorField_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STVectorField_Initiate2()"
#endif

! INTEGER(I4B) :: tsize, ii, nn(7)
! TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL param%Initiate()
!
! tsize = SIZE(obj)
!
! nn = [ tsize, SIZE(names), SIZE(spaceCompo), SIZE(timeCompo), SIZE(fieldType), &
!       SIZE(engine), SIZE(fedof)]
!
! CALL Assert(nn=nn, &
!        msg="[ARG ERROR] :: The size of obj, names, spaceCompo, timeCompo,"// &
!             "fieldType, engine, fedof should be the same", &
!             file=__FILE__, line=__LINE__, routine=myName)
!
! DO ii = 1, tsize
!   IF (ASSOCIATED(obj(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                 '[ALLOCATION ERROR] :: STVectorField_::obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       ", as it may cause memory leak.")
!     RETURN
!   END IF
!
!   IF (.NOT. ASSOCIATED(fedof(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[POINTER ERROR] :: FEDOF_::fedof('//ToString(ii)// &
!                    ") is not associated. It will lead to segmentation fault.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => STVectorFieldFactory(engine(ii)%Chars())
!
!   CALL SetSTVectorFieldParam(param=param, name=names(ii)%Chars(), &
!                          spaceCompo=spaceCompo(ii), timeCompo=timeCompo(ii), &
!                            fieldType=fieldType(ii), engine=engine(ii)%chars())
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof(ii)%ptr, &
!                             geofedof=geofedof(ii)%ptr)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER
END PROCEDURE STVectorField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarField_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarField_Initiate1()"
#endif
! INTEGER(I4B) :: tsize, ii
! TYPE(ParameterList_) :: param
! LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

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
!
!   problem = ASSOCIATED(obj(ii)%ptr)
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[ALLOCATION ERROR] :: obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       " as it may cause memory leak.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => ScalarFieldFactory(engine)
!
!   CALL SetScalarFieldParam(param=param, name=names(ii)%chars(), &
!                            fieldType=fieldType, engine=engine)
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof, geofedof=geofedof)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE ScalarField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarField_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarField_Initiate2()"
#endif

! INTEGER(I4B) :: tsize, ii, nn(5)
! TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL param%Initiate()
!
! tsize = SIZE(obj)
!
! nn = [tsize, SIZE(names), SIZE(fieldType), SIZE(engine), SIZE(fedof)]
!
! CALL Assert(nn=nn, &
!             msg="[ARG ERROR] :: The size of obj, names, fieldType, "// &
!             "engine, fedof should be the same", &
!             file=__FILE__, line=__LINE__, routine=myName)
!
! DO ii = 1, tsize
!
!   IF (ASSOCIATED(obj(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                   '[ALLOCATION ERROR] :: ScalarField_::obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       ", as it may cause memory leak.")
!     RETURN
!   END IF
!
!   IF (.NOT. ASSOCIATED(fedof(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[POINTER ERROR] :: FEDOF_::fedof('//ToString(ii)// &
!                    ") is not associated. It will lead to segmentation fault.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => ScalarFieldFactory(engine(ii)%Chars())
!
!   CALL SetScalarFieldParam(param=param, name=names(ii)%Chars(), &
!                            fieldType=fieldType(ii), engine=engine(ii)%Chars())
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof(ii)%ptr, &
!                             geofedof=geofedof(ii)%ptr)
!
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER
END PROCEDURE ScalarField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarField_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STScalarField_Initiate1()"
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
! CALL AssertError1(isok, myname, &
!                   "Size of obj is not enough it is less than size of names")
!
! DO ii = 1, tsize
!
!   problem = ASSOCIATED(obj(ii)%ptr)
!   IF (problem) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[ALLOCATION ERROR] :: obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       " as it may cause memory leak.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => STScalarFieldFactory(engine)
!
!   CALL SetSTScalarFieldParam( &
!     param=param, name=names(ii)%Chars(), timeCompo=timeCompo, &
!     fieldType=fieldType, engine=engine)
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof, geofedof=geofedof)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE STScalarField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarField_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STScalarField_Initiate2()"
#endif

! INTEGER(I4B) :: tsize, ii, nn(6)
! TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL param%Initiate()
!
! tsize = SIZE(obj)
!
! nn = [tsize, SIZE(names), SIZE(timeCompo), SIZE(fieldType), SIZE(engine), &
!       SIZE(fedof)]
!
! CALL Assert(nn=nn, &
!        msg="[ARG ERROR] :: The size of obj, names, timeCompo, fieldType, "// &
!             "engine,  fedof should be the same", &
!             file=__FILE__, line=__LINE__, routine=myName)
!
! DO ii = 1, tsize
!
!   IF (ASSOCIATED(obj(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                 '[ALLOCATION ERROR] :: STScalarField_::obj('//ToString(ii)// &
!                     ") is already associated. We don't allocate like this"// &
!                       ", as it may cause memory leak.")
!     RETURN
!   END IF
!
!   IF (.NOT. ASSOCIATED(fedof(ii)%ptr)) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[POINTER ERROR] :: FEDOF_::fedof('//ToString(ii)// &
!                    ") is not associated. It will lead to segmentation fault.")
!     RETURN
!   END IF
!
!   obj(ii)%ptr => STScalarFieldFactory(engine(ii)%Chars())
!
!   CALL SetSTScalarFieldParam(param=param, name=names(ii)%Chars(), &
!                              timeCompo=timeCompo(ii), &
!                              fieldType=fieldType(ii), &
!                              engine=engine(ii)%Chars())
!
!   CALL obj(ii)%ptr%Initiate(param=param, fedof=fedof(ii)%ptr, &
!                             geofedof=geofedof(ii)%ptr)
! END DO
!
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE STScalarField_Initiate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE NodeFieldFactoryMethods
