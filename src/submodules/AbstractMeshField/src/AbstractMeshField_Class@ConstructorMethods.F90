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

SUBMODULE(AbstractMeshField_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setAbstractMeshFieldParam
  !!
  INTEGER( I4B ) :: ierr
  !!
  ierr = param%set( key=TRIM(prefix) // "/name", value=name )
  ierr = param%set( key=TRIM(prefix) // "/fieldType", value=fieldType )
  ierr = param%set( key=TRIM(prefix) // "/engine", value=engine )
  ierr = param%set( key=TRIM(prefix) // "/defineOn", value=defineOn )
  ierr = param%set( key=TRIM(prefix) // "/varType", value=varType )
  ierr = param%set( key=TRIM(prefix) // "/rank", value=rank )
  ierr = param%set( key=TRIM(prefix) // "/s", value=s )
  !!
END PROCEDURE setAbstractMeshFieldParam

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_addSurrogate
  CALL e%addSurrogate(UserObj)
END PROCEDURE aField_addSurrogate

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_checkEssentialParam
  CALL AbstractFieldCheckEssentialParam( obj=obj, prefix="MeshField", &
    & param=param )
END PROCEDURE aField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldCheckEssentialParam
  !!
  CHARACTER( LEN = * ), PARAMETER :: myName = &
    & "AbstractFieldCheckEssentialParam"
  !!
  !! fieldType
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/fieldType") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'fieldType should be present in param')
  !!
  !! name
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/name") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'names should be present in param')
  !!
  !! engine
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/engine") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'engine should be present in param')
  !!
  !! s
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/s") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 's should be present in param')
  !!
  !! defineOn
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/defineOn") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'defineOn should be present in param')
  !!
  !! varType
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/varType") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'varType should be present in param')
  !!
  !! rank
  !!
  IF( .NOT. param%isPresent(key=TRIM(prefix) // "/rank") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'rank should be present in param')
  !!
END PROCEDURE AbstractFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Deallocate
  obj%isInitiated=.FALSE.
  obj%fieldType=FIELD_TYPE_NORMAL
  obj%name=""
  obj%engine=""
  obj%tSize=0
  obj%s=0
  obj%defineOn=0
  obj%varType=0
  obj%rank=0
  IF( ALLOCATED( obj%val ) ) DEALLOCATE( obj%val )
  obj%mesh => NULL()
END PROCEDURE aField_Deallocate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Initiate1
END PROCEDURE aField_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMeshFieldInitiate
  TYPE( String ) :: dsetname
  INTEGER( I4B ) :: ierr, nrow
  CHARACTER( LEN=: ), ALLOCATABLE :: char_var
  CHARACTER( LEN = * ), PARAMETER :: myName = "AbstractMeshFieldInitiate"
  !!
  !! check
  !!
  IF( obj%isInitiated ) THEN
    CALL e%raiseError(modName //'::'//myName// ' - '// &
      & 'MeshField object is already initiated, deallocate first')
  END IF
  !!
  !! check
  !!
  CALL obj%checkEssentialParam( param )
  !!
  obj%isInitiated = .TRUE.
  !!
  !! fieldType
  !!
  dsetname = TRIM(prefix) // "/fieldType"
  IF( param%isPresent(key=dsetname%chars()) ) THEN
    ierr = param%get( key=dsetname%chars(), value=obj%fieldType )
  ELSE
    obj%fieldType = FIELD_TYPE_NORMAL
  END IF
  !!
  !! name
  !!
  dsetname = TRIM(prefix) // "/name"
  ALLOCATE( CHARACTER( LEN = &
    & param%DataSizeInBytes( key=dsetname%chars() ) ) :: char_var )
  ierr = param%get( key=dsetname%chars(), value=char_var )
  obj%name = char_var; DEALLOCATE( char_var )
  !!
  !! engine
  !!
  dsetname = TRIM(prefix) // "/engine"
  ALLOCATE( CHARACTER( LEN = &
    & param%DataSizeInBytes( key=dsetname%chars() ) ) :: char_var )
  ierr = param%get( key=dsetname%chars(), value=char_var )
  obj%engine = char_var; DEALLOCATE( char_var )
  !!
  !! defineOn
  !!
  dsetname = TRIM(prefix) // "/defineOn"
  ierr = param%get( key=dsetname%chars(), value=obj%defineOn )
  !!
  !! varType
  !!
  dsetname = TRIM(prefix) // "/varType"
  ierr = param%get( key=dsetname%chars(), value=obj%varType )
  !!
  !! rank
  !!
  dsetname = TRIM(prefix) // "/rank"
  ierr = param%get( key=dsetname%chars(), value=obj%rank )
  !!
  !! nrow
  !!
  SELECT CASE( obj%rank )
  !!
  !! Scalar
  !!
  CASE( Scalar )
    SELECT CASE( obj%varType )
    CASE( Constant )
      !! one dimension, single entry
      nrow = 1
    CASE( Space )
      !! one dimension, multiple entries in space
      nrow = 1
    CASE( Time )
      !! one dimension, multiple entries in time
      nrow = 1
    CASE( SpaceTime )
      !! two dimensions, multiple entries in space-time
      nrow = 2
    END SELECT
  !!
  !! Vector
  !!
  CASE( Vector )
    SELECT CASE( obj%varType )
    CASE( Constant )
      !! one dimension, only vector components
      nrow = 1
    CASE( Space )
      !! two dimension, vector components and space values
      nrow = 2
    CASE( Time )
      !! two dimension, vector components and time values
      nrow = 2
    CASE( SpaceTime )
      !! two dimension, vector components, space and time values
      nrow = 3
    END SELECT
  !!
  !! Matrix
  !!
  CASE( Matrix )
    SELECT CASE( obj%varType )
    CASE( Constant )
      !! two dimensions, matrix components
      nrow = 2
    CASE( Space )
      !! three dimensions, matrix components and space values
      nrow = 3
    CASE( Time )
      !! three dimensions, matrix components and time values
      nrow = 3
    CASE( SpaceTime )
      !! four dimensions, matrix components, space and time values
      nrow = 4
    END SELECT
  !!
  !!
  !!
  END SELECT
  !!
  !! s
  !!
  dsetname = TRIM(prefix) // "/s"
  ierr = param%get( key=dsetname%chars(), value=obj%s(1:nrow) )
  !!
  !! tSize
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    obj%tSize = 1
  ELSE
    obj%tSize = mesh%getTotalElements()
  END IF
  !!
  !! val
  !!
  CALL Reallocate( obj%val, PRODUCT(obj%s(1:nrow)),obj%tSize )
  !!
  !! mesh
  !!
  obj%mesh => mesh
  !!
END PROCEDURE AbstractMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_initiate2
  !!
  obj%isInitiated = obj2%isInitiated
  obj%fieldType = obj2%fieldType
  obj%name = obj2%name
  obj%engine = obj2%engine
  obj%tSize = obj2%tSize
  obj%s = obj2%s
  obj%defineOn = obj2%defineOn
  obj%varType = obj2%varType
  obj%rank = obj2%rank
  obj%mesh => obj2%mesh
  IF( ALLOCATED( obj2%val ) ) obj%val = obj2%val
  !!
END PROCEDURE aField_initiate2

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_getPointer
  IF( ALLOCATED( obj%val ) ) THEN
    ans => obj%val
  ELSE
    ans => NULL()
  END IF
END PROCEDURE aField_getPointer

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Size
  !!
  IF( PRESENT( dim ) ) THEN
    ans = obj%s(dim)
  ELSE
    !!
    SELECT CASE (obj%rank)
    CASE (Scalar)
      ans = 1
    CASE (Vector)
      ans = obj%s(1)
    CASE (Matrix)
      ans = obj%s(1)*obj%s(2)
    END SELECT
    !!
  END IF
  !!
END PROCEDURE aField_Size

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Shape
  SELECT CASE (obj%rank)
  !!
  !! Scalar
  !!
  CASE (Scalar)
    SELECT CASE (obj%vartype)
    CASE (Constant)
      ans = [1]
    CASE (Space, Time)
      ans = obj%s(1:1)
    CASE (SpaceTime)
      ans = obj%s(1:2)
    END SELECT
  !!
  !! Vector
  !!
  CASE (Vector)
    SELECT CASE (obj%vartype)
    CASE (Constant)
      ans = obj%s(1:1)
    CASE (Space, Time)
      ans = obj%s(1:2)
    CASE (SpaceTime)
      ans = obj%s(1:3)
    END SELECT
  !!
  !! Matrix
  !!
  CASE (Matrix)
    SELECT CASE (obj%vartype)
    CASE (Constant)
      ans = obj%s(1:2)
    CASE (Space, Time)
      ans = obj%s(1:3)
    CASE (SpaceTime)
      ans = obj%s(1:4)
    END SELECT
  END SELECT
  !!
  !!
  !!
END PROCEDURE aField_Shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods