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

SUBMODULE(UserFunction_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Display
  CALL Display( msg, unitNo=unitNo )
  IF( obj%isUserFunctionSet ) THEN
    CALL Display( "# isUserFunctionSet: TRUE", unitNo=unitNo )
    CALL obj%userFunction%Display("display from useFunction: ", &
      & unitNo=unitNo)
  ELSE
    CALL Display( "# isUserFunctionSet: FALSE", unitNo=unitNo )
    CALL Display(NAME_RETURN_TYPE(obj%returnType), "# returnType: ",  &
      & unitNo=unitNo )
    CALL Display(NAME_ARG_TYPE(obj%argType), "# argType: ",  &
      & unitNo=unitNo )
    IF( obj%argType .EQ. CONSTANT ) THEN
      SELECT CASE( obj%returnType )
      CASE( Scalar )
        CALL Display( obj%scalarValue, "# scalarValue: ", unitNo=unitNo )
      CASE( Vector )
        IF( ALLOCATED(obj%vectorValue )) THEN
          CALL Display( obj%vectorValue, "# vectorValue: ", unitNo=unitNo )
        ELSE
          CALL Display( "# vectorValue: NOT ALLOCATED", unitNo=unitNo)
        END IF
      CASE( Matrix )
        IF( ALLOCATED(obj%matrixValue )) THEN
          CALL Display( obj%matrixValue, "# matrixValue: ", unitNo=unitNo )
        ELSE
          CALL Display( "# matrixValue: NOT ALLOCATED", unitNo=unitNo)
        END IF
      END SELECT
    END IF
  END IF
END PROCEDURE auf_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Import
  CHARACTER( LEN = * ), PARAMETER :: myName= "auf_Import"
  TYPE( String ) :: dsetname, strval
  !> main
  !> info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing the instance of UserFunction_ data")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have read permission')
  END IF
  !> isUserFunctionSet
  dsetname=trim(group)//"/isUserFunctionSet"
  IF( hdf5%pathExists(dsetname%chars()) ) THEN
    CALL hdf5%read(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
  ELSE
    obj%isUserFunctionSet = .FALSE.
  END IF
  !> isUserFunctionSet
  IF( obj%isUserFunctionSet ) THEN
    dsetname=trim(group)//"/userFunction"
    ALLOCATE(obj%userFunction)
    CALL obj%userFunction%Import(hdf5=hdf5, group=dsetname%chars())
  ELSE
    !> returnType
    dsetname=TRIM(group)//"/returnType"
    IF( .NOT. hdf5%pathExists(dsetname%chars()) ) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'dsetname '// dsetname%chars() // 'is not present in HDFFile_')
    ELSE
      CALL hdf5%read(dsetname=dsetname%chars(), vals=strval)
      obj%returnType=UserFunctionGetReturnType( strval%chars() )
    END IF
    !> argType
    dsetname=TRIM(group)//"/argType"
    IF( .NOT. hdf5%pathExists(dsetname%chars()) ) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'dsetname '// dsetname%chars() // 'is not present in HDFFile_')
    ELSE
      CALL hdf5%read(dsetname=dsetname%chars(), vals=strval)
      obj%argType=UserFunctionGetArgType( strval%chars() )
    END IF
    !> check the argType, and decide the importer
    IF( obj%argType .EQ. CONSTANT ) THEN
      !> scalarValue, vectorValue, matrixValue
      SELECT CASE( obj%returnType )
      CASE( SCALAR )
        !> scalarValue
        dsetname=trim(group)//"/scalarValue"
        IF( hdf5%pathExists(dsetname%chars()) ) THEN
          CALL hdf5%read(dsetname=dsetname%chars(), vals=obj%scalarValue)
        ENDIF
      CASE( VECTOR )
        !> vectorValue
        dsetname=trim(group)//"/vectorValue"
        IF( hdf5%pathExists(dsetname%chars()) ) THEN
          CALL hdf5%read(dsetname=dsetname%chars(), vals=obj%vectorValue)
        ENDIF
      CASE( MATRIX )
        !> matrixValue
        dsetname=trim(group)//"/matrixValue"
        IF( hdf5%pathExists(dsetname%chars()) ) THEN
          CALL hdf5%read(dsetname=dsetname%chars(), vals=obj%matrixValue)
        ENDIF
      END SELECT
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'Currently, EASIFEM Supports import of constant userFunction.')
    END IF
  END IF
END PROCEDURE auf_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Export
  CHARACTER( LEN = * ), PARAMETER :: myName= "auf_Export"
  TYPE( String ) :: dsetname, strval
  !> main
  !> info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Exporting the instance of UserFunction_ data")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF
  !> check
  IF( .NOT. hdf5%isWrite() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have write permission')
  END IF
  !> isUserFunctionSet
  IF( obj%isUserFunctionSet ) THEN
    !> isUserFunctionSet
    dsetname=trim(group)//"/isUserFunctionSet"
    CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
    !> returnType
    dsetname=trim(group)//"/userFunction"
    CALL obj%userFunction%Export(hdf5=hdf5, group=dsetname%chars())
  ELSE
    !> isUserFunctionSet
    dsetname=trim(group)//"/isUserFunctionSet"
    CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
    !> returnType
    dsetname=trim(group)//"/returnType"
    strval = NAME_RETURN_TYPE(obj%returnType)
    CALL hdf5%write(dsetname=dsetname%chars(), vals=strval)
    !> argType
    dsetname=trim(group)//"/argType"
    strval = NAME_ARG_TYPE(obj%argType)
    CALL hdf5%write(dsetname=dsetname%chars(), vals=strval)
    !>
    IF( obj%argType .EQ. CONSTANT ) THEN
      SELECT CASE( obj%returnType )
      CASE( SCALAR )
        !> scalarValue
        dsetname=trim(group)//"/scalarValue"
        CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%scalarValue)
      CASE( VECTOR )
        !> vectorValue
        dsetname=trim(group)//"/vectorValue"
        IF( ALLOCATED(obj%vectorValue)) &
          & CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%vectorValue)
      CASE( MATRIX )
        !> matrixValue
        dsetname=trim(group)//"/matrixValue"
        IF( ALLOCATED(obj%matrixValue)) &
          & CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%matrixValue)
      END SELECT
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'Currently, EASIFEM Supports import of constant userFunction.')
    END IF
  END IF
END PROCEDURE auf_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
