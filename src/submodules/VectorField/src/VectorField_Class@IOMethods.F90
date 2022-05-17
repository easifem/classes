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

SUBMODULE(VectorField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Display
  IF (LEN_TRIM(msg) .NE. 0) THEN
    CALL Display("# "//TRIM(msg), unitNo=unitNo)
  END IF
  CALL Display(obj%name, "# name : ")
  CALL Display("# engine : NATIVE_SERIAL")
  IF (obj%isInitiated) THEN
    CALL Display("# isInitiated : TRUE", unitNo=unitNo)
  ELSE
    CALL Display("# isInitiated : FALSE, Nothing to Display!", &
      & unitNo=unitNo)
    RETURN
  END IF
  CALL Display(obj%spaceCompo, "# space components : ", unitNo=unitNo)
  CALL Display(obj%tSize, "# tSize : ", unitNo=unitNo)
  IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
    CALL Display("# fieldType : CONSTANT", unitNo=unitNo)
  ELSE
    CALL Display("# fieldType : NORMAL", unitNo=unitNo)
  END IF
  IF (ASSOCIATED(obj%domain)) THEN
    CALL Display("# domain : ASSOCIATED", unitNo=unitNo)
  ELSE
    CALL Display("# domain : .NOT. ASSOCIATED", unitNo=unitNo)
  END IF
  CALL Display(obj%realVec, obj%dof, msg="# realVec : ", unitNo=unitNo)
END PROCEDURE vField_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Import
  CHARACTER(LEN=*), PARAMETER :: myName = "vField_Import"
  TYPE(String) :: strval, dsetname, name, engine
  INTEGER(I4B) :: fieldType, spaceCompo
  TYPE(ParameterList_) :: param
  !!
  !! info
  !!
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing VectorField_")
  !!
  !! check
  !!
  IF (obj%isInitiated) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Vector field object is already initiated')
  !!
  !! check
  !!
  IF (.NOT. hdf5%isOpen()) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  !!
  !! check
  !!
  IF (.NOT. hdf5%isRead()) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have read permission')
  !!
  !!
  !! fieldType
  !!
  dsetname=trim(group)//"/fieldType"
  IF( hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
    fieldType = FIELD_TYPE_NUMBER( TRIM(strval%chars()) )
  ELSE
    fieldType = FIELD_TYPE_NORMAL
  END IF
  !!
  !! name
  !!
  dsetname=trim(group)//"/name"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset name should be present')
  CALL hdf5%read(dsetname=dsetname%chars(),vals=name)
  !!
  !! engine
  !!
  dsetname=trim(group)//"/engine"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset named engine should be present')
  CALL hdf5%read(dsetname=dsetname%chars(),vals=engine)
  !!
  !! tSize
  !!
  dsetname=trim(group)//"/tSize"
  IF( hdf5%pathExists(dsetname%chars())) &
    & CALL hdf5%read(dsetname=dsetname%chars(),vals=obj%tSize)
  !!
  !! spaceCompo
  !!
  dsetname = trim(group)//"/spaceCompo"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset spaceCompo should be present')
  CALL hdf5%read(dsetname=dsetname%chars(), vals=spaceCompo)
  !!
  !! Initiate
  !!
  CALL FPL_INIT(); CALL param%initiate()
  CALL setVectorFieldParam( &
    & param=param, &
    & name=trim(name%chars()), &
    & fieldType = fieldType, &
    & spaceCompo=spaceCompo )
  !!
  CALL obj%initiate( param=param, dom=dom )
  CALL param%Deallocate(); CALL FPL_FINALIZE()
  !!
  !! dof
  !!
  dsetname=trim(group)//"/dof"
  IF( hdf5%pathExists(dsetname%chars())) &
    & CALL ImportDOF( obj=obj%dof, hdf5=hdf5, group=dsetname%chars())
  !!
  !! realVec
  !!
  dsetname=trim(group)//"/realVec"
  IF( hdf5%pathExists(dsetname%chars())) &
    & CALL ImportRealVector( obj=obj%realVec, hdf5=hdf5, &
    & group=dsetname%chars() )
  !!
  !! info
  !!
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing VectorField_ [OK!]")
  !!
  !!
  !!
END PROCEDURE vField_Import

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Export
  CHARACTER(LEN=*), PARAMETER :: myName = "vField_Export"
  TYPE(String) :: dsetname, strval
  !!
  !! info
  !!
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Exporting VectorField_")
  !!
  !! main program
  !!
  IF (.NOT. obj%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'VectorField_ object is not initiated, initiate it first')
  !!
  !! check
  !!
  IF (.NOT. hdf5%isOpen()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF
  !!
  !! check
  !!
  IF (.NOT. hdf5%isWrite()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have write permission')
  END IF
  !!
  !! fieldType
  !!
  dsetname=trim(group)//"/fieldType"
  strval = FIELD_TYPE_NAME( obj%fieldType )
  CALL hdf5%write(dsetname=dsetname%chars(),vals=strval)
  !!
  !! name
  !!
  dsetname = trim(group)//"/name"
  CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%name)
  !!
  !! engine
  !!
  dsetname = trim(group)//"/engine"
  CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%engine)
  !!
  !! tSize
  !!
  dsetname = trim(group)//"/tSize"
  CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%tSize)
  !!
  !! spaceCompo
  !!
  dsetname = trim(group)//"/spaceCompo"
  CALL hdf5%write(dsetname=dsetname%chars(), vals=obj%spaceCompo)
  !!
  !! dof
  !!
  dsetname = trim(group)//"/dof"
  CALL ExportDOF(obj=obj%dof, hdf5=hdf5, group=dsetname%chars())
  !!
  !! realVec
  !!
  dsetname = trim(group)//"/realVec"
  CALL ExportRealVector(obj=obj%realVec, hdf5=hdf5, &
    & group=dsetname%chars())
  !!
  !! info
  !!
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Exporting VectorField_ [OK!]")
  !!
  !!
  !!
END PROCEDURE vField_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
