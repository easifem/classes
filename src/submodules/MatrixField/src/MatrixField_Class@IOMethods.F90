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
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
USE MatrixFieldUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Display
INTEGER(I4B) :: ii
!!
!! main
!!
IF (.NOT. obj%isInitiated) THEN
  CALL Display("MatrixField is not initiated", unitNo=unitNo)
  RETURN
END IF
!!
CALL Display("#"//TRIM(msg), unitNo=unitNo)
CALL Display(obj%name//'', msg="# name : ", unitNo=unitNo)
CALL Display(obj%fieldType, msg='# fieldType : ', unitNo=unitNo)
CALL Display(obj%engine, msg='# engine : ', unitNo=unitNo)
IF (obj%isRectangle) THEN
  CALL Display("# Shape: Rectangle", unitNo=unitNo)
ELSE
  CALL Display("# Shape: Square", unitNo=unitNo)
END IF
!!
IF (ASSOCIATED(obj%domain)) THEN
  CALL Display("# domain : ASSOCIATED", unitNo=unitNo)
ELSE
  CALL Display("# domain : NOT ASSOCIATED", unitNo=unitNo)
END IF
!!
IF (ALLOCATED(obj%domains)) THEN
  CALL Display("# domains : ALLOCATED [" &
    & //TOSTRING(SIZE(obj%domains)) &
    & //"]", unitNo=unitNo)
  DO ii = 1, SIZE(obj%domains)
    IF (ASSOCIATED(obj%domains(ii)%ptr)) THEN
      CALL Display("# domains("//TOSTRING(ii) &
        & //")%ptr : ASSOCIATED", unitNo=unitNo)
    ELSE
      CALL Display("# domains("//TOSTRING(ii)  &
        & //")%ptr : NOT ASSOCIATED", unitNo=unitNo)
    END IF
  END DO
ELSE
  CALL Display("# domains : NOT ALLOCATED", unitNo=unitNo)
END IF
!!
CALL Display(obj%mat, msg="# mat : ", unitNo=unitNo)
!!
END PROCEDURE mField_Display

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Export
CHARACTER(LEN=*), PARAMETER :: myName = "mField_Export"
TYPE(String) :: dname, matprop
TYPE(DOF_), POINTER :: dofobj
INTEGER(I4B) :: ii
!!
!! Check error
!!
CALL Export_CheckError(obj=obj, hdf5=hdf5, group=group, &
  & myName=myName, modName=modName)
!!
!! export header from MatrixFieldUtility
!!
CALL Export_Header(obj=obj, hdf5=hdf5, group=group, &
  & dname=dname, matprop=matprop)
!!
!! mat
!!
CALL ExportCSRMatrix(obj=obj%mat, hdf5=hdf5, group=TRIM(group)//"/mat")
!!
!! pmat
!!
CALL obj%ExportPmat(hdf5=hdf5, group=group)
!!
NULLIFY (dofobj)
!!
END PROCEDURE mField_Export

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ExportPmat
CHARACTER(LEN=*), PARAMETER :: myName = "mField_ExportPmat"
TYPE(String) :: dname
IF (obj%isPmatInitiated) THEN
  !!
  !! pmat/pmatName
  !!
  dname = TRIM(group)//"/pmat/pmatName"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%pmatName)
  !!
  !! pmat/nnz
  !!
  dname = TRIM(group)//"/pmat/nnz"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%nnz)
  !!
  !! pmat/ncol
  !!
  dname = TRIM(group)//"/pmat/ncol"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%ncol)
  !!
  !! pmat/nrow
  !!
  dname = TRIM(group)//"/pmat/nrow"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%nrow)
  !!
  !! pmat/isInitiated
  !!
  dname = TRIM(group)//"/pmat/isInitiated"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%isInitiated)
  !!
  !! pmat/lfil
  !!
  dname = TRIM(group)//"/pmat/lfil"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%lfil)
  !!
  !! pmat/mbloc
  !!
  dname = TRIM(group)//"/pmat/mbloc"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%mbloc)
  !!
  !! pmat/alpha
  !!
  dname = TRIM(group)//"/pmat/alpha"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%alpha)
  !!
  !! pmat/droptol
  !!
  dname = TRIM(group)//"/pmat/droptol"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%droptol)
  !!
  !! pmat/permtol
  !!
  dname = TRIM(group)//"/pmat/permtol"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%pmat%permtol)
  !!
  !! pmat/A
  !!
  IF (ALLOCATED(obj%pmat%A)) THEN
    dname = TRIM(group)//"/pmat/A"
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%A)
  END IF
  !!
  !! pmat/JA
  !!
  IF (ALLOCATED(obj%pmat%JA)) THEN
    dname = TRIM(group)//"/pmat/JA"
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%JA)
  END IF
  !!
  !! pmat/IA
  !!
  IF (ALLOCATED(obj%pmat%IA)) THEN
    dname = TRIM(group)//"/pmat/IA"
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%IA)
  END IF
  !!
  !! pmat/JU
  !!
  IF (ALLOCATED(obj%pmat%JU)) THEN
    dname = TRIM(group)//"/pmat/JU"
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%JU)
  END IF
  !!
  !! pmat/IPERM
  !!
  IF (ALLOCATED(obj%pmat%IPERM)) THEN
    dname = TRIM(group)//"/pmat/IPERM"
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%IPERM)
  END IF
  !!
  !! pmat/LEVS
  !!
  IF (ALLOCATED(obj%pmat%LEVS)) THEN
    dname = TRIM(group)//"/pmat/LEVS"
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%pmat%LEVS)
  END IF
  !!
END IF
!!
END PROCEDURE mField_ExportPmat

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Import
CHARACTER(LEN=*), PARAMETER :: myName = "mField_Import"
TYPE(String) :: strval, dsetname, name, matrixProp, engine
INTEGER(I4B) :: timeCompo1, spaceCompo1
INTEGER(I4B) :: timeCompo2, spaceCompo2
INTEGER(I4B) :: tvar1, tvar2
TYPE(String) :: name1, name2
CHARACTER(LEN=20) :: varnames(2)
INTEGER(I4B) :: fieldType
LOGICAL(LGT) :: isRectangle
TYPE(ParameterList_) :: param
!!
!! main program
!!
CALL Import_CheckError(obj=obj, hdf5=hdf5, group=group, &
  & myName=myName, modName=modName)
!!
!! Import header
!!
CALL Import_Header( &
  & obj=obj, hdf5=hdf5, group=group, &
  & modName=modName, myName=myName, &
  & fieldType=fieldType, name=name, engine=engine, &
  & matrixProp=matrixProp, &
  & isRectangle=isRectangle)
!!
!! mat
!!
dsetname = TRIM(group)//"/mat"
!!
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "Importing "//dsetname%chars())
!!
IF (hdf5%PathExists(dsetname%chars())) THEN
  !!
  obj%engine = engine
  obj%name = name
  obj%fieldType = fieldType
  obj%isRectangle = isRectangle
  !!
  IF (ASSOCIATED(obj%domain)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'obj%domain is associated, deallocate first')
  END IF
  !!
  IF (ALLOCATED(obj%domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'obj%domains is allocated, deallocate first')
  END IF
  !!
  IF (PRESENT(dom)) THEN
    obj%domain => dom
  ELSE IF (PRESENT(domains)) THEN
    ALLOCATE (obj%domains(2))
    obj%domains(1)%ptr => domains(1)%ptr
    obj%domains(2)%ptr => domains(2)%ptr
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'For non-rectangle matrix dom should be present, &
      & for rectangle matrix matrix domains should be present')
  END IF
  !!
  CALL ImportCSRMatrix(obj=obj%mat, &
    & hdf5=hdf5, group=dsetname%chars())
  !!
  obj%isInitiated = .TRUE.
  obj%isPmatInitiated = .FALSE.
  !!
ELSE
  !!
  !! Import Physical Variables
  !!
  CALL Import_PhysicalVar( &
    & obj=obj, hdf5=hdf5, &
    & group=group, myName=myName, &
    & modName=modName, matrixProp=matrixProp, &
    & tvar1=tvar1, tvar2=tvar2, name1=name1, &
    & name2=name2, spaceCompo1=spaceCompo1, &
    & spaceCompo2=spaceCompo2, &
    & timeCompo1=timeCompo1, &
    & timeCompo2=timeCompo2)
  !!
  varnames(1) = name1%chars()
  varnames(2) = name2%chars()
  !!
  CALL FPL_INIT(); CALL param%initiate()
  !!
  IF (matrixProp .EQ. "RECTANGLE") THEN
    !!
    CALL SetRectangleMatrixFieldParam( &
      & param=param, &
      & name=name%chars(), &
      & matrixProp=matrixProp%chars(), &
      & physicalVarNames=varnames, &
      & spaceCompo=[spaceCompo1, spaceCompo2], &
      & timeCompo=[timeCompo1, timeCompo2], &
      & fieldType=fieldType)
    !!
    CALL obj%Initiate(param=param, dom=domains)
    !!
  ELSE
    !!
    CALL SetMatrixFieldParam(param=param, &
      & name=name%chars(), &
      & matrixProp=matrixProp%chars(), &
      & spaceCompo=spaceCompo1, &
      & timeCompo=timeCompo1, &
      & fieldType=fieldType)
    !!
    CALL obj%Initiate(param=param, dom=dom)
    !!
  END IF
  !!
  CALL param%DEALLOCATE(); CALL FPL_FINALIZE()
  !!
END IF
!!
dsetname = TRIM(group)//"/pmat"
!!
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "Importing "//dsetname%chars())
!!
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL obj%ImportPmat(hdf5=hdf5, group=dsetname%chars(), &
    & dom=dom, domains=domains)
ELSE
  !Issue: #70
  CALL e%raiseDebug(modName//"::"//myName//" - "// &
    & "Issue #70: At this moment, we cannot &
    & create preconditioning matrix, when /pmat is absent. &
    & This routine needs further attention")
END IF
!!
END PROCEDURE mField_Import

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ImportPmat
CHARACTER(LEN=*), PARAMETER :: myName = "mField_ImportPmat"
TYPE(String) :: dsetname
!!
!! info
!!
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "Importing an Instance of MatrixFieldPrecondition_")
obj%isPmatInitiated = .TRUE.
!!
!! pmatName
!!
dsetname = TRIM(group)//"/pmatName"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%pmatName)
END IF
!!
!! nnz
!!
dsetname = TRIM(group)//"/nnz"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%nnz)
END IF
!!
!! ncol
!!
dsetname = TRIM(group)//"/ncol"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%ncol)
END IF
!!
!! nrow
!!
dsetname = TRIM(group)//"/nrow"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%nrow)
END IF
!!
!! isInitiated
!!
dsetname = TRIM(group)//"/isInitiated"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%isInitiated)
END IF
!!
!! lfil
!!
dsetname = TRIM(group)//"/lfil"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%lfil)
END IF
!!
!! mbloc
!!
dsetname = TRIM(group)//"/mbloc"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%mbloc)
END IF
!!
!! alpha
!!
dsetname = TRIM(group)//"/alpha"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%alpha)
END IF
!!
!! droptol
!!
dsetname = TRIM(group)//"/droptol"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%droptol)
END IF
!!
!! permtol
!!
dsetname = TRIM(group)//"/permtol"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%permtol)
END IF
!!
!! A
!!
dsetname = TRIM(group)//"/A"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%A)
END IF
!!
!! JA
!!
dsetname = TRIM(group)//"/JA"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%JA)
END IF
!!
!! IA
!!
dsetname = TRIM(group)//"/IA"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%IA)
END IF
!!
!! JU
!!
dsetname = TRIM(group)//"/JU"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%JU)
END IF
!!
!! IPERM
!!
dsetname = TRIM(group)//"/IPERM"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%IPERM)
END IF
!!
!! LEVS
!!
dsetname = TRIM(group)//"/LEVS"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
  & vals=obj%pmat%LEVS)
END IF
!!
END PROCEDURE mField_ImportPmat

!----------------------------------------------------------------------------
!                                                                      SPY
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_SPY
CALL SPY(obj=obj%mat, filename=filename, ext=ext)
END PROCEDURE mField_SPY

END SUBMODULE IOMethods
