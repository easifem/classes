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

MODULE MatrixFieldUtility
USE BaseMethod
USE MatrixField_Class
USE HDF5File_Class
USE HDF5File_Method
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
IMPLICIT NONE
PRIVATE

PUBLIC :: Export_CheckError
PUBLIC :: Import_CheckError
PUBLIC :: Export_Header
PUBLIC :: Import_Header
PUBLIC :: Export_PhysicalVar
PUBLIC :: Import_PhysicalVar

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Import_PhysicalVar(obj, hdf5, group, myName, modName, &
  & matrixProp, tvar1, tvar2, name1, name2, spaceCompo1, spaceCompo2, &
  & timeCompo1, timeCompo2)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: modName
  TYPE(String), INTENT(INOUT) :: matrixProp
  INTEGER(I4B), INTENT(INOUT) :: tvar1, tvar2
  TYPE(String), INTENT(INOUT) :: name1, name2
  INTEGER(I4B), INTENT(INOUT) :: spaceCompo1, spaceCompo2
  INTEGER(I4B), INTENT(INOUT) :: timeCompo1, timeCompo2
  !
  ! internal variables
  !
  TYPE(DOF_), POINTER :: dofobj
  TYPE(String) :: dsetname
  INTEGER(I4B) :: ii
  !
  dofobj => NULL()
  !
  IF (matrixProp .EQ. "RECTANGLE") THEN
    !
    ! tPhysicalVarNames
    !
    dsetname = TRIM(group)//"/ivar/tPhysicalVarNames"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar1)
    ELSE
      tvar1 = 1_I4B
    END IF
    !
    ! physicalVarName
    !
    DO ii = 1, tvar1
      dsetname = TRIM(group)//"/ivar/physicalVarName"//TOSTRING(ii)
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=TRIM(dsetname%chars()), &
        & vals=name1)
      ELSE
        CALL e%raiseError(modName//'::'//myName//' - '// &
          & dsetname%chars()//' not found!')
      END IF
    END DO
    !
    ! spaceCompo
    !
    dsetname = TRIM(group)//"/ivar/spaceCompo"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo1)
    ELSE
      spaceCompo1 = 1_I4B
    END IF
    !
    ! timeCompo
    !
    dsetname = TRIM(group)//"/ivar/timeCompo"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo1)
    ELSE
      timeCompo1 = 1_I4B
    END IF
    !
    !
    ! tPhysicalVarNames
    !
    dsetname = TRIM(group)//"/jvar/tPhysicalVarNames"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar2)
    ELSE
      tvar2 = 1_I4B
    END IF
    !
    ! physicalVarName
    !
    DO ii = 1, tvar2
      dsetname = TRIM(group)//"/jvar/physicalVarName"//TOSTRING(ii)
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=TRIM(dsetname%chars()), &
        & vals=name2)
      ELSE
        CALL e%raiseError(modName//'::'//myName//' - '// &
          & dsetname%chars()//' not found!')
      END IF
    END DO
    !
    ! spaceCompo
    !
    dsetname = TRIM(group)//"/jvar/spaceCompo"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo2)
    ELSE
      spaceCompo2 = 1_I4B
    END IF
    !
    ! timeCompo
    !
    dsetname = TRIM(group)//"/jvar/timeCompo"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo2)
    ELSE
      timeCompo2 = 1_I4B
    END IF
    !
  ELSE
    !
    ! tPhysicalVarNames
    !
    dsetname = TRIM(group)//"/tPhysicalVarNames"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar1)
    ELSE
      tvar1 = 1_I4B
    END IF
    !
    tvar2 = tvar1
    !
    ! physicalVarName
    !
    DO ii = 1, tvar1
      dsetname = TRIM(group)//"/physicalVarName"//TOSTRING(ii)
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=TRIM(dsetname%chars()), &
        & vals=name1)
      ELSE
        CALL e%raiseError(modName//'::'//myName//' - '// &
          & dsetname%chars()//' not found!')
      END IF
    END DO
    !
    name2 = name1
    !
    ! spaceCompo
    !
    dsetname = TRIM(group)//"/spaceCompo"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo1)
    ELSE
      spaceCompo1 = 1_I4B
    END IF
    !
    spaceCompo2 = spaceCompo1
    !
    !
    ! timeCompo
    !
    dsetname = TRIM(group)//"/timeCompo"
    IF (hdf5%pathExists(dsetname%chars())) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo1)
    ELSE
      timeCompo1 = 1_I4B
    END IF
    !
    timeCompo2 = timeCompo1
    !
  END IF
END SUBROUTINE Import_PhysicalVar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Import_Header(  &
  & obj, &
  & hdf5, &
  & group, &
  & modName, &
  & myName, &
  & fieldType, &
  & name, &
  & engine, &
  & matrixProp, &
  & isRectangle)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: modName
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(INOUT) :: fieldType
  TYPE(String), INTENT(INOUT) :: name, engine, matrixProp
  LOGICAL(LGT), INTENT(OUT) :: isRectangle
  !
  TYPE(String) :: dsetname, strval

  ! fieldType
  dsetname = TRIM(group)//"/fieldType"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    fieldType = FIELD_TYPE_NUMBER(strval%chars())
  ELSE
    fieldType = FIELD_TYPE_NORMAL
  END IF

  ! name
  dsetname = TRIM(group)//"/name"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset name should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=name)
  END IF

  ! engine
  dsetname = TRIM(group)//"/engine"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    engine = "NATIVE_SERIAL"
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=engine)
  END IF

  ! matrixProp
  dsetname = TRIM(group)//"/matrixProp"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset matrixProp should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=matrixProp)
  END IF

  ! isRectangle
  dsetname = TRIM(group)//"/isRectangle "
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    isRectangle = .FALSE.
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=isRectangle)
  END IF
END SUBROUTINE Import_Header

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Import_CheckError(obj, hdf5, group, myName, modName)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: modName

  ! main program
  IF (obj%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The instance of MatrixField_ is already initiated')

  ! print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Importing an Instance of MatrixField_")

  ! check
  IF (.NOT. hdf5%isOpen()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF

  ! check
  IF (.NOT. hdf5%isRead()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have read permission')
  END IF

END SUBROUTINE Import_CheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Export_CheckError(obj, hdf5, group, myName, modName)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: modName
  !
  IF (.NOT. obj%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Instnace of MatrixField_ is not initiated')
  !
  ! print info
  !
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "Exporting Instance of MatrixField_")
  !
  ! check
  !
  IF (.NOT. hdf5%isOpen()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF
  !
  ! check
  !
  IF (.NOT. hdf5%isWrite()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have write permission')
  END IF
  !
END SUBROUTINE Export_CheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Export_Header(obj, hdf5, group, dname, matprop)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  TYPE(String), INTENT(INOUT) :: dname
  TYPE(String), INTENT(INOUT) :: matprop

  ! isPmatInitiated
  dname = TRIM(group)//"/isPmatInitiated"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%isPmatInitiated)

  ! isRectangle
  dname = TRIM(group)//"/isRectangle"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=obj%isRectangle)

  ! matrixProp
  dname = TRIM(group)//"/matrixProp"
  matprop = STRING(.MatrixProp.obj%mat)
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
    & vals=matprop)

  ! physical variables from MatrixFieldUtility
  CALL Export_PhysicalVar(obj=obj, hdf5=hdf5, group=group, &
    & dname=dname, matprop=matprop)

END SUBROUTINE Export_Header

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Export_PhysicalVar(obj, hdf5, group, dname, matprop)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  TYPE(String), INTENT(INOUT) :: dname
  TYPE(String), INTENT(INOUT) :: matprop

  ! internal variables
  TYPE(DOF_), POINTER :: dofobj
  INTEGER(I4B) :: ii

  dofobj => NULL()

  IF (matprop .EQ. "RECTANGLE") THEN
    dofobj => NULL()
    dofobj => getDOFPointer(obj%mat, 1)
    dname = TRIM(group)//"/ivar/tPhysicalVarNames"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.tNames.dofobj))

    DO ii = 1, (.tNames.dofobj)
      dname = TRIM(group)//"/ivar/physicalVarName"//TOSTRING(ii)
      CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=STRING(dofobj.Names.ii))
    END DO

    dname = TRIM(group)//"/ivar/spaceCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.SpaceComponents.dofobj))

    dname = TRIM(group)//"/ivar/timeCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.TimeComponents.dofobj))

    dofobj => NULL()
    dofobj => getDOFPointer(obj%mat, 2)
    dname = TRIM(group)//"/jvar/tPhysicalVarNames"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.tNames.dofobj))

    DO ii = 1, (.tNames.dofobj)
      dname = TRIM(group)//"/jvar/physicalVarName"//TOSTRING(ii)
      CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=STRING(dofobj.Names.ii))
    END DO

    dname = TRIM(group)//"/jvar/spaceCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.SpaceComponents.dofobj))

    dname = TRIM(group)//"/jvar/timeCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.TimeComponents.dofobj))

  ELSE

    dofobj => NULL()
    dofobj => getDOFPointer(obj%mat, 1)
    dname = TRIM(group)//"/tPhysicalVarNames"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.tNames.dofobj))

    DO ii = 1, (.tNames.dofobj)
      dname = TRIM(group)//"/physicalVarName"//TOSTRING(ii)
      CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=STRING(dofobj.Names.ii))
    END DO

    dname = TRIM(group)//"/spaceCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.SpaceComponents.dofobj))

    dname = TRIM(group)//"/timeCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.TimeComponents.dofobj))

  END IF
END SUBROUTINE Export_PhysicalVar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! SUBROUTINE LIS_Matrix_Initiate(obj, nrow)
!   CLASS(MatrixField_), INTENT(INOUT) :: obj
!   INTEGER(I4B), INTENT(IN) :: nrow
! !
!   INTEGER(I4B) :: ierror
!   INTEGER(I4B) :: nnz
!
! #ifdef USE_LIS
!   IF (obj%engine%chars() .EQ. "LIS_OMP") THEN
!     obj%comm = 0_I4B
!     CALL lis_matrix_create(obj%comm, obj%lis_ptr, ierror)
!     CALL CHKERR(ierror)
!     CALL lis_matrix_set_size(obj%lis_ptr, 0, nrow, ierror)
!     CALL CHKERR(ierror)
!
!     nnz = getNNZ(obj%mat)
!     obj%lis_ia = obj%mat%csr%ia - 1
!     obj%lis_ja = obj%mat%csr%ja - 1
!
!     CALL lis_matrix_set_csr( &
!     & nnz, &
!     & obj%lis_ia, &
!     & obj%lis_ja, &
!     & obj%mat%a, &
!     & obj%lis_ptr, &
!     & ierror)
!     CALL CHKERR(ierror)
!
!     CALL lis_matrix_assemble(obj%lis_ptr, ierror)
!     CALL CHKERR(ierror)
!
!     CALL lis_matrix_get_size(obj%lis_ptr, obj%local_n, obj%global_n, ierror)
!     CALL CHKERR(ierror)
!
!     CALL lis_matrix_get_range(obj%lis_ptr, obj%is, obj%ie, ierror)
!     CALL CHKERR(ierror)
!
!   END IF
! #endif
! END SUBROUTINE LIS_Matrix_Initiate

END MODULE MatrixFieldUtility
