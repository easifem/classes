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
USE AbstractField_Class, ONLY: AbstractField_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: math => TypeMathOpt
USE CSRMatrix_Method, ONLY: GetDOFPointer
USE CSRMatrix_Method, ONLY: OPERATOR(.MatrixProp.)
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: OPERATOR(.Names.)
USE DOF_Method, ONLY: OPERATOR(.SpaceComponents.)
USE DOF_Method, ONLY: OPERATOR(.TimeComponents.)
USE DOF_Method, ONLY: OPERATOR(.tNames.)
USE ExceptionHandler_Class, ONLY: e
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE GlobalData, ONLY: LGT, I4B, DFP
USE HDF5File_Class, ONLY: HDF5File_
USE MatrixField_Class, ONLY: MatrixField_
USE String_Class, ONLY: String
IMPLICIT NONE

PRIVATE
PUBLIC :: Export_CheckError
PUBLIC :: Import_CheckError
PUBLIC :: Export_Header
PUBLIC :: Import_Header
PUBLIC :: Export_PhysicalVar
PUBLIC :: Import_PhysicalVar

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "MatrixFieldUtility.F90"
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Import_PhysicalVar( &
  obj, hdf5, group, matrixProp, tvar1, tvar2, name1, name2, &
  spaceCompo1, spaceCompo2, timeCompo1, timeCompo2)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  TYPE(String), INTENT(INOUT) :: matrixProp
  !! matrix properties
  INTEGER(I4B), INTENT(INOUT) :: tvar1, tvar2
  !! total physical variable in row and column dimension
  TYPE(String), INTENT(INOUT) :: name1, name2
  !! name of physical variable in row and column dimension
  INTEGER(I4B), INTENT(INOUT) :: spaceCompo1, spaceCompo2
  INTEGER(I4B), INTENT(INOUT) :: timeCompo1, timeCompo2

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Import_PhysicalVar()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(DOF_), POINTER :: dofobj
  TYPE(String) :: dsetname
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dofobj => NULL()

  IF (matrixProp .EQ. "RECTANGLE") THEN
    ! tPhysicalVarNames
    dsetname = TRIM(group)//"/ivar/tPhysicalVarNames"
    isok = hdf5%pathExists(dsetname%Chars())
    tvar1 = math%one_i
    IF (isok) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar1)
    END IF

    ! physicalVarName
    DO ii = 1, tvar1
      dsetname = TRIM(group)//"/ivar/physicalVarName"//ToString(ii)
#ifdef DEBUG_VER
      isok = hdf5%pathExists(dsetname%Chars())
      CALL AssertError1(isok, myName, &
                        dsetname%Chars()//" not found.")
#endif
      CALL hdf5%READ(dsetname=TRIM(dsetname%chars()), &
                     vals=name1)
    END DO

    ! spaceCompo
    dsetname = TRIM(group)//"/ivar/spaceCompo"
    spaceCompo1 = math%one_i
    isok = hdf5%pathExists(dsetname%chars())
    IF (isok) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo1)
    END IF

    ! timeCompo
    dsetname = TRIM(group)//"/ivar/timeCompo"
    timeCompo1 = math%one_i
    isok = hdf5%pathExists(dsetname%chars())
    IF (isok) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo1)
    END IF

    ! tPhysicalVarNames
    dsetname = TRIM(group)//"/jvar/tPhysicalVarNames"
    tvar2 = math%one_i
    isok = hdf5%pathExists(dsetname%chars())
    IF (isok) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar2)
    END IF

    ! physicalVarName
    DO ii = 1, tvar2
      dsetname = TRIM(group)//"/jvar/physicalVarName"//ToString(ii)
#ifdef DEBUG_VER
      isok = hdf5%pathExists(dsetname%Chars())
      CALL AssertError1(isok, myName, &
                        dsetname%Chars()//" not found.")
#endif
      CALL hdf5%READ(dsetname=TRIM(dsetname%chars()), &
                     vals=name2)
    END DO

    ! spaceCompo
    dsetname = TRIM(group)//"/jvar/spaceCompo"
    spaceCompo2 = math%one_i
    isok = hdf5%pathExists(dsetname%Chars())
    IF (isok) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo2)
    END IF

    ! timeCompo
    dsetname = TRIM(group)//"/jvar/timeCompo"
    timeCompo2 = math%one_i
    isok = hdf5%pathExists(dsetname%Chars())
    IF (isok) THEN
      CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo2)
    END IF

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! tPhysicalVarNames
  dsetname = TRIM(group)//"/tPhysicalVarNames"
  tvar1 = math%one_i
  isok = hdf5%pathExists(dsetname%chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar1)
  END IF

  tvar2 = tvar1

  ! physicalVarName
  DO ii = 1, tvar1
    dsetname = TRIM(group)//"/physicalVarName"//ToString(ii)
#ifdef DEBUG_VER
    isok = hdf5%pathExists(dsetname%Chars())
    CALL AssertError1(isok, myName, &
                      dsetname//" not found.")
#endif
    CALL hdf5%READ(dsetname=dsetname%Chars(), vals=name1)
  END DO

  name2 = name1

  dsetname = TRIM(group)//"/spaceCompo"
  spaceCompo1 = math%one_i
  isok = hdf5%pathExists(dsetname%chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo1)
  END IF

  spaceCompo2 = spaceCompo1

  dsetname = TRIM(group)//"/timeCompo"
  timeCompo1 = math%one_i
  isok = hdf5%pathExists(dsetname%chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo1)
  END IF

  timeCompo2 = timeCompo1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Import_PhysicalVar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Import_Header( &
  obj, hdf5, group, fieldType, name, engine, matrixProp, isRectangle)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), INTENT(INOUT) :: fieldType
  TYPE(String), INTENT(INOUT) :: name, engine, matrixProp
  LOGICAL(LGT), INTENT(OUT) :: isRectangle
  TYPE(String) :: dsetname, strval

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Import_Header()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! fieldType
  dsetname = TRIM(group)//"/fieldType"
  isok = hdf5%pathExists(dsetname%Chars())
  fieldType = TypeField%constant
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    fieldType = TypeField%ToNumber(strval%chars())
  END IF

  ! name
  dsetname = TRIM(group)//"/name"
#ifdef DEBUG_VER
  isok = hdf5%pathExists(dsetname%Chars())
  CALL AssertError1(isok, myName, &
                    dsetname//" should be present.")
#endif
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=name)

  ! engine
  dsetname = TRIM(group)//"/engine"
  isok = hdf5%pathExists(dsetname%Chars())
  engine = "NATIVE_SERIAL"
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%Chars(), vals=engine)
  END IF

  ! matrixProp
  dsetname = TRIM(group)//"/matrixProp"
#ifdef DEBUG_VER
  isok = hdf5%pathExists(dsetname%Chars())
  CALL AssertError1(isok, myName, &
                    dsetname//" should be present.")
#endif
  CALL hdf5%READ(dsetname=dsetname%Chars(), vals=matrixProp)

  ! isRectangle
  dsetname = TRIM(group)//"/isRectangle "
  isRectangle = math%no
  isok = hdf5%pathExists(dsetname%Chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%Chars(), vals=isRectangle)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Import_Header

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Import_CheckError(obj, hdf5, group)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Import_CheckError()"
  LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = .NOT. obj%IsInitiated()
  CALL AssertError1(isok, myName, &
                    'The instance of MatrixField_ is already initiated')
#endif

#ifdef DEBUG_VER
  isok = hdf5%isOpen()
  CALL AssertError1(isok, myName, &
                    'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
  isok = hdf5%isRead()
  CALL AssertError1(isok, myName, &
                    'HDF5 file does not have read permission')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Import_CheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Export_CheckError(obj, hdf5, group)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Export_CheckError()"
  LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, &
                    'The instance of MatrixField_ is not initiated')
#endif

#ifdef DEBUG_VER
  isok = hdf5%isOpen()
  CALL AssertError1(isok, myName, &
                    'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
  isok = hdf5%isWrite()
  CALL AssertError1(isok, myName, &
                    'HDF5 file does not have write permission')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
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

  !internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Export_Header()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! isPmatInitiated
  dname = TRIM(group)//"/isPmatInitiated"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                  vals=obj%isPmatInitiated)

  ! isRectangle
  dname = TRIM(group)//"/isRectangle"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                  vals=obj%isRectangle)

  ! matrixProp
  dname = TRIM(group)//"/matrixProp"
  matprop = STRING(.MatrixProp.obj%mat)
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                  vals=matprop)

  ! physical variables from MatrixFieldUtility
  CALL Export_PhysicalVar(obj=obj, hdf5=hdf5, group=group, &
                          dname=dname, matprop=matprop)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Export_PhysicalVar()"
#endif
  TYPE(DOF_), POINTER :: dofobj
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dofobj => NULL()

  IF (matprop .EQ. "RECTANGLE") THEN
    dofobj => NULL()
    dofobj => getDOFPointer(obj%mat, 1)
    dname = TRIM(group)//"/ivar/tPhysicalVarNames"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                    vals=(.tNames.dofobj))

    DO ii = 1, (.tNames.dofobj)
      dname = TRIM(group)//"/ivar/physicalVarName"//ToString(ii)
      CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                      vals=STRING(dofobj.Names.ii))
    END DO

    dname = TRIM(group)//"/ivar/spaceCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                    vals=(.SpaceComponents.dofobj))

    dname = TRIM(group)//"/ivar/timeCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                    vals=(.TimeComponents.dofobj))

    dofobj => NULL()
    dofobj => getDOFPointer(obj%mat, 2)
    dname = TRIM(group)//"/jvar/tPhysicalVarNames"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                    vals=(.tNames.dofobj))

    DO ii = 1, (.tNames.dofobj)
      dname = TRIM(group)//"/jvar/physicalVarName"//ToString(ii)
      CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                      vals=STRING(dofobj.Names.ii))
    END DO

    dname = TRIM(group)//"/jvar/spaceCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.SpaceComponents.dofobj))

    dname = TRIM(group)//"/jvar/timeCompo"

    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
      & vals=(.TimeComponents.dofobj))

    RETURN
  END IF

  dofobj => NULL()
  dofobj => GetDOFPointer(obj%mat, 1)
  dname = TRIM(group)//"/tPhysicalVarNames"

  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                  vals=(.tNames.dofobj))

  DO ii = 1, (.tNames.dofobj)
    dname = TRIM(group)//"/physicalVarName"//ToString(ii)
    CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                    vals=STRING(dofobj.Names.ii))
  END DO

  dname = TRIM(group)//"/spaceCompo"

  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                  vals=(.SpaceComponents.dofobj))

  dname = TRIM(group)//"/timeCompo"

  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
                  vals=(.TimeComponents.dofobj))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Export_PhysicalVar

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END MODULE MatrixFieldUtility
