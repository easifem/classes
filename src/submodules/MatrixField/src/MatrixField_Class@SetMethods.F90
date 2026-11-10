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

SUBMODULE(MatrixField_Class) SetMethods
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: DOF_
USE CSRMatrix_Method, ONLY: Add
USE CSRMatrix_Method, ONLY: AddToSTMatrix
USE CSRMatrix_Method, ONLY: GetDOFPointer
USE CSRMatrix_Method, ONLY: Set
USE CSRMatrix_Method, ONLY: SetToSTMatrix
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: OPERATOR(.spacecomponents.)
USE DOF_Method, ONLY: OPERATOR(.tdof.)
USE DOF_Method, ONLY: OPERATOR(.timecomponents.)
USE InputUtility, ONLY: Input
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MatrixField_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: val1, val2, val3
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! check: this routine should not be called for rectangle matrix
#ifdef DEBUG_VER
isok = .NOT. obj%isRectangle
CALL AssertError1(isok, myName, &
                  'This routine is not for rectangle matrix')
#endif

#ifdef DEBUG_VER
val1 = SIZE(VALUE, 1)
val2 = SIZE(VALUE, 2)
CALL AssertError2(val1, val2, myName, &
                  "a=size(value,1), b=size(value,2)")
#endif

#ifdef DEBUG_VER
val3 = (.tdof. obj%mat%csr%idof) * SIZE(globalNode)
isok = val1 .EQ. val3
CALL AssertError2(val1, val3, myName, &
                  "a=size(value,1), "// &
                  "b=(.tdof. obj%mat%csr%idof) * SIZE(globalNode)")
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, VALUE=VALUE, nodenum=globalNode, scale=scale0, &
           storageFMT=storageFMT)
ELSE
  CALL Set(obj=obj%mat, nodenum=globalNode, VALUE=VALUE, &
           storageFMT=storageFMT)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
LOGICAL(LGT) :: add0, isnode
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (PRESENT(islocal)) THEN
#include "./localNodeError.F90"
END IF
#endif

isnode = PRESENT(globalNode)

#ifdef DEBUG_VER
! check: this routine should not be called for rectangle matrix
IF (isnode .AND. obj%isRectangle) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'This routine is not for rectangle matrix')
END IF
#endif

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)
! Add
IF (add0 .AND. isnode) THEN
  CALL Add(obj=obj%mat, nodenum=globalNode, scale=scale0, VALUE=VALUE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, VALUE=VALUE, scale=scale0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

! Set
IF (isnode) THEN
  ! check: this routine should not be called for rectangle matrix
  CALL Set(obj=obj%mat, nodenum=globalNode, VALUE=VALUE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Set(obj=obj%mat, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
#endif

LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           idof=idof, jdof=jdof, VALUE=VALUE, scale=scale0)
ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, idof=idof, &
           jdof=jdof, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, ivar=ivar, &
           jvar=jvar, VALUE=VALUE, scale=scale0)
ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, ivar=ivar, &
           jvar=jvar, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE, &
           scale=scale0)
ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE, &
           scale=scale0)
ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, VALUE=VALUE, &
           scale=scale0)
ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, VALUE=VALUE)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, VALUE=VALUE, &
           scale=scale0)

ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, ivar=ivar, &
           jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
           jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE, &
           scale=scale0)
ELSE

  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
#endif
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

IF (add0) THEN
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, &
           VALUE=VALUE, scale=scale0)
ELSE
  CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, &
           itimecompo=itimecompo, jspacecompo=jspacecompo, &
           jtimecompo=jtimecompo, VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
#endif
REAL(DFP) :: scale0
LOGICAL(LGT) :: add0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

add0 = Input(default=math%no, option=addContribution)
scale0 = Input(default=math%one, option=scale)

SELECT TYPE (VALUE)
CLASS IS (MatrixField_)

  IF (add0) THEN
    CALL Add(obj=obj%mat, VALUE=VALUE%mat, scale=scale0, &
             isSameStructure=math%yes)
  ELSE
    CALL Set(obj=obj%mat, VALUE=VALUE%mat, scale=scale0)
  END IF

CLASS DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'This method is callable for  MatrixField_::Value only')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                          SetFromSTMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromSTMatrix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFromSTMatrix()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%isRectangle
CALL AssertError1(isok, myName, &
                  'This routine is not for rectangle matrix.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFromSTMatrix

!----------------------------------------------------------------------------
!                                                             SetToSTMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetToSTMatrix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetToSTMatrix()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%isRectangle
CALL AssertError1(isok, myName, &
                  'This routine is not for rectangle matrix.')
#endif

! check that obj has space-time components
! check that number of space components is same in both obj and value
! Make sure that the storage patter is fmt_dof in both obj and value

IF (addContribution) THEN
  CALL AddToSTMatrix( &
    obj=obj%mat, VALUE=VALUE%mat, itimecompo=itimecompo, &
    jtimecompo=jtimecompo, scale=scale)
ELSE

  CALL SetToSTMatrix( &
    obj=obj%mat, VALUE=VALUE%mat, itimecompo=itimecompo, &
    jtimecompo=jtimecompo, scale=scale)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetToSTMatrix

!
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
