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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: val1, val2, val3
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

! check: this routine should not be called for rectangle matrix
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

#ifdef DEBUG_VER
! check:
val1 = SIZE(VALUE, 1)
val2 = SIZE(VALUE, 2)
val3 = (.tdof.obj%mat%csr%idof) * SIZE(globalNode)
IF (&
    &      val1 .NE. val2  &
    & .OR. val1 .NE. val3) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & "value is not square matrix, or its shape is inconsistent "// &
   & "with the degree of freedom stored in MatrixField")
END IF
#endif

IF (add0) THEN
  CALL add(obj=obj%mat,  &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode),  &
    & VALUE=VALUE, &
    & storageFMT=storageFMT, &
    & scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat,  &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & VALUE=VALUE, &
  & storageFMT=storageFMT)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
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
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)
isnode = PRESENT(globalNode)

! check: this routine should not be called for rectangle matrix
IF (isnode .AND. obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

! Add
IF (add0 .AND. isnode) THEN
  CALL add(obj=obj%mat,  &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & scale=scale0, &
    & VALUE=VALUE)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, VALUE=VALUE, scale=scale0)
  RETURN
END IF

! Set
IF (isnode) THEN
  ! check: this routine should not be called for rectangle matrix
  CALL Set(obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & idof=idof, &
  & jdof=jdof, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & idof=idof, &
  & jdof=jdof, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & idof=idof, &
  & jdof=jdof, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
  & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE, &
  & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE, &
  & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (add0 .AND. obj%isRectangle) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (add0) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

IF (obj%isRectangle) THEN
  CALL Set(obj=obj%mat, &
  & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
  & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
REAL(DFP) :: scale0
LOGICAL(LGT) :: add0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set11

END SUBMODULE SetMethods
