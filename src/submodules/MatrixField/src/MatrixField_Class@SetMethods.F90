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
CHARACTER(*), PARAMETER :: myName = "obj_Set1"
INTEGER(I4B) :: val1, val2, val3

! check: this routine should not be called for rectangle matrix
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine should not be called for rectangle matrix')
END IF
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

IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat,  &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode),  &
    & VALUE=VALUE, &
    & storageFMT=storageFMT, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat,  &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & VALUE=VALUE, &
    & storageFMT=storageFMT)
END IF
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2"

IF (PRESENT(addContribution)) THEN
  ! Add
  IF (PRESENT(globalNode)) THEN
    ! check: this routine should not be called for rectangle matrix
    IF (obj%isRectangle) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'This routine should not be called for rectangle matrix')
    ELSE
      CALL add(obj=obj%mat,  &
        & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
        & scale=INPUT(default=1.0_DFP, option=scale), &
        & VALUE=VALUE)
    END IF
  ELSE
    CALL add(obj=obj%mat,  &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  ! Set
ELSE
  IF (PRESENT(globalNode)) THEN
    ! check: this routine should not be called for rectangle matrix
    IF (obj%isRectangle) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'This routine should not be called for rectangle matrix')
    ELSE
      CALL Set(obj=obj%mat, &
        & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
        & VALUE=VALUE)
    END IF
  ELSE
    CALL Set(obj=obj%mat, VALUE=VALUE)
  END IF
END IF
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
IF (PRESENT(addContribution)) THEN
  IF (obj%isRectangle) THEN
    CALL add(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
    CALL add(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
ELSE
  IF (obj%isRectangle) THEN
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE)
  ELSE
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE)
  END IF
END IF
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
    CALL add(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
    CALL add(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
  IF (obj%isRectangle) THEN
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & VALUE=VALUE)
  ELSE
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & VALUE=VALUE)
  END IF

  !
END IF
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
    CALL add(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
    CALL add(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
  IF (obj%isRectangle) THEN
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE)
  ELSE
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE)
  END IF
  !
END IF
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
    CALL add(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
    CALL add(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
  IF (obj%isRectangle) THEN
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domains(1)%ptr%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domains(2)%ptr%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE)
  ELSE
    CALL Set(obj=obj%mat, &
      & inodenum=obj%domain%getLocalNodeNumber(inodenum), &
      & jnodenum=obj%domain%getLocalNodeNumber(jnodenum), &
      & ivar=ivar, &
      & jvar=jvar, &
      & idof=idof, &
      & jdof=jdof, &
      & VALUE=VALUE)
  END IF
  !
END IF
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
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
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
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
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
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
  ELSE
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
  END IF
  !
END IF
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
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
    & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
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
    & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
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
  ELSE
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
  END IF
  !
END IF
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
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
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
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
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
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
  ELSE
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
  END IF
  !
END IF
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
IF (PRESENT(addContribution)) THEN
  !
  IF (obj%isRectangle) THEN
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
      & scale=INPUT(default=1.0_DFP, option=scale))
  ELSE
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
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
  !
ELSE
  !
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
  ELSE
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
  END IF
  !
END IF
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set11

END SUBMODULE SetMethods
