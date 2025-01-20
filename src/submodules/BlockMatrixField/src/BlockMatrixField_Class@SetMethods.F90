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

SUBMODULE(BlockMatrixField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1"
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat,  &
    & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode),  &
    & VALUE=VALUE, &
    & storageFMT=storageFMT, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat,  &
    & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode),  &
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
  IF (PRESENT(globalNode)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'This routine is not callable for BlockMatrixField_ Class')
  ELSE
    CALL add(obj=obj%mat,  &
      & VALUE=VALUE, &
      & scale=INPUT(default=1.0_DFP, option=scale))
  END IF
ELSE
  IF (PRESENT(globalNode)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'This routine is not callable for BlockMatrixField_ Class')
  ELSE
    CALL Set(obj=obj%mat, VALUE=VALUE)
  END IF
END IF
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is not callable for BlockMatrixField_ Class')
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
IF (PRESENT(addContribution)) THEN
  CALL add(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=INPUT(default=1.0_DFP, option=scale))
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(inodenum), &
    & jnodenum=obj%domains(jvar)%ptr%getLocalNodeNumber(jnodenum), &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_Set10

END SUBMODULE SetMethods
