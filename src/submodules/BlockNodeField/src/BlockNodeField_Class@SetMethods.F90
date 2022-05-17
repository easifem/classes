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

SUBMODULE(BlockNodeField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set1
  IF( PRESENT( addContribution ) ) THEN
    CALL add( &
      & obj%realVec, &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP) &
      & )
  ELSE
    CALL set( obj%realVec, value=value )
  END IF
END PROCEDURE bnField_set1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set2
  IF( PRESENT( addContribution ) ) THEN
    CALL add( &
      & obj%realVec, &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP) &
      & )
  ELSE
    CALL set( obj%realVec, value=value )
  END IF
END PROCEDURE bnField_set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set3
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    !!
    !!
    CALL Add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( [globalNode] ), &
      & value=[value], &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & idof=idof )
    !!
  ELSE
    !!
    !!
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( [globalNode] ), &
      & value=[value], &
      & ivar=ivar, &
      & idof=idof )
    !!
  END IF
  !!
END PROCEDURE bnField_set3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set4
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    !!
    !!
    CALL add( &
      & obj=obj%realVec, &
      & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP) &
      & )
    !!
    !!
    !!
  ELSE
    !!
    CALL set( &
      & obj=obj%realVec, &
      & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar ), &
      & value=value )
    !!
  END IF
  !!
END PROCEDURE bnField_set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set5
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    !!
    !!
    CALL add( &
      & obj=obj%realVec, &
      & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP) &
      & )
    !!
    !!
    !!
  ELSE
    !!
    CALL set( &
      & obj=obj%realVec, &
      & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar ), &
      & value=value )
    !!
  END IF
  !!
END PROCEDURE bnField_set5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set6
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & idof=idof )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & idof=idof )
    !!
  END IF
  !!
END PROCEDURE bnField_set6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set7
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & idof=idof )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & idof=idof )
    !!
  END IF
  !!
END PROCEDURE bnField_set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set8
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set9
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set10
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set11
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set12
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set13
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set14
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set15
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set16
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & scale=INPUT(option=scale, default=1.0_DFP), &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  ELSE
    !!
    CALL Set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & value=value, &
      & ivar=ivar, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo &
      & )
    !!
  END IF
  !!
END PROCEDURE bnField_set16

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set17
  IF( PRESENT( addContribution ) ) THEN
    CALL AXPY( X=obj2%realvec, Y=obj%realvec, A=scale )
  ELSE
    obj%realVec = obj2%realVec
  END IF
END PROCEDURE bnField_set17

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_assign
  CALL set( obj%realVec, value=value )
END PROCEDURE bnField_assign

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods