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

SUBMODULE(VectorField_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get1
  INTEGER(I4B) :: localNode
  CHARACTER(LEN=*), PARAMETER :: myName = "vField_get1"
  !!
  !! main
  !!
  IF (PRESENT(globalNode)) THEN
    !!
    SELECT CASE (obj%fieldType)
    !!
    !!
    !!
    !!
    CASE (FIELD_TYPE_CONSTANT)
      !!
      CALL getValue( &
        & obj=obj%realvec, &
        & dofobj=obj%dof, &
        & idof=arange(1, obj%spaceCompo), &
        & value=value, &
        & nodenum=[1])
    !!
    !!
    !!
    !!
    CASE (FIELD_TYPE_NORMAL)
      !!
      CALL getValue( &
        & obj=obj%realvec, &
        & dofobj=obj%dof, &
        & idof=arange(1, obj%spaceCompo), &
        & value=value, &
        & nodenum=obj%domain%getLocalNodeNumber([globalNode]) )
      !!
    END SELECT
    !!
  END IF
  !!
  !!
  !!
  !!
  IF (PRESENT(spaceCompo)) THEN
    !!
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=spaceCompo, &
      & value=value )
    !!
  END IF
  !!
END PROCEDURE vField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get2
  !!
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=arange(1, obj%spaceCompo), &
    & value=value, &
    & force3D=force3D )
  !!
END PROCEDURE vField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get3
  REAL(DFP), ALLOCATABLE :: v(:)
  !!
  !! main
  !!
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=arange(1, obj%spaceCompo), &
    & value=v, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode))
  !!
  IF( PRESENT( force3D ) ) THEN
    CALL Reallocate( value, 3, SIZE( globalNode) )
    value( 1:obj%spaceCompo, : ) = &
      & RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)])
  ELSE
    value = RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)])
  END IF
  !!
  DEALLOCATE (v)
  !!
END PROCEDURE vField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get4
  !!
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & idof=spaceCompo, &
    & value=value, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode))
  !!
END PROCEDURE vField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get5
  !!
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & idof=spaceCompo, &
    & value=value, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode))
  !!
END PROCEDURE vField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get6
  !!
  INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
  !!
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode(jj) = ii
  END DO
  !!
  CALL obj%get(globalNode=globalNode, value=value)
  !!
END PROCEDURE vField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get7
  !!
  INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
  !!
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode(jj) = ii
  END DO
  !!
  CALL obj%get(globalNode=globalNode, value=value, spaceCompo=spaceCompo)
  !!
END PROCEDURE vField_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get8
  REAL(DFP), ALLOCATABLE :: v(:)
  !!
  !! main
  !!
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=arange(1, obj%spaceCompo), &
    & value=v, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode))
  !!
  value = NodalVariable( &
    & RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)]), &
    & TypeFEVariableVector, TypeFEVariableSpace )
  !!
  DEALLOCATE (v)
  !!
END PROCEDURE vField_get8

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get9
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_get9"
  INTEGER( I4B ) :: n
  !!
  !! check
  !!
  n = (obj%dof .tspacecomponents. 1)
  !!
  IF( spacecompo .GT. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .tspacecomponents. 1)='//tostring(n)// &
    & ' is lesser than ' // &
    & ' spacecompo='//tostring(spacecompo) )
  !!
#endif
  !!
  !!
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & value=value%realvec, &
    & idof=spacecompo )
  !!
END PROCEDURE vField_get9

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get10
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & value=value%realvec )
  !!
END PROCEDURE vField_get10

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_getPointerOfComponent
  !!
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = "vField_getPointerOfComponent"
  IF (spaceCompo .GT. obj%spaceCompo) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo')
#endif
  !!
  ans => getPointer( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=spaceCompo )
  !!
END PROCEDURE vField_getPointerOfComponent

END SUBMODULE GetMethods
