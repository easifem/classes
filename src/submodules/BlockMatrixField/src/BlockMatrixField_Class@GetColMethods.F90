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

SUBMODULE(BlockMatrixField_Class) GetColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_getColumn1"
  CALL e%raiseError(modName//'::'//myName// " - "// &
  & 'This routine is not callable for BlockMatrixField')
END PROCEDURE mField_getColumn1

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn2
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( value ) ) THEN
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & idof=idof, &
      & value=value, &
      & scale=scale, &
      & addContribution=addContribution )
  END IF
  !!
  !!
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & idof=idof, &
      & value=realvec, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END IF
  !!
  NULLIFY( realvec )
  !!
END PROCEDURE mField_getColumn2

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn3
  !!
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( value ) ) THEN
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=value, &
      & scale=scale, &
      & addContribution=addContribution )
  END IF
  !!
  !!
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=realvec, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END IF
  !!
  NULLIFY( realvec )
  !!
END PROCEDURE mField_getColumn3

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn4
  !!
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( value ) ) THEN
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=value, &
      & scale=scale, &
      & addContribution=addContribution )
  END IF
  !!
  !!
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=realvec, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END IF
  !!
  NULLIFY( realvec )
  !!
END PROCEDURE mField_getColumn4

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn5
  !!
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( value ) ) THEN
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=value, &
      & scale=scale, &
      & addContribution=addContribution )
  END IF
  !!
  !!
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=realvec, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END IF
  !!
  NULLIFY( realvec )
  !!
END PROCEDURE mField_getColumn5

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn6
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( value ) ) THEN
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=value, &
      & scale=scale, &
      & addContribution=addContribution )
  END IF
  !!
  !!
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=realvec, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END IF
  !!
  NULLIFY( realvec )
  !!
END PROCEDURE mField_getColumn6

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn7
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( value ) ) THEN
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=value, &
      & scale=scale, &
      & addContribution=addContribution )
  END IF
  !!
  !!
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=realvec, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END IF
  !!
  NULLIFY( realvec )
  !!
END PROCEDURE mField_getColumn7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetColMethods