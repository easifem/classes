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

SUBMODULE(BlockMatrixField_Class) SetColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_setColumn1"
  CALL e%raiseError(modName//'::'//myName// " - "// &
  & 'This routine is not callable for BlockMatrixField')
END PROCEDURE mField_setColumn1

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn2
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( scalarVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & idof=idof, &
    & value=scalarVal )
  !!
  IF( PRESENT( vecVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & idof=idof, &
    & value=vecVal )
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & idof=idof, &
    & value=realvec )
  END IF
  !!
  realvec => NULL()
  !!
END PROCEDURE mField_setColumn2

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn3
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( scalarVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=scalarVal )
  !!
  IF( PRESENT( vecVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=vecVal )
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=realvec )
  END IF
  !!
  realvec => NULL()
  !!
END PROCEDURE mField_setColumn3

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn4
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( scalarVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=scalarVal )
  !!
  IF( PRESENT( vecVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=vecVal )
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=realvec )
  END IF
  !!
  realvec => NULL()
  !!
END PROCEDURE mField_setColumn4

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn5
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( scalarVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=scalarVal )
  !!
  IF( PRESENT( vecVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=vecVal )
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=realvec )
  END IF
  !!
  realvec => NULL()
  !!
END PROCEDURE mField_setColumn5

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn6
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( scalarVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=scalarVal )
  !!
  IF( PRESENT( vecVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=vecVal )
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=realvec )
  END IF
  !!
  realvec => NULL()
  !!
END PROCEDURE mField_setColumn6

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn7
  REAL( DFP ), POINTER :: realvec( : )
  !!
  !!
  !!
  IF( PRESENT( scalarVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=scalarVal )
  !!
  IF( PRESENT( vecVal ) ) &
    & CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=vecVal )
  !!
  IF( PRESENT( nodeFieldVal ) ) THEN
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=realvec )
  END IF
  !!
  realvec => NULL()
  !!
END PROCEDURE mField_setColumn7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetColMethods