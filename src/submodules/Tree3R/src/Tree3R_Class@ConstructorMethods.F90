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

SUBMODULE(Tree3R_Class) ConstructorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE TR_Deallocate
  obj%n = 0
  obj%leftCoeff = 0.0_DFP
  obj%rightCoeff = 0.0_DFP
  !!
  IF( ASSOCIATED( obj%left ) ) THEN
    CALL obj%left%Deallocate()
    obj%left => NULL()
  END IF
  !!
  IF( ASSOCIATED( obj%right ) ) THEN
    CALL obj%right%Deallocate()
    obj%right => NULL()
  END IF
  !!
END PROCEDURE TR_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE TR_Final
  CALL obj%Deallocate()
END PROCEDURE TR_Final

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE TR_Initiate
  IF( obj%isInitiated() ) THEN
    RETURN
  ELSE
    IF( n .EQ. 1 ) THEN
      obj%n = 1
      obj%leftCoeff = leftCoeff( 0 )
      obj%rightCoeff = rightCoeff( 0 )
      obj%left => lastLeft
      obj%right => lastRight
    ELSE
      obj%n = n
      obj%leftCoeff = leftCoeff(obj%n-1)
      obj%rightCoeff = rightCoeff(obj%n-1)
      !!
      CALL obj%left%Initiate( n=n-1, leftCoeff=leftCoeff(0:n-2), &
        & rightCoeff=rightCoeff(0:n-2), lastLeft=lastLeft, &
        & lastRight=lastRight )
      END IF
      !!
  END IF
END PROCEDURE TR_Initiate


!----------------------------------------------------------------------------
!                                                              isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE TR_isInitiated
  ans = .FALSE.
  IF( ASSOCIATED( obj%left ) ) ans = .TRUE.
  IF( ASSOCIATED( obj%right ) ) ans = .TRUE.
END PROCEDURE TR_isInitiated


END SUBMODULE ConstructorMethods