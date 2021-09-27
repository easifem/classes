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

SUBMODULE( BlockNodeField_Class ) ConstructorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_addSurrogate
  CALL e%addSurrogate( Userobj )
END PROCEDURE Block_addSurrogate

!----------------------------------------------------------------------------
!                                                         setBlockNodeField
!----------------------------------------------------------------------------

MODULE PROCEDURE setBlockNodeFieldParam
  CHARACTER( LEN = * ), PARAMETER :: myName="setBlockNodeFieldParam"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE setBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_checkEssentialParam"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Initiate1
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Initiate1"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Initiate2
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Initiate2"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_Initiate2

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_DeallocateData
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_DeallocateData"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_DeallocateData

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Final
  CALL obj%DeallocateData()
END PROCEDURE Block_Final

END SUBMODULE ConstructorMethods