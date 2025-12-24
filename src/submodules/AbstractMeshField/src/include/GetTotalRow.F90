! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

!----------------------------------------------------------------------------
!                                                                 GetTotalRow
!----------------------------------------------------------------------------

FUNCTION GetTotalRow(rank, varType) RESULT(nrow)
  INTEGER(I4B), INTENT(IN) :: rank, varType
  INTEGER(I4B) :: nrow

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetTotalRow()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (rank)

  CASE (fevaropt%scalar)
    SELECT CASE (varType)
    CASE (typefield%constant, typefield%space, typefield%time)
      nrow = 1
      ! one dimension, single entry
      ! one dimension, multiple entries in space
      ! one dimension, multiple entries in time

    CASE (typefield%spaceTime)
      ! two dimensions, multiple entries in space-time
      nrow = 2

#ifdef DEBUG_VER
    CASE DEFAULT
      CALL AssertError1(.FALSE., myName, &
                        'No case found for varType='//ToString(varType))
#endif
    END SELECT

  CASE (fevaropt%vector)
    SELECT CASE (varType)
    CASE (typefield%constant)
      ! one dimension, only vector components
      nrow = 1
    CASE (typefield%space, typefield%time)
      nrow = 2
      ! two dimension, vector components and space values
      ! two dimension, vector components and time values
    CASE (typefield%spaceTime)
      ! two dimension, vector components, space and time values
      nrow = 3

#ifdef DEBUG_VER
    CASE DEFAULT
      CALL AssertError1(.FALSE., myName, &
                        'No case found for varType='//ToString(varType))
#endif
    END SELECT

  CASE (fevaropt%matrix)
    SELECT CASE (varType)
    CASE (typefield%constant)
      ! two dimensions, matrix components
      nrow = 2
    CASE (typefield%space, typefield%time)
      ! three dimensions, matrix components and space values
      ! three dimensions, matrix components and time values
      nrow = 3
    CASE (typefield%spaceTime)
      ! four dimensions, matrix components, space and time values
      nrow = 4

#ifdef DEBUG_VER
    CASE DEFAULT
      CALL AssertError1(.FALSE., myName, &
                        'No case found for varType='//ToString(varType))
#endif
    END SELECT

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      'No case found for rank='//ToString(rank))
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION GetTotalRow

