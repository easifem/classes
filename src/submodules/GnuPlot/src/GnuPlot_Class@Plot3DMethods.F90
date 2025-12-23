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

SUBMODULE(GnuPlot_Class) Plot3DMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 plot3d_1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot3d_vvv

CHARACTER(*), PARAMETER :: myName = "obj_plot3d_vvv()"
INTEGER :: nrx, ii
LOGICAL :: xyz_data
CHARACTER(:), ALLOCATABLE :: pltstring
CHARACTER(*), PARAMETER :: datablock = '$xyz'

pltstring = ''
!   Check the input data
! TODO: separate the plot method when only x data is sent
nrx = SIZE(x)
IF (PRESENT(y) .AND. PRESENT(z)) THEN
  xyz_data = .TRUE.
ELSEIF (PRESENT(y)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ERROR] :: Z matrix was not sent to 3D plot routine')
ELSE
  xyz_data = .FALSE.
END IF

obj%datastyle = 'lines'
CALL obj%Initiate()

CALL obj%WritePlotSetup()

CALL obj%pltfile%WRITE('#data x y z')

! write the $xyz datablocks
CALL obj%pltfile%WRITE(datablock//' << EOD')
IF (xyz_data) THEN
  DO ii = 1, nrx
    CALL obj%pltfile%WRITE([x(ii), y(ii), z(ii)], &
                           orient="ROW")
  END DO
ELSE
  DO ii = 1, nrx
    CALL obj%pltfile%WRITE([REAL(ii, dfp), x(ii)], &
                           orient="ROW")
  END DO
END IF
CALL obj%pltfile%WriteBlank()
CALL obj%pltfile%WRITE('EOD')

IF (PRESENT(paletteName)) THEN
  CALL obj%pltfile%WRITE(GetColorPaletteScript(paletteName))
  CALL obj%pltfile%WRITE('set pm3d')
END IF

pltstring = "splot "//datablock//" "
IF (.NOT. xyz_data) THEN
  pltstring = pltstring//"u 1:2:(0) "
END IF

IF (PRESENT(lspec)) THEN
  pltstring = pltstring//TRIM(lspec)
ELSE
  pltstring = pltstring//" with lines"
END IF

CALL obj%pltfile%WRITE(TRIM(pltstring))

CALL obj%DEALLOCATE()

END PROCEDURE obj_plot3d_vvv

END SUBMODULE Plot3DMethods
