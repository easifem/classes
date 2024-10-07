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
nrx = SIZE(x)
IF (PRESENT(y) .AND. PRESENT(z)) THEN
  xyz_data = .TRUE.
ELSEIF (PRESENT(y)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ERROR] :: Z matrix was not sent to 3D plot routine')
ELSE
  xyz_data = .FALSE.
END IF

obj%txtdatastyle = 'lines'
CALL create_outputfile(obj)

IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
CALL processcmd(obj)
obj%plotscale = "linear"

CALL obj%writeScript(script='#data x y z')
! Rev 0.20
! write the $xyz datablocks
CALL obj%writeScript(script=datablock//' << EOD')
IF (xyz_data) THEN
  DO ii = 1, nrx
    WRITE (obj%file_unit, *) x(ii), y(ii), z(ii)
  END DO
ELSE !only Z has been sent (i.e. single matrix data)
  DO ii = 1, nrx
    WRITE (obj%file_unit, *) ii, x(ii)
  END DO
END IF
CALL obj%writeScript() ! an empty line
CALL obj%writeScript(script='EOD') ! an empty line

IF (PRESENT(paletteName)) THEN
  CALL obj%writeScript(script=color_palettes(paletteName))
  CALL obj%writeScript(script='set pm3d') ! a conflict with lspec
END IF

pltstring = "splot "//datablock//" "
IF (.NOT. xyz_data) THEN
  pltstring = pltstring//"u 1:2:(0) "
END IF

IF (.NOT. hastitle(lspec)) THEN
  pltstring = pltstring//"notitle "
END IF

IF (PRESENT(lspec)) THEN
  pltstring = pltstring//TRIM(lspec)
ELSE
  pltstring = pltstring//" with lines"
END IF

CALL obj%writeScript(script=TRIM(pltstring))

IF (.NOT. (obj%hasanimation)) THEN
  CALL finalize_plot(obj)
ELSE
  WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
END IF

END PROCEDURE obj_plot3d_vvv

END SUBMODULE Plot3DMethods
