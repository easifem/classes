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

SUBMODULE(GnuPlot_Class) SurfacePlotMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SurfacePlot
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_surf1

CHARACTER(*), PARAMETER :: myName = "obj_surf1()"
INTEGER :: ncx, nrx, ii, jj
LOGICAL :: xyz_data
CHARACTER(:), ALLOCATABLE :: pltstring
CHARACTER(*), PARAMETER :: datablock = '$xyz'

pltstring = ''
ncx = SIZE(x, dim=2)
nrx = SIZE(x, dim=1)
IF (PRESENT(y) .AND. PRESENT(z)) THEN
  xyz_data = .TRUE.
ELSEIF (PRESENT(y)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ERROR] :: Z matrix was not sent to 3D plot routine')
ELSE
  xyz_data = .FALSE.
END IF

obj%txtdatastyle = 'lines'
CALL obj%Initiate()

IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
CALL processcmd(obj)
obj%plotscale = "linear"

CALL obj%pltfile%WRITE('#data x y z')
! Rev 0.20
! write the $xyz datablocks
CALL obj%pltfile%WRITE(datablock//' << EOD')
IF (xyz_data) THEN
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([x(ii, jj), y(ii, jj), z(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WRITE() ! an empty line
  END DO
ELSE
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([REAL(ii, dfp), REAL(jj, dfp), x(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WRITE()
  END DO
END IF
CALL obj%pltfile%WRITE('EOD')

IF (PRESENT(paletteName)) THEN
  CALL obj%pltfile%WRITE(color_palettes(paletteName))
  CALL obj%pltfile%WRITE('set pm3d')
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
END IF

CALL obj%pltfile%WRITE(TRIM(pltstring))

CALL obj%DEALLOCATE()

END PROCEDURE obj_surf1

END SUBMODULE SurfacePlotMethods
