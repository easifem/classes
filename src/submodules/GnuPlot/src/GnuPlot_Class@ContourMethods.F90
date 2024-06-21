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

SUBMODULE(GnuPlot_Class) ContourMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Contour
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Contour1
INTEGER(I4B) :: ncx, nrx, ii, jj
LOGICAL(LGT) :: xyz_data
CHARACTER(80) :: pltstring
CHARACTER(*), PARAMETER :: datablock = '$xyz'

pltstring = ''
ncx = SIZE(x, dim=2)
nrx = SIZE(x, dim=1)
IF (PRESENT(y) .AND. PRESENT(z)) THEN
  xyz_data = .TRUE.
ELSEIF (PRESENT(y)) THEN
  PRINT *, "GnuPlot_ error: Z matrix was not sent to 3D plot routine"
  RETURN
ELSE
  xyz_data = .FALSE.
END IF

! set default line style for 3D plot, can be overwritten
obj%txtdatastyle = 'lines'
! create the script file for writting gnuplot commands and data
CALL obj%create_outputfile()
! Write titles and other annotations
CALL obj%processcmd()

! Write xy data into file
WRITE (obj%file_unit, '(a)') '#data x y z'
! write the $xyz datablocks
WRITE (obj%file_unit, '(a)') datablock//' << EOD'
IF (xyz_data) THEN
  DO jj = 1, ncx
    DO ii = 1, nrx
      WRITE (obj%file_unit, fmt=*) x(ii, jj), y(ii, jj), z(ii, jj)
    END DO
    WRITE (obj%file_unit, '(a)') !put an empty line
  END DO
  WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
ELSE !only Z has been sent (i.e. single matrix data)
  DO jj = 1, ncx
    DO ii = 1, nrx
      WRITE (obj%file_unit, fmt=*) ii, jj, x(ii, jj)
    END DO
    WRITE (obj%file_unit, '(a)') !put an empty line
  END DO
  WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
END IF

! create the contour lines
WRITE (obj%file_unit, '(a)') ! empty line
WRITE (obj%file_unit, '(a)') '# create the contour'
WRITE (obj%file_unit, '(a)') 'set contour base'

IF (obj%hasCBRange) THEN
  WRITE (obj%file_unit, '(a)') 'set cbrange ['//tostring(obj%cbrange(1))// &
    ':'//tostring(obj%cbrange(2))//']'
END IF

IF (ALLOCATED(obj%cbTicks_stmt)) THEN
  WRITE (obj%file_unit, '(a)') obj%cbTicks_stmt
  IF (fill) THEN
    WRITE (obj%file_unit, '(a)') 'set contourfill cbtics'
  END IF
END IF

IF (ALLOCATED(obj%cntrLevels_stmt)) THEN
  WRITE (obj%file_unit, '(a)') obj%cntrLevels_stmt
ELSE
  WRITE (obj%file_unit, '(a)') 'set cntrparam levels 14'
END IF

WRITE (obj%file_unit, '(a)') 'unset surface'
WRITE (obj%file_unit, '(a)') 'set view map'

!write the color palette into gnuplot script file
IF (PRESENT(palette)) THEN
  WRITE (obj%file_unit, '(a)') obj%getColorPalettes(palette)

  IF (ALLOCATED(obj%pm3dOpts_stmt)) THEN
    WRITE (obj%file_unit, '(a)') obj%pm3dOpts_stmt
  ELSE
    WRITE (obj%file_unit, '(a)') 'set pm3d'
  END IF
END IF

WRITE (obj%file_unit, '(a)') ! empty line

IF (PRESENT(lspec)) THEN
  IF (obj%checkTitle(lspec)) THEN
    pltstring = 'splot '//datablock//' '//TRIM(lspec)
  ELSE
    pltstring = 'splot '//datablock//' notitle '//TRIM(lspec)
  END IF
ELSE
  pltstring = 'splot '//datablock//' notitle '
END IF

WRITE (obj%file_unit, '(a)') TRIM(pltstring)

! if there is no animation finalize
IF (.NOT. (obj%hasanimation)) THEN
  CALL obj%finalize_plot()
ELSE
  WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
END IF

END PROCEDURE obj_Contour1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Contour2
REAL(DFP), ALLOCATABLE :: x_arr(:, :), y_arr(:, :)
INTEGER(I4B) :: xsize, ysize, ii, jj

xsize = SIZE(x)
ysize = SIZE(y)

ALLOCATE (x_arr(xsize, ysize), y_arr(xsize, ysize))

DO jj = 1, ysize
  DO ii = 1, xsize
    x_arr(ii, jj) = x(ii)
  END DO
  y_arr(:, jj) = y(jj)
END DO

CALL obj%contour(x_arr, y=y_arr, z=z, lspec=lspec, palette=palette, &
                 fill=fill)

DEALLOCATE (x_arr, y_arr)

END PROCEDURE obj_Contour2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ContourMethods

