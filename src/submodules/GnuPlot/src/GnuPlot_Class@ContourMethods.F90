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

obj%txtdatastyle = 'lines'
CALL obj%Initiate()
CALL obj%processcmd()

! Write xy data into file
CALL obj%pltfile%WRITE('#data x y z')
! write the $xyz datablocks
CALL obj%pltfile%WRITE(datablock//' << EOD')
IF (xyz_data) THEN
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([x(ii, jj), y(ii, jj), z(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WriteBlank()
  END DO
  CALL obj%pltfile%WRITE('EOD')
ELSE
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([REAL(ii, dfp), REAL(jj, dfp), x(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WriteBlank()
  END DO
  CALL obj%pltfile%WRITE('EOD')
END IF

CALL obj%pltfile%WriteBlank()
CALL obj%pltfile%WRITE('# create the contour')
CALL obj%pltfile%WRITE('set contour base')

IF (obj%hasCBRange) THEN
  CALL obj%pltfile%WRITE('set cbrange ['//tostring(obj%cbrange(1))// &
                         ':'//tostring(obj%cbrange(2))//']')
END IF

IF (ALLOCATED(obj%cbTicks_stmt)) THEN
  CALL obj%pltfile%WRITE(obj%cbTicks_stmt)
  IF (fill) THEN
    CALL obj%pltfile%WRITE('set contourfill cbtics')
  END IF
END IF

IF (ALLOCATED(obj%cntrLevels_stmt)) THEN
  CALL obj%pltfile%WRITE(obj%cntrLevels_stmt)
ELSE
  CALL obj%pltfile%WRITE('set cntrparam levels 14')
END IF

CALL obj%pltfile%WRITE('unset surface')
CALL obj%pltfile%WRITE('set view map')

IF (PRESENT(paletteName)) THEN
  CALL obj%pltfile%WRITE(color_palettes(paletteName))

  IF (ALLOCATED(obj%pm3dOpts_stmt)) THEN
    CALL obj%pltfile%WRITE(obj%pm3dOpts_stmt)
  ELSE
    CALL obj%pltfile%WRITE('set pm3d')
  END IF
END IF

CALL obj%pltfile%WriteBlank()

IF (PRESENT(lspec)) THEN
  IF (hasTitle(lspec)) THEN
    pltstring = 'splot '//datablock//' '//TRIM(lspec)
  ELSE
    pltstring = 'splot '//datablock//' notitle '//TRIM(lspec)
  END IF
ELSE
  pltstring = 'splot '//datablock//' notitle '
END IF

CALL obj%pltfile%WRITE(TRIM(pltstring))

CALL obj%DEALLOCATE()

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

CALL obj%contour(x_arr, y=y_arr, z=z, lspec=lspec, &
                 paletteName=paletteName, fill=fill)

DEALLOCATE (x_arr, y_arr)

END PROCEDURE obj_Contour2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Contour3
INTEGER(I4B) :: ncx, nrx, ii, jj
LOGICAL(LGT) :: xyz_data
CHARACTER(80) :: pltstring
CHARACTER(*), PARAMETER :: datablock = '$xyz'

ncx = SIZE(x1, dim=2)
nrx = SIZE(x1, dim=1)
IF (PRESENT(y1) .AND. PRESENT(z1)) THEN
  xyz_data = .TRUE.
ELSEIF (PRESENT(y1)) THEN
  PRINT *, "GnuPlot_ error: Z matrix was not sent to 3D plot routine"
  RETURN
ELSE
  xyz_data = .FALSE.
END IF

obj%txtdatastyle = 'lines'
CALL obj%Initiate()
CALL obj%processcmd()

! Write xy data into file
CALL obj%pltfile%WRITE('#data x y z')
! write the $xyz datablocks
CALL obj%pltfile%WRITE(datablock//' << EOD')
IF (xyz_data) THEN
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([x1(ii, jj), y1(ii, jj), z1(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WriteBlank()
  END DO
  CALL obj%pltfile%WRITE('EOD')
ELSE
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([REAL(ii, dfp), REAL(jj, dfp), x1(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WriteBlank()
  END DO
  CALL obj%pltfile%WRITE('EOD')
END IF

CALL obj%pltfile%WriteBlank()

ncx = SIZE(x2, dim=2)
nrx = SIZE(x2, dim=1)
IF (PRESENT(y2) .AND. PRESENT(z2)) THEN
  xyz_data = .TRUE.
ELSEIF (PRESENT(y2)) THEN
  PRINT *, "GnuPlot_ error: Z matrix was not sent to 3D plot routine"
  RETURN
ELSE
  xyz_data = .FALSE.
END IF

! Write xy data into file
CALL obj%pltfile%WRITE('#data x y z')
! write the $xyz datablocks
CALL obj%pltfile%WRITE(datablock//"2"//' << EOD')
IF (xyz_data) THEN
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([x2(ii, jj), y2(ii, jj), z2(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WriteBlank()
  END DO
  CALL obj%pltfile%WRITE('EOD')
ELSE
  DO jj = 1, ncx
    DO ii = 1, nrx
      CALL obj%pltfile%WRITE([REAL(ii, dfp), REAL(jj, dfp), x2(ii, jj)], &
                             orient="ROW")
    END DO
    CALL obj%pltfile%WriteBlank()
  END DO
  CALL obj%pltfile%WRITE('EOD')
END IF

CALL obj%pltfile%WriteBlank()
CALL obj%pltfile%WRITE('# create the contour')
CALL obj%pltfile%WRITE('set contour base')

IF (obj%hasCBRange) THEN
  CALL obj%pltfile%WRITE('set cbrange ['//tostring(obj%cbrange(1))// &
                         ':'//tostring(obj%cbrange(2))//']')
END IF

IF (ALLOCATED(obj%cbTicks_stmt)) THEN
  CALL obj%pltfile%WRITE(obj%cbTicks_stmt)
  IF (fill) THEN
    CALL obj%pltfile%WRITE('set contourfill cbtics')
  END IF
END IF

IF (ALLOCATED(obj%cntrLevels_stmt)) THEN
  CALL obj%pltfile%WRITE(obj%cntrLevels_stmt)
ELSE
  CALL obj%pltfile%WRITE('set cntrparam levels 14')
END IF

CALL obj%pltfile%WRITE('unset surface')
CALL obj%pltfile%WRITE('set view map')

IF (PRESENT(paletteName)) THEN
  CALL obj%pltfile%WRITE(color_palettes(paletteName))

  IF (ALLOCATED(obj%pm3dOpts_stmt)) THEN
    CALL obj%pltfile%WRITE(obj%pm3dOpts_stmt)
  ELSE
    CALL obj%pltfile%WRITE('set pm3d')
  END IF
END IF

CALL obj%pltfile%WriteBlank()

pltstring = ''
IF (PRESENT(lspec1)) THEN
  IF (hasTitle(lspec1)) THEN
    pltstring = 'splot '//datablock//' '//TRIM(lspec1)//", \"
  ELSE
    pltstring = 'splot '//datablock//' notitle '//TRIM(lspec1)//", \"
  END IF
ELSE
  pltstring = 'splot '//datablock//' notitle, \'
END IF
CALL obj%pltfile%WRITE(TRIM(pltstring))

pltstring = ""
IF (PRESENT(lspec2)) THEN
  IF (hasTitle(lspec2)) THEN
    pltstring = '   '//datablock//'2 '//TRIM(lspec2)
  ELSE
    pltstring = '  '//datablock//'2  notitle '//TRIM(lspec2)
  END IF
ELSE
  pltstring = '   '//datablock//'2 notitle '
END IF

CALL obj%pltfile%WRITE(TRIM(pltstring))

CALL obj%DEALLOCATE()

END PROCEDURE obj_Contour3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ContourMethods

