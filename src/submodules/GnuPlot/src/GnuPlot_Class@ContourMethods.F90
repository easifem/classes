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

SUBMODULE(GnuPlot_Class) ContourMethods
USE GlobalData, ONLY: CHAR_BSLASH
USE InputUtility, ONLY: Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Contour
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Contour1
TYPE(String) :: pltstring
CHARACTER(*), PARAMETER :: datablock = '$xyz'
LOGICAL(LGT) :: fill0
CHARACTER(:), ALLOCATABLE :: paletteName0

obj%opts%plotOpts%datastyle = 'lines'
CALL obj%Initiate()
CALL obj%WritePlotSetup()

CALL Help_WriteDataBlock_xyz(obj, x, y, z, blockName=datablock)

CALL obj%pltfile%WriteBlank()
CALL obj%pltfile%WRITE('# create the contour')
CALL obj%pltfile%WRITE('set contour base')

fill0 = Input(default=obj%opts%plotOpts%fill, option=fill)
IF (fill0) CALL obj%pltfile%WRITE('set contourfill cbtics')

CALL obj%pltfile%WRITE('set cntrparam levels '// &
                       tostring(obj%opts%plotOpts%numLevels))

CALL obj%pltfile%WRITE('unset surface')
CALL obj%pltfile%WRITE('set view map')

paletteName0 = Input(default=obj%opts%plotOpts%paletteName%chars(), &
                     option=paletteName)
IF (LEN(paletteName0) .GT. 0) THEN
  CALL obj%pltfile%WRITE(GetColorPaletteScript(paletteName0))
  CALL obj%pltfile%WRITE('set pm3d')
END IF

CALL obj%pltfile%WriteBlank()

pltstring = ''
IF (PRESENT(lspec)) THEN
  pltstring = "splot "//datablock//' '//TRIM(lspec)
ELSE
  pltstring = 'splot '//datablock
END IF

CALL obj%pltfile%WRITE(pltstring%chars())

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
TYPE(String) :: pltstring
CHARACTER(*), PARAMETER :: datablock1 = '$xyz', &
                           datablock2 = '$xyz2'
LOGICAL(LGT) :: fill0
CHARACTER(:), ALLOCATABLE :: paletteName0

obj%opts%plotOpts%datastyle = 'lines'
CALL obj%Initiate()
CALL obj%WritePlotSetup()

CALL Help_WriteDataBlock_xyz(obj, x1, y1, z1, blockName=datablock1)
CALL obj%pltfile%WriteBlank()

CALL Help_WriteDataBlock_xyz(obj, x2, y2, z2, blockName=datablock2)

CALL obj%pltfile%WriteBlank()

CALL obj%pltfile%WRITE('# create the contour')
CALL obj%pltfile%WRITE('set contour base')

fill0 = Input(default=obj%opts%plotOpts%fill, option=fill)
IF (fill0) CALL obj%pltfile%WRITE('set contourfill cbtics')

CALL obj%pltfile%WRITE('set cntrparam levels 14')

CALL obj%pltfile%WRITE('unset surface')
CALL obj%pltfile%WRITE('set view map')

paletteName0 = Input(default=obj%opts%plotOpts%paletteName%chars(), &
                     option=paletteName)
IF (LEN(paletteName0) .GT. 0) THEN
  CALL obj%pltfile%WRITE(GetColorPaletteScript(paletteName0))
  CALL obj%pltfile%WRITE('set pm3d')
END IF

CALL obj%pltfile%WriteBlank()

IF (PRESENT(lspec1)) THEN
  pltstring = "splot "//datablock1//' '//TRIM(lspec1)
ELSE
  pltstring = 'splot '//datablock1
END IF
pltstring = pltstring//", "//CHAR_BSLASH

CALL obj%pltfile%WRITE(pltstring%chars())

IF (PRESENT(lspec2)) THEN
  pltstring = "splot "//datablock2//' '//TRIM(lspec2)
ELSE
  pltstring = 'splot '//datablock1
END IF
pltstring = pltstring//", "//CHAR_BSLASH

CALL obj%pltfile%WRITE(pltstring%chars())

CALL obj%DEALLOCATE()

END PROCEDURE obj_Contour3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteDataBlock_xyz(obj, x, y, z, blockName)
  CLASS(Gnuplot_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: x(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: y(:, :), z(:, :)
  CHARACTER(*), INTENT(IN) :: blockName

  INTEGER(I4B) :: tsize_x, tsize_y, ii, jj

  LOGICAL(LGT) :: xyzData

  xyzData = PRESENT(y) .AND. PRESENT(z)

  tsize_x = SIZE(x, dim=1)
  tsize_y = SIZE(x, dim=2)

  CALL obj%pltfile%WRITE('#data x y z')

  CALL obj%pltfile%WRITE(blockName//' << EOD')

  IF (xyzData) THEN
    DO jj = 1, tsize_y
      DO ii = 1, tsize_x
        CALL obj%pltfile%WRITE([x(ii, jj), y(ii, jj), z(ii, jj)], &
                               orient="ROW")
      END DO
      CALL obj%pltfile%WriteBlank()
    END DO
    CALL obj%pltfile%WRITE('EOD')
  ELSE
    DO jj = 1, tsize_y
      DO ii = 1, tsize_x
        CALL obj%pltfile%WRITE([REAL(ii, dfp), REAL(jj, dfp), x(ii, jj)], &
                               orient="ROW")
      END DO
      CALL obj%pltfile%WriteBlank()
    END DO
    CALL obj%pltfile%WRITE('EOD')
  END IF

END SUBROUTINE Help_WriteDataBlock_xyz

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ContourMethods

