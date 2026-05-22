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
CHARACTER(*), PARAMETER :: datablock = '$xyz'

obj%opts%plotOpts%datastyle = 'lines'

CALL obj%Initiate()

CALL obj%WritePlotSetup()

CALL Help_WriteDataBlock_xyz(obj, x, y, z, blockName=datablock)

CALL Help_WriteCntrBasicOptions(obj)

CALL Help_SetCntrFill(obj, fill)

CALL Help_SetCntrLevels(obj)

CALL Help_WritePaletteInfo(obj, paletteName)

CALL Help_WriteSplotString(obj, datablock, lspec)

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
CHARACTER(*), PARAMETER :: datablock1 = '$xyz1', &
                           datablock2 = '$xyz2'

obj%opts%plotOpts%datastyle = 'lines'
CALL obj%Initiate()
CALL obj%WritePlotSetup()

CALL Help_WriteDataBlock_xyz(obj, x1, y1, z1, blockName=datablock1)
CALL obj%pltfile%WriteBlank()

CALL Help_WriteDataBlock_xyz(obj, x2, y2, z2, blockName=datablock2)
CALL obj%pltfile%WriteBlank()

CALL Help_WriteCntrBasicOptions(obj)

CALL Help_SetCntrFill(obj, fill)

CALL Help_SetCntrLevels(obj)

CALL Help_WritePaletteInfo(obj, paletteName)

CALL Help_WriteSplotString(obj, datablock1, lspec1, append=.TRUE.)

CALL Help_WriteSplotString(obj, datablock1, lspec1, append=.FALSE.)

CALL obj%DEALLOCATE()

END PROCEDURE obj_Contour3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SetCntrLevels(obj)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj

  INTEGER(I4B) :: ilevel

  IF (obj%opts%splotOpts%discreteLevel) THEN
    CALL obj%pltfile%WRITE('set cntrparam levels discrete ', &
                           advance="NO")
    DO ilevel = 1, SIZE(obj%opts%splotOpts%levels) - 1
      CALL obj%pltfile%WRITE(tostring(obj%opts%splotOpts%levels(ilevel)) &
                             //",", advance="NO")
    END DO
    CALL obj%pltfile%WRITE(tostring(obj%opts%splotOpts%levels(ilevel)))
  ELSE
    CALL obj%pltfile%WRITE('set cntrparam levels '// &
                           tostring(obj%opts%splotOpts%numLevels))
  END IF

END SUBROUTINE Help_SetCntrLevels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SetCntrFill(obj, fill)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: fill

  LOGICAL(LGT) :: fill0

  fill0 = Input(default=obj%opts%splotOpts%fill, option=fill)
  IF (fill0) CALL obj%pltfile%WRITE('set contourfill cbtics')

END SUBROUTINE Help_SetCntrFill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteCntrBasicOptions(obj)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj

  CALL obj%pltfile%WriteBlank()
  CALL obj%pltfile%WRITE('# create the contour')
  CALL obj%pltfile%WRITE('set contour base')
  CALL obj%pltfile%WRITE('unset surface')
  CALL obj%pltfile%WRITE('set view map')

END SUBROUTINE Help_WriteCntrBasicOptions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WritePaletteInfo(obj, paletteName)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj
  CHARACTER(*), OPTIONAL, INTENT(IN) :: paletteName

  CHARACTER(:), ALLOCATABLE :: paletteName0

  paletteName0 = Input(default=obj%opts%splotOpts%paletteName%chars(), &
                       option=paletteName)

  IF (LEN(paletteName0) .GT. 0) THEN
    CALL obj%pltfile%WRITE(GetColorPaletteScript(paletteName0))
    CALL obj%pltfile%WRITE('set pm3d')
  END IF

  CALL obj%pltfile%WriteBlank()

END SUBROUTINE Help_WritePaletteInfo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteSplotString(obj, dataBlock, lspec, append)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: dataBlock
  CHARACTER(*), OPTIONAL, INTENT(IN) :: lspec
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: append

  LOGICAL(LGT) :: append0
  TYPE(String) :: pltString

  append0 = Input(default=.FALSE., option=append)

  IF (PRESENT(lspec)) THEN
    pltString = "splot "//dataBlock//' '//TRIM(lspec)
  ELSE
    pltString = "splot "//dataBlock
  END IF

  IF (append0) pltString = pltString//", "//CHAR_BSLASH

  CALL obj%pltfile%WRITE(pltString%chars())

END SUBROUTINE Help_WriteSplotString

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

