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

SUBMODULE(GnuPlot_Class) PlotMethods
USE InputUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot1
CHARACTER(*), PARAMETER :: myName = "obj_plot1()"

INTEGER :: ii, nplot
INTEGER(I4B), PARAMETER :: maxplot = 4
CHARACTER(3) :: plottype
CHARACTER(80) :: pltstring(4)
LOGICAL(LGT) :: isok, doplot(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

plottype = ''
pltstring = ''
nplot = 0

doplot = .FALSE.
doplot(1) = CheckInput(x1, y1)
IF (.NOT. doplot(1)) CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[ERROR] :: x1 and y1 must be present for plot1')
nplot = nplot + 1
CALL process_linespec(1, pltstring(1), ls1, axes1)

doplot(2) = CheckInput(x2, y2)
IF (doplot(2)) THEN
  nplot = nplot + 1
  CALL process_linespec(nplot, pltstring(2), ls2, axes2)
END IF
doplot(3) = CheckInput(x3, y3)
IF (doplot(3)) THEN
  nplot = nplot + 1
  CALL process_linespec(nplot, pltstring(3), ls3, axes3)
END IF
doplot(4) = CheckInput(x4, y4)
IF (doplot(4)) THEN
  nplot = nplot + 1
  CALL process_linespec(nplot, pltstring(4), ls4, axes4)
END IF

CALL obj%Initiate()

IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
ELSE
  obj%plotscale = defaultPlotScale
END IF
CALL processcmd(obj)

isok = nplot .EQ. 1
IF (isok) THEN
  CALL obj%pltfile%WRITE(TRIM(pltstring(1)))
ELSE
  CALL obj%pltfile%WRITE(TRIM(pltstring(1)), advance="NO")
  DO ii = 2, maxplot
    IF (doplot(ii)) THEN
      CALL obj%pltfile%WRITE(" \", advance="YES")
      CALL obj%pltfile%WRITE(TRIM(pltstring(ii)), advance="NO")
    END IF
  END DO
  CALL obj%pltfile%WRITE("", advance="YES")
END IF

CALL obj%writeData(x1, y1)
IF (doplot(2)) CALL obj%writeData(x2, y2)
IF (doplot(3)) CALL obj%writeData(x3, y3)
IF (doplot(4)) CALL obj%writeData(x4, y4)

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_plot1

!----------------------------------------------------------------------------
!                                                                 plot2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot2
CHARACTER(*), PARAMETER :: myName = "obj_plot2"

INTEGER :: nx, ny, ns, number_of_curves, ii, jj, ierr
CHARACTER(80), ALLOCATABLE :: pltstring(:), lst(:)

nx = SIZE(xv)
ny = SIZE(ymat, dim=1)
IF (.NOT. nx .EQ. ny) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The length of xv and ymat does not match')
END IF

CALL obj%Initiate()

IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
! Write plot title, axis labels and other annotations
CALL processcmd(obj)

obj%plotscale = defaultPlotScale

number_of_curves = SIZE(ymat, dim=2)
ALLOCATE (pltstring(number_of_curves), stat=ierr)
IF (ierr /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for pltstring')
END IF

pltstring(1:number_of_curves) = ''

IF (PRESENT(lspec)) THEN

  CALL splitstring2array(lspec, lst, ';')
  ns = SIZE(lst, dim=1)

  IF (ns == number_of_curves) THEN
    pltstring = lst
  ELSEIF (ns < number_of_curves) THEN
    DO ii = 1, ns
      pltstring(ii) = lst(ii)
    END DO
  ELSE ! ns > number_of curves
    CALL e%RaiseWarning(modName//'::'//myName//' - '// &
      & '[WARNING]:: wrong number of linespec,'// &
      'semicolon ";" acts as delimiter')
  END IF
END IF

IF (PRESENT(lspec)) THEN

  CALL process_linespec(1, pltstring(1), lst(1))
  ns = SIZE(lst)
  ! gpf will cylce through line specification, if number of specification passed
  ! is less than number of plots
  DO ii = 1, number_of_curves
    jj = MOD(ii - 1, ns) + 1
    CALL process_linespec(ii, pltstring(ii), lst(jj))
  END DO
ELSE !No lspec is available
  pltstring(1) = ' plot "-" notitle,'
  pltstring(2:number_of_curves - 1) = '"-" notitle,'
  IF (number_of_curves .GT. 1) THEN
    pltstring(number_of_curves) = '"-" notitle'
  END IF
END IF

! Write plot command and line styles and legend if any
DO ii = 1, number_of_curves - 1
  CALL obj%pltfile%WRITE(TRIM(pltstring(ii))//' \')
END DO
CALL obj%pltfile%WRITE(TRIM(pltstring(number_of_curves)))

! Write data into script file
DO jj = 1, number_of_curves
  DO ii = 1, nx
    CALL obj%pltfile%WRITE([xv(ii), ymat(ii, jj)], &
                           orient="ROW")
  END DO
  CALL obj%pltfile%WRITE("e")
END DO

CALL obj%DEALLOCATE()

!Release memory
IF (ALLOCATED(pltstring)) THEN
  DEALLOCATE (pltstring)
END IF

END PROCEDURE obj_plot2

!----------------------------------------------------------------------------
!                                                                 plot3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot3
CHARACTER(*), PARAMETER :: myName = "obj_plot3"
INTEGER(I4B) :: mx, nx, my, ny, ns, number_of_curves, &
                ii, jj, ierr
CHARACTER(80), ALLOCATABLE :: pltstring(:), lst(:)

mx = SIZE(xmat, dim=1)
my = SIZE(ymat, dim=1)
IF (.NOT. mx .EQ. my) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The row length of xmat and ymat does not match')
END IF

nx = SIZE(xmat, dim=2)
ny = SIZE(ymat, dim=2)
IF (.NOT. nx .EQ. ny) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The column length of xmat and ymat does not match')
END IF

CALL obj%Initiate()

IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
! Write plot title, axis labels and other annotations
CALL processcmd(obj)

obj%plotscale = defaultPlotScale

number_of_curves = SIZE(ymat, dim=2)
ALLOCATE (pltstring(number_of_curves), stat=ierr)
IF (ierr /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for pltstring')
END IF

pltstring(1:number_of_curves) = ''

IF (PRESENT(lspec)) THEN

  CALL splitstring2array(lspec, lst, ';')
  ns = SIZE(lst, dim=1)

  IF (ns == number_of_curves) THEN
    ! there is a linespec for each curve
    pltstring = lst
  ELSEIF (ns < number_of_curves) THEN
    ! not enough linespec
    DO ii = 1, ns
      pltstring(ii) = lst(ii)
    END DO
  ELSE ! ns > number_of curves
    CALL e%RaiseWarning(modName//'::'//myName//' - '// &
      & '[WARNING]:: wrong number of linespec,'// &
      'semicolon ";" acts as delimiter')
  END IF
END IF

IF (PRESENT(lspec)) THEN

  CALL process_linespec(1, pltstring(1), lst(1))
  ns = SIZE(lst)
  ! GnuPlot_ will cylce through line specification, if number of specification passed
  ! is less than number of plots
  DO ii = 1, number_of_curves
    jj = MOD(ii - 1, ns) + 1
    CALL process_linespec(ii, pltstring(ii), lst(jj))
  END DO
ELSE !No lspec is available
  pltstring(1) = ' plot "-" notitle,'
  pltstring(2:number_of_curves - 1) = '"-" notitle,'
  IF (number_of_curves .GT. 1) THEN
    pltstring(number_of_curves) = '"-" notitle'
  END IF
END IF

DO ii = 1, number_of_curves - 1
  CALL obj%pltfile%WRITE(TRIM(pltstring(ii))//' \')
END DO
CALL obj%pltfile%WRITE(TRIM(pltstring(number_of_curves)))

DO jj = 1, number_of_curves
  DO ii = 1, mx
    CALL obj%pltfile%WRITE([xmat(ii, jj), ymat(ii, jj)], &
                           orient="ROW")
  END DO
  CALL obj%pltfile%WRITE("e")
END DO

CALL obj%DEALLOCATE()

IF (ALLOCATED(pltstring)) THEN
  DEALLOCATE (pltstring)
END IF

END PROCEDURE obj_plot3

!----------------------------------------------------------------------------
!                                                                    plot4
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot4
CHARACTER(*), PARAMETER :: myName = "obj_plot4"
INTEGER :: np0, ii, alloc_err
REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP), ALLOCATABLE :: y(:)

IF (PRESENT(np)) THEN
  np0 = np
ELSE
  np0 = 50
END IF

ALLOCATE (x(1:np0), y(1:np0), stat=alloc_err)

IF (alloc_err /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for x and y')
END IF
!Create set of xy data
x = Linspace(xrange(1), xrange(2), np0)
y = [(func(x(ii)), ii=1, np0)]

CALL obj%plot(x, y, logScale=logScale)

! cleanup memory
IF (ALLOCATED(x)) DEALLOCATE (x)
IF (ALLOCATED(y)) DEALLOCATE (y)

END PROCEDURE obj_plot4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plotData1

CHARACTER(:), ALLOCATABLE :: xlabel0, ylabel0
REAL(DFP) :: xlim0(2), ylim0(2), areal

CALL obj%filename(filename//'.plt')
CALL obj%options('set terminal pngcairo; set output "' &
                 //filename//'.png"')

IF (PRESENT(xlim)) THEN
  xlim0 = xlim
ELSE
  xlim0 = [MINVAL(xDATA(:)), MAXVAL(xDATA(:))]
  areal = (xlim0(2) - xlim0(1))
  xlim0(1) = xlim0(1) - 0.1_DFP * areal
  xlim0(2) = xlim0(2) + 0.1_DFP * areal
END IF

IF (PRESENT(ylim)) THEN
  ylim0 = ylim
ELSE
  ylim0 = [MINVAL(yDATA(:)), MAXVAL(yDATA(:))]
  areal = (ylim0(2) - ylim0(1))
  ylim0(1) = ylim0(1) - 0.1 * areal
  ylim0(2) = ylim0(2) + 0.1 * areal
END IF

xlabel0 = Input(default="x", option=xlabel)
ylabel0 = Input(default="y", option=ylabel)

CALL obj%xlim(xlim0)
CALL obj%ylim(ylim0)
CALL obj%xlabel(xlabel0)
CALL obj%ylabel(ylabel0)
CALL obj%plot(x1=xDATA(:), y1=yDATA(:), ls1="w l")
CALL obj%reset()

END PROCEDURE obj_plotData1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION CheckInput(xdata, ydata) RESULT(isok)
  REAL(DFP), OPTIONAL, INTENT(IN) :: xdata(:), ydata(:)
  LOGICAL(LGT) :: isok

  LOGICAL(LGT) :: abool
  INTEGER(I4B) :: nx, ny

  abool = PRESENT(xdata) .AND. PRESENT(ydata)

  IF (abool) THEN
    nx = SIZE(xdata)
    ny = SIZE(ydata)

    isok = nx .EQ. ny
  ELSE
    isok = .FALSE.
  END IF

END FUNCTION CheckInput

END SUBMODULE PlotMethods
