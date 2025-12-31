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

SUBMODULE(GnuPlot_Class) PlotMethods
USE InputUtility, ONLY: Input
USE GridPointUtility, ONLY: Linspace
USE ReallocateUtility, ONLY: Reallocate
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
doplot(1) = Help_CheckInput(x1, y1)
IF (.NOT. doplot(1)) CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[ERROR] :: x1 and y1 must be present for plot1')
nplot = nplot + 1
CALL GetPlotCommand(1, pltstring(1), ls1, axes1)

doplot(2) = Help_CheckInput(x2, y2)
IF (doplot(2)) THEN
  nplot = nplot + 1
  CALL GetPlotCommand(nplot, pltstring(2), ls2, axes2)
END IF
doplot(3) = Help_CheckInput(x3, y3)
IF (doplot(3)) THEN
  nplot = nplot + 1
  CALL GetPlotCommand(nplot, pltstring(3), ls3, axes3)
END IF
doplot(4) = Help_CheckInput(x4, y4)
IF (doplot(4)) THEN
  nplot = nplot + 1
  CALL GetPlotCommand(nplot, pltstring(4), ls4, axes4)
END IF

CALL obj%Initiate()

CALL obj%WritePlotSetup()

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

CALL obj%WriteDataBlock(x1, y1)
IF (doplot(2)) CALL obj%WriteDataBlock(x2, y2)
IF (doplot(3)) CALL obj%WriteDataBlock(x3, y3)
IF (doplot(4)) CALL obj%WriteDataBlock(x4, y4)

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

CONTAINS

FUNCTION Help_CheckInput(xdata, ydata) RESULT(isok)
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

END FUNCTION Help_CheckInput

END PROCEDURE obj_plot1

!----------------------------------------------------------------------------
!                                                                 plot2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot2
CHARACTER(*), PARAMETER :: myName = "obj_plot2"

INTEGER :: nx, ny, ns, number_of_curves, ii, jj, ierr
CHARACTER(80), ALLOCATABLE :: pltstring(:)
TYPE(String), ALLOCATABLE :: lspecs(:)

nx = SIZE(xv)
ny = SIZE(ymat, dim=1)
IF (.NOT. nx .EQ. ny) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The length of xv and ymat does not match')
END IF

CALL obj%Initiate()

CALL obj%WritePlotSetup()

number_of_curves = SIZE(ymat, dim=2)
ALLOCATE (pltstring(number_of_curves), stat=ierr)
IF (ierr /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for pltstring')
END IF

pltstring(1:number_of_curves) = ''

IF (PRESENT(lspec)) THEN

  CALL lspec%Split(tokens=lspecs, sep=';')
  ns = SIZE(lspecs)

  IF (ns .LE. number_of_curves) THEN
    DO ii = 1, ns
      pltstring(ii) = lspecs(ii)%chars()
    END DO
  ELSE
    DO ii = 1, number_of_curves
      pltstring(ii) = lspecs(ii)%chars()
    END DO
  END IF

END IF

IF (PRESENT(lspec)) THEN

  CALL GetPlotCommand(1, pltstring(1), lspecs(1)%chars())
  ns = SIZE(lspecs)
  ! cylce through line specification will happen,
  ! when the number of lspces is less than number of curves
  DO ii = 1, number_of_curves
    jj = MOD(ii - 1, ns) + 1
    CALL GetPlotCommand(ii, pltstring(ii), lspecs(jj)%chars())
  END DO
ELSE
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
CHARACTER(80), ALLOCATABLE :: pltstring(:)
TYPE(String), ALLOCATABLE :: lspecs(:)

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

CALL obj%WritePlotSetup()

number_of_curves = SIZE(ymat, dim=2)
ALLOCATE (pltstring(number_of_curves), stat=ierr)
IF (ierr /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for pltstring')
END IF

pltstring(1:number_of_curves) = ''

IF (PRESENT(lspec)) THEN

  CALL lspec%Split(tokens=lspecs, sep=';')
  ns = SIZE(lspecs)

  IF (ns .LE. number_of_curves) THEN
    DO ii = 1, ns
      pltstring(ii) = lspecs(ii)%chars()
    END DO
  ELSE
    DO ii = 1, number_of_curves
      pltstring(ii) = lspecs(ii)%chars()
    END DO
  END IF

END IF

IF (PRESENT(lspec)) THEN

  CALL GetPlotCommand(1, pltstring(1), lspecs(1)%chars())
  ns = SIZE(lspecs)

  DO ii = 1, number_of_curves
    jj = MOD(ii - 1, ns) + 1
    CALL GetPlotCommand(ii, pltstring(ii), lspecs(jj)%chars())
  END DO
ELSE
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

MODULE PROCEDURE obj_plotFunc1
CHARACTER(*), PARAMETER :: myName = "obj_plotFunc1"
INTEGER(I4B) :: np0, ii, alloc_err
REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP), ALLOCATABLE :: y(:)

np0 = Input(default=50, option=np)

CALL Reallocate(x, np0)
CALL Reallocate(y, np0)

!Create set of xy data
x = Linspace(xMin, xMax, np)

ii = yFunc%GetNumReturns()
IF (.NOT. ii .EQ. 1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: yFunc must be scalar function')
END IF

DO ii = 1, np0
  CALL yFunc%Get(y(ii), args=[x(ii)])
END DO

CALL obj%plot(x, y, ls1=lspec%chars())

DEALLOCATE (x, y)

END PROCEDURE obj_plotFunc1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plotFunc2
CHARACTER(*), PARAMETER :: myName = "obj_plotFunc2"
INTEGER(I4B) :: np0, ii
REAL(DFP), ALLOCATABLE :: y(:)

np0 = SIZE(xVec)

CALL Reallocate(y, np0)

ii = yFunc%GetNumReturns()
IF (.NOT. ii .EQ. 1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: yFunc must be scalar function')
END IF

DO ii = 1, np0
  CALL yFunc%Get(y(ii), args=[xVec(ii)])
END DO

CALL obj%plot(xVec, y, ls1=lspec%chars())

DEALLOCATE (y)

END PROCEDURE obj_plotFunc2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plotFunc3
CHARACTER(*), PARAMETER :: myName = "obj_plotFunc3"
INTEGER(I4B) :: np0, ii
REAL(DFP), ALLOCATABLE :: x(:), y(:)
REAL(DFP) :: args(2)
LOGICAL(LGT) :: isXFunc, isYFunc

isXFunc = ASSOCIATED(xFunc)
isYFunc = ASSOCIATED(yFunc)

IF (.NOT. isXFunc .AND. .NOT. isYFunc) THEN
  CALL obj%plot(argVec1, argVec2)
  RETURN
END IF

np0 = SIZE(argVec1)

IF (isXFunc .AND. isYFunc) THEN
  CALL Reallocate(x, np0)
  CALL Reallocate(y, np0)

  DO ii = 1, np0
    args = [argVec1(ii), argVec2(ii)]
    CALL xFunc%Get(x(ii), args=args)
    CALL yFunc%Get(y(ii), args=args)
  END DO

  CALL obj%plot(x, y, ls1=lspec%chars())

ELSE IF (isXFunc) THEN
  CALL Reallocate(x, np0)

  DO ii = 1, np0
    args = [argVec1(ii), argVec2(ii)]
    CALL xFunc%Get(x(ii), args=args)
  END DO

  CALL obj%plot(x, argVec2, ls1=lspec%chars())

ELSE

  CALL Reallocate(y, np0)

  DO ii = 1, np0
    args = [argVec1(ii), argVec2(ii)]
    CALL xFunc%Get(y(ii), args=args)
  END DO

  CALL obj%plot(argVec1, y, ls1=lspec%chars())

END IF

IF (ALLOCATED(x)) DEALLOCATE (x)
IF (ALLOCATED(y)) DEALLOCATE (y)

END PROCEDURE obj_plotFunc3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plotData1

CHARACTER(:), ALLOCATABLE :: xlabel0, ylabel0
REAL(DFP) :: xlim0(2), ylim0(2), areal
TYPE(String) :: astr

CALL obj%SetFilename(filename//'.plt')

astr = 'set terminal pngcairo; set output "'//filename//'.png"'
CALL obj%SetOptions(astr)

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

CALL obj%SetXlim(xlim0)
CALL obj%SetYlim(ylim0)
CALL obj%SetXLabel(xlabel0)
CALL obj%SetYLabel(ylabel0)
CALL obj%plot(x1=xDATA(:), y1=yDATA(:), ls1="w l")
CALL obj%reset()

END PROCEDURE obj_plotData1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Plot4
CHARACTER(*), PARAMETER :: myName = "obj_plot5()"

INTEGER :: ii, nplot
INTEGER(I4B) :: tsize_x, tsize_y
INTEGER(I4B), PARAMETER :: maxplot = 4
CHARACTER(3) :: plottype
CHARACTER(80) :: pltstring(4)
LOGICAL(LGT) :: isok, doplot(4), isXVecs, isYVecs, &
                isXMats, isYMats

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isXVecs = ALLOCATED(obj%xVecs)
isYVecs = ALLOCATED(obj%yVecs)

IF (.NOT. isXVecs .OR. .NOT. isXVecs) THEN
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
    & '[WARNING] :: xVecs and yVecs pointers must be allocated'// &
    " return with doing nothing")
  RETURN
END IF

tsize_x = SIZE(obj%xVecs)
tsize_y = SIZE(obj%yVecs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Plot4

END SUBMODULE PlotMethods
