! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(Gnuplot_Class) UtilityMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE process_axes_set
CHARACTER(*), PARAMETER :: myName = 'process_axes_set()'
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: astr

astr = LowerCase(TRIM(ADJUSTL(axes_set)))

isok = LEN(astr) .EQ. 0
IF (isok) THEN
  axes = ''
  RETURN
END IF

SELECT CASE (astr)
CASE ('x1y1')
  axes = 'x1y1'
CASE ('x1y2')
  axes = 'x1y2'
CASE ('x2y1')
  axes = 'x2y1'
CASE ('x2y2')
  axes = 'x2y2'
CASE default
  ! wrong strings
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: wrong axes set is sent. &
    &    axes set can be on of: x1y1, x1y2, x2y1, x2y2')
  axes = ''
  RETURN
END SELECT

END PROCEDURE process_axes_set

!----------------------------------------------------------------------------
!                                                         process_linespec
!----------------------------------------------------------------------------

MODULE PROCEDURE process_linespec
!local variables
CHARACTER(4) :: axes
CHARACTER(10) :: axes_setting
LOGICAL(LGT) :: isok, abool, acase

!check the axes set
axes_setting = ''
isok = PRESENT(axes_set)
IF (isok) THEN

  CALL process_axes_set(axes_set, axes)
  abool = LEN_TRIM(axes) > 0
  IF (abool) axes_setting = ' axes '//axes

END IF

acase = PRESENT(lspec)

SELECT CASE (order)
CASE (1)

  IF (acase) THEN

    isok = hastitle(lspec)
    IF (isok) THEN
      lsstring = 'plot "-" '//TRIM(lspec)//axes_setting
    ELSE
      lsstring = 'plot "-" notitle '//TRIM(lspec)//axes_setting
    END IF

    RETURN
  END IF

  lsstring = 'plot "-" notitle'//axes_setting

CASE default !e.g. 2, 3, 4, ...

  IF (acase) THEN

    isok = hastitle(lspec)
    IF (isok) THEN
      lsstring = ', "-" '//TRIM(lspec)//axes_setting
    ELSE
      lsstring = ', "-" notitle '//TRIM(lspec)//axes_setting
    END IF

    RETURN
  END IF

  lsstring = ', "-" notitle'//axes_setting
END SELECT
END PROCEDURE process_linespec

!----------------------------------------------------------------------------
!                                                       create_outputfile
!----------------------------------------------------------------------------

MODULE PROCEDURE create_outputfile

IF (obj%hasfileopen) THEN
  ! there is nothing to do, file has been already open!
  RETURN
END IF

!> Rev 0.2 animation

! animation handling
IF (obj%hasanimation) THEN
  obj%frame_number = obj%frame_number + 1 ! for future use
END IF

! Open the output file

IF (.NOT. (obj%hasfilename)) THEN ! check if no file has been set by user
  obj%txtfilename = gnuplot_output_filename
END IF

        open ( newunit = obj%file_unit, file = obj%txtfilename, status = 'replace', iostat = obj%status )

IF (obj%status /= 0) THEN
  PRINT *, "md_helperproc, create_outputfile: cannot open file for output"
  STOP
END IF

! Set the gnuplot terminal, write oGnuPlot_ configuration (customized setting)
! Can be overwritten by options

! write signature
WRITE (obj%file_unit, '(a)') '# '//md_name
WRITE (obj%file_unit, '(a)') '# '//md_rev
WRITE (obj%file_unit, '(a)') '# '//md_lic
WRITE (obj%file_unit, '(a)') ! emptyline

! write the global settings
WRITE (obj%file_unit, '(a)') '# gnuplot global setting'
WRITE (unit=obj%file_unit, fmt='(a)') 'set term '//gnuplot_term_type// &
  ' size '//gnuplot_term_size//' enhanced font "'// &
  gnuplot_term_font//'"'// &
  ' title "'//md_name//': '//md_rev//'"' ! library name and version

! write the preset configuration for gnuplot (ogpf customized settings)
IF (obj%preset_configuration) THEN
  CALL obj%preset_gnuplot_config()
END IF
! write multiplot setting
IF (obj%hasmultiplot) THEN
  WRITE (obj%file_unit, fmt='(a, I2, a, I2)') 'set multiplot layout ', &
    obj%multiplot_rows, ',', obj%multiplot_cols
END IF
! set flag true for file is opened
obj%hasfileopen = .TRUE.

END PROCEDURE create_outputfile

!----------------------------------------------------------------------------
!                                                             processcmd
!----------------------------------------------------------------------------

MODULE PROCEDURE processcmd
! write the plot style for data
! obj is used only when 3D plots (splot, cplot) is used
IF (ALLOCATED(obj%txtdatastyle)) THEN
  WRITE (obj%file_unit, '("set style data ", a)') obj%txtdatastyle
  WRITE (obj%file_unit, '(a)')
END IF

! Write options
IF (obj%hasoptions) THEN
  WRITE (obj%file_unit, '(" ")')
  WRITE (obj%file_unit, '("# options")')
  WRITE (obj%file_unit, '(a)') obj%txtoptions
  WRITE (obj%file_unit, '(a)')
END IF

! Check with plot scale: i.e linear, logx, logy, or log xy
WRITE (obj%file_unit, '(" ")')
WRITE (obj%file_unit, '("# plot scale")')
SELECT CASE (obj%plotscale)
CASE ('semilogx')
  WRITE (obj%file_unit, '("set logscale  x")')
CASE ('semilogy')
  WRITE (obj%file_unit, '("set logscale  y")')
CASE ('loglog')
  WRITE (obj%file_unit, '("set logscale  xy")')
CASE default !for no setting
  !pass
END SELECT

        !!>0.22
! write annotation
WRITE (obj%file_unit, '(" ")')
WRITE (obj%file_unit, '("# Annotation: title and labels")')
CALL write_label(obj, 'plot_title')
CALL write_label(obj, 'xlabel')
CALL write_label(obj, 'x2label')
CALL write_label(obj, 'ylabel')
CALL write_label(obj, 'y2label')
CALL write_label(obj, 'zlabel')

! axes range
WRITE (obj%file_unit, '(" ")')
WRITE (obj%file_unit, '("# axes setting")')
IF (obj%hasxrange) THEN
  WRITE (obj%file_unit, '("set xrange [",G0,":",G0,"]")') obj%xrange
END IF
IF (obj%hasyrange) THEN
  WRITE (obj%file_unit, '("set yrange [",G0,":",G0,"]")') obj%yrange
END IF
IF (obj%haszrange) THEN
  WRITE (obj%file_unit, '("set zrange [",G0,":",G0,"]")') obj%zrange
END IF

! secondary axes range
IF (obj%hasx2range) THEN
  WRITE (obj%file_unit, '("set x2range [",G0,":",G0,"]")') obj%x2range
END IF
IF (obj%hasy2range) THEN
  WRITE (obj%file_unit, '("set y2range [",G0,":",G0,"]")') obj%y2range
END IF
! finish by new line
WRITE (obj%file_unit, '(a)') ! emptyline

END PROCEDURE processcmd

!----------------------------------------------------------------------------
!                                                              write_xydata
!----------------------------------------------------------------------------

MODULE PROCEDURE write_xydata
INTEGER :: i

! TODO (Mohammad#1#12/22/17): The format string shall be modified to write the
! number in more suitable form
! Rev 0.18
IF (PRESENT(y)) THEN !both x and y are present, data are xy set
  DO i = 1, ndata
    WRITE (file_unit, *) x(i), y(i)
  END DO
ELSE !only x is passed, data are index-x set
  DO i = 1, ndata
    WRITE (file_unit, *) x(i)
  END DO
END IF
WRITE (file_unit, '(a)') 'e' !end of set of data

END PROCEDURE write_xydata

!----------------------------------------------------------------------------
!                                                                  hasTitle
!----------------------------------------------------------------------------

MODULE PROCEDURE hasTitle
INTEGER :: idx1
INTEGER :: idx2

idx1 = INDEX(LowerCase(chars), 'title')
!Check if title is passed
idx2 = INDEX(' '//LowerCase(chars), ' t ')
!Check if the abbreviated title 't' is passed. Extra space is added
! at the beginning of chars to find starting 't'
IF (idx1 /= 0 .OR. idx2 /= 0) THEN
  hastitle = .TRUE.
ELSE
  hastitle = .FALSE.
END IF

END PROCEDURE hasTitle

!----------------------------------------------------------------------------
!                                                                write_label
!----------------------------------------------------------------------------

MODULE PROCEDURE write_label
! local var
CHARACTER(:), ALLOCATABLE :: lblstring
CHARACTER(:), ALLOCATABLE :: lblset
TYPE(Label_) :: label

SELECT CASE (lblname)
CASE ('xlabel')
  IF (.NOT. (obj%tpxlabel%hasLabel)) THEN
    RETURN ! there is no label
  END IF
  lblset = 'set xlabel "'
  label = obj%tpxlabel
CASE ('x2label')
  IF (.NOT. (obj%tpx2label%hasLabel)) THEN
    RETURN ! there is no label
  END IF
  lblset = 'set x2label "'
  label = obj%tpx2label
CASE ('ylabel')
  IF (.NOT. (obj%tpylabel%hasLabel)) THEN
    RETURN ! there is no label
  END IF
  lblset = 'set ylabel "'
  label = obj%tpylabel
CASE ('y2label')
  IF (.NOT. (obj%tpy2label%hasLabel)) THEN
    RETURN ! there is no label
  END IF
  lblset = 'set y2label "'
  label = obj%tpy2label
CASE ('zlabel')
  IF (.NOT. (obj%tpzlabel%hasLabel)) THEN
    RETURN ! there is no label
  END IF
  lblset = 'set zlabel "'
  label = obj%tpzlabel
CASE ('plot_title')
  IF (.NOT. (obj%tpplottitle%hasLabel)) THEN
    RETURN ! there is no label
  END IF
  lblset = 'set title "'
  label = obj%tpplottitle
END SELECT

lblstring = ''
! if there is a label continue to set it
lblstring = lblstring//lblset//TRIM(label%text)//'"'
IF (ALLOCATED(label%color)) THEN
  lblstring = lblstring//' tc "'//TRIM(label%color)//'"'
END IF
! set font and size
IF (ALLOCATED(obj%tpxlabel%fontname)) THEN
  lblstring = lblstring//' font "'//TRIM(label%fontname)//','
  IF (label%fontsize /= NOT_INITIALIZED) THEN
    lblstring = lblstring//tostring(label%fontsize)//'"'
  ELSE
    lblstring = lblstring//'"'
  END IF
ELSE ! check if only font size has been given
  IF (label%fontsize /= NOT_INITIALIZED) THEN
    lblstring = lblstring//' font ",'//tostring(label%fontsize)//'"'
  END IF
END IF
! set rotation
IF (label%rotate /= NOT_INITIALIZED) THEN
  lblstring = lblstring//' rotate by '//tostring(label%rotate)
END IF

! write to ogpf script file
WRITE (obj%file_unit, '(a)') lblstring

END PROCEDURE write_label

!----------------------------------------------------------------------------
!                                                                 splitstr
!----------------------------------------------------------------------------

MODULE PROCEDURE splitstr
! local variables
CHARACTER, PARAMETER :: delimiter = ';'
INTEGER :: n
INTEGER :: m
INTEGER :: k

k = LEN_TRIM(chars) !length with removed trailing blanks
n = SCAN(chars, delimiter)
IF (n == 0) THEN ! obj is a single statement
  spstr = ADJUSTL(chars)//NEW_LINE(' ')
  RETURN
END IF

! for two or more statements separated by ;
spstr = ''
m = 1
DO WHILE (n /= 0 .AND. m < k)
  IF (n /= 1) THEN
    spstr = spstr//ADJUSTL(chars(m:m + n - 2))//NEW_LINE(' ')
  END IF
  m = n + m
  n = SCAN(chars(m:k), delimiter)
END DO
IF (m < k) THEN !write the last statement
  spstr = spstr//ADJUSTL(chars(m:k))//NEW_LINE(' ')
END IF
END PROCEDURE splitstr

!----------------------------------------------------------------------------
!                                                        splitstring2array
!----------------------------------------------------------------------------

MODULE PROCEDURE splitstring2array

! local variables
INTEGER :: m, n
INTEGER :: i, idx
CHARACTER(LEN(chars)) :: strtmp
CHARACTER(1) :: delimiter_

! 0. check the existance of delimiter
IF (PRESENT(delimiter)) THEN
  delimiter_ = delimiter
ELSE
  delimiter_ = ';'
END IF

! 1. remove initial blanks if any
strtmp = TRIM(ADJUSTL(chars))

! 2. count the number substrings separated by delimiter
n = COUNT([(strtmp(i:i) == delimiter_, i=1, LEN_TRIM(strtmp))])

! 3. allocate the output string array
ALLOCATE (strarray(n + 1))

! 4. extract substrings and store in array one by one
m = 1
DO i = 1, n
  idx = INDEX(strtmp(m:), delimiter_)
  strarray(i) = ADJUSTL(strtmp(m:m + idx - 2))
  m = m + idx
END DO
strarray(n + 1) = ADJUSTL(strtmp(m:))

END PROCEDURE splitstring2array

!----------------------------------------------------------------------------
!                                                            preset_config
!----------------------------------------------------------------------------

MODULE PROCEDURE preset_gnuplot_config
!..............................................................................
! To write the preset configuration for gnuplot (ogpf customized settings)
!..............................................................................

CALL obj%writeScript()
CALL obj%writeScript(script='# ogpf extra configuration')
CALL obj%writeScript(script=commentLineGnuplot)

! color definition
CALL obj%writeScript(script='# color definitions')
CALL obj%writeScript(script='set style line 1 lc rgb "#800000" lt 1 lw 2')
CALL obj%writeScript(script='set style line 2 lc rgb "#ff0000" lt 1 lw 2')

CALL obj%writeScript(script='set style line 3 lc rgb "#ff4500" lt 1 lw 2')
CALL obj%writeScript(script='set style line 4 lc rgb "#ffa500" lt 1 lw 2')
CALL obj%writeScript(script='set style line 5 lc rgb "#006400" lt 1 lw 2')
CALL obj%writeScript(script='set style line 6 lc rgb "#0000ff" lt 1 lw 2')
CALL obj%writeScript(script='set style line 7 lc rgb "#9400d3" lt 1 lw 2')
CALL obj%writeScript()
! axes setting
CALL obj%writeScript(script='# Axes')
CALL obj%writeScript(script='set border linewidth 1.15')
CALL obj%writeScript(script='set tics nomirror')
CALL obj%writeScript()

CALL obj%writeScript(script='# grid')
CALL obj%writeScript(script='# Add light grid to plot')
CALL obj%writeScript(script='set style line 102 lc rgb "#d6d7d9" lt 0 lw 1')
CALL obj%writeScript(script='set grid back ls 102')
CALL obj%writeScript()
! set the plot style
CALL obj%writeScript(script='# plot style')
CALL obj%writeScript(script='set style data linespoints')
CALL obj%writeScript()

CALL obj%writeScript(script=commentLineGnuplot)
CALL obj%writeScript()

END PROCEDURE preset_gnuplot_config

!----------------------------------------------------------------------------
!                                                            writeScript
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_writeScript
CHARACTER(:), ALLOCATABLE :: fmt0

IF (PRESENT(fmt)) THEN
  fmt0 = fmt
ELSE
  fmt0 = defaultFmtGnuplot
END IF

IF (PRESENT(script)) THEN
  WRITE (obj%file_unit, fmt0) script
ELSE
  WRITE (obj%file_unit, fmt0)
END IF

END PROCEDURE obj_writeScript

!----------------------------------------------------------------------------
!                                                            color_palettes
!----------------------------------------------------------------------------

MODULE PROCEDURE color_palettes
CHARACTER(1) :: strnumber
CHARACTER(11) :: strblank
INTEGER :: j
INTEGER :: maxcolors

CHARACTER(:), ALLOCATABLE :: pltname
CHARACTER(7) :: palette(10) ! palettes with maximum 9 colors

maxcolors = 8 ! default number of discrete colors
palette = ''

#include "./include/colorPalettes.F90"

! generate the gnuplot palette as a single multiline string
paletteScript = '# Define the '//pltname//' pallete'//NEW_LINE(' ')
paletteScript = paletteScript//'set palette defined ( \'//NEW_LINE(' ')
strblank = '           ' ! pad certain number of paces
DO j = 1, maxcolors - 1
  WRITE (unit=strnumber, fmt='(I1)') j - 1
  paletteScript = paletteScript//strblank//strnumber// &
                  ' "'//palette(j)//'",\'//NEW_LINE(' ')
END DO

j = maxcolors - 1
WRITE (strnumber, fmt='(I1)') j
paletteScript = paletteScript//strblank//strnumber// &
                ' "'//palette(j)//'" )'//NEW_LINE(' ')

END PROCEDURE color_palettes

!----------------------------------------------------------------------------
!                                                                 addscript
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_addscript

IF (.NOT. ALLOCATED(obj%txtscript)) obj%txtscript = ''
IF (LEN_TRIM(obj%txtscript) == 0) THEN
  obj%txtscript = '' ! initialize string
END IF
IF (LEN_TRIM(scripts) > 0) THEN
  obj%txtscript = obj%txtscript//splitstr(scripts)
END IF

END PROCEDURE obj_addscript

!----------------------------------------------------------------------------
!                                                                 runscript
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_runscript

CALL create_outputfile(obj)

CALL processcmd(obj)
CALL obj%writeScript(script=obj%txtscript)

CALL finalize_plot(obj)

END PROCEDURE obj_runscript

END SUBMODULE UtilityMethods
