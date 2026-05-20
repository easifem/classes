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

MODULE GnuPlotOpt_Class

USE GlobalData, ONLY: I4B, DFP, LGT

USE BaseMethod, ONLY: TOSTRING

USE BaseType, ONLY: RealVectorPointer_, &
                    RealMatrixPointer_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

USE StringUtility, ONLY: LowerCase

USE TxtFile_Class, ONLY: TxtFile_

USE tomlf, ONLY: toml_table

USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE
PRIVATE

PUBLIC :: GnuPlotOpt_
PUBLIC :: DefaultGnuplotOpt_
PUBLIC :: GnuPlotLabel_
PUBLIC :: GnuPlotAxis_
PUBLIC :: GnuPlotTick_
PUBLIC :: GnuPlotPlotOpts_

CHARACTER(*), PARAMETER :: modName = 'GnuplotOpt_Class'
INTEGER(I4B), PARAMETER :: NOT_INITIALIZED = -32000

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE DefaultGnuplotOpt_
  CHARACTER(4) :: termType = 'wxt'
  CHARACTER(15) :: termFont = 'Times New Roman'
  CHARACTER(3) :: paletteName = "jet"
  INTEGER(I4B) :: termFontSize = 10
  INTEGER(I4B) :: numLevels = 10
  INTEGER(I4B) :: maxNumberPlots = 20
  INTEGER(I4B) :: termSize(2) = [640, 480]
  CHARACTER(18) :: filename = "gnuplot_script"
  CHARACTER(11) :: dataStyle = "linespoints"
  CHARACTER(45) :: commentLine = &
                   '# -------------------------------------------'
  CHARACTER(17) :: commandLine = "gnuplot --persist"
  REAL(DFP) :: pauseSeconds = 2.0_DFP
  LOGICAL(LGT) :: fill = .FALSE.
END TYPE DefaultGnuplotOpt_

TYPE(DefaultGnuplotOpt_), PARAMETER :: defaultOpt = DefaultGnuplotOpt_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Label_ for title and axis labels

TYPE :: GnuPlotLabel_
  LOGICAL(LGT) :: isConfigured = .FALSE.
  CHARACTER(:), ALLOCATABLE :: text
  CHARACTER(:), ALLOCATABLE :: color
  CHARACTER(:), ALLOCATABLE :: fontname
  INTEGER(I4B) :: fontsize = NOT_INITIALIZED
  INTEGER(I4B) :: rotate = NOT_INITIALIZED
END TYPE GnuPlotLabel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Tick_ for axis ticks

TYPE :: GnuPlotTick_
  LOGICAL(LGT) :: isConfigured = .FALSE.
  INTEGER(I4B) :: plotscale = 0
  INTEGER(I4B) :: logBase = 10
  REAL(DFP) :: lims(2)
  ! CHARACTER(:), ALLOCATABLE :: color
  ! CHARACTER(:), ALLOCATABLE :: fontname
  ! INTEGER(I4B) :: fontsize = NOT_INITIALIZED
  ! INTEGER(I4B) :: rotate = NOT_INITIALIZED
END TYPE GnuPlotTick_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Axis_ type consist of label and tick

TYPE :: GnuPlotAxis_
  TYPE(String) :: name
  TYPE(GnuPlotLabel_) :: label
  TYPE(GnuPlotTick_) :: tick
END TYPE GnuPlotAxis_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-31
! summary:  options for plotting (plot, contour etc)

TYPE :: GnuPlotPlotOpts_
  TYPE(String), ALLOCATABLE :: lspecs(:)
  LOGICAL(LGT) :: fill = .FALSE.
  INTEGER(I4B) :: numLevels
  REAL(DFP), ALLOCATABLE :: levels(:)
  TYPE(String) :: paletteName
  TYPE(String) :: dataStyle
  ! datastyle: lines, points, linespoints
  LOGICAL(LGT) :: scaleData = .FALSE.
  TYPE(String) :: dataScale(3)
END TYPE GnuPlotPlotOpts_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary: Gnuplot Default Options

TYPE :: GnuplotOpt_

  TYPE(GnuPlotPlotOpts_) :: plotOpts

  LOGICAL(LGT) :: isInitiated = .FALSE.

  LOGICAL(LGT) :: runAfterWrite = .TRUE.

  LOGICAL(LGT) :: pauseAfterDraw = .FALSE.

  TYPE(GnuPlotLabel_) :: title
  TYPE(GnuPlotAxis_) :: xaxis, yaxis, zaxis, x2axis, y2axis, &
                        cbAxis

  TYPE(String) :: filename
  ! the name of physical file
  ! to write the gnuplot script
  TYPE(String) :: commandline

  TYPE(String), ALLOCATABLE :: options(:)
  ! vector of strings for gnuplot options
  TYPE(String), ALLOCATABLE :: scripts(:)
  ! vector of strings for gnuplot scripts

  ! terminal
  LOGICAL(LGT) :: useDefaultTerm = .TRUE.
  TYPE(String) :: termType, termFont
  ! termtype: wxt, qt, pngcairo, svg etc
  ! termfont: Times New Roman etc
  INTEGER(I4B) :: termFontSize
  INTEGER(I4B) :: termSize(2)

  ! animation
  LOGICAL(LGT) :: showAnimation = .FALSE.
  INTEGER(I4B) :: frameIndex
  REAL(DFP) :: pauseSeconds = 0.0_DFP

  ! multiplot parameters
  LOGICAL(LGT) :: setMultiplot = .FALSE.
  INTEGER(I4B) :: multiplotDims(2) ! row and col
  INTEGER :: multiplotIndex

  ! TODO: separate some configs
  LOGICAL(LGT) :: useDefaultPreset = .TRUE.

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  FINAL :: obj_Finalize

  !! @SET methods
  PROCEDURE, PUBLIC, PASS(obj) :: SetTerm => obj_SetTerm

  PROCEDURE, PUBLIC, PASS(obj) :: SetTitle => obj_SetTitle

  PROCEDURE, PUBLIC, PASS(obj) :: SetAxisLabel => obj_SetAxisLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLabel => obj_setxlabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Label => obj_setx2label
  PROCEDURE, PUBLIC, PASS(obj) :: Setylabel => obj_setylabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Label => obj_sety2label
  PROCEDURE, PUBLIC, PASS(obj) :: SetZLabel => obj_SetZLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetCBLabel => obj_SetCBLabel

  PROCEDURE, PUBLIC, PASS(obj) :: SetAxisLim => obj_SetAxisLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLim => obj_SetXLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Lim => obj_SetX2Lim
  PROCEDURE, PUBLIC, PASS(obj) :: SetYLim => obj_SetYLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Lim => obj_SetY2Lim
  PROCEDURE, PUBLIC, PASS(obj) :: SetZLim => obj_SetZLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetCBLim => obj_SetCBLim

  PROCEDURE, PUBLIC, PASS(obj) :: SetPlotScale => obj_SetPlotScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetXScale => obj_SetXScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Scale => obj_SetX2Scale
  PROCEDURE, PUBLIC, PASS(obj) :: SetYScale => obj_SetYScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Scale => obj_SetY2Scale
  PROCEDURE, PUBLIC, PASS(obj) :: SetZScale => obj_SetZScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetCBScale => obj_SetCBScale

  PROCEDURE, PUBLIC, PASS(obj) :: SetFilename => obj_SetFilename
  PROCEDURE, PUBLIC, PASS(obj) :: SetCommandLine => obj_SetCommandLine
  PROCEDURE, PUBLIC, PASS(obj) :: SetOptions => obj_SetOptions
  PROCEDURE, PUBLIC, PASS(obj) :: SetScripts => obj_SetScripts

  PROCEDURE, PUBLIC, PASS(obj) :: Reset => obj_Reset
  PROCEDURE, PUBLIC, PASS(obj) :: SetUseDefaultPreset => &
    obj_SetUseDefaultPreset

  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2

  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

END TYPE GnuplotOpt_

!----------------------------------------------------------------------------
!                                             Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-06
! update: 2025-02-21
! summary:  Initialize the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2024-06-06
! update: 2025-02-21
! summary: Deallocate the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Finalize@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2025-05-07
! summary:  Finalize the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Finalize(obj)
    TYPE(GnuPlotOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Finalize
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Display@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-06
! summary: Display the info

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GnuPlotOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                  multiplot@MultiPlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  initialize the multiplot

INTERFACE
  MODULE SUBROUTINE obj_multiplot(obj, dims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dims(2)
  END SUBROUTINE obj_multiplot
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_filename@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set filename
!
!# Introduction
!
!Set a file name for plot command output
!obj file can be used later by gnuplot as an script file to reproduce the plot

INTERFACE
  MODULE SUBROUTINE obj_SetFilename(obj, name)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetFilename
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setCommand
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-06-23
! summary:  set command which is executed at the end
!          if the length of chars is 0, then no command is executed

INTERFACE
  MODULE SUBROUTINE obj_SetCommandLine(obj, chars)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: chars
  END SUBROUTINE obj_SetCommandLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_options@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set options

!> author: Shion Shimizu
! date: 2026-05-05
! update: add index to overwrite the options
!
!# Introduction
!
! Set the plot options. obj is a very powerfull procedure accepts many types
! of gnuplot command and customization
! If reset is false (default is true), then the new options are added to the existing options
! if index is given, then (index)th component will be overwritten
! (automatically reset=false)

INTERFACE
  MODULE SUBROUTINE obj_SetOptions(obj, optionStr, reset, index)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: optionStr
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: reset
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: INDEX(:)
  END SUBROUTINE obj_SetOptions
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_options@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set gnuplot scripts from a long string
! string will be split by semicolon

INTERFACE
  MODULE SUBROUTINE obj_SetScripts(obj, scriptStr, reset)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: scriptStr
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: reset
  END SUBROUTINE obj_SetScripts
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetTerm@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set gnuplot terminal

INTERFACE
  MODULE SUBROUTINE obj_SetTerm(obj, termType, termSize, &
                                termFont, termFontSize)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: termType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: termSize(2)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: termFont
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: termFontSize
  END SUBROUTINE obj_SetTerm
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_xlim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set x limit
!
!# Introduction
!
!Set the x axis limits in form of [xmin, xmax]

INTERFACE
  MODULE SUBROUTINE obj_SetXLim(obj, lims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetXLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! data: 2025-12-22
! summary:  set x2 limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE obj_SetX2Lim(obj, lims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetX2Lim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set y limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE obj_SetYLim(obj, lims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetYLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! data: 2025-12-22
! summary:  set y2 limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE obj_SetY2Lim(obj, lims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetY2Lim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set z limit
!
!# Introduction
!
!Set the z axis limits in form of [zmin, zmax]

INTERFACE
  MODULE SUBROUTINE obj_SetZLim(obj, lims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetZLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-31
! summary:  set colorbar limits

INTERFACE
  MODULE SUBROUTINE obj_SetCBLim(obj, lims)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetCBLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_axis@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set axis with specific direction

INTERFACE
  MODULE SUBROUTINE obj_SetAxisLim(obj, lims, direction)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
    CHARACTER(*), INTENT(IN), OPTIONAL :: direction
  END SUBROUTINE obj_SetAxisLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary: Set plot scale

INTERFACE
  MODULE SUBROUTINE obj_SetPlotScale(obj, scaleChar, direction, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    CHARACTER(*), INTENT(IN) :: direction
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetPlotScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetXScale(obj, scaleChar, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetXScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetX2Scale(obj, scaleChar, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetX2Scale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetYScale(obj, scaleChar, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetYScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetY2Scale(obj, scaleChar, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetY2Scale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetZScale(obj, scaleChar, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetZScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetCBScale(obj, scaleChar, logBase)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetCBScale
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_plottitle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set plot title

INTERFACE
  MODULE SUBROUTINE obj_SetTitle(obj, title, color, fontSize, fontName, &
                                 rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetTitle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_xlabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set x label

INTERFACE
  MODULE SUBROUTINE obj_SetXLabel(obj, label, color, fontSize, fontName, &
                                  rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetXLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_x2label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set x2 label

INTERFACE
  MODULE SUBROUTINE obj_SetX2Label(obj, label, color, fontSize, fontName, &
                                   rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetX2Label
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_ylabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set y label

INTERFACE
  MODULE SUBROUTINE obj_SetYLabel(obj, label, color, fontSize, fontName, &
                                  rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetYLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_y2label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set y2 label

INTERFACE
  MODULE SUBROUTINE obj_SetY2Label(obj, label, color, fontSize, fontName, &
                                   rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetY2Label
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_zblabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set z label

INTERFACE
  MODULE SUBROUTINE obj_SetZLabel(obj, label, color, fontSize, fontName, &
                                  rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetZLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_zblabel@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-31
! summary:  Set colorbar label

INTERFACE
  MODULE SUBROUTINE obj_SetCBLabel(obj, label, color, fontSize, fontName, &
                                   rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetCBLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22

! summary:  set label
!
!# Introduction
!
! Set the text, color, font, size and rotation for labels including
! title, xlabel, x2label, ylabel, ....

INTERFACE
  MODULE SUBROUTINE obj_SetAxisLabel(obj, direction, label, color, &
                                     fontSize, fontName, rotate)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: direction
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetAxisLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                             reset_to_defaults@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  reset to defaults

INTERFACE
  MODULE SUBROUTINE obj_Reset(obj)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Reset
END INTERFACE

!----------------------------------------------------------------------------
!                                         use_preset_configuration@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  set useDefaultPreset

INTERFACE
  MODULE SUBROUTINE obj_SetUseDefaultPreset(obj, abool)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: abool
  END SUBROUTINE obj_SetUseDefaultPreset
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-23
! summary:  Import settings from toml

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-23
! summary:  Import settings from toml

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(GnuPlotOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GnuPlotOpt_Class
