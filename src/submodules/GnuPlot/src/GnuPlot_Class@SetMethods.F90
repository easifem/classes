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

SUBMODULE(GnuPlot_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setPm3dOpts
obj%pm3dOpts_stmt = "set pm3d "//TRIM(opts)
END PROCEDURE obj_setPm3dOpts

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCBTicks
obj%cbTicks_stmt = "set cbtics "//TRIM(opts)
END PROCEDURE obj_setCBTicks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCntrLevels
obj%cntrLevels_stmt = "set cntrparam levels "//TRIM(opts)
END PROCEDURE obj_setCntrLevels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCBLim
obj%hasCBRange = .TRUE.
obj%CBRange = avec
END PROCEDURE obj_setCBLim

!----------------------------------------------------------------------------
!                                                           set_filename
!----------------------------------------------------------------------------

MODULE PROCEDURE set_filename
obj%txtfilename = TRIM(chars)
obj%hasfilename = .TRUE.
END PROCEDURE set_filename

!----------------------------------------------------------------------------
!                                                                set_options
!----------------------------------------------------------------------------

MODULE PROCEDURE set_options
IF (.NOT. ALLOCATED(obj%txtoptions)) obj%txtoptions = ''
IF (LEN_TRIM(obj%txtoptions) == 0) THEN
  obj%txtoptions = '' ! initialize chars
END IF
IF (LEN_TRIM(stropt) > 0) THEN
  obj%txtoptions = obj%txtoptions//splitstr(stropt)
END IF

obj%hasoptions = .TRUE.
END PROCEDURE set_options

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE set_xlim
obj%hasxrange = .TRUE.
obj%xrange = rng
END PROCEDURE set_xlim

!----------------------------------------------------------------------------
!                                                                   set_ylim
!----------------------------------------------------------------------------

MODULE PROCEDURE set_ylim
obj%hasyrange = .TRUE.
obj%yrange = rng
END PROCEDURE set_ylim

!----------------------------------------------------------------------------
!                                                                   set_zlim
!----------------------------------------------------------------------------

MODULE PROCEDURE set_zlim
obj%haszrange = .TRUE.
obj%zrange = rng
END PROCEDURE set_zlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE set_axis
INTEGER :: n

n = SIZE(rng, dim=1)

SELECT CASE (n)
CASE (2) !Only the range for x-axis has been sent
  obj%hasxrange = .TRUE.
  obj%xrange = rng(1:2)
CASE (4)
  obj%hasxrange = .TRUE.
  obj%hasyrange = .TRUE.
  obj%xrange = rng(1:2)
  obj%yrange = rng(3:4)
CASE (6)
  obj%hasxrange = .TRUE.
  obj%hasyrange = .TRUE.
  obj%haszrange = .TRUE.
  obj%xrange = rng(1:2)
  obj%yrange = rng(3:4)
  obj%zrange = rng(5:6)
CASE default
  PRINT *, 'GnuPlot_ error: wrong axis range setting!'
  RETURN
END SELECT

END PROCEDURE set_axis

!----------------------------------------------------------------------------
!                                                        set_secondary_axis
!----------------------------------------------------------------------------

MODULE PROCEDURE set_secondary_axis
INTEGER :: n

n = SIZE(rng, dim=1)

SELECT CASE (n)
CASE (2) !Only the range for x2-axis has been sent
  obj%hasx2range = .TRUE.
  obj%x2range = rng(1:2)
CASE (4)
  obj%hasx2range = .TRUE.
  obj%hasy2range = .TRUE.
  obj%x2range = rng(1:2)
  obj%y2range = rng(3:4)
CASE default
  PRINT *, 'GnuPlot_ error: wrong axis range setting!'
  RETURN
END SELECT

END PROCEDURE set_secondary_axis

!----------------------------------------------------------------------------
!                                                             set_plottitle
!----------------------------------------------------------------------------

MODULE PROCEDURE set_plottitle
CALL obj%set_label('plot_title', chars, textcolor, font_size, &
                   font_name, rotate)
END PROCEDURE set_plottitle

!----------------------------------------------------------------------------
!                                                                 set_xlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE set_xlabel
CALL obj%set_label('xlabel', chars, textcolor, font_size, font_name, rotate)
END PROCEDURE set_xlabel

!----------------------------------------------------------------------------
!                                                                set_x2label
!----------------------------------------------------------------------------

MODULE PROCEDURE set_x2label
CALL obj%set_label('x2label', chars, textcolor, font_size, font_name, rotate)
END PROCEDURE set_x2label

!----------------------------------------------------------------------------
!                                                                 set_ylabel
!----------------------------------------------------------------------------

MODULE PROCEDURE set_ylabel
CALL obj%set_label('ylabel', chars, textcolor, font_size, font_name, rotate)
END PROCEDURE set_ylabel

!----------------------------------------------------------------------------
!                                                                set_y2label
!----------------------------------------------------------------------------

MODULE PROCEDURE set_y2label
CALL obj%set_label('y2label', chars, textcolor, font_size, font_name, rotate)
END PROCEDURE set_y2label

!----------------------------------------------------------------------------
!                                                                 set_zlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE set_zlabel
CALL obj%set_label('zlabel', chars, textcolor, font_size, font_name, rotate)
END PROCEDURE set_zlabel

!----------------------------------------------------------------------------
!                                                                 set_label
!----------------------------------------------------------------------------

MODULE PROCEDURE set_label
! local variable
TYPE(Label_) :: label

label%hasLabel = .TRUE.
label%text = TRIM(lbltext)

IF (PRESENT(lblcolor)) THEN
  label%color = lblcolor
END IF

IF (PRESENT(font_name)) THEN
  label%fontname = font_name
ELSE
  IF (.NOT. ALLOCATED(label%fontname)) THEN
    label%fontname = ''
  END IF
END IF

IF (PRESENT(font_size)) THEN
  label%fontsize = font_size
END IF

IF (PRESENT(rotate)) THEN
  label%rotate = rotate
END IF

SELECT CASE (lblname)
CASE ('xlabel')
  obj%tpxlabel = label
CASE ('x2label')
  obj%tpx2label = label
CASE ('ylabel')
  obj%tpylabel = label
CASE ('y2label')
  obj%tpy2label = label
CASE ('zlabel')
  obj%tpzlabel = label
CASE ('plot_title')
  obj%tpplottitle = label
END SELECT

END PROCEDURE set_label

!----------------------------------------------------------------------------
!                                                       reset_to_defaults
!----------------------------------------------------------------------------

MODULE PROCEDURE reset_to_defaults
obj%preset_configuration = .TRUE.
obj%txtfilename = gnuplot_output_filename

IF (ALLOCATED(obj%txtoptions)) DEALLOCATE (obj%txtoptions)
IF (ALLOCATED(obj%txtscript)) DEALLOCATE (obj%txtscript)
IF (ALLOCATED(obj%txtdatastyle)) DEALLOCATE (obj%txtdatastyle)
IF (ALLOCATED(obj%msg)) DEALLOCATE (obj%msg)

obj%hasoptions = .FALSE.

obj%hasxrange = .FALSE.
obj%hasx2range = .FALSE.
obj%hasyrange = .FALSE.
obj%hasy2range = .FALSE.
obj%haszrange = .FALSE.

obj%pause_seconds = 0.0
obj%status = 0
obj%hasanimation = .FALSE.
obj%hasfileopen = .FALSE.
obj%hasmultiplot = .FALSE.

obj%plotscale = ''
obj%tpplottitle%hasLabel = .FALSE.
obj%tpxlabel%hasLabel = .FALSE.
obj%tpx2label%hasLabel = .FALSE.
obj%tpylabel%hasLabel = .FALSE.
obj%tpy2label%hasLabel = .FALSE.
obj%tpzlabel%hasLabel = .FALSE.

END PROCEDURE reset_to_defaults

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE use_preset_configuration
! default is true
obj%preset_configuration = flag
END PROCEDURE use_preset_configuration

END SUBMODULE SetMethods
