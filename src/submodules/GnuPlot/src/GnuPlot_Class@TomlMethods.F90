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

SUBMODULE(GnuPlot_Class) TomlMethods
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: DOF_FMT, stdout, CHAR_LF
USE StringUtility, ONLY: UpperCase
USE InputUtility, ONLY: Input

USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_array

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportPauseAfterDrawFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL GetValue(table=table, key="pauseAfterDraw", &
                VALUE=obj%pauseAfterDraw, &
                default_value=.FALSE., &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportPauseAfterDrawFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportRunAfterWriteFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL GetValue(table=table, key="runAfterWrite", &
                VALUE=obj%runAfterWrite, &
                default_value=.TRUE., &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportRunAfterWriteFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportUseDefaultPresetFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL GetValue(table=table, key="useDefaultPreset", &
                VALUE=obj%useDefaultPreset, &
                default_value=.TRUE., &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportUseDefaultPresetFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportScriptsFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  ! array of strings will be imported
  CALL GetValue(table=table, key="scripts", &
                VALUE=obj%scripts, &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportScriptsFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportOptionsFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  ! array of strings will be imported
  CALL GetValue(table=table, key="options", &
                VALUE=obj%options, &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportOptionsFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportCommandLineFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL GetValue(table=table, key="commandline", &
                VALUE=obj%commandline, &
                default_value=defaultOpt%commandLine, &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportCommandLineFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportFileNameFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL GetValue(table=table, key="filename", &
                VALUE=obj%filename, default_value=defaultOpt%filename, &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportFileNameFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportDataStyleFromToml(obj, table)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL GetValue(table=table, key="dataStyle", &
                VALUE=obj%dataStyle, default_value=defaultOpt%dataStyle, &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportDataStyleFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportTickSettingsFromToml(obj, table)
  TYPE(Tick_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin, tsize
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr

  obj%isConfigured = .TRUE.

  CALL GetValue(table=table, key="plotScale", &
                VALUE=astr, default_value="", &
                origin=origin, stat=stat, isfound=isok)
  IF (isok) CALL Help_SetPlotScaleFromChar(astr%chars())

  CALL GetValue(table=table, key="logBase", &
                VALUE=obj%logBase, default_value=10, &
                origin=origin, stat=stat, isfound=isok)

  CALL GetValue_(table=table, key="lims", &
                 VALUE=obj%lims, tsize=tsize, &
                 origin=origin, stat=stat, isfound=isok)

CONTAINS

  SUBROUTINE Help_SetPlotScaleFromChar(char)
    CHARACTER(*), INTENT(IN) :: char

    SELECT CASE (UpperCase(CHAR(1:1)))
    CASE ("A") ! autoscale
      obj%plotscale = 1_I4B
    CASE ("L") ! log scale
      obj%plotscale = 2_I4B
    CASE DEFAULT
      obj%plotscale = 0_I4B ! no scale
    END SELECT

  END SUBROUTINE Help_SetPlotScaleFromChar

END SUBROUTINE ImportTickSettingsFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportLabelSettingsFromToml(obj, table)
  TYPE(Label_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr

  obj%isConfigured = .TRUE.

  CALL GetValue(table=table, key="text", &
                VALUE=astr, default_value="", &
                origin=origin, stat=stat, isfound=isok)
  IF (isok) obj%text = astr%chars()

  astr = ""
  CALL GetValue(table=table, key="color", &
                VALUE=astr, default_value="", &
                origin=origin, stat=stat, isfound=isok)
  IF (isok) obj%color = astr%chars()

  astr = ""
  CALL GetValue(table=table, key="fontname", &
                VALUE=astr, default_value="", &
                origin=origin, stat=stat, isfound=isok)
  IF (isok) obj%fontname = astr%chars()

  CALL GetValue(table=table, key="fontsize", &
                VALUE=obj%fontsize, default_value=NOT_INITIALIZED, &
                origin=origin, stat=stat, isfound=isok)

  CALL GetValue(table=table, key="rotate", &
                VALUE=obj%rotate, default_value=NOT_INITIALIZED, &
                origin=origin, stat=stat, isfound=isok)

END SUBROUTINE ImportLabelSettingsFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! This method should be called after ImportAxisNamesFromToml

SUBROUTINE ImportAxisSettingsFromToml(obj, table)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B) :: stat, origin

  CALL Help_GetAxisSettings(obj%xaxis, "x")

  CALL Help_GetAxisSettings(obj%x2axis, "x2")

  CALL Help_GetAxisSettings(obj%yaxis, "y")

  CALL Help_GetAxisSettings(obj%y2axis, "y2")

  CALL Help_GetAxisSettings(obj%zaxis, "z")

CONTAINS

  SUBROUTINE Help_GetAxisSettings(ax, direction)
    TYPE(Axis_), INTENT(INOUT) :: ax
    CHARACTER(*), INTENT(IN) :: direction
    TYPE(toml_table), POINTER :: node => NULL(), &
                                 label_node => NULL(), &
                                 tick_node => NULL()

    CALL toml_get(table, ax%name%chars(), node, &
                  origin=origin, requested=.FALSE., stat=stat)
    IF (.NOT. ASSOCIATED(node)) RETURN

    CALL toml_get(node, "label", label_node, &
                  origin=origin, requested=.FALSE., stat=stat)
    IF (ASSOCIATED(label_node)) &
      CALL ImportLabelSettingsFromToml(ax%label, label_node)

    label_node => NULL()

    CALL toml_get(node, "tick", tick_node, &
                  origin=origin, requested=.FALSE., stat=stat)
    IF (ASSOCIATED(tick_node)) &
      CALL ImportTickSettingsFromToml(ax%tick, tick_node)

    tick_node => NULL()

  END SUBROUTINE Help_GetAxisSettings

END SUBROUTINE ImportAxisSettingsFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportAxisNamesFromToml(obj, table)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B) :: stat, origin
  LOGICAL(LGT) :: isok

  CALL Help_GetAxisName(obj%xaxis, "x")

  CALL Help_GetAxisName(obj%x2axis, "x2")

  CALL Help_GetAxisName(obj%yaxis, "y")

  CALL Help_GetAxisName(obj%y2axis, "y2")

  CALL Help_GetAxisName(obj%zaxis, "z")

CONTAINS

  SUBROUTINE Help_GetAxisName(ax, direction)
    TYPE(Axis_), INTENT(INOUT) :: ax
    CHARACTER(*), INTENT(IN) :: direction

    CALL GetValue(table=table, key=direction//"AxisName", &
                  VALUE=ax%name, default_value=direction, &
                  origin=origin, stat=stat, isfound=isok)

  END SUBROUTINE Help_GetAxisName

END SUBROUTINE ImportAxisNamesFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportTermOptsFromToml(obj, table)
  CLASS(GnuPlot_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: stat, origin, tsize
  LOGICAL(LGT) :: isok

  CHARACTER(*), PARAMETER :: tomlName = "term"

  CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
                stat=stat)

  isok = ASSOCIATED(node)
  IF (.NOT. isok) RETURN

  obj%useDefaultTerm = .FALSE.

  CALL GetValue(table=node, key="termType", VALUE=obj%termType, &
                default_value=defaultOpt%termType, origin=origin, &
                stat=stat, isfound=isok)

  CALL GetValue_(table=node, key="termSize", VALUE=obj%termSize, &
                 origin=origin, stat=stat, isfound=isok, tsize=tsize)
  IF (.NOT. isok) obj%termSize = defaultOpt%termSize

  CALL GetValue(table=node, key="termFont", VALUE=obj%termFont, &
                default_value=defaultOpt%termFont, origin=origin, &
                stat=stat, isfound=isok)

  CALL GetValue(table=node, key="termFontSize", VALUE=obj%termFontSize, &
                default_value=defaultOpt%termFontSize, origin=origin, &
                stat=stat, isfound=isok)

END SUBROUTINE ImportTermOptsFromToml

!----------------------------------------------------------------------------
!                                                          ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ImportFileNameFromToml(obj, table)

CALL ImportCommandLineFromToml(obj, table)

CALL ImportTermOptsFromToml(obj, table)

CALL ImportDataStyleFromToml(obj, table)

CALL ImportUseDefaultPresetFromToml(obj, table)

CALL ImportOptionsFromToml(obj, table)

CALL ImportScriptsFromToml(obj, table)

CALL ImportAxisNamesFromToml(obj, table)

CALL ImportAxisSettingsFromToml(obj, table)

CALL ImportRunAfterWriteFromToml(obj, table)

CALL ImportPauseAfterDrawFromToml(obj, table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                          ImportFromToml2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myname, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myname, &
                  'Cannot find '//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), myname//" Domain toml config: "// &
               CHAR_LF, unitno=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
