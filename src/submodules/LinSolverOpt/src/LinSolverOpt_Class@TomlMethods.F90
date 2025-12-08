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
!

SUBMODULE(LinSolverOpt_Class) TomlMethods
USE GlobalData, ONLY: CHAR_LF, stdout
USE Display_Method, ONLY: Display, ToString
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue, GetValue_
USE StringUtility, ONLY: LowerCase, UpperCase
USE String_Class, ONLY: String

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok
TYPE(toml_table), POINTER :: node, child
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL ImportEngineFromToml(obj=obj, table=table, origin=origin, &
                          stat=stat)

CALL ImportSolverNameFromToml(obj=obj, table=table, origin=origin, &
                              stat=stat)

CALL ImportConvergenceInFromToml(obj=obj, table=table, origin=origin, &
                                 stat=stat)

CALL ImportConvergenceTypeFromToml(obj=obj, table=table, origin=origin, &
                                   stat=stat)

CALL ImportScaleFromToml(obj=obj, table=table, origin=origin, &
                         stat=stat)

CALL ImportMaxIterFromToml(obj=obj, table=table, origin=origin, &
                           stat=stat)

CALL ImportKrylovSubspaceSizeFromToml(obj=obj, table=table, &
                                      origin=origin, stat=stat)

CALL ImportBicgstabEllFromToml(obj=obj, table=table, &
                               origin=origin, stat=stat)

CALL ImportRtolFromToml(obj=obj, table=table, origin=origin, &
                        stat=stat)

CALL ImportAtolFromToml(obj=obj, table=table, origin=origin, &
                        stat=stat)

CALL ImportRelativeToRHSFromToml(obj=obj, table=table, &
                                 origin=origin, stat=stat)

CALL ImportInitXZerosFromToml(obj=obj, table=table, &
                              origin=origin, stat=stat)

CALL ImportSorOmegaFromToml(obj=obj, table=table, &
                            origin=origin, stat=stat)

node => NULL()
CALL toml_get(table, "precondition", node, origin=origin, &
              stat=stat, requested=.FALSE.)

isok = ASSOCIATED(node)
IF (.NOT. isok) THEN
  node => NULL()
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL ImportPreconditionOptionFromToml(obj=obj, table=node, &
                                      origin=origin, stat=stat)

CALL ImportPreconditionNameFromToml(obj=obj, table=node, &
                                    origin=origin, stat=stat)

astr = TRIM(obj%p_name_char)
child => NULL()
CALL toml_get(node, astr, child, origin=origin, stat=stat, &
              requested=.FALSE.)

isok = ASSOCIATED(child)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  astr = ""
  RETURN
END IF

astr = UpperCase(astr)
SELECT CASE (astr)
CASE ("NONE")
  ! do nothing
CASE ("ILU")
  CALL ilu_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("HYBRID")
  CALL hybrid_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("IS")
  CALL is_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("ADDS")
  CALL adds_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("SSOR")
  CALL ssor_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("SAINV")
  CALL sainv_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("SAAMG")
  CALL saamg_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

CASE ("ILUC")
  CALL iluc_import_from_toml(obj=obj, table=child, origin=origin, stat=stat)

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node)

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                        ImportEngineFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportEngineFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportEngineFromToml()"
#endif

  TYPE(String) :: engine

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="engine", VALUE=engine, &
              default_value=TypeLinSolverOpt%engine, origin=origin, stat=stat)

  obj%engine = engine%chars()

  engine = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportEngineFromToml

!----------------------------------------------------------------------------
!                                                   ImportSolverNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportSolverNameFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportSolverNameFromToml()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="solverName", VALUE=astr, &
    default_value=TypeLinSolverOpt%solverName_char, origin=origin, stat=stat)

  obj%solverName = TypeLinSolverOpt%SolverNameToInteger(astr%chars())
  obj%solverName_char = astr%chars()

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportSolverNameFromToml

!----------------------------------------------------------------------------
!                                                 ImportConvergenceInFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportConvergenceInFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportConvergenceInFromToml()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="convergenceIn", VALUE=astr, &
    default_value=TypeLinSolverOpt%convergenceIn_char, &
    origin=origin, stat=stat)

  obj%convergenceIn = TypeLinSolverOpt%ConvergenceInToInteger( &
                      astr%chars())

  obj%convergenceIn_char = astr%chars()

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportConvergenceInFromToml

!----------------------------------------------------------------------------
!                                               ImportConvergenceTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportConvergenceTypeFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportConvergenceTypeFromToml()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="convergenceType", VALUE=astr, &
    default_value=TypeLinSolverOpt%convergenceType_char, &
    origin=origin, stat=stat)

  obj%convergenceType = TypeLinSolverOpt%ConvergenceTypeToInteger( &
                        astr%chars())
  obj%convergenceType_char = astr%chars()

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportconvergenceTypeFromToml

!----------------------------------------------------------------------------
!                                                         ImportScaleFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportScaleFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportScaleFromToml()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="scale", VALUE=astr, &
                default_value=TypeLinSolverOpt%scale_char, &
                origin=origin, stat=stat)

  obj%scale = TypeLinSolverOpt%ScaleToInteger(astr%chars())
  obj%scale_char = astr%chars()

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportScaleFromToml

!----------------------------------------------------------------------------
!                                                      ImportMaxIterFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportMaxIterFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportMaxIterFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="maxIter", VALUE=obj%maxIter, &
                default_value=TypeLinSolverOpt%maxIter, &
                origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportMaxIterFromToml

!----------------------------------------------------------------------------
!                                            ImportKrylovSubspaceSizeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportKrylovSubspaceSizeFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportKrylovSubspaceSizeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="krylovSubspaceSize", VALUE=obj%krylovSubspaceSize, &
    default_value=TypeLinSolverOpt%krylovSubspaceSize, origin=origin, &
    stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportKrylovSubspaceSizeFromToml

!----------------------------------------------------------------------------
!                                                  ImportBicgstabEllFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBicgstabEllFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBicgstabEllFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="bicgstab_ell", VALUE=obj%bicgstab_ell, &
    default_value=TypeLinSolverOpt%bicgstab_ell, origin=origin, &
    stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBicgstabEllFromToml

!----------------------------------------------------------------------------
!                                                          ImportRtolFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportRtolFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportRtolFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="rtol", VALUE=obj%rtol, &
    default_value=TypeLinSolverOpt%rtol, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportRtolFromToml

!----------------------------------------------------------------------------
!                                                          ImportAtolFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportAtolFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportAtolFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="atol", VALUE=obj%atol, &
    default_value=TypeLinSolverOpt%atol, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportAtolFromToml

!----------------------------------------------------------------------------
!                                                 ImportRelativeToRHSFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportRelativeToRHSFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportRelativeToRHSFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="relativeToRHS", VALUE=obj%relativeToRHS, &
    default_value=TypeLinSolverOpt%relativeToRHS, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportRelativeToRHSFromToml

!----------------------------------------------------------------------------
!                                                    ImportInitXZerosFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportInitXZerosFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportInitZerosFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="initx_zeros", VALUE=obj%initx_zeros, &
    default_value=TypeLinSolverOpt%initx_zeros, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportInitXZerosFromToml

!----------------------------------------------------------------------------
!                                                     ImportSorOmegaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportSorOmegaFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportSorOmegaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="sor_omega", VALUE=obj%sor_omega, &
    default_value=TypeLinSolverOpt%sor_omega, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportSorOmegaFromToml

!----------------------------------------------------------------------------
!                                            ImportPreconditionOptionFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportPreconditionOptionFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportPreconditionOptionFromToml()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="option", VALUE=astr, &
    default_value=TypeLinSolverOpt%preconditionOption_char, origin=origin, &
    stat=stat)

  obj%preconditionOption_char = astr%chars()
  obj%preconditionOption = TypeLinSolverOpt%PrecondOptToInteger(astr%chars())

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportPreconditionOptionFromToml

!----------------------------------------------------------------------------
!                                            ImportPreconditionNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportPreconditionNameFromToml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportPreconditionNameFromToml()"
#endif

  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="name", VALUE=astr, &
    default_value=TypeLinSolverOpt%p_name_char, origin=origin, &
    stat=stat)

  obj%p_name_char = astr%chars()
  obj%p_name = TypeLinSolverOpt%PrecondNameToInteger(astr%chars())

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportPreconditionNameFromToml

!----------------------------------------------------------------------------
!                                                       ilu_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE ilu_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ilu_import_from_toml()"
#endif
  INTEGER(I4B) :: p_ilu_lfil, p_ilu_mbloc, p_ilu_fill
  REAL(DFP) :: p_ilu_droptol, p_ilu_permtol, p_ilu_alpha

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="lfil", VALUE=p_ilu_lfil, &
            default_value=TypeLinSolverOpt%ilu_lfil, origin=origin, stat=stat)

  CALL GetValue(table=table, key="mbloc", VALUE=p_ilu_mbloc, &
           default_value=TypeLinSolverOpt%ilu_mbloc, origin=origin, stat=stat)

  CALL GetValue(table=table, key="fill", VALUE=p_ilu_fill, &
            default_value=TypeLinSolverOpt%ilu_fill, origin=origin, stat=stat)

  CALL GetValue(table=table, key="droptol", VALUE=p_ilu_droptol, &
         default_value=TypeLinSolverOpt%ilu_droptol, origin=origin, stat=stat)

  CALL GetValue(table=table, key="permtol", VALUE=p_ilu_permtol, &
         default_value=TypeLinSolverOpt%ilu_permtol, origin=origin, stat=stat)

  CALL GetValue(table=table, key="alpha", VALUE=p_ilu_alpha, &
           default_value=TypeLinSolverOpt%ilu_alpha, origin=origin, stat=stat)

  CALL obj%SetPrecondIlu( &
    p_ilu_lfil=p_ilu_lfil, p_ilu_mbloc=p_ilu_mbloc, &
    p_ilu_droptol=p_ilu_droptol, p_ilu_permtol=p_ilu_permtol, &
    p_ilu_alpha=p_ilu_alpha, p_ilu_fill=p_ilu_fill)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ilu_import_from_toml

!----------------------------------------------------------------------------
!                                                    hybrid_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE hybrid_import_from_toml(obj, table, stat, origin)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "hybrid_import_from_toml()"
#endif
  TYPE(String) :: p_hybrid_i_char
  INTEGER(I4B) :: p_hybrid_maxiter, p_hybrid_ell, p_hybrid_restart, &
                  p_hybrid_i
  REAL(DFP) :: p_hybrid_tol, p_hybrid_omega

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="name", VALUE=p_hybrid_i_char, &
    default_value=TypeLinSolverOpt%hybrid_i_char, origin=origin, stat=stat)

  p_hybrid_i = obj%SolverNameToInteger(p_hybrid_i_char%chars())
  ! tempint = obj%solverName_ToInteger(p_hybrid_i%chars())

  CALL GetValue( &
    table=table, key="maxIter", VALUE=p_hybrid_maxiter, &
    default_value=TypeLinSolverOpt%hybrid_maxiter, origin=origin, stat=stat)

  CALL GetValue(table=table, key="ell", VALUE=p_hybrid_ell, &
          default_value=TypeLinSolverOpt%hybrid_ell, origin=origin, stat=stat)

  CALL GetValue(table=table, key="restart", VALUE=p_hybrid_restart, &
      default_value=TypeLinSolverOpt%hybrid_restart, origin=origin, stat=stat)

  CALL GetValue(table=table, key="tol", VALUE=p_hybrid_tol, &
          default_value=TypeLinSolverOpt%hybrid_tol, origin=origin, stat=stat)

  CALL GetValue(table=table, key="omega", VALUE=p_hybrid_omega, &
        default_value=TypeLinSolverOpt%hybrid_omega, origin=origin, stat=stat)

  CALL obj%SetPrecondHybrid( &
    p_hybrid_i=p_hybrid_i, p_hybrid_maxiter=p_hybrid_maxiter, &
    p_hybrid_tol=p_hybrid_tol, p_hybrid_omega=p_hybrid_omega, &
    p_hybrid_ell=p_hybrid_ell, p_hybrid_restart=p_hybrid_restart)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE hybrid_import_from_toml

!----------------------------------------------------------------------------
!                                                        is_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE is_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "is_import_from_toml()"
#endif

  INTEGER(I4B) :: p_is_m
  REAL(DFP) :: p_is_alpha

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="m", VALUE=p_is_m, &
    default_value=TypeLinSolverOpt%is_m, origin=origin, stat=stat)

  CALL GetValue( &
    table=table, key="alpha", VALUE=p_is_alpha, &
    default_value=TypeLinSolverOpt%is_alpha, origin=origin, stat=stat)

  CALL obj%SetPrecondIS(p_is_m=p_is_m, p_is_alpha=p_is_alpha)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE is_import_from_toml

!----------------------------------------------------------------------------
!                                                      adds_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE adds_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "adds_import_from_toml()"
#endif

  INTEGER(I4B) :: p_adds_iter
  LOGICAL(LGT) :: p_adds

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="iter", VALUE=p_adds_iter, &
    default_value=TypeLinSolverOpt%adds_iter, origin=origin, stat=stat)

  CALL GetValue( &
    table=table, key="isAdditiveSchwarz", VALUE=p_adds, &
    default_value=TypeLinSolverOpt%adds, origin=origin, stat=stat)

  CALL obj%SetPrecondADDS(p_adds_iter=p_adds_iter, p_adds=p_adds)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE adds_import_from_toml

!----------------------------------------------------------------------------
!                                                       ssor_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE ssor_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ssor_import_from_toml()"
#endif

  REAL(DFP) :: p_ssor_omega

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="omega", VALUE=p_ssor_omega, &
    default_value=TypeLinSolverOpt%ssor_omega, origin=origin, stat=stat)

  CALL obj%SetPrecondSSOR(p_ssor_omega=p_ssor_omega)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ssor_import_from_toml

!----------------------------------------------------------------------------
!                                                       sainv_import_from_tom
!----------------------------------------------------------------------------

SUBROUTINE sainv_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "sainv_import_from_toml()"
#endif

  REAL(DFP) :: p_sainv_drop

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="drop", VALUE=p_sainv_drop, &
    default_value=TypeLinSolverOpt%sainv_drop, origin=origin, stat=stat)

  CALL obj%SetPrecondSAINV(p_sainv_drop=p_sainv_drop)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE sainv_import_from_toml

!----------------------------------------------------------------------------
!                                                      saamg_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE saamg_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "saamg_import_from_toml()"
#endif

  REAL(DFP) :: p_saamg_theta
  LOGICAL(LGT) :: p_saamg_unsym

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="theta", VALUE=p_saamg_theta, &
    default_value=TypeLinSolverOpt%saamg_theta, origin=origin, stat=stat)

  CALL GetValue( &
    table=table, key="unsym", VALUE=p_saamg_unsym, &
    default_value=TypeLinSolverOpt%saamg_unsym, origin=origin, stat=stat)

  CALL obj%SetPrecondSAAMG(p_saamg_theta=p_saamg_theta, &
                           p_saamg_unsym=p_saamg_unsym)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE saamg_import_from_toml

!----------------------------------------------------------------------------
!                                                       iluc_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE iluc_import_from_toml(obj, table, origin, stat)
  CLASS(LinSolverOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "iluc_import_from_toml()"
#endif
  REAL(DFP) :: p_iluc_drop, p_iluc_rate

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="drop", VALUE=p_iluc_drop, &
    default_value=TypeLinSolverOpt%iluc_drop, origin=origin, stat=stat)

  CALL GetValue( &
    table=table, key="rate", VALUE=p_iluc_rate, &
    default_value=TypeLinSolverOpt%iluc_rate, origin=origin, stat=stat)

  CALL obj%SetPrecondILUC(p_iluc_rate=p_iluc_rate, p_iluc_drop=p_iluc_drop)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE iluc_import_from_toml

!----------------------------------------------------------------------------
!                                                            Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
