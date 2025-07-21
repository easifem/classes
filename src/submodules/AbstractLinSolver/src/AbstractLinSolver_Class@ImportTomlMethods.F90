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

SUBMODULE(AbstractLinSolver_Class) ImportTomlMethods
USE GlobalData, ONLY: CHAR_LF, stdout
USE Display_Method, ONLY: Display, ToString
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue, GetValue_
USE StringUtility, ONLY: LowerCase, UpperCase

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       ilu_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE ilu_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

  INTEGER(I4B) :: p_ilu_lfil, p_ilu_mbloc, p_ilu_fill
  REAL(DFP) :: p_ilu_droptol, p_ilu_permtol, p_ilu_alpha

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ilu_import_from_toml()"
#endif

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

  CALL SetPrecondIluParam(param=param, prefix=prefix, &
                          p_ilu_lfil=p_ilu_lfil, &
                          p_ilu_mbloc=p_ilu_mbloc, &
                          p_ilu_droptol=p_ilu_droptol, &
                          p_ilu_permtol=p_ilu_permtol, &
                          p_ilu_alpha=p_ilu_alpha, &
                          p_ilu_fill=p_ilu_fill)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ilu_import_from_toml

!----------------------------------------------------------------------------
!                                                        hybrid_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE hybrid_import_from_toml(obj, param, prefix, table)
  CLASS(AbstractLinSolver_), INTENT(IN) :: obj
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "hybrid_import_from_toml()"
#endif

  TYPE(String) :: p_hybrid_i
  INTEGER(I4B) :: p_hybrid_maxiter, p_hybrid_ell, p_hybrid_restart, tempint
  REAL(DFP) :: p_hybrid_tol, p_hybrid_omega

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="name", VALUE=p_hybrid_i, &
       default_value=TypeLinSolverOpt%hybrid_i_char, origin=origin, stat=stat)

  CALL GetValue(table=table, key="maxIter", VALUE=p_hybrid_maxiter, &
      default_value=TypeLinSolverOpt%hybrid_maxiter, origin=origin, stat=stat)

  CALL GetValue(table=table, key="ell", VALUE=p_hybrid_ell, &
          default_value=TypeLinSolverOpt%hybrid_ell, origin=origin, stat=stat)

  CALL GetValue(table=table, key="restart", VALUE=p_hybrid_restart, &
      default_value=TypeLinSolverOpt%hybrid_restart, origin=origin, stat=stat)

  CALL GetValue(table=table, key="tol", VALUE=p_hybrid_tol, &
          default_value=TypeLinSolverOpt%hybrid_tol, origin=origin, stat=stat)

  CALL GetValue(table=table, key="omega", VALUE=p_hybrid_omega, &
        default_value=TypeLinSolverOpt%hybrid_omega, origin=origin, stat=stat)

  tempint = obj%solverName_ToInteger(p_hybrid_i%chars())
  CALL SetPrecondHybridParam(param=param, prefix=prefix, &
                             p_hybrid_i=tempint, &
                             p_hybrid_maxiter=p_hybrid_maxiter, &
                             p_hybrid_tol=p_hybrid_tol, &
                             p_hybrid_omega=p_hybrid_omega, &
                             p_hybrid_ell=p_hybrid_ell, &
                             p_hybrid_restart=p_hybrid_restart)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE hybrid_import_from_toml

!----------------------------------------------------------------------------
!                                                        is_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE is_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

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

  CALL GetValue(table=table, key="m", VALUE=p_is_m, &
                default_value=TypeLinSolverOpt%is_m, origin=origin, stat=stat)

  CALL GetValue(table=table, key="p_is_alpha", VALUE=p_is_alpha, &
            default_value=TypeLinSolverOpt%is_alpha, origin=origin, stat=stat)

  CALL SetPrecondIsParam(param=param, prefix=prefix, &
                         p_is_m=p_is_m, p_is_alpha=p_is_alpha)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE is_import_from_toml

!----------------------------------------------------------------------------
!                                                        ssor_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE adds_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

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

  CALL GetValue(table=table, key="iter", VALUE=p_adds_iter, &
           default_value=TypeLinSolverOpt%adds_iter, origin=origin, stat=stat)

  CALL GetValue(table, "isAdditiveSchwarz", p_adds, &
                default_value=TypeLinSolverOpt%adds, origin=origin, stat=stat)

  CALL SetPrecondAddsParam(param=param, prefix=prefix, &
                           p_adds_iter=p_adds_iter, p_adds=p_adds)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE adds_import_from_toml

!----------------------------------------------------------------------------
!                                                        ssor_import_from_toml
!----------------------------------------------------------------------------

SUBROUTINE ssor_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ssor_import_from_toml()"
#endif

  REAL(DFP) :: p_ssor_omega

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="omega", VALUE=p_ssor_omega, &
          default_value=TypeLinSolverOpt%ssor_omega, origin=origin, stat=stat)

  CALL SetPrecondSsorParam(param=param, prefix=prefix, &
                           p_ssor_omega=p_ssor_omega)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ssor_import_from_toml

!----------------------------------------------------------------------------
!                                                        sainv_import_from_tom
!----------------------------------------------------------------------------

SUBROUTINE sainv_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "sainv_import_from_toml()"
#endif

  REAL(DFP) :: p_sainv_drop

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="drop", VALUE=p_sainv_drop, &
          default_value=TypeLinSolverOpt%sainv_drop, origin=origin, stat=stat)

  CALL SetPrecondSainvParam(param=param, prefix=prefix, &
                            p_sainv_drop=p_sainv_drop)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE sainv_import_from_toml

!----------------------------------------------------------------------------
!                                                        saamg_import_from_tom
!----------------------------------------------------------------------------

SUBROUTINE saamg_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

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

  CALL GetValue(table=table, key="theta", VALUE=p_saamg_theta, &
         default_value=TypeLinSolverOpt%saamg_theta, origin=origin, stat=stat)

  CALL GetValue(table=table, key="unsym", VALUE=p_saamg_unsym, &
         default_value=TypeLinSolverOpt%saamg_unsym, origin=origin, stat=stat)

  CALL SetPrecondSaamgParam(param=param, prefix=prefix, &
                            p_saamg_theta=p_saamg_theta, &
                            p_saamg_unsym=p_saamg_unsym)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE saamg_import_from_toml

!----------------------------------------------------------------------------
!                                                        iluc_import_from_tom
!----------------------------------------------------------------------------

SUBROUTINE iluc_import_from_toml(param, prefix, table)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix
  TYPE(toml_table), POINTER, INTENT(IN) :: table
  INTEGER(I4B) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "iluc_import_from_toml()"
#endif
  REAL(DFP) :: p_iluc_drop, p_iluc_rate

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="drop", VALUE=p_iluc_drop, &
           default_value=TypeLinSolverOpt%iluc_drop, origin=origin, stat=stat)

  CALL GetValue(table=table, key="rate", VALUE=p_iluc_rate, &
           default_value=TypeLinSolverOpt%iluc_rate, origin=origin, stat=stat)

  CALL SetPrecondIlucParam(param=param, prefix=prefix, &
                           p_iluc_rate=p_iluc_rate, p_iluc_drop=p_iluc_drop)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE iluc_import_from_toml

!----------------------------------------------------------------------------
!                                                     ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportParamFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportParamFromToml()"
#endif

TYPE(toml_table), POINTER :: child, node
CHARACTER(:), ALLOCATABLE :: prefix, tempstr

TYPE(String) :: engine, solverName, preconditionOption, &
                p_name, convergenceIn, convergenceType, scale, p_hybrid_i

INTEGER(I4B) :: maxIter, krylovSubspaceSize, bicgstab_ell, &
                p_ilu_lfil, p_ilu_mbloc, p_ilu_fill, p_hybrid_maxiter, &
                p_hybrid_ell, p_hybrid_restart, p_is_m, p_adds_iter, &
                origin, stat, solverName_int, preconditionOption_int, &
                convergenceIn_int, convergenceType_int, scale_int, &
                p_name_int, p_hybrid_i_int

REAL(DFP) :: atol, rtol, sor_omega, p_is_alpha, &
             p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, p_ssor_omega, &
             p_hybrid_tol, p_hybrid_omega, p_sainv_drop, p_saamg_theta, &
             p_iluc_drop, p_iluc_rate

LOGICAL(LGT) :: relativeToRHS, initx_zeros, p_saamg_unsym, p_adds, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, key="engine", VALUE=engine, &
              default_value=TypeLinSolverOpt%engine, origin=origin, stat=stat)

CALL GetValue(table=table, key="solverName", VALUE=solverName, &
     default_value=TypeLinSolverOpt%solverName_char, origin=origin, stat=stat)

CALL GetValue(table=table, key="convergenceIn", VALUE=convergenceIn, &
  default_value=TypeLinSolverOpt%convergenceIn_char, origin=origin, stat=stat)

CALL GetValue(table=table, key="convergenceType", VALUE=convergenceType, &
default_value=TypeLinSolverOpt%convergenceType_char, origin=origin, stat=stat)

CALL GetValue(table=table, key="scale", VALUE=scale, &
          default_value=TypeLinSolverOpt%scale_char, origin=origin, stat=stat)

CALL GetValue(table=table, key="maxIter", VALUE=maxIter, &
             default_value=TypeLinSolverOpt%maxIter, origin=origin, stat=stat)

CALL GetValue(table=table, key="krylovSubspaceSize", &
              VALUE=krylovSubspaceSize, &
              default_value=TypeLinSolverOpt%krylovSubspaceSize, &
              origin=origin, stat=stat)

CALL GetValue(table=table, key="bicgstab_ell", VALUE=bicgstab_ell, &
        default_value=TypeLinSolverOpt%bicgstab_ell, origin=origin, stat=stat)

CALL GetValue(table=table, key="atol", VALUE=atol, &
              default_value=TypeLinSolverOpt%atol, origin=origin, stat=stat)

CALL GetValue(table=table, key="rtol", VALUE=rtol, &
              default_value=TypeLinSolverOpt%rtol, origin=origin, stat=stat)

CALL GetValue(table=table, key="relativeToRHS", VALUE=relativeToRHS, &
       default_value=TypeLinSolverOpt%relativeToRHS, origin=origin, stat=stat)

CALL GetValue(table=table, key="initx_zeros", VALUE=initx_zeros, &
         default_value=TypeLinSolverOpt%initx_zeros, origin=origin, stat=stat)

CALL GetValue(table=table, key="sor_omega", VALUE=sor_omega, &
           default_value=TypeLinSolverOpt%sor_omega, origin=origin, stat=stat)

node => NULL()

p_hybrid_i = TypeLinSolverOpt%hybrid_i_char
p_hybrid_maxiter = TypeLinSolverOpt%hybrid_maxiter
p_hybrid_ell = TypeLinSolverOpt%hybrid_ell
p_hybrid_restart = TypeLinSolverOpt%hybrid_restart
p_hybrid_tol = TypeLinSolverOpt%hybrid_tol
p_hybrid_omega = TypeLinSolverOpt%hybrid_omega
p_is_m = TypeLinSolverOpt%is_m
p_is_alpha = TypeLinSolverOpt%is_alpha
p_adds_iter = TypeLinSolverOpt%adds_iter
p_adds = TypeLinSolverOpt%adds
p_ssor_omega = TypeLinSolverOpt%ssor_omega
p_sainv_drop = TypeLinSolverOpt%sainv_drop
p_saamg_theta = TypeLinSolverOpt%saamg_theta
p_saamg_unsym = TypeLinSolverOpt%saamg_unsym
p_iluc_drop = TypeLinSolverOpt%iluc_drop
p_iluc_rate = TypeLinSolverOpt%iluc_rate

CALL toml_get(table, "precondition", node, origin=origin, &
              stat=stat, requested=.FALSE.)

preconditionOption = TypeLinSolverOpt%preconditionOption_char
p_name = TypeLinSolverOpt%p_name_char

isok = ASSOCIATED(node)
IF (isok) THEN
  CALL GetValue(table=node, key="option", VALUE=preconditionOption, &
                default_value=TypeLinSolverOpt%preconditionOption_char, &
                origin=origin, stat=stat)

  CALL GetValue(table=node, key="name", VALUE=p_name, &
                default_value=TypeLinSolverOpt%p_name_char, &
                origin=origin, stat=stat)
END IF

prefix = obj%GetPrefix()
solverName_int = obj%solverName_ToInteger(solverName%chars())
preconditionOption_int = TypeLinSolverOpt%PrecondOptToInteger( &
                         preconditionOption%chars())
convergenceIn_int = TypeLinSolverOpt%ConvergenceInToInteger( &
                    convergenceIn%chars())
convergenceType_int = TypeLinSolverOpt%ConvergenceTypeToInteger( &
                      convergenceType%chars())
scale_int = TypeLinSolverOpt%ScaleToInteger(scale%chars())
p_name_int = TypeLinSolverOpt%PrecondNameToInteger(p_name%chars())
p_hybrid_i_int = obj%solverName_ToInteger(p_hybrid_i%chars())

CALL SetAbstractLinSolverParam(param=param, &
                               prefix=prefix, &
                               engine=engine%chars(), &
                               solverName=solverName_int, &
                               preconditionOption=preconditionOption_int, &
                               maxIter=maxIter, &
                               atol=atol, &
                               rtol=rtol, &
                               convergenceIn=convergenceIn_int, &
                               convergenceType=convergenceType_int, &
                               relativeToRHS=relativeToRHS, &
                               krylovSubspaceSize=krylovSubspaceSize, &
                               scale=scale_int, &
                               initx_zeros=initx_zeros, &
                               bicgstab_ell=bicgstab_ell, &
                               sor_omega=sor_omega, &
                               p_name=p_name_int, &
                               p_ilu_lfil=p_ilu_lfil, &
                               p_ilu_mbloc=p_ilu_mbloc, &
                               p_ilu_droptol=p_ilu_droptol, &
                               p_ilu_permtol=p_ilu_permtol, &
                               p_ilu_alpha=p_ilu_alpha, &
                               p_ilu_fill=p_ilu_fill, &
                               p_ssor_omega=p_ssor_omega, &
                               p_hybrid_i=p_hybrid_i_int, &
                               p_hybrid_maxiter=p_hybrid_maxiter, &
                               p_hybrid_tol=p_hybrid_tol, &
                               p_hybrid_omega=p_hybrid_omega, &
                               p_hybrid_ell=p_hybrid_ell, &
                               p_hybrid_restart=p_hybrid_restart, &
                               p_is_alpha=p_is_alpha, &
                               p_is_m=p_is_m, &
                               p_sainv_drop=p_sainv_drop, &
                               p_saamg_unsym=p_saamg_unsym, &
                               p_saamg_theta=p_saamg_theta, &
                               p_iluc_drop=p_iluc_drop, &
                               p_iluc_rate=p_iluc_rate, &
                               p_adds=p_adds, &
                               p_adds_iter=p_adds_iter)

child => NULL()
isok = ASSOCIATED(node)

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  prefix = ""
  tempstr = ""
  RETURN
END IF

tempstr = LowerCase(p_name%chars())
CALL toml_get(node, tempstr, child, origin=origin, stat=stat, &
              requested=.FALSE.)

isok = ASSOCIATED(child)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  prefix = ""
  tempstr = ""
  RETURN
END IF

tempstr = UpperCase(tempstr)
SELECT CASE (tempstr)
CASE ("NONE")
  ! do nothing
CASE ("ILU")
  CALL ilu_import_from_toml(param=param, prefix=prefix, table=child)
CASE ("HYBRID")
  CALL hybrid_import_from_toml(obj=obj, param=param, prefix=prefix, &
                               table=child)
CASE ("IS")
  CALL is_import_from_toml(param=param, prefix=prefix, table=child)

CASE ("ADDS")
  CALL adds_import_from_toml(param=param, prefix=prefix, table=child)

CASE ("SSOR")
  CALL ssor_import_from_toml(param=param, prefix=prefix, table=child)

CASE ("SAINV")
  CALL sainv_import_from_toml(param=param, prefix=prefix, table=child)

CASE ("SAAMG")
  CALL saamg_import_from_toml(param=param, prefix=prefix, table=child)

CASE ("ILUC")
  CALL iluc_import_from_toml(param=param, prefix=prefix, table=child)

END SELECT

child => NULL()
prefix = ""
tempstr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml()"
#endif

TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ImportFromToml()')
#endif

CALL param%Initiate()
CALL obj%ImportParamFromToml(param=param, table=table)
CALL obj%Initiate(param)
CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ImportFromToml()')
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
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ImportTomlMethods
