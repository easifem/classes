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

SUBMODULE(LinSolverLis_Class) ConstructorMethods
USE BaseType, ONLY: TypePrecondOpt, &
                    TypeSolverNameOpt
USE LinSolver_Class, ONLY: LinSolverInitiate, &
                           LinSolverDeallocate
USE Display_Method, ONLY: ToString
USE String_Class, ONLY: String

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                        GetSolverNameString
!----------------------------------------------------------------------------

SUBROUTINE ConfigSolverName_bicgstabl(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigSolverName_bicgstabl()"
#endif

  INTEGER(I4B) :: bicgstab_ell

  bicgstab_ell = obj%opt%GetBicgstabEll()
  opt = opt//' -i bicgstabl -ell '//ToString(bicgstab_ell)
END SUBROUTINE ConfigSolverName_bicgstabl

!----------------------------------------------------------------------------
!                                                        GetSolverNameString
!----------------------------------------------------------------------------

SUBROUTINE ConfigSolverName_gmres(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigSolverName_gmres()"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetKrylovSubspaceSize()
  opt = opt//' -i gmres -restart '//ToString(i1)
END SUBROUTINE ConfigSolverName_gmres

!----------------------------------------------------------------------------
!                                                        GetSolverNameString
!----------------------------------------------------------------------------

SUBROUTINE ConfigSolverName_orthomin(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigSolverName_orthomin()"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetKrylovSubspaceSize()
  opt = opt//' -i orthomin -restart '//ToString(i1)
END SUBROUTINE ConfigSolverName_orthomin

!----------------------------------------------------------------------------
!                                                        GetSolverNameString
!----------------------------------------------------------------------------

SUBROUTINE ConfigSolverName_fgmres(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigSolverName_fgmres()"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetKrylovSubspaceSize()
  opt = opt//' -i fgmres -restart '//ToString(i1)
END SUBROUTINE ConfigSolverName_fgmres

!----------------------------------------------------------------------------
!                                                        GetSolverNameString
!----------------------------------------------------------------------------

SUBROUTINE ConfigSolverName_idrs(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigSolverName_idrs()"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetKrylovSubspaceSize()
  opt = opt//' -i idrs -irestart '//ToString(i1)
END SUBROUTINE ConfigSolverName_idrs

!----------------------------------------------------------------------------
!                                                        GetSolverNameString
!----------------------------------------------------------------------------

SUBROUTINE ConfigSolverName_sor(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigSolverName_sor()"
#endif

  REAL(DFP) :: i1

  i1 = obj%opt%GetSorOmega()
  opt = opt//' -i sor -omega '//ToString(i1)
END SUBROUTINE ConfigSolverName_sor

!----------------------------------------------------------------------------
!                                                              ConfigMaxIter
!----------------------------------------------------------------------------

SUBROUTINE ConfigMaxIter(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigMaxIter"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetMaxIter()
  opt = opt//' -maxiter '//ToString(i1)
END SUBROUTINE ConfigMaxIter

!----------------------------------------------------------------------------
!                                                              ConfigPrint
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrint(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrint"
#endif

  INTEGER(I4B) :: i1

  i1 = 3
  opt = opt//' -print '//ToString(i1)
END SUBROUTINE ConfigPrint

!----------------------------------------------------------------------------
!                                                              ConfigScale
!----------------------------------------------------------------------------

SUBROUTINE ConfigScale(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigScale"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetScale()
  opt = opt//' -scale '//ToString(i1)
END SUBROUTINE ConfigScale

!----------------------------------------------------------------------------
!                                                                  ConfigRtol
!----------------------------------------------------------------------------

SUBROUTINE ConfigRelativeTolerance(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigRelativeTolerance"
#endif

  REAL(DFP) :: i1

  i1 = obj%opt%GetRelativeTolerance()
  opt = opt//' -tol '//ToString(i1)
END SUBROUTINE ConfigRelativeTolerance

!----------------------------------------------------------------------------
!                                                           ConfigInitxZeros
!----------------------------------------------------------------------------

SUBROUTINE ConfigInitxZeros(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigInitxZeros"
#endif

  LOGICAL(LGT) :: i1

  i1 = obj%opt%GetInitxZeros()

  IF (i1) THEN
    opt = opt//' -initx_zeros true '
  ELSE
    opt = opt//' -initx_zeros false '
  END IF
END SUBROUTINE ConfigInitxZeros

!----------------------------------------------------------------------------
!                                                         ConfigRelativeToRHS
!----------------------------------------------------------------------------

SUBROUTINE ConfigRelativeToRHS(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigRelativeToRHS()"
#endif

  LOGICAL(LGT) :: i1

  i1 = obj%opt%GetRelativeToRHS()

  IF (i1) THEN
    opt = opt//" -conv_cond 1 "
  ELSE
    opt = opt//" -conv_cond 0 "
  END IF

END SUBROUTINE ConfigRelativeToRHS

!----------------------------------------------------------------------------
!                                                          ConfigPrecond_none
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_none(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_none()"
#endif

  opt = opt//' -p none '
END SUBROUTINE ConfigPrecond_none

!----------------------------------------------------------------------------
!                                                       ConfigPrecond_jacobi
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_jacobi(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_jacobi()"
#endif

  opt = opt//' -p jacobi '
END SUBROUTINE ConfigPrecond_jacobi

!----------------------------------------------------------------------------
!                                                       ConfigPrecond_ilu
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_ilu(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_ilu()"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetIluFill()
  opt = opt//' -p ilu -ilu_fill '//ToString(i1)

END SUBROUTINE ConfigPrecond_ilu

!----------------------------------------------------------------------------
!                                                       ConfigPrecond_ssor
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_ssor(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_ssor()"
#endif

  REAL(DFP) :: i1

  i1 = obj%opt%GetSsorOmega()
  opt = opt//' -p ssor -ssor_omega '//ToString(i1)

END SUBROUTINE ConfigPrecond_ssor

!----------------------------------------------------------------------------
!                                                       ConfigPrecond_hybrid
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_hybrid(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_hybrid()"
#endif

  INTEGER(I4B) :: i1, i2, i3, i4
  REAL(DFP) :: r1, r2

  i1 = obj%opt%GetHybridI()
  i2 = obj%opt%GetHybridMaxIter()
  i3 = obj%opt%GetHybridEll()
  i4 = obj%opt%GetHybridRestart()
  r1 = obj%opt%GetHybridTol()
  r2 = obj%opt%GetHybridOmega()

  opt = opt//' -p hybrid -hybrid_i '//ToString(i1)// &
        ' -hybrid_maxiter '//ToString(i2)// &
        ' -hybrid_ell '//ToString(i3)// &
        ' -hybrid_restart '//ToString(i4)// &
        ' -hybrid_tol '//ToString(r1)// &
        ' -hybrid_omega '//ToString(r2)

END SUBROUTINE ConfigPrecond_hybrid

!----------------------------------------------------------------------------
!                                                           ConfigPrecond_is
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_is(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_is()"
#endif

  INTEGER(I4B) :: i1
  REAL(DFP) :: r1

  i1 = obj%opt%GetIsM()
  r1 = obj%opt%GetIsAlpha()

  opt = opt//' -p is '//' -is_m '//ToString(i1)// &
        ' -is_alpha '//ToString(r1)

END SUBROUTINE ConfigPrecond_is

!----------------------------------------------------------------------------
!                                                           ConfigPrecond_sainv
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_sainv(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_sainv()"
#endif

  REAL(DFP) :: r1

  r1 = obj%opt%GetSainvDrop()
  opt = opt//' -p sainv -sainv_drop '//ToString(r1)

END SUBROUTINE ConfigPrecond_sainv

!----------------------------------------------------------------------------
!                                                        ConfigPrecond_saamg
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_saamg(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_saamg()"
#endif

  LOGICAL(LGT) :: b1
  REAL(DFP) :: r1

  b1 = obj%opt%GetSaamgUnsym()
  r1 = obj%opt%GetSaamgTheta()

  IF (b1) THEN
    opt = opt//' -p saamg -sammg_unsym true -saamg_theta '// &
          ToString(r1)
  ELSE

    opt = opt//' -p saamg -sammg_unsym false -saamg_theta '// &
          ToString(r1)
  END IF

END SUBROUTINE ConfigPrecond_saamg

!----------------------------------------------------------------------------
!                                                        ConfigPrecond_iluc
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_iluc(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_iluc()"
#endif

  REAL(DFP) :: r1, r2

  r1 = obj%opt%GetIlucDrop()
  r2 = obj%opt%GetIlucRate()

  opt = opt//' -p iluc -iluc_drop '//ToString(r1)// &
        ' -iluc_rate '//ToString(r2)
END SUBROUTINE ConfigPrecond_iluc

!----------------------------------------------------------------------------
!                                                        ConfigPrecond_adds
!----------------------------------------------------------------------------

SUBROUTINE ConfigPrecond_adds(obj, opt)
  CLASS(LinSolverLis_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: opt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ConfigPrecond_adds()"
#endif

  INTEGER(I4B) :: i1

  i1 = obj%opt%GetAddsIter()

  opt = opt//' -p ilut -adds true -adds_iter '//ToString(i1)

END SUBROUTINE ConfigPrecond_adds

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: ierr, solverName, precond
LOGICAL(LGT) :: isPrecondition
TYPE(String) :: opt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL LinSolverInitiate(obj=obj)

CALL lis_solver_create(obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

opt = ""
solverName = obj%opt%GetSolverName()

SELECT CASE (solverName)

CASE (TypeSolverNameOpt%CG)
  opt = opt//' -i cg '

CASE (TypeSolverNameOpt%BICG)
  opt = opt//' -i bicg '

CASE (TypeSolverNameOpt%CGS)
  opt = opt//' -i cgs '

CASE (TypeSolverNameOpt%BICGSTAB)
  opt = opt//' -i bicgstab '

CASE (TypeSolverNameOpt%BICGSTABL)
  CALL ConfigSolverName_bicgstabl(obj=obj, opt=opt)

CASE (TypeSolverNameOpt%GPBiCG)
  opt = opt//' -i gpbicg '

CASE (TypeSolverNameOpt%tfqmr)
  opt = opt//' -i tfqmr '

CASE (TypeSolverNameOpt%ORTHOMIN)
  CALL ConfigSolverName_orthomin(obj=obj, opt=opt)

CASE (TypeSolverNameOpt%GMRES)
  CALL ConfigSolverName_gmres(obj=obj, opt=opt)

CASE (TypeSolverNameOpt%jacobi)
  opt = opt//' -i jacobi '

CASE (TypeSolverNameOpt%gs)
  opt = opt//' -i gs '

CASE (TypeSolverNameOpt%SOR)
  CALL ConfigSolverName_sor(obj=obj, opt=opt)

CASE (TypeSolverNameOpt%bicgsafe)
  opt = opt//' -i bicgsafe '

CASE (TypeSolverNameOpt%cr)
  opt = opt//' -i cr '

CASE (TypeSolverNameOpt%bicr)
  opt = opt//' -i bicr '

CASE (TypeSolverNameOpt%crs)
  opt = opt//' -i crs '

CASE (TypeSolverNameOpt%bicrstab)
  opt = opt//' -i bicrstab '

CASE (TypeSolverNameOpt%gpbicr)
  opt = opt//' -i gpbicr '

CASE (TypeSolverNameOpt%bicrsafe)
  opt = opt//' -i bicrsafe '

CASE (TypeSolverNameOpt%FGMRES)
  CALL ConfigSolverName_fgmres(obj=obj, opt=opt)

CASE (TypeSolverNameOpt%IDRS)
  CALL ConfigSolverName_idrs(obj=obj, opt=opt)

CASE (TypeSolverNameOpt%idr1)
  opt = opt//' -i idr1 '

CASE (TypeSolverNameOpt%minres)
  opt = opt//' -i minres '

CASE (TypeSolverNameOpt%cocg)
  opt = opt//' -i cocg '

CASE (TypeSolverNameOpt%cocr)
  opt = opt//' -i cocr '

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, "No case found for solver name")
#endif

END SELECT

CALL ConfigMaxIter(obj=obj, opt=opt)
CALL ConfigPrint(obj=obj, opt=opt)
CALL ConfigScale(obj=obj, opt=opt)
CALL ConfigRelativeTolerance(obj=obj, opt=opt)
CALL ConfigInitxZeros(obj=obj, opt=opt)
CALL ConfigRelativeToRHS(obj=obj, opt=opt)

precond = obj%opt%GetPreconditionOption()
isPrecondition = precond .NE. TypePrecondOpt%NONE
IF (.NOT. isPrecondition) THEN
  CALL lis_solver_set_option(opt%chars(), obj%lis_solver, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

precond = obj%opt%GetPreconditionName()

SELECT CASE (precond)
CASE (TypePrecondOpt%NONE)
  CALL ConfigPrecond_none(obj=obj, opt=opt)

CASE (TypePrecondOpt%JACOBI)
  CALL ConfigPrecond_jacobi(obj=obj, opt=opt)

CASE (TypePrecondOpt%ILU)
  CALL ConfigPrecond_ilu(obj=obj, opt=opt)

CASE (TypePrecondOpt%SSOR)
  CALL ConfigPrecond_ssor(obj=obj, opt=opt)

CASE (TypePrecondOpt%HYBRID)
  CALL ConfigPrecond_hybrid(obj=obj, opt=opt)

CASE (TypePrecondOpt%IS)
  CALL ConfigPrecond_is(obj=obj, opt=opt)

CASE (TypePrecondOpt%SAINV)
  CALL ConfigPrecond_sainv(obj=obj, opt=opt)

CASE (TypePrecondOpt%SAAMG)
  CALL ConfigPrecond_saamg(obj=obj, opt=opt)

CASE (TypePrecondOpt%ILUC)
  CALL ConfigPrecond_iluc(obj=obj, opt=opt)

CASE (TypePrecondOpt%ADDS)
  CALL ConfigPrecond_adds(obj=obj, opt=opt)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, "No case found for precondition name")
#endif

END SELECT

CALL lis_solver_set_option(opt%chars(), obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_solver_destroy(obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL LinSolverDeallocate(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_final
CALL obj%DEALLOCATE()
END PROCEDURE obj_final

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
