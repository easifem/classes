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

SUBMODULE(AbstractLinSolver_Class) IOMethods
USE BaseMethod
USE tomlf, ONLY:  &
  & toml_error,  &
  & toml_load,  &
  & toml_parser_config,  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_context,  &
  & toml_terminal,  &
  & toml_load,  &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE als_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "als_ImportParamFromToml()"
INTEGER(I4B) :: origin, stat
TYPE(toml_table), POINTER :: child, node

TYPE(String) :: engine, solverName, preconditionOption,  &
  & p_name, convergenceIn, convergenceType, scale,  &
  & p_hybrid_i

INTEGER(I4B) :: maxIter, krylovSubspaceSize, bicgstab_ell,  &
  & p_ilu_lfil, p_ilu_mbloc, p_ilu_fill, p_hybrid_maxiter, &
  & p_hybrid_ell, p_hybrid_restart, p_is_m, p_adds_iter

REAL(DFP) :: atol, rtol, sor_omega, p_is_alpha,  &
  & p_ilu_droptol, p_ilu_permtol, p_ilu_alpha, p_ssor_omega,  &
  & p_hybrid_tol, p_hybrid_omega, p_sainv_drop, p_saamg_theta,  &
  & p_iluc_drop, p_iluc_rate

LOGICAL(LGT) :: relativeToRHS, initx_zeros, p_saamg_unsym,  &
  & p_adds

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportParamFromToml()')
#endif

CALL toml_get(table, "engine", engine%raw,  &
  & default_engine, origin=origin, stat=stat)

CALL toml_get(table, "solverName", solverName%raw,  &
  & default_solverName_char, origin=origin, stat=stat)

CALL toml_get(table, "convergenceIn", convergenceIn%raw,  &
  & default_convergenceIn_char, origin=origin, stat=stat)

CALL toml_get(table, "convergenceType", convergenceType%raw,  &
  & default_convergenceType_char, origin=origin, stat=stat)

CALL toml_get(table, "scale", scale%raw,  &
  & default_scale_char, origin=origin, stat=stat)

CALL toml_get(table, "maxIter", maxIter,  &
  & default_maxIter, origin=origin, stat=stat)

CALL toml_get(table, "krylovSubspaceSize", krylovSubspaceSize,  &
  & default_krylovSubspaceSize, origin=origin, stat=stat)

CALL toml_get(table, "bicgstab_ell", bicgstab_ell,  &
  & default_bicgstab_ell, origin=origin, stat=stat)

CALL toml_get(table, "atol", atol,  &
  & default_atol, origin=origin, stat=stat)

CALL toml_get(table, "rtol", rtol,  &
  & default_rtol, origin=origin, stat=stat)

CALL toml_get(table, "relativeToRHS", relativeToRHS,  &
  & default_relativeToRHS, origin=origin, stat=stat)

CALL toml_get(table, "initx_zeros", initx_zeros,  &
  & default_initx_zeros, origin=origin, stat=stat)

CALL toml_get(table, "initx_zeros", initx_zeros,  &
  & default_initx_zeros, origin=origin, stat=stat)

CALL toml_get(table, "sor_omega", sor_omega,  &
  & default_sor_omega, origin=origin, stat=stat)

node => NULL()
CALL toml_get(table, "precondition", node, origin=origin,  &
  & stat=stat, requested=.FALSE.)

preconditionOption = default_preconditionOption_char
p_name = default_p_name_char
p_ilu_lfil = default_ilu_lfil
p_ilu_mbloc = default_ilu_mbloc
p_ilu_fill = default_ilu_fill
p_ilu_droptol = default_ilu_droptol
p_ilu_permtol = default_ilu_permtol
p_ilu_alpha = default_ilu_alpha
p_hybrid_i = default_hybrid_i_char
p_hybrid_maxiter = default_hybrid_maxiter
p_hybrid_ell = default_hybrid_ell
p_hybrid_restart = default_hybrid_restart
p_hybrid_tol = default_hybrid_tol
p_hybrid_omega = default_hybrid_omega
p_is_m = default_is_m
p_is_alpha = default_is_alpha
p_adds_iter = default_adds_iter
p_adds = default_adds
p_ssor_omega = default_ssor_omega
p_sainv_drop = default_sainv_drop
p_saamg_theta = default_saamg_theta
p_saamg_unsym = default_saamg_unsym
p_iluc_drop = default_iluc_drop
p_iluc_rate = default_iluc_rate

IF (ASSOCIATED(node)) THEN
  CALL toml_get(node, "option", preconditionOption%raw, origin=origin, &
    & stat=stat)
  CALL toml_get(node, "name", p_name%raw, origin=origin, stat=stat)

  child => NULL()
  CALL toml_get(node, "ilu", child, origin=origin, stat=stat, &
  & requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "lfil", p_ilu_lfil, origin=origin, stat=stat)

    CALL toml_get(child, "mbloc", p_ilu_mbloc, origin=origin, stat=stat)

    CALL toml_get(child, "fill", p_ilu_fill, origin=origin, stat=stat)

    CALL toml_get(child, "droptol", p_ilu_droptol, origin=origin, stat=stat)

    CALL toml_get(child, "permtol", p_ilu_permtol, origin=origin, stat=stat)

    CALL toml_get(child, "alpha", p_ilu_alpha, origin=origin, stat=stat)

  END IF

  child => NULL()
  CALL toml_get(node, "hybrid", child, origin=origin, stat=stat,  &
    & requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "name", p_hybrid_i%raw, default_hybrid_i_char,  &
      & origin=origin, stat=stat)
    CALL toml_get(child, "maxIter", p_hybrid_maxiter,  &
      & default_hybrid_maxiter, origin=origin, stat=stat)
    CALL toml_get(child, "ell", p_hybrid_ell,  &
      & default_hybrid_ell, origin=origin, stat=stat)
    CALL toml_get(child, "restart",  &
      & p_hybrid_restart,  &
      & default_hybrid_restart, origin=origin, stat=stat)
    CALL toml_get(child, "tol", p_hybrid_tol,  &
      & default_hybrid_tol, origin=origin, stat=stat)
    CALL toml_get(child, "omega",  &
      & p_hybrid_omega,  &
      & default_hybrid_omega, origin=origin, stat=stat)
  END IF

  child => NULL()
  CALL toml_get(node, "is", child, origin=origin, stat=stat,  &
    & requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "m", p_is_m,  &
      & default_is_m, origin=origin, stat=stat)
    CALL toml_get(child, "p_is_alpha", p_is_alpha,  &
      & default_is_alpha, origin=origin, stat=stat)
  END IF

  child => NULL()
  CALL toml_get(node, "adds", child, origin=origin,  &
    & stat=stat, requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "iter", p_adds_iter, origin=origin, stat=stat)
    CALL toml_get(child, "isAdditiveSchwarz", p_adds,  &
      & origin=origin, stat=stat)
  END IF

  child => NULL()
  CALL toml_get(node, "ssor", child, origin=origin,  &
    & stat=stat, requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "omega", p_ssor_omega, origin=origin, stat=stat)
  END IF

  child => NULL()
  CALL toml_get(node, "sainv", child, origin=origin,  &
    & stat=stat, requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "drop", p_sainv_drop, origin=origin, stat=stat)
  END IF

  child => NULL()
  CALL toml_get(node, "saamg", child, origin=origin,  &
    & stat=stat, requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "theta", p_saamg_theta, origin=origin, stat=stat)
    CALL toml_get(child, "unsym", p_saamg_unsym, origin=origin, stat=stat)
  END IF

  child => NULL()
  CALL toml_get(node, "iluc", child, origin=origin,  &
    & stat=stat, requested=.FALSE.)
  IF (ASSOCIATED(child)) THEN
    CALL toml_get(child, "drop", p_iluc_drop, origin=origin, stat=stat)
    CALL toml_get(child, "rate", p_iluc_rate, origin=origin, stat=stat)
  END IF
END IF

CALL SetAbstractLinSolverParam( &
    & param=param, &
    & prefix=obj%GetPrefix(), &
    & engine=engine%chars(), &
    & solverName=obj%solverName_ToInteger(solverName%chars()), &
    & preconditionOption= &
    & obj%preconditionOption_ToInteger(preconditionOption%chars()), &
    & maxIter=maxIter, &
    & atol=atol, &
    & rtol=rtol, &
    & convergenceIn= &
    & obj%convergenceIn_ToInteger(convergenceIn%chars()), &
    & convergenceType= &
    & obj%convergenceType_ToInteger(convergenceType%chars()), &
    & relativeToRHS=relativeToRHS, &
    & krylovSubspaceSize=krylovSubspaceSize, &
    & scale=obj%scale_ToInteger(scale%chars()), &
    & initx_zeros=initx_zeros, &
    & bicgstab_ell=bicgstab_ell, &
    & sor_omega=sor_omega, &
    & p_name=obj%preconditionName_ToInteger(p_name%chars()), &
    & p_ilu_lfil=p_ilu_lfil, &
    & p_ilu_mbloc=p_ilu_mbloc, &
    & p_ilu_droptol=p_ilu_droptol, &
    & p_ilu_permtol=p_ilu_permtol, &
    & p_ilu_alpha=p_ilu_alpha, &
    & p_ilu_fill=p_ilu_fill, &
    & p_ssor_omega=p_ssor_omega, &
    & p_hybrid_i=obj%solverName_ToInteger(p_hybrid_i%chars()), &
    & p_hybrid_maxiter=p_hybrid_maxiter, &
    & p_hybrid_tol=p_hybrid_tol, &
    & p_hybrid_omega=p_hybrid_omega, &
    & p_hybrid_ell=p_hybrid_ell, &
    & p_hybrid_restart=p_hybrid_restart, &
    & p_is_alpha=p_is_alpha, &
    & p_is_m=p_is_m, &
    & p_sainv_drop=p_sainv_drop, &
    & p_saamg_unsym=p_saamg_unsym, &
    & p_saamg_theta=p_saamg_theta, &
    & p_iluc_drop=p_iluc_drop, &
    & p_iluc_rate=p_iluc_rate, &
    & p_adds=p_adds, &
    & p_adds_iter=p_adds_iter &
& )

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE als_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE als_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "als_ImportFromToml()"
TYPE(ParameterList_) :: param
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportFromToml()')
#endif

CALL param%Initiate()
CALL obj%ImportParamFromToml(param=param, table=table)
CALL obj%Initiate(param)
CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportFromToml()')
#endif
END PROCEDURE als_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE als_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "als_ImportFromToml2()"
LOGICAL(LGT) :: isNotOpen, isNotRead
LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportFromToml2()')
#endif

terminal = toml_terminal(color)

IF (PRESENT(afile)) THEN
  isNotOpen = .NOT. afile%IsOpen()
  isNotRead = .NOT. afile%IsRead()

  IF (isNotRead .OR. isNotOpen) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The file is not open or does not have '//  &
      & 'the access to read!')
  END IF

  CALL toml_load(table,  &
    & afile%GetUnitNo(),  &
    & context=context,  &
    & config=toml_parser_config(color=terminal, context_detail=detail), &
    & error=error  &
    & )

ELSEIF (PRESENT(filename)) THEN
  CALL toml_load(table,  &
    & filename,  &
    & context=context,  &
    & config=toml_parser_config(color=terminal, context_detail=detail), &
    & error=error  &
    & )
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

IF (ALLOCATED(error)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Some error occured while parsing toml file'//  &
    & ' with following message: '//error%message)
END IF

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE.,  &
  & stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find '//tomlName//" table in config.")
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node),  &
  & "abstractLinSolver toml config = "//CHAR_LF,  &
  & unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportFromToml2()')
#endif
END PROCEDURE als_ImportFromToml2

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE als_Display
CALL Display(msg, unitNo=unitno)
CALL Display("engine : "//obj%engine%chars(), unitNo=unitno)
CALL Display(obj%isInitiated, "isInitiated : ", unitNo=unitno)
CALL Display(obj%solverName, "solverName : ", unitNo=unitno)
CALL Display(obj%preconditionOption, "preconditionOption : ", unitNo=unitno)
CALL Display(obj%convergenceIn, "convergenceIn : ", unitNo=unitno)
CALL Display(obj%convergenceType, "convergenceType : ", unitNo=unitno)
CALL Display(obj%maxIter, "maxIter : ", unitNo=unitno)
CALL Display(obj%relativeToRHS, "relativeToRHS : ", unitNo=unitno)
CALL Display(obj%KrylovSubspaceSize, "KrylovSubspaceSize : ", unitNo=unitno)
CALL Display(obj%atol, "atol : ", unitNo=unitno)
CALL Display(obj%rtol, "rtol : ", unitNo=unitno)
CALL Display(obj%ierr, "ierr : ", unitNo=unitno)
CALL Display(obj%iter, "iter : ", unitNo=unitno)
IF (ASSOCIATED(obj%Amat)) THEN
  CALL Display("Amat is ASSOCIATED", unitNo=unitno)
ELSE
  CALL Display("Amat is NOT ASSOCIATED", unitNo=unitno)
END IF
IF (ALLOCATED(obj%RES)) &
  & CALL Display("obj%RES is ALLOCATED", unitNo=unitno)
END PROCEDURE als_Display

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE als_Export
CHARACTER(*), PARAMETER :: myName = "als_Export"
TYPE(String) :: dsetname, strval

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Export()")

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The object is not initiated, initiate it first!')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have write permission')
END IF

dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/solverName"
strval = obj%getLinSolverNameFromCode(obj%solverName)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/preconditionOption"
SELECT CASE (obj%preconditionOption)
CASE (NO_PRECONDITION)
  strval = "NONE"
CASE (LEFT_PRECONDITION)
  strval = "LEFT"
CASE (RIGHT_PRECONDITION)
  strval = "RIGHT"
CASE (LEFT_RIGHT_PRECONDITION)
  strval = "LEFT_RIGHT"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/convergenceIn"
SELECT CASE (obj%convergenceIn)
CASE (convergenceInRes)
  strval = "RESIDUAL"
CASE (convergenceInSol)
  strval = "SOLUTION"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/convergenceType"
SELECT CASE (obj%convergenceType)
CASE (absoluteConvergence)
  strval = "ABSOLUTE"
CASE (relativeConvergence)
  strval = "RELATIVE"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/relativeToRHS"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%relativeToRHS)

dsetname = TRIM(group)//"/maxIter"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%maxIter)

dsetname = TRIM(group)//"/KrylovSubspaceSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%KrylovSubspaceSize)

dsetname = TRIM(group)//"/relativeTolerance"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rtol)

dsetname = TRIM(group)//"/absoluteTolerance"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%atol)

CALL e%raiseInformation(modName//"::"//myName//" - "// &
& "[END] Export()")
END PROCEDURE als_Export

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE als_Import
CHARACTER(*), PARAMETER :: myName = "als_Import"
TYPE(String) :: dsetname, strval

CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by child of AbstractLinSolver_')

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The object is already initiated, deallocate first!')
END IF
obj%isInitiated = .TRUE.

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF
IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF

dsetname = TRIM(group)//"/engine"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset engine should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/solverName"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset solverName should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
obj%solverName = obj%getLinSolverCodeFromName(TRIM(strval%chars()))

dsetname = TRIM(group)//"/preconditionOption"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset preconditionOption should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
SELECT CASE (TRIM(strval%chars()))
CASE ("NONE")
  obj%preconditionOption = NO_PRECONDITION
CASE ("LEFT")
  obj%preconditionOption = LEFT_PRECONDITION
CASE ("RIGHT")
  obj%preconditionOption = RIGHT_PRECONDITION
CASE ("LEFT_RIGHT")
  obj%preconditionOption = LEFT_RIGHT_PRECONDITION
END SELECT

dsetname = TRIM(group)//"/convergenceIn"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset convergenceIn should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
SELECT CASE (TRIM(strval%chars()))
CASE ("RESIDUAL")
  obj%convergenceIn = convergenceInRes
CASE ("SOLUTION")
  obj%convergenceIn = convergenceInSol
END SELECT

dsetname = TRIM(group)//"/convergenceType"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset convergenceType should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
SELECT CASE (TRIM(strval%chars()))
CASE ("ABSOLUTE")
  obj%convergenceType = absoluteConvergence
CASE ("RELATIVE")
  obj%convergenceType = relativeConvergence
END SELECT

IF (obj%convergenceType .EQ. relativeConvergence) THEN
  dsetname = TRIM(group)//"/relativeToRHS"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%relativeToRHS)
  ELSE
    obj%relativeToRHS = .FALSE.
  END IF
ELSE
  obj%relativeToRHS = .FALSE.
END IF

dsetname = TRIM(group)//"/maxIter"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset maxIter should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%maxIter)

dsetname = TRIM(group)//"/KrylovSubspaceSize"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%KrylovSubspaceSize)
ELSE
  obj%KrylovSubspaceSize = 20
END IF

dsetname = TRIM(group)//"/relativeTolerance"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset relativeTolerance should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%rtol)

dsetname = TRIM(group)//"/absoluteTolerance"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset absoluteTolerance should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%atol)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[END] Import()')

END PROCEDURE als_Import

END SUBMODULE IOMethods
