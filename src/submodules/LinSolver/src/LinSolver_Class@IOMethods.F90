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

SUBMODULE(LinSolver_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Display
CALL AbstractLinSolverDisplay(obj, msg, unitno)
IF (ALLOCATED(obj%W)) &
  & CALL Display("# obj%W is ALLOCATED : ", unitNo=unitno)
CALL Display(obj%IPAR, "# IPAR : ", unitNo=unitno)
CALL Display(obj%FPAR, "# FPAR : ", unitNo=unitno)
END PROCEDURE ls_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Import
CHARACTER(*), PARAMETER :: myName = "ls_Import"
TYPE(ParameterList_) :: param

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")

CALL AbstractLinSolverImport(obj=obj, hdf5=hdf5, group=group)
obj%isInitiated = .FALSE.

CALL param%initiate()
CALL SetAbstractLinSolverParam( &
  & param=param, &
  & prefix=obj%GetPrefix(), &
  & engine=myengine, &
  & solverName=obj%solverName,&
  & preconditionOption=obj%preconditionOption, &
  & convergenceIn=obj%convergenceIn, &
  & convergenceType=obj%convergenceType, &
  & maxIter=obj%maxIter, &
  & relativeToRHS=obj%relativeToRHS, &
  & KrylovSubspaceSize=obj%KrylovSubspaceSize, &
  & rtol=obj%rtol, &
  & atol=obj%atol)

CALL obj%Initiate(param)
CALL param%DEALLOCATE()

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[END] Import()')

END PROCEDURE ls_Import

END SUBMODULE IOMethods
