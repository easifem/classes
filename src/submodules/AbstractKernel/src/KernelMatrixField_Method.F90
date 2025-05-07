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

MODULE KernelMatrixField_Method
USE GlobalData, ONLY: I4B, LGT, DFP

USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_

USE AbstractLinSolver_Class, ONLY: AbstractLinSolver_

USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_

USE Mesh_Class, ONLY: Mesh_

USE ExceptionHandler_Class, ONLY: e

USE FPL, ONLY: ParameterList_

USE AbstractField_Class, ONLY: TypeField

USE MatrixField_Class, ONLY: MatrixField_, &
                             SetMatrixFieldParam

USE BlockMatrixField_Class, ONLY: SetBlockMatrixFieldParam, &
                                  BlockMatrixField_

USE AbstractKernelParam, ONLY: TypeKernelProblemOpt

USE Display_Method, ONLY: ToString

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "KernelMatrixField_Method"

PUBLIC :: KernelInitiateSpaceMatrix
PUBLIC :: KernelInitiateTangentMatrix

INTERFACE KernelInitiateTangentMatrix
  MODULE PROCEDURE KernelInitiateTangentMatrix1, &
    KernelInitiateTangentMatrix2
END INTERFACE KernelInitiateTangentMatrix

CONTAINS

!----------------------------------------------------------------------------
!                                                 KernelInitiateTangentMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-25
! summary:  Initite non block type tangent matrix

SUBROUTINE KernelInitiateTangentMatrix1(mat, linsol, fedof, nsd, nnt, engine, &
                                    name, matrixProp, comm, local_n, global_n)
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractLinSolver_), INTENT(INOUT) :: linsol
  CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof
  INTEGER(I4B), INTENT(IN) :: nsd
  !! number of spatial dimension
  INTEGER(I4B), INTENT(IN) :: nnt
  !! number of nodes in time domain
  CHARACTER(*), INTENT(IN) :: engine
  !! engine name
  CHARACTER(*), INTENT(IN) :: name
  !! name of the matrix
  CHARACTER(*), INTENT(IN) :: matrixProp
  !! matrix property
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
  !! communicator
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
  !! local size
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  !! global size

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTangentMatrix1()"
  TYPE(ParameterList_) :: param
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER
  problem = (nsd .EQ. 0_I4B) .OR. (nnt .EQ. 0_I4B)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: nsd=0 or nnt=0 found')
    RETURN
  END IF
#endif

  CALL param%Initiate()

  CALL SetMatrixFieldParam(param=param, name=name, matrixProp=matrixProp, &
   engine=engine, spaceCompo=nsd, timeCompo=nnt, fieldType=TypeField%normal, &
                           comm=comm, local_n=local_n, global_n=global_n)

  CALL mat%Initiate(param=param, fedof=fedof)
  CALL linsol%Set(mat)

  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateTangentMatrix1

!----------------------------------------------------------------------------
!                                                 KernelInitiateTangentMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-25
! summary:  Initiate block tangent matrix

SUBROUTINE KernelInitiateTangentMatrix2(mat, linsol, fedof, fedofs, name, &
                matrixProp, physicalVarNames, spaceCompo, timeCompo, engine, &
                                        comm, local_n, global_n)
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: mat
  !! abstract matrix field
  CLASS(AbstractLinSolver_), INTENT(INOUT) :: linsol
  !! linear solver
  CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(INOUT) :: fedof
  !! finite element degree of freedom
  TYPE(FEDOFPointer_), OPTIONAL, INTENT(INOUT) :: fedofs(:)
  !! finite element degrees of freedom
  CHARACTER(*), INTENT(IN) :: name
  !! name of the tangent matrix
  CHARACTER(*), INTENT(IN) :: matrixProp
  !! matrix property
  CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
  !! name of physical variables
  INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
  !! spatial dimension of each physical variable
  INTEGER(I4B), INTENT(IN) :: timeCompo(:)
  !! number of time components in the physical variable
  CHARACTER(*), INTENT(IN) :: engine
  !! name of the engine
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
  !! communicator
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
  !! local number of rows
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  !! global number of rows

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTangentMatrix2()"
  TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: a, b
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  a = SIZE(physicalVarNames)
  b = SIZE(spaceCompo)
  isok = a .EQ. b
  CALL AssertError1(isok, myname, &
                    "size of physicalVarNames and spaceCompo are not same")

  a = SIZE(timeCompo)
  isok = a .EQ. b
  CALL AssertError1(isok, myname, &
                    "size of timeCompo and spaceCompo are not same")

  isok = ALL(spaceCompo .GT. 0_I4B)
  CALL AssertError1(isok, myname, &
                    "some spaceCompo are zero")

  isok = ALL(timeCompo .GT. 0_I4B)
  CALL AssertError1(isok, myname, &
                    "some timeCompo are zero")

  isok = PRESENT(fedof) .OR. PRESENT(fedofs)
  CALL AssertError1(isok, myname, &
                    "Neither fedof or fedofs are present")

  isok = .NOT. (PRESENT(fedof) .AND. PRESENT(fedofs))
  CALL AssertError1(isok, myname, &
                    "Both fedof or fedofs are present")

#endif

  CALL param%Initiate()

CALL SetBlockMatrixFieldParam(param=param, name=name, matrixProp=matrixProp, &
                   physicalVarNames=physicalVarNames, spaceCompo=spaceCompo, &
             timeCompo=timeCompo, engine=engine, fieldType=TypeField%normal, &
                                comm=comm, local_n=local_n, global_n=global_n)

  IF (PRESENT(fedof)) THEN
    CALL mat%Initiate(param=param, fedof=fedof)
  END IF

  IF (PRESENT(fedofs)) THEN
    CALL mat%Initiate(param=param, fedof=fedofs)
  END IF

  CALL linsol%Set(mat)

  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateTangentMatrix2

!----------------------------------------------------------------------------
!                                                        InitiateMassMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateSpaceMatrix(mat, fedof, nsd, engine, problemType, &
                                    name, matrixProp, comm, local_n, global_n)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof
  INTEGER(I4B), INTENT(IN) :: nsd
  !! number of spatial dimension
  CHARACTER(*), INTENT(IN) :: engine
  !! engine
  INTEGER(I4B), INTENT(IN) :: problemType
  !! problem type
  CHARACTER(*), INTENT(IN) :: name
  !! name
  CHARACTER(*), INTENT(IN) :: matrixProp
  !! matrix property
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
  !! communicator
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
  !! local number of rows
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  !! global number of row

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateSpaceMatrix()"
  TYPE(ParameterList_) :: param
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: spaceCompo

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER

  problem = nsd .EQ. 0_I4B
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: obj%nsd=0 found')
    RETURN
  END IF

#endif

  CALL param%Initiate()

  SELECT CASE (problemType)
  CASE (TypeKernelProblemOpt%scalar)
    spaceCompo = 1
  CASE (TypeKernelProblemOpt%vector)
    spaceCompo = nsd
  CASE default
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: Unknown problemType = '// &
                      ToString(problemType))
    RETURN
  END SELECT

  CALL SetMatrixFieldParam(param=param, name=name, matrixProp=matrixProp, &
         spaceCompo=spaceCompo, timeCompo=1_I4B, fieldType=TypeField%normal, &
                 engine=engine, comm=comm, local_n=local_n, global_n=global_n)

  CALL mat%Initiate(param=param, fedof=fedof)
  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateSpaceMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE KernelMatrixField_Method
