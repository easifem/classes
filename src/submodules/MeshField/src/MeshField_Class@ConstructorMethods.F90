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

SUBMODULE(MeshField_Class) ConstructorMethods
USE AbstractMeshField_Class, ONLY: SetAbstractMeshFieldParam
USE Display_Method, ONLY: ToString
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                     SetScalarMeshFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetScalarMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetScalarMeshFieldParam()"
#endif

INTEGER(I4B) :: s(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = 1
ELSE
  s = nns
END IF

CALL SetAbstractMeshFieldParam(param=param, &
                               prefix=myprefix, &
                               name=name, &
                               fieldType=fieldType, &
                               varType=varType, &
                               engine=engine, &
                               defineOn=defineOn, &
                               rank=TypeFieldOpt%scalar, &
                               s=s)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetScalarMeshFieldParam

!----------------------------------------------------------------------------
!                                                     ScalarMeshFieldInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarMeshFieldInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarMeshFieldInitiate()"
#endif

INTEGER(I4B) :: s(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = 1
ELSE
  s = nns
END IF

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=defineOn, &
                  rank=TypeFieldOpt%scalar, &
                  s=s, mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                  SetSTScalarMeshFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTScalarMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetSTScalarMeshFieldParam()"
#endif

INTEGER(I4B) :: s(2), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = 1
  n = 1
ELSE
  s(1) = nns
  s(2) = nnt
  n = 2
END IF

isok = varType .EQ. TypeFieldOpt%time
IF (isok) THEN
  CALL SetAbstractMeshFieldParam(param=param, &
                                 prefix=myprefix, &
                                 name=name, &
                                 fieldType=fieldType, &
                                 varType=varType, &
                                 engine=engine, &
                                 defineOn=defineOn, &
                                 rank=TypeFieldOpt%scalar, &
                                 s=s(n:n))
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL SetAbstractMeshFieldParam(param=param, &
                               prefix=myprefix, &
                               name=name, &
                               fieldType=fieldType, &
                               varType=varType, &
                               engine=engine, &
                               defineOn=defineOn, &
                               rank=TypeFieldOpt%scalar, &
                               s=s(1:n))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetSTScalarMeshFieldParam

!----------------------------------------------------------------------------
!                                                  STScalarMeshFieldInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarMeshFieldInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STScalarMeshFieldInitiate()"
#endif

INTEGER(I4B) :: s(2), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = 1
  n = 1
ELSE
  s(1) = nns
  s(2) = nnt
  n = 2
END IF

isok = varType .EQ. TypeFieldOpt%time
IF (isok) THEN
  CALL obj%Initiate(name=name, &
                    fieldType=fieldType, &
                    varType=varType, &
                    engine=engine, &
                    defineOn=defineOn, &
                    rank=TypeFieldOpt%scalar, &
                    s=s(n:n), &
                    mesh=mesh)
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=defineOn, &
                  rank=TypeFieldOpt%scalar, &
                  s=s(1:n), &
                  mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE STScalarMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                    SetVectorMeshFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetVectorMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetVectorMeshFieldParam()"
#endif

INTEGER(I4B) :: s(2), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = spaceCompo
  n = 1
ELSE
  s(1) = spaceCompo
  s(2) = nns
  n = 2
END IF

CALL SetAbstractMeshFieldParam(param=param, &
                               prefix=myprefix, &
                               name=name, &
                               fieldType=fieldType, &
                               varType=varType, &
                               engine=engine, &
                               defineOn=defineOn, &
                               rank=TypeFieldOpt%vector, &
                               s=s(1:n))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetVectorMeshFieldParam

!----------------------------------------------------------------------------
!                                                    VectorMeshFieldInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorMeshFieldInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VectorMeshFieldInitiate()"
#endif

INTEGER(I4B) :: s(2), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = spaceCompo
  n = 1
ELSE
  s(1) = spaceCompo
  s(2) = nns
  n = 2
END IF

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=defineOn, &
                  rank=TypeFieldOpt%vector, &
                  s=s(1:n), &
                  mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VectorMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                   SetSTVectorMeshFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTVectorMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetSTVectorMeshFieldParam()"
#endif

INTEGER(I4B) :: s(3), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = spaceCompo
  n = 1
ELSE
  s(1) = spaceCompo
  s(2) = nns
  s(3) = nnt
  n = 3
END IF

CALL SetAbstractMeshFieldParam(param=param, &
                               prefix=myprefix, &
                               name=name, &
                               fieldType=fieldType, &
                               varType=varType, &
                               engine=engine, &
                               defineOn=defineOn, &
                               rank=TypeFieldOpt%vector, &
                               s=s(1:n))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetSTVectorMeshFieldParam

!----------------------------------------------------------------------------
!                                                   STVectorMeshFieldInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorMeshFieldInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STVectorMeshFieldInitiate()"
#endif
INTEGER(I4B) :: s(3), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  s = spaceCompo
  n = 1
ELSE
  s(1) = spaceCompo
  s(2) = nns
  s(3) = nnt
  n = 3
END IF

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=defineOn, &
                  rank=TypeFieldOpt%vector, &
                  s=s(1:n), &
                  mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE STVectorMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                     SetTensorMeshFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTensorMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetTensorMeshFieldParam()"
#endif

INTEGER(I4B) :: s(3), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  n = 2
  s(1) = dim1
  s(2) = dim2
ELSE
  n = 3
  s(1) = dim1
  s(2) = dim2
  s(3) = nns
END IF

CALL SetAbstractMeshFieldParam(param=param, &
                               prefix=myprefix, &
                               name=name, &
                               fieldType=fieldType, &
                               varType=varType, &
                               engine=engine, &
                               defineOn=defineOn, &
                               rank=TypeFieldOpt%matrix, &
                               s=s(1:n))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetTensorMeshFieldParam

!----------------------------------------------------------------------------
!                                                     TensorMeshFieldInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorMeshFieldInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "TensorMeshFieldInitiate()"
#endif

INTEGER(I4B) :: s(3), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  n = 2
  s(1) = dim1
  s(2) = dim2
ELSE
  n = 3
  s(1) = dim1
  s(2) = dim2
  s(3) = nns
END IF

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=defineOn, &
                  rank=TypeFieldOpt%matrix, &
                  s=s(1:n), &
                  mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TensorMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                  SetSTTensorMeshFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTTensorMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetSTTensorMeshFieldParam()"
#endif

INTEGER(I4B) :: s(4), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  n = 2
  s(1) = dim1
  s(2) = dim2
ELSE
  n = 4
  s(1) = dim1
  s(2) = dim2
  s(3) = nns
  s(4) = nnt
END IF

CALL SetAbstractMeshFieldParam(param=param, &
                               prefix=myprefix, &
                               name=name, &
                               fieldType=fieldType, &
                               varType=varType, &
                               engine=engine, &
                               defineOn=defineOn, &
                               rank=TypeFieldOpt%matrix, &
                               s=s(1:n))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetSTTensorMeshFieldParam

!----------------------------------------------------------------------------
!                                                   STTensorMeshFieldInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE STTensorMeshFieldInitiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STTensorMeshFieldInitiate()"
#endif

INTEGER(I4B) :: s(4), n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = varType .EQ. TypeFieldOpt%constant
IF (isok) THEN
  n = 2
  s(1) = dim1
  s(2) = dim2
ELSE
  n = 4
  s(1) = dim1
  s(2) = dim2
  s(3) = nns
  s(4) = nnt
END IF

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=defineOn, &
                  rank=TypeFieldOpt%matrix, &
                  s=s(1:n), &
                  mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE STTensorMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                   ScalarMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarMeshFieldInitiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarMeshFieldInitiate4()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()
returnType = func%GetReturnType()

#ifdef DEBUG_VER
isok = returnType .EQ. TypeFieldOpt%scalar
CALL AssertError1(isok, myName, "returnType should be scalar.")
#endif

fieldType = TypeFieldOpt%normal
argType = func%GetArgType()
varType = argType

IF (argType .EQ. TypeFieldOpt%constant) THEN
  fieldType = TypeFieldOpt%constant
  varType = fieldType
END IF

CALL ScalarMeshFieldInitiate(obj=obj, &
                             name=name, &
                             fieldType=fieldType, &
                             varType=varType, &
                             engine=engine, &
                             defineOn=TypeFieldOpt%nodal, &
                             nns=nns, &
                             mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarMeshFieldInitiate4

!----------------------------------------------------------------------------
!                                                  STScalarMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarMeshFieldInitiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STScalarMeshFieldInitiate4()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()
returnType = func%GetReturnType()

#ifdef DEBUG_VER
isok = returnType .EQ. TypeFieldOpt%scalar
CALL AssertError1(isok, myName, 'returnType should be Scalar.')
#endif

fieldType = TypeFieldOpt%normal
argType = func%GetArgType()
varType = argType

IF (argType .EQ. TypeFieldOpt%constant) THEN
  fieldType = TypeFieldOpt%constant
  varType = fieldType
END IF

#ifdef DEBUG_VER
isok = PRESENT(nnt)
CALL AssertError1(isok, myName, &
   'NNT should be present when varType in userFunction is Time or SpaceTime.')
#endif

CALL STScalarMeshFieldInitiate(obj=obj, name=name, fieldType=fieldType, &
                               varType=varType, engine=engine, &
                               defineOn=TypeFieldOpt%nodal, nns=nns, &
                               nnt=nnt, mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE STScalarMeshFieldInitiate4

!----------------------------------------------------------------------------
!                                                   VectorMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorMeshFieldInitiate4
CHARACTER(*), PARAMETER :: myName = "VectorMeshFieldInitiate4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType, numReturns

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()

returnType = func%GetReturnType()

#ifdef DEBUG_VER
isok = returnType .EQ. TypeFieldOpt%vector
CALL AssertError1(isok, myName, &
                  'For VectorMeshField returnType should be Vector.')
#endif

argType = func%GetArgType()
numReturns = func%GetNumReturns()
fieldType = TypeFieldOpt%normal
varType = argType

IF (argType .EQ. TypeFieldOpt%constant) THEN
  fieldType = TypeFieldOpt%constant
  varType = fieldType
END IF

CALL VectorMeshFieldInitiate(obj=obj, name=name, fieldType=fieldType, &
                             varType=varType, engine=engine, &
                             defineOn=TypeFieldOpt%nodal, &
                             spaceCompo=numReturns, nns=nns, mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VectorMeshFieldInitiate4

!----------------------------------------------------------------------------
!                                                  STVectorMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorMeshFieldInitiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STVectorMeshFieldInitiate4()"
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType, numReturns

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()
returnType = func%GetReturnType()

#ifdef DEBUG_VER
isok = returnType .EQ. TypeFieldOpt%vector
CALL AssertError1(isok, myName, 'returnType should be Vector.')
#endif

argType = func%GetArgType()
numReturns = func%GetNumReturns()
fieldType = TypeFieldOpt%normal
varType = argType

IF (argType .EQ. TypeFieldOpt%constant) THEN
  fieldType = TypeFieldOpt%constant
  varType = fieldType
END IF

#ifdef DEBUG_VER
isok = PRESENT(nnt)
CALL AssertError1(isok, myName, &
                  'nnt should be present when varType is Time or SpaceTime.')
#endif

CALL STVectorMeshFieldInitiate(obj=obj, name=name, fieldType=fieldType, &
                               varType=varType, engine=engine, &
                               defineOn=TypeFieldOpt%nodal, &
                               spaceCompo=numReturns, nns=nns, nnt=nnt, &
                               mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE STVectorMeshFieldInitiate4

!----------------------------------------------------------------------------
!                                                   TensorMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorMeshFieldInitiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "TensorMeshFieldInitiate4()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType, &
                numReturns, dims(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()

returnType = func%GetReturnType()

#ifdef DEBUG_VER
isok = returnType .EQ. TypeFieldOpt%matrix
CALL AssertError1(isok, myName, 'Return type should be Matrix.')
#endif

argType = func%GetArgType()
numReturns = func%GetNumReturns()
dims = func%GetReturnShape()
fieldType = TypeFieldOpt%normal
varType = argType

IF (argType .EQ. TypeFieldOpt%constant) THEN
  fieldType = argType
  varType = argType
END IF

CALL TensorMeshFieldInitiate(obj=obj, name=name, fieldType=fieldType, &
                             varType=varType, engine=engine, &
                             defineOn=TypeFieldOpt%nodal, dim1=dims(1), &
                             dim2=dims(2), nns=nns, mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TensorMeshFieldInitiate4

!----------------------------------------------------------------------------
!                                                  STTensorMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE STTensorMeshFieldInitiate4
CHARACTER(*), PARAMETER :: myName = "STTensorMeshFieldInitiate4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType, &
                numReturns, dims(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()

returnType = func%GetReturnType()

#ifdef DEBUG_VER
isok = returnType .EQ. TypeFieldOpt%matrix
CALL AssertError1(isok, myName, 'returnType should be Matrix')
#endif

argType = func%GetArgType()
numReturns = func%GetNumReturns()
dims = func%GetReturnShape()
fieldType = TypeFieldOpt%normal
varType = argType
IF (argType .EQ. TypeFieldOpt%constant) THEN
  fieldType = argType
  varType = argType
END IF

#ifdef DEBUG_VER
isok = PRESENT(nnt)
CALL AssertError1(isok, myName, 'nnt should be present')
#endif

CALL STTensorMeshFieldInitiate(obj=obj, name=name, fieldType=fieldType, &
                               varType=varType, engine=engine, &
                               defineOn=TypeFieldOpt%nodal, dim1=dims(1), &
                               dim2=dims(2), nns=nns, nnt=nnt, mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE STTensorMeshFieldInitiate4

!----------------------------------------------------------------------------
!                                                  STTensorMeshFieldInitiate4
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "STTensorMeshFieldInitiate4()"
#endif

INTEGER(I4B) :: rank, nns, varType, fieldType, &
                spaceCompo, dims(2), s(4), tsize, rank

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()
rank = func%GetReturnType()
varType = func%GetArgType()
spaceCompo = func%GetNumReturns()
dims = func%GetReturnShape()

fieldType = TypeFieldOpt%normal
IF (varType .EQ. TypeFieldOpt%constant) fieldType = varType

CALL MeshFieldGetShapeAndSize(rank=rank, varType=varType, s=s, tsize=tsize, &
                              nns=nns, spaceCompo=spaceCompo, dim1=dims(1), &
                              dim2=dims(2), nnt=nnt)

CALL obj%Initiate(name=name, &
                  fieldType=fieldType, &
                  varType=varType, &
                  engine=engine, &
                  defineOn=TypeFieldOpt%nodal, &
                  rank=rank, &
                  s=s(1:tsize), &
                  mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Vector()"
#endif
#include "../../include/deallocate_vector.F90"
END PROCEDURE obj_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Ptr_Vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
