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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) ConstructorMethods
USE InputUtility, ONLY: INPUT
USE String_Class, ONLY: String
USE Display_Method, ONLY: ToString
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypePrecondOpt
USE CSRMatrix_Method, ONLY: CSRMatrix_Deallocate => DEALLOCATE
USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate
USE CSRMatrix_Method, ONLY: ASSIGNMENT(=)
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldDeallocate
USE AbstractField_Class, ONLY: AbstractFieldInitiate
USE BaseType, ONLY: DOF_
USE DOF_Method, ONLY: DOF_Initiate => Initiate
USE DOF_Method, ONLY: DOF_Deallocate => DEALLOCATE
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE SafeSizeUtility, ONLY: SafeSize
USE ReallocateUtility, ONLY: Reallocate
USE FEDOF_Class, ONLY: FEDOFSetSparsity

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       setMatrixFieldParam
!----------------------------------------------------------------------------

! MODULE PROCEDURE SetMatrixFieldParam
! CHARACTER(*), PARAMETER :: prefix = myprefix
! CHARACTER(*), PARAMETER :: myName = "SetMatrixFieldParam()"
! TYPE(ParameterList_), POINTER :: sublist
! INTEGER(I4B) :: ierr
!
! CALL SetAbstractFieldParam(param=param, prefix=prefix, name=name, &
!                            engine=engine, fieldType=fieldType, comm=comm, &
!                            local_n=local_n, global_n=global_n)
!
! sublist => NULL()
! ierr = param%GetSubList(key=prefix, sublist=sublist)
! IF (ierr .NE. 0_I4B) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
!   RETURN
! END IF
!
! CALL Set(obj=sublist, datatype="Char", prefix=prefix, key="matrixProp", &
!          VALUE=matrixProp)
!
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="spaceCompo", &
!          VALUE=Input(default=1_I4B, option=spaceCompo))
!
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="timeCompo", &
!          VALUE=Input(default=1_I4B, option=timeCompo))
!
! sublist => NULL()
! END PROCEDURE SetMatrixFieldParam

!----------------------------------------------------------------------------
!                                                 setMatrixFieldPrecondParam
!----------------------------------------------------------------------------

! MODULE PROCEDURE SetMatrixFieldPrecondParam
! CHARACTER(*), PARAMETER :: myName = "SetMatrixFieldPrecondParam()"
! CHARACTER(*), PARAMETER :: prefix = "Precond"
! LOGICAL(LGT) :: abool, isSublist
! INTEGER(I4B) :: ierr
! TYPE(ParameterList_), POINTER :: sublist
!
! sublist => NULL()
!
! ! Create a new sublist
! isSublist = param%isSubList(prefix)
!
! IF (isSublist) THEN
!
!   ierr = param%GetSubList(key=prefix, sublist=sublist)
!   IF (ierr .NE. 0) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
!     RETURN
!   END IF
!
! ELSE
!
!   sublist => param%NewSubList(key=prefix)
!
! END IF
!
! IF (.NOT. ASSOCIATED(sublist)) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
!   RETURN
! END IF
!
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="name", VALUE=name)
! CALL Set(obj=sublist, datatype="Char", prefix=prefix, key="engine", VALUE=engine)
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="comm", &
!          VALUE=INPUT(option=comm, default=0_I4B))
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="local_n", &
!          VALUE=INPUT(option=local_n, default=0_I4B))
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="global_n", &
!          VALUE=INPUT(option=global_n, default=0_I4B))
!
! SELECT CASE (name)
! CASE (PRECOND_ILUT)
!
!   abool = (.NOT. PRESENT(droptol)) .OR. (.NOT. PRESENT(lfil))
!
!   IF (abool) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[INTERNAL ERROR] :: for PRECOND_ILUT '// &
!                       'droptol and lfil should be present!!!')
!     RETURN
!   END IF
!
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="droptol", &
!            VALUE=droptol)
!
!   CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="lfil", &
!            VALUE=lfil)
!
! CASE (PRECOND_ILUTP)
!
!   abool = (.NOT. PRESENT(droptol)) &
!           .OR. (.NOT. PRESENT(lfil)) &
!           .OR. (.NOT. PRESENT(permtol)) &
!           .OR. (.NOT. PRESENT(mbloc))
!
!   IF (abool) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                    '[INTERNAL ERROR] :: for PRECOND_ILUTP droptol, lfil, '// &
!                       ' permtol, mbloc should be present!!!')
!     RETURN
!   END IF
!
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="droptol", &
!            VALUE=droptol)
!
!   CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="lfil", &
!            VALUE=lfil)
!
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="permtol", &
!            VALUE=permtol)
!
!   CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="mbloc", &
!            VALUE=mbloc)
!
! CASE (PRECOND_ILUD)
!
!   abool = (.NOT. PRESENT(droptol)) .OR. (.NOT. PRESENT(alpha))
!
!   IF (abool) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[INTERNAL ERROR] :: for PRECOND_ILUTP droptol '// &
!                       ' and alpha should be present!!!')
!     RETURN
!   END IF
!
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="droptol", &
!            VALUE=droptol)
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="alpha", &
!            VALUE=alpha)
!
! CASE (PRECOND_ILUDP)
!
!   abool = (.NOT. PRESENT(droptol)) &
!           .OR. (.NOT. PRESENT(alpha)) &
!           .OR. (.NOT. PRESENT(permtol)) &
!           .OR. (.NOT. PRESENT(mbloc))
!
!   IF (abool) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[INTERNAL ERROR] :: for PRECOND_ILUTP droptol, '// &
!                       ' alpha, permtol, mbloc should be present!!!')
!     RETURN
!   END IF
!
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="droptol", &
!            VALUE=droptol)
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="alpha", &
!            VALUE=alpha)
!   CALL Set(obj=sublist, datatype=1.0_DFP, prefix=prefix, key="permtol", &
!            VALUE=permtol)
!   CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="mbloc", &
!            VALUE=mbloc)
!
! CASE (PRECOND_ILUK)
!
!   abool = .NOT. PRESENT(lfil)
!   IF (abool) THEN
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!              '[INTERNAL ERROR] :: for PRECOND_ILUK lfil should be present!!!')
!     RETURN
!   END IF
!
!   CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="lfil", &
!            VALUE=lfil)
!
! CASE DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!               '[INTERNAL ERROR] :: No case found for given precondition name')
!   RETURN
! END SELECT
!
! sublist => NULL()
!
! END PROCEDURE SetMatrixFieldPrecondParam

!----------------------------------------------------------------------------
!                                               SetRectangleMatrixFieldParam
!----------------------------------------------------------------------------

! MODULE PROCEDURE SetRectangleMatrixFieldParam
! CHARACTER(*), PARAMETER :: myName = "SetRectangleMatrixFieldParam()"
! CHARACTER(*), PARAMETER :: prefix = myprefix
! TYPE(ParameterList_), POINTER :: sublist
! INTEGER(I4B) :: ii, ierr
!
! CALL SetAbstractFieldParam(param=param, prefix=prefix, name=name, &
!              engine=engine, fieldType=fieldType, comm=comm, local_n=local_n, &
!                            global_n=global_n)
!
! sublist => NULL()
! ierr = param%GetSubList(key=prefix, sublist=sublist)
! IF (ierr .NE. 0_I4B) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
!   RETURN
! END IF
!
! CALL Set(obj=sublist, datatype="Char", prefix=prefix, key="matrixProp", &
!          VALUE=matrixProp)
!
! CALL Set(obj=sublist, datatype=[1_I4B], prefix=prefix, key="spaceCompo", &
!          VALUE=spaceCompo)
!
! CALL Set(obj=sublist, datatype=[1_I4B], prefix=prefix, key="timeCompo", &
!          VALUE=timeCompo)
!
! ii = SIZE(physicalVarNames)
! CALL Set(obj=sublist, datatype=1_I4B, prefix=prefix, key="tPhysicalVarNames", &
!          VALUE=ii)
!
! DO ii = 1, SIZE(physicalVarNames)
!   CALL Set(obj=sublist, datatype="Char", prefix=prefix, &
!            key="physicalVarName"//TOSTRING(ii), VALUE=physicalVarNames(ii))
! END DO
!
! sublist => NULL()
! END PROCEDURE SetRectangleMatrixFieldParam

!----------------------------------------------------------------------------
!                                             MatrixFieldCheckEssentialParam
!----------------------------------------------------------------------------

! MODULE PROCEDURE MatrixFieldCheckEssentialParam
! CHARACTER(*), PARAMETER :: myName = "MatrixFieldCheckEssentialParam()"
! TYPE(String) :: astr
! TYPE(String), ALLOCATABLE :: essentialParam(:)
! INTEGER(I4B) :: ii
! LOGICAL(LGT) :: isok
!
! astr = "/name/matrixProp/engine/fieldType/comm/local_n/global_n"
!
! CALL astr%Split(essentialParam, sep="/")
! CALL CheckEssentialParam(obj=param, keys=essentialParam, prefix=myprefix, &
!                          myName=myName, modName=modName)
! !CheckEssentialParam param is defined in easifemClasses FPL_Method
!
! astr = ""
! isok = ALLOCATED(essentialParam)
! IF (.NOT. isok) RETURN
!
! DO ii = 1, SIZE(essentialParam)
!   essentialParam(ii) = ""
! END DO
! DEALLOCATE (essentialParam)
! END PROCEDURE MatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                    RectangleMatrixFieldCheckEssentialParam
!----------------------------------------------------------------------------

! MODULE PROCEDURE RectangleMatrixFieldCheckEssentialParam
! CHARACTER(*), PARAMETER :: myName = "RectangleMatrixFieldCheckEssentialParam()"
! TYPE(String) :: astr
! TYPE(String), ALLOCATABLE :: essentialParam(:)
! INTEGER(I4B) :: ii, n
! LOGICAL(LGT) :: isok
!
! astr = "/name/matrixProp/engine/tPhysicalVarNames/spaceCompo/timeCompo/fieldType/comm/local_n/global_n"
!
! CALL astr%Split(essentialParam, sep="/")
! CALL CheckEssentialParam(obj=param, keys=essentialParam, prefix=myprefix, &
!                          myName=myName, modName=modName)
! !CheckEssentialParam param is defined in easifemClasses FPL_Method
!
! astr = ""
! isok = ALLOCATED(essentialParam)
! IF (.NOT. isok) RETURN
!
! DO ii = 1, SIZE(essentialParam)
!   essentialParam(ii) = ""
! END DO
! DEALLOCATE (essentialParam)
!
! CALL GetValue(obj=param, prefix=myprefix, key="tPhysicalVarNames", VALUE=n)
! ! GetValue is defined in FPL_Method
!
! DO ii = 1, n
!   isok = param%isPresent(key=myprefix//"/physicalVarName"//ToString(ii))
!   IF (.NOT. isok) THEN
!     CALL e%RaiseError(modName//'::'//myName//" - "// &
!                       myprefix//'/physicalVarName' &
!                       //ToString(ii) &
!                       //' should be present in param')
!   END IF
! END DO
!
! END PROCEDURE RectangleMatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate1
! CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
! INTEGER(I4B) :: ierr, nrow, ncol, nnz, storageFMT, tNodes(1), timeCompo(1), &
!                 spaceCompo(1)
! CHARACTER(1) :: names_char(1)
! TYPE(DOF_) :: dofobj
! TYPE(String) :: astr
! TYPE(ParameterList_), POINTER :: sublist
! LOGICAL(LGT) :: isok
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START]')
! #endif
!
! sublist => NULL()
! ierr = param%GetSubList(key=myprefix, sublist=sublist)
!
! #ifdef DEBUG_VER
! isok = ierr .EQ. 0_I4B
! CALL AssertError1(isok, myName, "Some error occured in getting sublist(1)")
! #endif
!
! #ifdef DEBUG_VER
! isok = ASSOCIATED(sublist)
! CALL AssertError1(isok, myName, "sublist is not associated")
! #endif
!
! CALL obj%CheckEssentialParam(sublist)
!
! CALL obj%DEALLOCATE()
!
! CALL AbstractFieldInitiate(obj=obj, param=param, fedof=fedof, &
!                            geofedof=geofedof)
!
! CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", &
!               VALUE=spaceCompo(1))
!
! CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", &
!               VALUE=timeCompo(1))
!
! ! storage format
! storageFMT = mystorageformat
! tNodes = fedof%GetTotalDOF()
!
! ! make [[DOF_]]
! CALL DOF_Initiate(obj=dofobj, tNodes=tNodes, names=names_char, &
!                   spaceCompo=spaceCompo, timeCompo=timeCompo, &
!                   storageFMT=storageFMT)
!
! ! matrixProp
! astr = ""
! CALL GetValue(obj=sublist, prefix=myprefix, key="matrixProp", VALUE=astr)
!
! nrow = tNodes(1) * spaceCompo(1) * timeCompo(1)
!
! ncol = nrow
!
! CALL CSRMatrix_Initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=dofobj, &
!                         jdof=dofobj, matrixProp=astr%chars())
!
! obj%isInit = .TRUE.
! obj%isPmatInitiated = .FALSE.
! obj%isRectangle = .FALSE.
!
! IF (obj%local_n .EQ. 0) obj%local_n = nrow
! IF (obj%global_n .EQ. 0) obj%global_n = nrow
!
! ! setting the sparsity
! CALL obj%fedof%SetSparsity(mat=obj%mat)
!
! CALL DOF_Deallocate(dofobj)
! sublist => NULL()
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
!
! END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

TYPE(DOF_) :: dofobj
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize, tNodes(1), timeCompo(1), spaceCompo(1), &
                storageFMT, nrow, ncol
CHARACTER(*), PARAMETER :: matrixProp = "UNSYM"
TYPE(String) :: astr
CHARACTER(1) :: names_char(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
isok = obj2%IsInitiated()
CALL AssertError1(isok, myName, "obj2 is not initiated")
#endif

CALL AbstractFieldInitiate( &
  obj=obj, obj2=obj2, copyFull=copyFull, copyStructure=copyStructure, &
  usePointer=usePointer)

SELECT TYPE (obj2)
CLASS IS (AbstractNodeField_)
  ! Initiate dof object
  tNodes = obj2%GetTotalDOF(tPhysicalVars=1)
  spaceCompo = obj2%GetSpaceCompo(tPhysicalVars=1)
  timeCompo = obj2%GetTimeCompo(tPhysicalVars=1)
  astr = obj2%GetName()
  storageFMT = mystorageformat
  names_char(1) (1:1) = astr%Slice(1, 1)
  CALL DOF_Initiate( &
    obj=dofobj, tNodes=tNodes, names=names_char, spaceCompo=spaceCompo, &
    timeCompo=timeCompo, storageFMT=storageFMT)

  ! Get nrow and ncol from obj2
  nrow = obj2%SIZE()
  ncol = nrow

#ifdef DEBUG_VER
  isok = nrow .EQ. (tNodes(1) * spaceCompo(1) * timeCompo(1))
  CALL AssertError1(isok, myName, &
            "nrow should be same as tNodes(1) * spaceCompo(1) * timeCompo(1)")
#endif

  ! Initiate CSRMatrix
  CALL CSRMatrix_Initiate( &
    obj=obj%mat, nrow=nrow, ncol=ncol, idof=dofobj, jdof=dofobj, &
    matrixProp=astr%chars())

  isok = obj%local_n .EQ. 0
  IF (isok) obj%local_n = nrow

  isok = obj%global_n .EQ. 0
  IF (isok) obj%global_n = nrow

  CALL obj%fedof%SetSparsity(mat=obj%mat)

  CALL DOF_Deallocate(dofobj)

CLASS IS (MatrixField_)
  obj%mat = obj2%mat
  obj%submat = obj2%submat
  obj%isPmatInitiated = obj2%isPMatInitiated
  obj%isRectangle = obj2%isRectangle

  CALL MatrixFieldPreconditionCopy(obj=obj%pmat, obj2=obj2%pmat)

  tsize = SafeSize(obj2%dbcPtrs)
  CALL Reallocate(obj%dbcPtrs, tsize)
  DO ii = 1, tsize
    obj%dbcPtrs(ii) = obj2%dbcPtrs(ii)
  END DO

  tsize = SafeSize(obj2%subIndices)
  CALL Reallocate(obj%subIndices, tsize)
  DO ii = 1, tsize
    obj%subIndices(ii) = obj2%subIndices(ii)
  END DO

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'obj2 should an instance of MatrixField_ or its child')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                               MatrixFieldPreconditionCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldPreconditionCopy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "MatrixFieldPreconditionCopy()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInitiated = obj2%isInitiated
obj%PmatName = obj2%PmatName
obj%nnz = obj2%nnz
obj%ncol = obj2%ncol
obj%nrow = obj2%nrow
obj%lfil = obj2%lfil
obj%mbloc = obj2%mbloc
obj%alpha = obj2%alpha
obj%droptol = obj2%droptol
obj%permtol = obj2%permtol

tsize = SafeSize(obj2%A)
CALL Reallocate(obj%A, tsize)
DO ii = 1, tsize
  obj%A(ii) = obj2%A(ii)
END DO

tsize = SafeSize(obj2%JA)
CALL Reallocate(obj%JA, tsize)
DO ii = 1, tsize
  obj%JA(ii) = obj2%JA(ii)
END DO

tsize = SafeSize(obj2%IA)
CALL Reallocate(obj%IA, tsize)
DO ii = 1, tsize
  obj%IA(ii) = obj2%IA(ii)
END DO

tsize = SafeSize(obj2%JU)
CALL Reallocate(obj%JU, tsize)
DO ii = 1, tsize
  obj%JU(ii) = obj2%JU(ii)
END DO

tsize = SafeSize(obj2%IPERM)
CALL Reallocate(obj%IPERM, tsize)
DO ii = 1, tsize
  obj%IPERM(ii) = obj2%IPERM(ii)
END DO

tsize = SafeSize(obj2%LEVS)
CALL Reallocate(obj%LEVS, tsize)
DO ii = 1, tsize
  obj%LEVS(ii) = obj2%LEVS(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE MatrixFieldPreconditionCopy

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate3
! CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
! INTEGER(I4B), PARAMETER :: tVar = 2
!
! INTEGER(I4B) :: ierr, nrow, ncol, nnz, storageFMT, tNodes(tVar), &
!                 timeCompo(tVar), spaceCompo(tVar), ii
! CHARACTER(1) :: physicalVarNames(tVar)
! TYPE(DOF_) :: idofobj, jdofobj
! ! CLASS( AbstractMesh_ ), POINTER :: dom(tVar)
! TYPE(ParameterList_), POINTER :: sublist
! LOGICAL(LGT) :: isok
! TYPE(String) :: astr, matrixProp
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! sublist => NULL()
! ierr = param%GetSubList(key=myprefix, sublist=sublist)
! isok = ierr .EQ. 0_I4B
! CALL AssertError1(isok, myName, "Some error occured in getting sublist(1)")
!
! isok = ASSOCIATED(sublist)
! CALL AssertError1(isok, myName, "sublist is not associated")
!
! CALL RectangleMatrixFieldCheckEssentialParam(obj=obj, param=sublist)
! CALL obj%DEALLOCATE()
!
! ! matrixProp
! CALL GetValue(obj=sublist, prefix=myprefix, key="matrixProp", &
!               VALUE=matrixProp)
! isok = matrixProp%chars() == "RECTANGLE"
! CALL AssertError1(isok, myName, "matrixProp should be RECTANGLE")
!
! ! engine
! CALL GetValue(obj=sublist, prefix=myprefix, key="engine", &
!               VALUE=obj%engine)
!
! ! name
! CALL GetValue(obj=sublist, prefix=myprefix, key="name", &
!               VALUE=obj%name)
!
! ! fieldType
! CALL GetValue(obj=sublist, prefix=myprefix, key="fieldType", &
!               VALUE=obj%fieldType)
!
! ! check domain
! isok = SIZE(fedof) .EQ. tVar
! CALL AssertError1(isok, myName, &
!                   "Size of dom should be equal to 2, that is two domains.")
!
! DO ii = 1, tVar
!   isok = ASSOCIATED(fedof(ii)%ptr)
!   CALL AssertError1(isok, myName, &
!                     "fedof("//TOSTRING(ii)//")%ptr is not associated")
! END DO
!
! ! physicalVarName
! DO ii = 1, tVar
!   CALL GetValue(obj=sublist, prefix=myprefix, &
!                 key="physicalVarName"//tostring(ii), VALUE=astr)
!
!   physicalVarNames(ii) (1:1) = astr%Slice(1, 1)
!   astr = ""
!
! END DO
!
! ! spaceCompo
! CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", &
!               VALUE=spaceCompo)
!
! ! timeCompo
! CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", VALUE=timeCompo)
!
! ! storage format
! storageFMT = mystorageformat
!
! ! domains
! ALLOCATE (obj%fedofs(tvar))
! DO ii = 1, tVar
!   obj%fedofs(ii)%ptr => fedof(ii)%ptr
!   tNodes(ii) = obj%fedofs(ii)%ptr%GetTotalDOF()
! END DO
!
! ! make [[DOF_]]
! CALL DOF_Initiate( &
!   obj=idofobj, tNodes=tNodes(1:1), names=physicalVarNames(1:1), &
!   spaceCompo=spaceCompo(1:1), timeCompo=timeCompo(1:1), &
!   storageFMT=storageFMT)
!
! CALL DOF_Initiate( &
!   obj=jdofobj, tNodes=tNodes(2:2), names=physicalVarNames(2:2), &
!   spaceCompo=spaceCompo(2:2), timeCompo=timeCompo(2:2), &
!   storageFMT=storageFMT)
!
! ! CSRMatrix/Initiate
! nrow = .tNodes.idofobj
! ncol = .tNodes.jdofobj
!
! CALL CSRMatrix_Initiate( &
!   obj=obj%mat, nrow=nrow, ncol=ncol, idof=idofobj, jdof=jdofobj, &
!   matrixProp=matrixProp%chars())
!
! matrixProp = ""
!
! obj%isInit = .TRUE.
! obj%isPmatInitiated = .FALSE.
! obj%isRectangle = .TRUE.
!
! ! setting the sparsity
! CALL FEDOFSetSparsity(mat=obj%mat, fedofs=obj%fedofs)
!
! ! comm
! CALL GetValue(obj=sublist, prefix=myprefix, key="comm", VALUE=obj%comm)
! CALL GetValue(obj=sublist, prefix=myprefix, key="global_n", VALUE=obj%global_n)
! CALL GetValue(obj=sublist, prefix=myprefix, key="local_n", VALUE=obj%local_n)
!
! IF (obj%local_n .EQ. 0) obj%local_n = nrow
! IF (obj%global_n .EQ. 0) obj%global_n = nrow
!
! CALL DOF_Deallocate(idofobj)
! CALL DOF_Deallocate(jdofobj)
! END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMatrixFieldDeallocate(obj)
CALL CSRMatrix_Deallocate(obj%mat)
CALL CSRMatrix_Deallocate(obj%submat)
CALL Pmat_Deallocate(obj%pmat)
obj%isRectangle = .FALSE.
obj%tdbcptrs = 0
obj%tsubindices = 0
IF (ALLOCATED(obj%dbcPtrs)) DEALLOCATE (obj%dbcPtrs)
IF (ALLOCATED(obj%subIndices)) DEALLOCATE (obj%subIndices)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Pmat_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Pmat_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%PmatName = 0
IF (ALLOCATED(obj%A)) DEALLOCATE (obj%A)
IF (ALLOCATED(obj%JA)) DEALLOCATE (obj%JA)
IF (ALLOCATED(obj%IA)) DEALLOCATE (obj%IA)
IF (ALLOCATED(obj%JU)) DEALLOCATE (obj%JU)
IF (ALLOCATED(obj%IPERM)) DEALLOCATE (obj%IPERM)
IF (ALLOCATED(obj%LEVS)) DEALLOCATE (obj%LEVS)
obj%nnz = 0
obj%ncol = 0
obj%nrow = 0
obj%isInitiated = .FALSE.
obj%lfil = 0
obj%mbloc = 0
obj%alpha = 0
obj%droptol = 0
obj%permtol = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Pmat_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_ptr_vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_ptr_vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate_ptr_vector

!----------------------------------------------------------------------------
!                                                               SafeAllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MatrixFieldAllocate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MatrixFieldAllocate1()"
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj)

IF (.NOT. isok) THEN
  ALLOCATE (obj(newsize))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

tsize = SIZE(obj)

isok = tsize .LT. newsize
IF (isok) THEN
  CALL MatrixFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MatrixFieldAllocate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
