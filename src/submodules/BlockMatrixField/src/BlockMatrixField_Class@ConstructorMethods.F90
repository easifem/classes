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
! summary: This module contains constructor method for [[BlockMatrixField_]]

SUBMODULE(BlockMatrixField_Class) ConstructorMethods

USE Display_Method, ONLY: ToString

USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam

USE AbstractField_Class, ONLY: SetAbstractFieldParam

USE MatrixField_Class, ONLY: SetMatrixFieldPrecondParam

USE String_Class, ONLY: String

USE InputUtility, ONLY: Input

USE BaseType, ONLY: DOF_

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      OPERATOR(.tNodes.), &
                      DOF_Deallocate => DEALLOCATE

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate

USE FEDOF_Class, ONLY: FEDOFSetSparsity

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                    SetBlockMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockMatrixFieldParam
CHARACTER(*), PARAMETER :: myName = "SetBlockMatrixFieldParam()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER

CALL AssertError2(SIZE(physicalVarNames), SIZE(spaceCompo), myName, &
                  "a= size of physicalVarNames, b= size of spaceCompo")

CALL AssertError2(SIZE(physicalVarNames), SIZE(timeCompo), myName, &
                  "a= size of physicalVarNames, b= size of timeCompo")

#endif

CALL SetAbstractFieldParam(param=param, prefix=myprefix, name=name, &
             engine=engine, fieldType=fieldType, comm=comm, local_n=local_n, &
                           global_n=global_n)

sublist => NULL()
ii = param%GetSubList(key=myprefix, sublist=sublist)
IF (ii .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

CALL Set(obj=sublist, datatype="Char", prefix=myprefix, key="matrixProp", &
         VALUE=matrixProp)

CALL Set(obj=sublist, datatype=spaceCompo, prefix=myprefix, key="spaceCompo", &
         VALUE=spaceCompo)

CALL Set(obj=sublist, datatype=timeCompo, prefix=myprefix, key="timeCompo", &
         VALUE=timeCompo)

tsize = SIZE(physicalVarNames)

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, &
         key="tPhysicalVarNames", VALUE=tsize)

DO ii = 1, tsize
  CALL Set(obj=sublist, datatype="Char", prefix=myprefix, &
           key="physicalVarName"//ToString(ii), VALUE=physicalVarNames(ii))
END DO

sublist => NULL()

END PROCEDURE SetBlockMatrixFieldParam

!----------------------------------------------------------------------------
!                                           SetBlockMatrixFieldPrecondParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockMatrixFieldPrecondParam
CALL SetMatrixFieldPrecondParam(param=param, name=name, engine=engine, &
      lfil=lfil, mbloc=mbloc, droptol=droptol, permtol=permtol, alpha=alpha, &
                                comm=comm, local_n=local_n, global_n=global_n)
END PROCEDURE SetBlockMatrixFieldPrecondParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
TYPE(String), ALLOCATABLE :: essentialParam(:)
TYPE(String) :: astr
INTEGER(I4B) :: ii, n
LOGICAL(LGT) :: isok

astr = "/name/matrixProp/engine/fieldType/comm/local_n/global_n/tPhysicalVarNames/spaceCompo/timeCompo/comm"

CALL astr%Split(essentialParam, sep="/")
CALL CheckEssentialParam(obj=param, keys=essentialParam, prefix=myprefix, &
                         myName=myName, modName=modName)
! INFO: CheckEssentialParam param is defined in easifemClasses FPL_Method

astr = ""
isok = ALLOCATED(essentialParam)
IF (.NOT. isok) RETURN

DO ii = 1, SIZE(essentialParam)
  essentialParam(ii) = ""
END DO
DEALLOCATE (essentialParam)

CALL GetValue(obj=param, prefix=myprefix, key="tPhysicalVarNames", VALUE=n)

DO ii = 1, n

  isok = param%isPresent(key=myprefix//"/physicalVarName"//ToString(ii))

  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      myprefix//'/physicalVarName' &
                      //tostring(ii) &
                      //' should be present in param')
  END IF

END DO

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
TYPE(FEDOFPointer_), ALLOCATABLE :: fedofs(:), geofedofs(:)
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: tPhysicalVarNames, ii, ierr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, "Some error occured in getting sublist(1)")

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, "sublist is not associated")

CALL GetValue(obj=sublist, prefix=myprefix, key="tPhysicalVarNames", &
              VALUE=tPhysicalVarNames)

ALLOCATE (fedofs(tPhysicalVarNames))
DO ii = 1, tPhysicalVarNames
  fedofs(ii)%ptr => fedof
END DO

ALLOCATE (geofedofs(tPhysicalVarNames))
DO ii = 1, tPhysicalVarNames
  geofedofs(ii)%ptr => geofedof
END DO

CALL obj%Initiate(param=param, fedof=fedofs, geofedof=geofedofs)

DO ii = 1, tPhysicalVarNames
  fedofs(ii)%ptr => NULL()
  geofedofs(ii)%ptr => NULL()
END DO

IF (ALLOCATED(fedofs)) DEALLOCATE (fedofs)
IF (ALLOCATED(geofedofs)) DEALLOCATE (geofedofs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
INTEGER(I4B) :: ierr, nrow, ncol, storageFMT, tVar, ii, nnz
INTEGER(I4B), ALLOCATABLE :: tNodes(:), timeCompo(:), spaceCompo(:)
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
CHARACTER(:), ALLOCATABLE :: char_var
TYPE(DOF_) :: dofobj
TYPE(ParameterList_), POINTER :: sublist
TYPE(String) :: matrixProp, astr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, "Some error occured in getting sublist(1)")

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, "sublist is not associated")

CALL obj%CheckEssentialParam(param=sublist)
CALL obj%DEALLOCATE()

! matrixProp
! engine
! name
! fieldType

! matrixProp

! engine
CALL GetValue(obj=sublist, prefix=myprefix, key="engine", &
              VALUE=obj%engine)

! name
CALL GetValue(obj=sublist, prefix=myprefix, key="name", &
              VALUE=obj%name)

! fieldType
CALL GetValue(obj=sublist, prefix=myprefix, key="fieldType", &
              VALUE=obj%fieldType)

! tPhysicalVarNames
CALL GetValue(obj=sublist, prefix=myprefix, key="tPhysicalVarNames", &
              VALUE=tVar)

! domain
CALL AssertError2(SIZE(fedof), tVar, myName, &
                  "Size of fedof and total variable is not equal")

DO ii = 1, tVar
  isok = ASSOCIATED(fedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'fedof('//ToString(ii)//')%ptr is NOT ASSOCIATED!')
END DO

! allocate
ALLOCATE (tNodes(tVar), timeCompo(tVar), spaceCompo(tVar), &
          physicalVarNames(tVar))

! spaceCompo
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", &
              VALUE=spaceCompo)

! timeCompo
CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo", &
              VALUE=timeCompo)

! physicalVarName
DO ii = 1, tVar
  CALL GetValue(obj=sublist, prefix=myprefix, &
                key="physicalVarName"//ToString(ii), VALUE=astr)
  physicalVarNames(ii) (1:1) = astr%Slice(1, 1)
  astr = ""
END DO

! storage format
storageFMT = mystorageformat

! fedofs
ALLOCATE (obj%fedofs(tvar))
DO ii = 1, tVar
  obj%fedofs(ii)%ptr => fedof(ii)%ptr
  tNodes(ii) = obj%fedofs(ii)%ptr%GetTotalDOF()
END DO

! make [[DOF_]]
CALL DOF_Initiate(obj=dofobj, tNodes=tNodes, names=physicalVarNames, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT)

! matrixProp
CALL GetValue(obj=sublist, prefix=myprefix, key="matrixProp", &
              VALUE=matrixProp)

! CSRMatrix/Initiate
nrow = .tNodes.dofobj
ncol = nrow

CALL CSRMatrix_Initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=dofobj, &
                        jdof=dofobj, matrixProp=matrixProp%chars())

obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.

! Setting the sparsity
CALL FEDOFSetSparsity(mat=obj%mat, fedofs=obj%fedofs)

CALL GetValue(obj=sublist, prefix=myprefix, key="comm", VALUE=obj%comm)
CALL GetValue(obj=sublist, prefix=myprefix, key="global_n", VALUE=obj%global_n)
CALL GetValue(obj=sublist, prefix=myprefix, key="local_n", VALUE=obj%local_n)

IF (obj%local_n .EQ. 0) obj%local_n = nrow
IF (obj%global_n .EQ. 0) obj%global_n = nrow

!cleanup

CALL DOF_Deallocate(dofobj)
IF (ALLOCATED(tNodes)) DEALLOCATE (tNodes)
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
