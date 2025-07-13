! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(FEDOF_Class) IOMethods
USE GlobalData, ONLY: stdout, CHAR_LF

USE Display_Method, ONLY: Display, ToString

USE TomlUtility, ONLY: GetValue, GetValue_

USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize

USE String_Class, ONLY: String

USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToInteger, &
                                    BaseType_ToInteger

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInitiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%isLagrange, "isLagrange: ", unitno=unitno)
CALL Display(obj%tdof, "tdof: ", unitno=unitno)
CALL Display(obj%tNodes, "tNodes: ", unitno=unitno)
CALL Display(obj%tEdges, "tEdges: ", unitno=unitno)
CALL Display(obj%tFaces, "tFaces: ", unitno=unitno)
CALL Display(obj%tCells, "tCells: ", unitno=unitno)
CALL Display(obj%maxTotalConnectivity, "maxTotalConnectivity: ", unitno=unitno)
CALL Display(obj%baseContinuity, "baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, "baseInterpolation: ", unitno=unitno)
CALL Display(obj%maxCellOrder, "maxCellOrder: ", unitno=unitno)
CALL Display(obj%maxFaceOrder, "maxFaceOrder: ", unitno=unitno)
CALL Display(obj%maxEdgeOrder, "maxEdgeOrder: ", unitno=unitno)

isok = ASSOCIATED(obj%mesh)
CALL Display(isok, "mesh ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%mesh%DisplayMeshInfo("Mesh information: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%faceOrder)
CALL Display(isok, "faceOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%faceOrder), "faceOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%edgeOrder)
CALL Display(isok, "edgeOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%edgeOrder), "edgeOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%edgeIA)
CALL Display(isok, "edgeIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%edgeIA), "edgeIA size: ", unitno=unitno)

isok = ALLOCATED(obj%faceIA)
CALL Display(isok, "faceIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%faceIA), "faceIA size: ", unitno=unitno)

isok = ALLOCATED(obj%cellIA)
CALL Display(isok, "cellIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellIA), "cellIA size: ", unitno=unitno)

DO ii = 1, SIZE(obj%fe)
  isok = ASSOCIATED(obj%fe(ii)%ptr)
  CALL Display(isok, "fe(ii)%ptr ASSOCIATED: ", unitno=unitno)
END DO

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                           DisplayCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayCellOrder
LOGICAL(LGT) :: isok
isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
                       unitno=unitno)
CALL Display(obj%cellOrder, "cellOrder: ", unitno=unitno, full=full)
END PROCEDURE obj_DisplayCellOrder

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
TYPE(String) :: baseContinuity, baseInterpolation, astr, baseTypeStr(3)
INTEGER(I4B) :: ipType, origin, stat, tBaseType, ii, baseType0(3), &
                tAlpha, tBeta, tLambda
REAL(DFP) :: alpha(3), beta(3), lambda(3)
LOGICAL(LGT) :: isFound, abool, islocal
INTEGER(I4B), ALLOCATABLE :: order(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseContinuity...')
#endif

CALL GetValue(table=table, key="baseContinuity", VALUE=baseContinuity, &
  default_value=obj%baseContinuity, origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseInterpolation...')
#endif

! baseInterpolation
CALL GetValue(table=table, key="baseInterpolation", VALUE=baseInterpolation, &
  default_value=obj%baseInterpolation, origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading ipType...')
#endif
! ipType
CALL GetValue(table=table, key="ipType", VALUE=astr, &
      default_value=DEFAULT_IPTYPE, origin=origin, stat=stat, isFound=isFound)
ipType = InterpolationPoint_ToInteger(astr%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseType...')
#endif

! baseType
CALL GetValue_(table=table, key="baseType", VALUE=baseTypeStr, &
               tsize=tBaseType, origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Total baseType found is '//ToString(tBaseType))
#endif

DO ii = 1, tBaseType
  baseType0(ii) = BaseType_ToInteger(baseTypeStr(ii)%chars())
END DO

! if baseType is not found, then set it to default which is DEFAULT_BASETYPE
abool = tBaseType .EQ. 0 .OR. (.NOT. isFound)
IF (abool) THEN
  tBaseType = SIZE(baseType0)
  DO ii = 1, tBaseType
    baseType0(ii) = BaseType_ToInteger(DEFAULT_BASETYPE)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading alpha...')
#endif

! alpha
CALL GetValue_(table=table, key="alpha", VALUE=alpha, &
               tsize=tAlpha, origin=origin, stat=stat, isFound=isFound)
abool = tAlpha .EQ. 0 .OR. (.NOT. isFound)
IF (abool) THEN
  tAlpha = SIZE(alpha)
  alpha = DEFAULT_ALPHA
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading beta...')
#endif

! beta
CALL GetValue_(table=table, key="beta", VALUE=beta, &
               tsize=tBeta, origin=origin, stat=stat, isFound=isFound)
abool = tBeta .EQ. 0 .OR. (.NOT. isFound)
IF (abool) THEN
  tBeta = SIZE(beta)
  beta = DEFAULT_BETA
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading lambda...')
#endif

! lambda
CALL GetValue_(table=table, key="lambda", VALUE=lambda, &
               tsize=tLambda, origin=origin, stat=stat, isFound=isFound)
abool = tLambda .EQ. 0 .OR. (.NOT. isFound)
IF (abool) THEN
  tLambda = SIZE(lambda)
  lambda = DEFAULT_LAMBDA
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading islocal...')
#endif

! islocal
CALL GetValue(table=table, key="islocal", VALUE=islocal, &
             default_value=.FALSE., origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading order...')
#endif

! order
CALL GetValue(table=table, key="order", VALUE=order, &
              origin=origin, stat=stat, isFound=isFound)
IF (.NOT. isFound) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'order not found in toml file.')
  RETURN
END IF

! Index in order vector is based on global numbering of cells
! we need to convert it to local numbering

CALL obj%Initiate(mesh=mesh, order=order, baseContinuity=baseContinuity%chars(), &
     baseInterpolation=baseInterpolation%chars(), ipType=ipType, basisType=baseType0, &
                  alpha=alpha, beta=beta, lambda=lambda, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

isok = ASSOCIATED(node)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
END IF

CALL obj%ImportFromToml(table=node, mesh=mesh)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
