!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Initiate a rectangle matrix field from a row and a column field
!

PROGRAM main
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ApproxUtility, ONLY: SOFTEQ
USE ApproxUtility, ONLY: SOFTEQ
USE BaseType, ONLY: math => TypeMathOpt
USE BoundingBox_Method, ONLY: Reallocate
USE CSRMatrix_Method, ONLY: Convert
USE CSRMatrix_UnaryMethods, ONLY: Convert
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MatrixField_Class, ONLY: MatrixField_
USE ReallocateUtility, ONLY: Reallocate
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE Test_Method, ONLY: done_testing
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "./SetBrokenH1LagrangeMatrixForVtk_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-5_DFP

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: rowNodeFedof, colNodeFedof, geofedof
TYPE(ScalarField_) :: rowNode, colNode
TYPE(MatrixField_) :: obj
INTEGER(I4B) :: tElements, maxCon, maxNNE, iel, ii, &
                rowFecon_size, colFecon_size
REAL(DFP), ALLOCATABLE :: rowNodeValue(:), colNodeValue(:)
INTEGER(I4B), ALLOCATABLE :: rowFecon(:), colFecon(:)
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr

CALL e%SetQuietMode(math%yes)

! Import domain
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")

! Import continuous space
CALL colNode%ImportFromToml(fedof=colNodeFedof, geofedof=geofedof, dom=dom, &
                            filename=tomlFileName, tomlName="colNode")

! Make rowNode from colNode, which will have brokenSpace true for fedof

! Step-1: make nowNodeFedof
CALL rowNodeFedof%InitiateBrokenLagrangeFedofForVTK(from=colNodeFedof)

! Step-2: make rowNode
CALL rowNode%Initiate2(name="rowNode", engine=colNode%GetEngineName(), &
                       fieldType=TypeFieldOpt%normal, &
                       fedof=rowNodeFedof, geofedof=geofedof)

! Initiate from the row field rowNode and the column field colNode
CALL obj%Initiate(rowObj=rowNode, colObj=colNode)

CALL obj%SetBrokenLagrangeMatrixForVTK(ivar=math%one_i, &
                                       jvar=math%one_i, nCopy=math%one_i)

! Below we will set element matrices to obj
mesh => dom%GetMeshPointer()
tElements = mesh%GetTotalElements()

maxCon = colNodeFedof%GetMaxTotalConnectivity()
maxNNE = geofedof%GetMaxTotalConnectivity()

CALL Reallocate(rowFecon, colNodeFedof%GetMaxTotalConnectivity())
CALL Reallocate(colFecon, rowNodeFedof%GetMaxTotalConnectivity())
CALL Reallocate(colNodeValue, SIZE(colFecon))
CALL Reallocate(rowNodeValue, SIZE(rowFecon))

elemLoop1: DO iel = 1, tElements

  isok = mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE elemLoop1

  CALL colNodeFedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => colNodeFedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL colNodeFedof%GetConnectivity_(ans=colFecon, &
                                     tsize=colFecon_size, opt="A", &
                                     globalElement=iel, islocal=math%yes)
  CALL feptr%FixOrientOfNptrs(colFecon, colFecon_size)

  CALL RANDOM_NUMBER(colNodeValue(1:colFecon_size))
  CALL colNode%Set(globalNode=colFecon(1:colFecon_size), &
                   islocal=math%yes, &
                   VALUE=colNodeValue(1:colFecon_size))
END DO elemLoop1

CALL obj%Matvec(x=colNode, y=rowNode)

elemLoop2: DO iel = 1, tElements

  isok = mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE elemLoop2

  CALL colNodeFedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => colNodeFedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL colNodeFedof%GetConnectivity_(ans=colFecon, &
                                     tsize=colFecon_size, opt="A", &
                                     globalElement=iel, islocal=math%yes)
  CALL feptr%FixOrientOfNptrs(colFecon, colFecon_size)

  CALL rowNodeFedof%GetConnectivity_(ans=rowFecon, &
                                     tsize=rowFecon_size, opt="A", &
                                     globalElement=iel, islocal=math%yes)

  CALL colNode%Get(globalNode=colFecon(1:colFecon_size), &
                   islocal=math%yes, &
                   VALUE=colNodeValue, tsize=ii)

  CALL rowNode%Get(globalNode=rowFecon(1:rowFecon_size), &
                   islocal=math%yes, &
                   VALUE=rowNodeValue, tsize=ii)

  CALL OK(ALL(SOFTEQ(colNodeValue(1:colFecon_size), &
                     rowNodeValue(1:rowFecon_size), tol=tol)), &
          "identity case: ")
END DO elemLoop2

CALL done_testing()
CALL obj%DEALLOCATE()
CALL rowNode%DEALLOCATE()
CALL colNode%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL rowNodeFedof%DEALLOCATE()
CALL colNodeFedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
