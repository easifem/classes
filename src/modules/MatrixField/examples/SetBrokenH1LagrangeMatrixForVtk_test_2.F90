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
USE BaseType, ONLY: TypeDOFOpt
USE BoundingBox_Method, ONLY: Reallocate
USE CSRMatrix_Method, ONLY: Convert
USE CSRMatrix_UnaryMethods, ONLY: Convert
USE Display_Method, ONLY: Display
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MatrixField_Class, ONLY: MatrixField_
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
USE Test_Method, ONLY: done_testing
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "./SetBrokenH1LagrangeMatrixForVtk_test_2.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-5_DFP

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: rowNodeFedof, colNodeFedof, geofedof
TYPE(VectorField_) :: rowNode, colNode
TYPE(MatrixField_) :: obj
INTEGER(I4B) :: tElements, iel, ii, jj, rowFecon_size, colFecon_size
INTEGER(I4B) :: spaceCompo(3)
REAL(DFP), ALLOCATABLE :: rowNodeValue(:), &
                          colNodeValue(:)
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
spaceCompo(1:1) = colNode%GetSpaceCompo(tPhysicalVars=1)

! Make rowNode from colNode, which will have brokenSpace true for fedof

! Step-1: make nowNodeFedof
CALL rowNodeFedof%InitiateBrokenLagrangeFedofForVTK(from=colNodeFedof)

! Step-2: make rowNode
CALL rowNode%Initiate(name="rowNode", engine=colNode%GetEngineName(), &
                      fieldType=TypeFieldOpt%normal, &
                      spaceCompo=spaceCompo(1:1), &
                      fedof=rowNodeFedof, geofedof=geofedof)

! Initiate from the row field rowNode and the column field colNode
CALL obj%Initiate(rowObj=rowNode, colObj=colNode)

CALL obj%SetBrokenLagrangeMatrixForVTK(ivar=math%one_i, &
                                       jvar=math%one_i, &
                                       nCopy=spaceCompo(1))

! Below we will set element matrices to obj
mesh => dom%GetMeshPointer()
tElements = mesh%GetTotalElements()
CALL Reallocate(rowFecon, colNodeFedof%GetMaxTotalConnectivity())
CALL Reallocate(colFecon, rowNodeFedof%GetMaxTotalConnectivity())
CALL Reallocate(colNodeValue, SIZE(colFecon))
CALL Reallocate(rowNodeValue, SIZE(rowFecon))

elemLoop: DO iel = 1, tElements
  isok = mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE elemLoop

  CALL colNodeFedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => colNodeFedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL colNodeFedof%GetConnectivity_(ans=colFecon, &
                                     tsize=colFecon_size, opt="A", &
                                     globalElement=iel, islocal=math%yes)
  CALL feptr%FixOrientOfNptrs(colFecon, colFecon_size)

  DO jj = 1, spaceCompo(1)
    CALL RANDOM_NUMBER(colNodeValue(1:colFecon_size))
    CALL colNode%Set(globalNode=colFecon(1:colFecon_size), &
                     islocal=math%yes, &
                     VALUE=colNodeValue(1:colFecon_size), &
                     spaceCompo=jj)
  END DO
END DO elemLoop

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

  DO jj = 1, spaceCompo(1)
    CALL colNode%Get(globalNode=colFecon(1:colFecon_size), &
                     islocal=math%yes, &
                     VALUE=colNodeValue, tsize=ii, spaceCompo=jj)

    CALL rowNode%Get(globalNode=rowFecon(1:rowFecon_size), &
                     islocal=math%yes, &
                     VALUE=rowNodeValue, tsize=ii, spaceCompo=jj)

    CALL OK(ALL(SOFTEQ(colNodeValue(1:ii), rowNodeValue(1:ii), tol=tol)), &
            "identity case: ")
  END DO

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
