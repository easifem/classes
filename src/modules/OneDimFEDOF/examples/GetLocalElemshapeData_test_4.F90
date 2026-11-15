!> author: Vikas Sharma, Ph. D.
! date: 2025-06-09
! summary:  This program tests the GetLocalElemshapeData
! method of the FEDOF class.

PROGRAM main
USE FEDOF_Class
USE FEDomain_Class
USE AbstractMesh_Class
USE HDF5File_Class
USE Display_Method
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType, ONLY: QuadraturePoint_, TypeQuadratureOpt, ElemshapeData_
USE QuadraturePoint_Method
USE ElemshapeData_Method
USE ApproxUtility

IMPLICIT NONE

TYPE(FEDOF_) :: obj, geoobj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
CHARACTER(*), PARAMETER :: &
  filename = "../../FEMesh/examples/meshdata/small_tri6_mesh.h5", &
  baseContinuity = "H1", &
  baseInterpolation = "Lagrange", &
  testname = baseContinuity//" "//baseInterpolation//" GetLocalElemshapeData "

TYPE(HDF5File_) :: meshfile

TYPE(QuadraturePoint_) :: quad

TYPE(ElemshapeData_) :: elemsd, geoelemsd

INTEGER(I4B) :: globalElement, telements, order, &
                nodecon(100), ii, jj, nsd

REAL(DFP) :: nodecoord(3, 100)

LOGICAL(LGT) :: islocal, isok

CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
meshptr => dom%GetMeshPointer()
nsd = meshptr%GetNSD()

CALL test1
CALL test2
CALL test3

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CONTAINS

!----------------------------------------------------------------------------
!                                                                     test1
!----------------------------------------------------------------------------

SUBROUTINE test1

  REAL(DFP) :: found(1), want(1)

  order = 1
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=meshptr, &
                    ipType=TypeQuadratureOpt%equidistance)

  globalElement = 1; islocal = .TRUE.
  CALL obj%GetQuadraturePoints(quad=quad, globalElement=globalElement, &
                             quadratureType=TypeQuadratureOpt%GaussLegendre, &
                               order=order, islocal=islocal)

  CALL obj%GetLocalElemshapeData(quad=quad, elemsd=elemsd, &
                                 globalElement=globalElement, islocal=islocal)

  CALL meshptr%GetNodeCoord(nodecoord=nodecoord, nrow=ii, ncol=jj, &
                            globalElement=globalElement, islocal=islocal)

  CALL obj%GetGlobalElemshapeData(elemsd=elemsd, xij=nodecoord(1:nsd, 1:jj), &
                                 globalElement=globalElement, islocal=islocal)

  found = SUM(elemsd%N, dim=1)
  want = 1.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname)

  found(1) = SUM(elemsd%dNdXi(:, 1, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname)

  found(1) = SUM(elemsd%dNdXi(:, 2, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname)

  found(1) = SUM(elemsd%dNdXt(:, 1, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname)

  found(1) = SUM(elemsd%dNdXt(:, 2, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname)

END SUBROUTINE test1

!----------------------------------------------------------------------------
!                                                                     test2
!----------------------------------------------------------------------------

SUBROUTINE test2

  REAL(DFP) :: found(1), want(1)

  order = 2
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=meshptr, &
                    ipType=TypeQuadratureOpt%equidistance)

  CALL geoobj%Initiate(baseContinuity="H1", &
                       baseInterpolation="Lagrange", &
                       order=1, mesh=meshptr, &
                       ipType=TypeQuadratureOpt%equidistance)

  globalElement = 1; islocal = .TRUE.
  CALL obj%GetQuadraturePoints(quad=quad, globalElement=globalElement, &
                             quadratureType=TypeQuadratureOpt%GaussLegendre, &
                               order=1, islocal=islocal)

  CALL obj%GetLocalElemshapeData(quad=quad, elemsd=elemsd, &
                                 globalElement=globalElement, islocal=islocal)

  CALL geoobj%GetLocalElemshapeData(quad=quad, elemsd=geoelemsd, &
                                 globalElement=globalElement, islocal=islocal)

  CALL meshptr%GetNodeCoord(nodecoord=nodecoord, nrow=ii, ncol=jj, &
                            globalElement=globalElement, islocal=islocal)

  CALL obj%GetGlobalElemshapeData(elemsd=elemsd, xij=nodecoord(1:nsd, 1:jj), &
                               globalElement=globalElement, islocal=islocal, &
                                  geoelemsd=geoelemsd)

  found = SUM(elemsd%N(:, :), dim=1)
  want = 1.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test2 elemsd%N")

  found(1) = SUM(elemsd%dNdXi(:, 1, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test2 elemsd%dNdXi")

  found(1) = SUM(elemsd%dNdXi(:, 2, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test2 elemsd%dNdXi")

  found(1) = SUM(elemsd%dNdXt(:, 1, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test2 elemsd%dNdXt")

  found(1) = SUM(elemsd%dNdXt(:, 2, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test2 elemsd%dNdXt")

END SUBROUTINE test2

!----------------------------------------------------------------------------
!                                                                    test3
!----------------------------------------------------------------------------

SUBROUTINE test3

  REAL(DFP) :: found(1), want(1)

  order = 3
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=meshptr, &
                    ipType=TypeQuadratureOpt%equidistance)

  CALL geoobj%Initiate(baseContinuity="H1", &
                       baseInterpolation="Lagrange", &
                       order=1, mesh=meshptr, &
                       ipType=TypeQuadratureOpt%equidistance)

  globalElement = 1; islocal = .TRUE.
  CALL obj%GetQuadraturePoints(quad=quad, globalElement=globalElement, &
                             quadratureType=TypeQuadratureOpt%GaussLegendre, &
                               order=1, islocal=islocal)

  CALL obj%GetLocalElemshapeData(quad=quad, elemsd=elemsd, &
                                 globalElement=globalElement, islocal=islocal)

  CALL geoobj%GetLocalElemshapeData(quad=quad, elemsd=geoelemsd, &
                                 globalElement=globalElement, islocal=islocal)

  CALL meshptr%GetNodeCoord(nodecoord=nodecoord, nrow=ii, ncol=jj, &
                            globalElement=globalElement, islocal=islocal)

  CALL obj%GetGlobalElemshapeData(elemsd=elemsd, xij=nodecoord(1:nsd, 1:jj), &
                               globalElement=globalElement, islocal=islocal, &
                                  geoelemsd=geoelemsd)

  found = SUM(elemsd%N(:, :), dim=1)
  want = 1.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test3 elemsd%N")

  found(1) = SUM(elemsd%dNdXi(:, 1, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test3 elemsd%dNdXi")

  found(1) = SUM(elemsd%dNdXi(:, 2, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test3 elemsd%dNdXi")

  found(1) = SUM(elemsd%dNdXt(:, 1, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test3 elemsd%dNdXt")

  found(1) = SUM(elemsd%dNdXt(:, 2, 1), dim=1)
  want(1) = 0.0_DFP
  isok = found(1) .approxeq.want(1)
  CALL OK(isok, testname//" test3 elemsd%dNdXt")

END SUBROUTINE test3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END PROGRAM main

! total nodes = 25
! total elements = 16
! total faces = 40
! total edges = 0
