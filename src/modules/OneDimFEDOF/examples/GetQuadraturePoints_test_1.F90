!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary:  Testing GetQuadraturePoints method of FEDOF class
! obj_GetQuadraturePoints1(obj, quad, globalElement, quadratureType, &
! order, alpha, beta, lambda, islocal)

PROGRAM main
USE FEDOF_Class
USE FEDomain_Class
USE AbstractMesh_Class
USE HDF5File_Class
USE Display_Method
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE BaseType, ONLY: QuadraturePoint_, TypeQuadratureOpt
USE QuadraturePoint_Method

IMPLICIT NONE

TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom

CLASS(AbstractMesh_), POINTER :: meshptr => NULL()

CHARACTER(*), PARAMETER :: &
  filename = "../../FEMesh/examples/meshdata/small_tri3_mesh.h5", &
  baseContinuity = "H1", &
  baseInterpolation = "Hierarchical", &
  testname = baseContinuity//" "//baseInterpolation// " GetQuadraturePoints test:"

TYPE(HDF5File_) :: meshfile
TYPE(QuadraturePoint_) :: quad

INTEGER(I4B) :: globalElement, telements, order

CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
meshptr => dom%GetMeshPointer()

CALL test1
CALL test2
CALL test3

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CONTAINS

!----------------------------------------------------------------------------
!                                                                      test1
!----------------------------------------------------------------------------

SUBROUTINE test1

  order = 1
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=meshptr, &
                    ipType=TypeQuadratureOpt%equidistance)

  telements = 1
  globalElement = 1
  order = 2

  CALL obj%GetQuadraturePoints(quad=quad, globalElement=globalElement, &
                             quadratureType=TypeQuadratureOpt%GaussLegendre, &
                               order=order, islocal=.TRUE.)

  CALL Display(quad, testname//" quad: ")

END SUBROUTINE test1

!----------------------------------------------------------------------------
!                                                                      test2
!----------------------------------------------------------------------------

SUBROUTINE test2

  order = 2
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=meshptr, &
                    ipType=TypeQuadratureOpt%equidistance)

  telements = 1
  globalElement = 1
  order = 2

  CALL obj%GetQuadraturePoints(quad=quad, globalElement=globalElement, &
                             quadratureType=TypeQuadratureOpt%GaussLegendre, &
                               order=order, islocal=.TRUE.)

  CALL Display(quad, testname//" quad: ")

END SUBROUTINE test2

!----------------------------------------------------------------------------
!                                                                     test3
!----------------------------------------------------------------------------

SUBROUTINE test3

  order = 2
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, mesh=meshptr, &
                    ipType=TypeQuadratureOpt%equidistance)

  telements = 1
  globalElement = 1
  order = 2

  CALL obj%GetQuadraturePoints(quad=quad, globalElement=globalElement, &
                             quadratureType=TypeQuadratureOpt%GaussLegendre, &
                               order=order, islocal=.TRUE.)

  CALL Display(quad, testname //" quad: ")

END SUBROUTINE test3

END PROGRAM main

! total nodes = 25
! total elements = 16
! total faces = 40
! total edges = 0
