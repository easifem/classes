SUBMODULE( FacetElement_Class ) ShapeData
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     get_elemsd_H1_Lagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elemsd_H1_Lagrange

  INTEGER( I4B ) :: nsd
  INTEGER( I4B ), ALLOCATABLE :: fln( : )
  REAL( DFP ), ALLOCATABLE :: ncf( :, : )
  REAL( DFP ), ALLOCATABLE :: clc( :, : )
  TYPE( QuadraturePoint_ ) :: cqp
  INTEGER( I4B ) :: nips, i
  TYPE( ElemShapeData_ ) :: CellElemSD
  REAL( DFP ) :: vec( 3, 3 )
  INTEGER( I4B ) :: xidim


  nsd = Obj % RefElem % NSD

  !<--- Quadrature point for facet element are given in Quad

  !<--- Setting up local shape data for facet element
  CALL Initiate( Obj = ElemSD, Quad = Quad, RefElem = Obj % RefElem, &
    & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )

  !<--- To set the Global data in ElemSD we need xiJ for facet element
  !<--- fln : facet_local_nptrs

  fln = Obj % FacetLocalNptrs( )

  !<-- ncf : nodal coord of facet
  ncf = xiJ( 1:nsd, fln )

  !<--- Computing global data of elemsd
  CALL setvalue( obj = elemsd, val = ncf, N=elemsd%N, dNdXi=elemsd%dNdXi)

  !<--- The above function will set dNdXt = 0
  !<--- dNdXt will be computed from the cell
  !<--- getting facet local node coord
  !<--- clc : Cell local coordinates
  clc = LocalNodeCoord( Obj % Cell % RefElem )

  !<--- cqp : cell quadrature points
  !<--- getting quad points (QP) for cell corresponding to facet QP
  !<--- we will achieve this by interpolating facet local coordinates, and
  !<--- store them inside cqp % Points whose last rows
  !<--- contains weights which are also set to zero as we dont need them.
  nips = SIZE( Quad, 2 )
  CALL initiate( Obj = cqp, tXi = nsd, tPoints = nips )
  cqp % Points ( 1 : nsd, 1 : nips ) = Interpolation(elemsd, clc( 1:nsd,fln))

  !<--- cellelemsd : element shape data for cell
  !<--- generating local shape data in cell at quad point cqp
  CALL Initiate( Obj = CellElemSD, Quad = cqp, &
    & RefElem = Obj % Cell % RefElem, &
    & ContinuityType= typeH1, &
    & InterpolType = TypeLagrangeInterpolation )
  CALL setValue( obj=CellElemSD, val=xiJ, N=cellElemSD%N, &
    & dNdXi=cellElemSD%dNdXi )


  !<--- set the value of cellelemsd % dndxt into the elemsd
  !<--- in addition, make the unit outward normal
  xidim = Obj % RefElem % XiDimension
  vec = 0.0_DFP; vec( 3, 2 ) = 1.0_DFP
  DO i = 1, nips
    ElemSD % dNdXt( :, :, i ) = CellElemSD % dNdXt( fln, :, i )
    Vec( 1:nsd, 1:xidim ) = ElemSD % Jacobian( 1:nsd, 1:xidim, i )
    ElemSD % Normal( :, i ) = &
      & VectorProduct( Vec(:, 1), Vec(:, 2) ) / ElemSD % Js(i)
  END DO

  CALL DeallocateData( CellElemSD )
  CALL DeallocateData( cqp )
  DEALLOCATE( clc, fln, ncf )

END PROCEDURE get_elemsd_H1_Lagrange

END SUBMODULE ShapeData