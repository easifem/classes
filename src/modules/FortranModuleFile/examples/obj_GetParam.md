 
# obj_GetParam
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_GetParam( &
obj, nsd, order, anisoOrder, edgeOrder, faceOrder, cellOrder, fetype, &
elemType, topoType, elemIndx, ipType, basisType, alpha, beta, lambda, &
dofType, transformType, refElemDomain, baseContinuity, &
baseInterpolation, isIsotropicOrder, isAnisotropicOrder, isEdgeOrder, &
isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
quadratureIsHomogeneous, quadratureType, quadratureOrder, &
quadratureNips, quadratureIsOrder, quadratureIsNips, quadratureAlpha, &
quadratureBeta, quadratureLambda)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
!! Number of spatial dimension
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
!! order of element (isotropic order)
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: anisoOrder(3)
!! order in x, y, and z directions
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: edgeOrder(:)
!! order of approximation on the edges of element
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: faceOrder(:, :)
!! order of approximation on the faces of element
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: cellOrder(:)
!! order of approximation in the cell of element
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fetype
!! finite element type
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemType
!! Reference element type
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoType
!! Get topology of element
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemIndx
!! Get the index of element
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
!! interpolation point type
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType(:)
!! Basis type in x, y, and z directions
REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha(:)
!! Jacobi parameter
REAL(DFP), OPTIONAL, INTENT(OUT) :: beta(:)
!! Jacobi parameter
REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda(:)
!! Ultraspherical parameter
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: dofType(:)
!! degree of freedom type
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: transformType
!! transformation type
CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseContinuity
!! String name of type of continuity used for basis functions
CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseInterpolation
!! String name of type of interpolation used for basis functions
CHARACTER(*), OPTIONAL, INTENT(OUT) :: refElemDomain
!! Domain of reference element
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isIsotropicOrder
!! True if isotropic order
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isAnisotropicOrder
!! True if anisoOrder
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isEdgeOrder
!! is edge order set
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isFaceOrder
!! is face order set
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isCellOrder
!! is cell order set
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEdgeOrder
!! total edge order
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tFaceOrder
!! total face order
INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tCellOrder
!! total cell order
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsHomogeneous
!! see QuadratureOpt_
INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureType(:)
!! see QuadratureOpt_
INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureOrder(:)
!! See QuadratureOpt_
INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureNips(:)
!! See QuadratureOpt_
REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureAlpha(:)
!! See QuadratureOpt_
REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureBeta(:)
!! See QuadratureOpt_
REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureLambda(:)
!! See QuadratureOpt_
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsOrder
!! See QuadratureOpt_
LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsNips
!! See QuadratureOpt_
END SUBROUTINE obj_GetParam
END INTERFACE
```
 
