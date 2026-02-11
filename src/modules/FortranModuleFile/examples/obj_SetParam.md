---
date: 27 Aug 2022
summary: Set the parameters

---
 
# obj_SetParam
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetParam( &
obj, nsd, xidim, order, anisoOrder, edgeOrder, faceOrder, cellOrder, &
fetype, elemType, topoType, elemIndx, ipType, basisType, alpha, beta, &
lambda, dofType, transformType, refElemDomain, refelemCoord, &
baseContinuity, baseInterpolation, isIsotropicOrder, isAnisotropicOrder, &
isEdgeOrder, isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
quadratureIsHomogeneous, quadratureType, quadratureType1, &
quadratureType2, quadratureType3, quadratureOrder, quadratureOrder1, &
quadratureOrder2, quadratureOrder3, quadratureIsOrder, quadratureNips, &
quadratureNips1, quadratureNips2, quadratureNips3, quadratureIsNips, &
quadratureAlpha, quadratureAlpha1, quadratureAlpha2, quadratureAlpha3, &
quadratureBeta, quadratureBeta1, quadratureBeta2, quadratureBeta3, &
quadratureLambda, quadratureLambda1, quadratureLambda2, quadratureLambda3)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
!! Number of spatial dimension
INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
!! Xidimension of the element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
!! order of element (isotropic order)
INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
!! order in x, y, and z directions
INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
!! order of approximation on the edges of element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
!! order of approximation on the faces of element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
!! order of approximation in the cell of element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
!! finite element type
INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
!! Reference element type
INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
!! Topology of reference element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemIndx
!! Element index of topoType
INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
!! interpolation point type
INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
!! Basis type in x, y, and z directions
REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
!! Jacobi parameter
REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
!! Jacobi parameter
REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
!! Ultraspherical parameter
INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
!! degree of freedom type
INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
!! transformation type
CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
!! String name of type of continuity used for basis functions
CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
!! String name of type of interpolation used for basis functions
CHARACTER(*), OPTIONAL, INTENT(IN) :: refElemDomain
!! Domain of reference element
REAL(DFP), INTENT(IN), OPTIONAL :: refelemCoord(:, :)
!! Reference element coordinates
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isIsotropicOrder
!! True if isotropic order
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isAnisotropicOrder
!! True if anisoOrder
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isEdgeOrder

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFaceOrder

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isCellOrder

INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdgeOrder

INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFaceOrder

INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCellOrder

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
!! is quadratur homogeneous in all dimensions
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
!! Quadrature type in x, y, and z directions
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
!! Quadrature type in x, y, and z directions
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
!! Quadrature type in x, y, and z directions
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
!! Quadrature type in x, y, and z directions
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
!! quadrature accuracy in x, y, and z direction
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder1
!! quadrature accuracy in x, y, and z direction
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder2
!! quadrature accuracy in x, y, and z direction
INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder3
!! quadrature accuracy in x, y, and z direction
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
!! Is quadrature order considered
INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
!! Number of interpolation points in x, y, and z directions
INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips1
!! Number of interpolation points in x, y, and z directions
INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips2
!! Number of interpolation points in x, y, and z directions
INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips3
!! Number of interpolation points in x, y, and z directions
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
!! Should we consider nips
REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
!! Jacobi parameters for quadrature
REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha1
!! Jacobi parameters for quadrature
REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha2
!! Jacobi parameters for quadrature
REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha3
!! Jacobi parameters for quadrature
REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta1

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta2

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta3

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda1

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda2

REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda3

END SUBROUTINE obj_SetParam
END INTERFACE
```
 
