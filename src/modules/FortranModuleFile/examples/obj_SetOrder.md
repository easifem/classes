---
date: 2025-10-13
summary:  Set the order and orientation in finite element

---
 
# obj_SetOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetOrder( &
obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, &
faceOrient, edgeOrient, tcell, tface, tedge, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! abstract finite element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
!! order
INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
!! aniso tropic order, necessary for Lagrange interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
!! cell order, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
!! face order, necessary for Hierarchical interpolation
!! number of rows in faceOrder is 3
!! number of columns in faceOrder is tfaceorder
INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
!! edge order, necessary for Hierarchical interpolation
!! size of edgeorder is tedgeorder
INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
!! cell orient, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
!! face orient, necessary for Hierarchical interpolation
!! number of rows in faceoriient is 3
!! number of columns in faceorient is tfaceorient
INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
!! edge orient, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
!! size of cellOrder, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
!! number of columns in faceOrder,
!! necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
!! size of edgeorder, necessary for Hierarchical interpolation
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
!! user can ignore this option
!! for dev: this option checks the errors in debug mode
END SUBROUTINE obj_SetOrder
END INTERFACE
```
 
