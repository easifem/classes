---
date: 2025-10-14
summary: Set isotropic order

---
 
# Introduction

 This routine is needed for Lagrage finite element

 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetIsotropicOrder(obj, order, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B), OPTIONAL, INTENT(IN) :: order

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck

END SUBROUTINE obj_SetIsotropicOrder
END INTERFACE
```
 
