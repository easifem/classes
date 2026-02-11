---
date: 2025-10-14
summary: Set isotropic order

---
 
# Introduction

 This routine is needed for Lagrange finite element

 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetAnisotropicOrder(obj, anisoOrder, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck

END SUBROUTINE obj_SetAnisotropicOrder
END INTERFACE
```
 
