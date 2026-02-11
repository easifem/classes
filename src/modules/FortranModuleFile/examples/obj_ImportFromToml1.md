---
date: 2025-07-12
summary:  Import data from toml table

---
 
# obj_ImportFromToml1
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_ImportFromToml1(obj, table, elemType, nsd)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(toml_table), INTENT(INOUT) :: table

INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
!! element type of finite element, optional
INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
!! Number of spatial dimension, optional
END SUBROUTINE obj_ImportFromToml1
END INTERFACE
```
 
