MODULE StdMaterials
  USE GlobalData
  USE Material_Class

#include "./water.inc"
#include "./ice.inc"
#include "./air.inc"
#include "./mineral.inc"


CONTAINS

#include "./waterfunctions.inc"
#include "./icefunctions.inc"
#include "./airfunctions.inc"
#include "./mineralfunctions.inc"

END MODULE StdMaterials