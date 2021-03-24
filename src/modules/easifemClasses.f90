MODULE easifemClasses
  USE FE
  USE Mesh_Class
  USE MeshData_Class
  USE MeshConnectivity_Class
  USE Domain_Class
  USE QuadratureVariables_Class
  USE gmsh_class
  USE gmshMesh_Class
  USE vtkType
  USE LinSolver_Class

  USE Material_Class
  USE stdMaterials
  USE ThermCondModel_Class
  USE VolHeatCapModel_Class
  USE SFCCModel_Class

  USE mod_functional
  USE ogpf

END MODULE easifemClasses