---
project: easifemClasses
summary: easifemClasses is part of easifem library, which is a platform for Expandable And Scalable Infrastructure for Finite Element Methods.
project_download: https://github.com/easifem/classes
project_github: https://github.com/easifem/classes
project_website: https://www.easifem.com
license: gfdl
src_dir: ../src/modules/H1FE/
         ../src/modules/AbstractOneDimFE/
         ../src/modules/BasisOpt/
         ../src/modules/AbstractFE/
         ../src/modules/FEDOF/
         ../src/modules/AbstractField/
         ../src/modules/AbstractNodeField/
         ../src/modules/ScalarField/
         ../src/modules/VectorField/
         ../src/modules/AbstractFile/
         ../src/modules/HDF5File/
         ../src/modules/VtkHDF5File/
         ../src/modules/UserFunction/
         ../src/modules/AbstractMesh/
         ../src/modules/FEMesh/
         ../src/modules/AbstractDomain/
         ../src/modules/FEDomain/
         ../src/modules/VTKMeshUtility/
         ../src/modules/AbstractMeshField/
         ../src/modules/MeshField/
         ../src/modules/ScalarFieldLis/
media_dir: ../docs/media
page_dir: ../docs/pages
output_dir: /tmp/ford/classes
exclude_dir:  ../src/submodules/
exclude: **/NodeDataList_Class.F90
         **/EdgeDataList_Class.F90
         **/ElemDataList_Class.F90
         **/FaceDataList_Class.F90
author: Vikas Sharma
author_description: Graduate School of Agriculture, Kyoto University, Kyoto, Japan
email: vickysharma0812@gmail.com
github: https://vickysharma0812.github.io/
author_pic: ../docs/media/vikas.png
twitter: https://twitter.com/vickysharma0812
website: http://vikas.easifem.com
graph: false
source: false
display: public
         protected
         private
sort: alpha
coloured_edges: true
extra_filetypes:  inc !
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
predocmark_alt: >
predocmark: <
docmark_alt: *
docmark: !
fpp_extensions: F90
preprocess: true
---
