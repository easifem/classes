 #!/bin/bash

cmake -DCMAKE_INSTALL_PREFIX=${EASIFEM_CLASSES} -S ./ -B ./build
cmake --build ./build --target install