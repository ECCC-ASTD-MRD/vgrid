Vertical grids management library and tools

# At CMC

## Build dependencies

- CMake 3.20+
- librmn

## Environment

Load the right environment, depending on the architecture you need.  This
will load the specified compiler and its parameters, and set the
`EC_CMAKE_MODULE_PATH` variable for the `cmake_rpn` modules.

- Example for ppp7/sc7 and icelake specific architecture:

```
. r.load.dot mrd/rpn/code-tools/latest/env/rhel-9-graniterapids-64@inteloneapi-2025.1.0
```

- Example for generic architecture on ppp7/sc7

```
. r.load.dot mrd/rpn/code-tools/latest/env/rhel-9-amd64-64@inteloneapi-2025.1.0
```

- Example for GNU on any architecture:

```
. r.load.dot mrd/rpn/code-tools/latest/env/gnu
```

You also need a version of librmn, and optionally tdpack: either load them
from ssm, both are in rpn/libs domain (. r.load.dot rpn/libs/...), or provide
the path to cmake install directories if you compiled them.

## Build and install
## For <make check> this requires to load rpn/utils (editfst,fstcomp)
## and data at: ECCI_DATA_DIR/vgrid/1.9.b3/data_tests_res/vgrid_sample/{EC_ARCH}
## And listing of each test will be in build/Testing/Temporary/LastTest.log
## To isolate specific test(s), limit files under test/lib/src
## Note that the vgrid_sample tests uses 'diff' to compare
## If the diff fails, it will be shown above the line DIFF ERROR

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=[your_install_path] [-Drmn_ROOT=[librmn_install_path] -Dtdpack_ROOT=[tdpack_install_path]]
make -j 4
make check
make package
```

# Outside CMC (external users)

## Build dependencies

- CMake 3.20+
- librmn: https://github.com/ECCC-ASTD-MRD/librmn/
- tdpack: https://github.com/ECCC-ASTD-MRD/tdpack/

`cmake_rpn` is included as a git submodule.  Please clone with the
`--recurse --remote-submodules` options, or run `git submodule update --init
--remote` in the git repo after having cloned.

## Build and install

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=[your_install_path] -Drmn_ROOT=[librmn_install_path] -Dtdpack_ROOT=[tdpack_install_path]
make -j 4
make install
```
