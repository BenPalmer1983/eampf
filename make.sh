# Make dirs
mkdir -p bin
mkdir -p mod

# Build
mpif90 -g \
-fbounds-check \
-mtune=native \
-fopenmp \
-O3 \
-ffast-math \
src/kinds/kinds_mod.f90 \
src/standard/standard_mod.f90 \
src/version/version_mod.f90 \
src/banner/banner_mod.f90 \
src/settings/settings_mod.f90 \
src/labels/labels_mod.f90 \
src/configs/config_mod.f90 \
src/configs/configs_mod.f90 \
src/eampa/eampa_mod.f90 \
src/main/main.f90 \
-J mod \
-o bin/eampa.x

