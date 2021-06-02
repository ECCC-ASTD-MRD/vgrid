cmake_minimum_required(VERSION 3.14)

project(vgrid C Fortran)

add_library(vgrid STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
