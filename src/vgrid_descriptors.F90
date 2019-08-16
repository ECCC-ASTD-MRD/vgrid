! * libdescrip - Vertical grid descriptor library for FORTRAN programming
! * Copyright (C) 2016  Direction du developpement des previsions nationales
! *                     Centre meteorologique canadien
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.

module vGrid_Descriptors

   ! Autor of C version : Andre Plante
   !                      Based on fortran version from:
   !                      Ron MacTaggart-Cowan, Andre Plante and Cecilien Charette

   ! Naming convention
   !
   ! var    may y a float or an integer
   ! var_8  is of type real(kind=8)
   ! var_L  is of type logical
   ! var_CP is of type type(c_ptr) from iso_c_binding

   use iso_c_binding, only : c_ptr, C_NULL_PTR, C_CHAR, C_NULL_CHAR, c_int, C_FLOAT, c_associated, c_loc

   implicit none
   private

   ! Public methods

   ! Public class constants

   ! Private class variables
   logical :: ALLOW_RESHAPE=.false.              ! Allow reshape of class pointer members
   integer, parameter :: KEY_LENGTH=4            !length of key string considered for get/put operations
   character(len=1), dimension(3), parameter :: MATCH_GRTYP=(/'X','Y','Z'/) !grid types with ip1,2 to ig1,2 mapping
   real(C_FLOAT), public, protected, bind(C, name="VGD_STDA76_SFC_T") :: &
        VGD_STDA76_SFC_T = 288.15
   real(C_FLOAT), public, protected, bind(C, name="VGD_STDA76_SFC_P") :: &
        VGD_STDA76_SFC_P = 101325

end module vGrid_Descriptors
