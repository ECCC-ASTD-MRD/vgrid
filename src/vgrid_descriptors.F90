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

   use iso_c_binding, only : c_ptr, C_NULL_PTR, C_CHAR, C_NULL_CHAR, c_int, C_FLOAT, c_associated, c_loc, c_f_pointer

   implicit none
   private

   ! Public methods
   public :: vgd_get                             !get instance variable value
   public :: vgd_put                             !set instance variable value
   public :: read_vgrid_from_file                !class constructor
   public :: vgd_create_from_ab_1001             !class constructor
   public :: vgd_create_from_ab_1002             !class constructor
   public :: vgd_create_from_ab_1003             !class constructor
   public :: vgd_create_from_ab_2001             !class constructor
   public :: vgd_create_from_ab_4001             !class constructor
   public :: vgd_create_from_ab_5001             !class constructor
   public :: vgd_create_from_ab_5002             !class constructor
   public :: vgd_create_from_ab_5003             !class constructor
   public :: vgd_create_from_ab_5004             !class constructor
   public :: vgd_create_from_ab_5005             !class constructor
   public :: vgd_create_from_ab_5100             !class constructor
   public :: vgd_create_from_ab_5999             !class constructor
   public :: vgd_create_from_ab_21001            !class constructor
   public :: vgd_create_from_ab_21002            !class constructor

   public :: vgd_create_from_hyb_1001            !class constructor
   public :: vgd_create_from_hyb_21001           !class constructor
   public :: vgd_create_from_hyb_21002           !class constructor

   public :: vgd_new                             !class constructor
   public :: vgd_getopt                          !get class variable value
   public :: vgd_putopt                          !set class variable value
   public :: vgd_print                           !dump plain-text contents of instance
   public :: vgd_write                           !write coordinates to a file
   public :: vgd_levels                          !compute physical level information
   public :: vgd_dpidpis                         !compute pressure derivative with respesct the sfc pressure
   public :: vgd_standard_atmosphere_1976   !Get standard atmosphere 1976 variable for a given coordinate
                                            ! Kept only for backward compatibility with vgrid 6.3
   public :: vgd_stda76                     !Get standard atmosphere 1976 variable for a given coordinate
   public :: vgd_stda76_pres_from_hgts_list ! Get standard atmosphere 1976 pressure in Pa from a height list in m
   public :: vgd_stda76_hgts_from_pres_list ! Get standard atmosphere 1976 heights in m from a pressure list in Pa

   ! Public class constants
#include "vgrid_descriptors.hf"

   ! Private class variables
   logical :: ALLOW_RESHAPE=.false.              ! Allow reshape of class pointer members
   integer, parameter :: KEY_LENGTH=4            !length of key string considered for get/put operations
   character(len=1), dimension(3), parameter :: MATCH_GRTYP=(/'X','Y','Z'/) !grid types with ip1,2 to ig1,2 mapping
   real(C_FLOAT), public, protected, bind(C, name="VGD_STDA76_SFC_T") :: &
        VGD_STDA76_SFC_T = 288.15
   real(C_FLOAT), public, protected, bind(C, name="VGD_STDA76_SFC_P") :: &
        VGD_STDA76_SFC_P = 101325

   type FSTD_ext
      sequence
      private
      integer :: ig1,ig2,ig3,ig4,dateo,deet,npas,datyp,nbits,ni,nj,nk
      integer :: ip1,ip2,ip3,swa,lng,dltf,ubc,extra1,extra2,extra3,datev
      character(len=4) :: grtyp
      character(len=4) :: typvar
      character(len=KEY_LENGTH) :: nomvar
      character(len=VGD_LEN_ETIK) :: etiket
   end type FSTD_ext
   
   interface

      integer(c_int) function f_diag_withref(vgdid, ni, nj, nk, ip1_list_CP, levels_CP,sfc_field_CP,sfc_field_ls_CP, in_log, dpidpis) bind(c, name='Cvgd_diag_withref_2ref')
        use iso_c_binding, only: c_ptr, c_int
        integer (c_int), value :: vgdid
        type(c_ptr), value :: ip1_list_CP, sfc_field_CP, sfc_field_ls_CP
        integer (c_int), value :: in_log, dpidpis
        type(c_ptr), value  :: levels_CP
        integer (c_int), value :: ni, nj, nk
      end function f_diag_withref
      
      integer(c_int) function f_diag_withref_8(vgdid, ni, nj, nk, ip1_list_CP, levels_CP,sfc_field_CP,sfc_field_ls_CP, in_log, dpidpis) bind(c, name='Cvgd_diag_withref_2ref_8')
         use iso_c_binding, only: c_ptr, c_int
         integer (c_int), value :: vgdid
         type(c_ptr), value :: ip1_list_CP, sfc_field_CP, sfc_field_ls_CP
         integer (c_int), value :: in_log, dpidpis
         type(c_ptr), value  :: levels_CP
         integer (c_int), value :: ni, nj, nk
      end function f_diag_withref_8

      integer(c_int) function f_get_int(vgdid, key, value_CP, quiet) bind(c, name='Cvgd_get_int')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr), value :: value_CP
         character(kind=c_char) :: key(*)
      end function f_get_int
     
      integer(c_int) function f_get_int_1d(vgdid, key, value_CP, nk_CP, quiet) bind(c, name='Cvgd_get_int_1d')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr) :: value_CP
         type(c_ptr), value :: nk_CP
         character(kind=c_char) :: key(*)
      end function f_get_int_1d
   
      integer(c_int) function f_getopt_int(key,value_CP,quiet) bind(c, name='Cvgd_getopt_int')
         use iso_c_binding, only: c_char, c_ptr, c_int
         character(kind=c_char) :: key(*)
         type(c_ptr), value :: value_CP
         integer (c_int), value :: quiet
      end function f_getopt_int

      integer(c_int) function f_get_real(vgdid, key, value_CP, quiet) bind(c, name='Cvgd_get_float')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr), value :: value_CP
         character(kind=c_char) :: key(*)
      end function f_get_real
      
      integer(c_int) function f_get_real_1d(vgdid, key, value_CP, nk_CP, quiet) bind(c, name='Cvgd_get_float_1d')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr) :: value_CP
         type(c_ptr), value :: nk_CP
         character(kind=c_char) :: key(*)
      end function f_get_real_1d

      integer(c_int) function f_get_real8(vgdid, key, value_CP, quiet) bind(c, name='Cvgd_get_double')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr), value :: value_CP
         character(kind=c_char) :: key(*)
      end function f_get_real8

      integer(c_int) function f_get_real8_1d(vgdid, key, value_CP, nk_CP, quiet) bind(c, name='Cvgd_get_double_1d')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr) :: value_CP
         type(c_ptr), value :: nk_CP
         character(kind=c_char) :: key(*)
      end function f_get_real8_1d

      integer(c_int) function f_get_real8_3d(vgdid, key, value_CP, ni_CP, nj_CP, nk_CP, quiet) bind(c, name='Cvgd_get_double_3d')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: quiet
         type(c_ptr) :: value_CP
         type(c_ptr), value :: ni_CP, nj_CP, nk_CP
         character(kind=c_char) :: key(*)
      end function f_get_real8_3d

      integer(c_int) function f_get_char(vgdid, key, my_char, quiet) bind(c, name='Cvgd_get_char')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer(c_int), value :: vgdid
         integer (c_int), value :: quiet
         character(kind=c_char) :: key(*)
         character(kind=c_char) :: my_char(*)
      end function f_get_char
      
      integer(c_int) function f_putopt_int(key, value) bind(c, name='Cvgd_putopt_int')
         use iso_c_binding, only: c_char, c_int
         integer (c_int), value :: value
         character(kind=c_char) :: key(*)
      end function f_putopt_int

      integer(c_int) function f_put_int(vgdid, key, value) bind(c, name='Cvgd_put_int')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: value
         character(kind=c_char) :: key(*)
      end function f_put_int

      integer(c_int) function f_put_char(vgdid, key, value) bind(c, name='Cvgd_put_char')
         use iso_c_binding, only: c_ptr, c_char, c_char, c_int
         integer(c_int), value :: vgdid
         character(kind=c_char) :: key(*), value(*)
      end function f_put_char

      integer(c_int) function f_is_valid(vgdid, valid_table_name) bind(c, name='Cvgd_is_valid')
         use iso_c_binding, only: c_ptr, c_char, c_int
         integer(c_int), value :: vgdid
         character(kind=c_char) :: valid_table_name(*)
       end function f_is_valid

      integer(c_int) function f_print_desc(vgdid, stdout, convip) bind(c, name='Cvgd_print_desc')
         use iso_c_binding, only : c_ptr, c_int
         integer (c_int), value :: vgdid
         integer (c_int), value :: stdout, convip
      end function f_print_desc

      integer(c_int) function f_print_vcode_description(vcode) bind(c, name='Cvgd_print_vcode_description')
         use iso_c_binding, only : c_int
         integer (c_int), value :: vcode
      end function f_print_vcode_description

      integer(c_int) function f_vgdcmp(vgdid1, vgdid2) bind(c, name='Cvgd_vgdcmp')
         use iso_c_binding, only: c_ptr, c_int
         integer (c_int), value :: vgdid1, vgdid2
      end function f_vgdcmp

      integer(c_int) function f_read_vgrid_from_file(vgdid,unit,ip1,ip2,kind,version) bind(c, name='Cvgd_read_vgrid_from_file')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr), value :: vgdid
         integer (c_int), value :: unit, ip1, ip2, kind, version
      end function f_read_vgrid_from_file

      integer(c_int) function f_create_from_ab_1001(vgdid, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m) bind(c, name='c_create_from_ab_1001')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_1001

      integer(c_int) function f_create_from_ab_1002(vgdid, ip1, ip2, ptop_8, a_m_8, &
                       b_m_8, ip1_m, nl_m) bind(c, name='c_create_from_ab_1002')
         use iso_c_binding, only : c_ptr, c_int, c_double
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: ptop_8
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_1002

      integer(c_int) function f_create_from_ab_1003(vgdid, ip1, ip2, ptop_8, pref_8, &
                    rcoef1, a_m_8, b_m_8, ip1_m, nl_m) bind(c, name='c_create_from_ab_1003')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: ptop_8, pref_8
         real (c_float),  value :: rcoef1
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_1003

      integer(c_int) function f_create_from_ab_2001(vgdid, ip1, ip2, a_m_8, b_m_8, &
                       ip1_m, nl_m) bind(c, name='c_create_from_ab_2001')
         use iso_c_binding, only : c_ptr, c_int, c_char
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_2001

      integer(c_int) function f_create_from_ab_4001(vgdid, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m) bind(c, name='c_create_from_ab_4001')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_4001

      integer(c_int) function f_create_from_ab_5001(vgdid, ip1, ip2, ptop_8, pref_8, &
                    rcoef1, a_m_8, b_m_8, ip1_m, nl_m) bind(c, name='c_create_from_ab_5001')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: ptop_8, pref_8
         real (c_float),  value :: rcoef1
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5001

      integer(c_int) function f_create_from_ab_5002(vgdid, ip1, ip2, ptop_8, pref_8, &
                                  rcoef1, rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, &
                                  ip1_m, ip1_t, nl_m) &
                                  bind(c, name='c_create_from_ab_5002')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: ptop_8, pref_8
         real (c_float),  value :: rcoef1, rcoef2
         type(c_ptr),     value :: a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5002

      integer(c_int) function f_create_from_ab_5003(vgdid, ip1, ip2, ptop_8, pref_8, &
                                     rcoef1, rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, &
                                     ip1_t, nl_m) &
                                     bind(c, name='c_create_from_ab_5003')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: ptop_8, pref_8
         real (c_float),  value :: rcoef1, rcoef2
         type(c_ptr),     value :: a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5003

      integer(c_int) function f_create_from_ab_5004(vgdid, ip1, ip2, ptop_8, pref_8, &
                                     rcoef1, rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, &
                                     ip1_t, nl_m) &
                                     bind(c, name='c_create_from_ab_5004')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: ptop_8, pref_8
         real (c_float),  value :: rcoef1, rcoef2
         type(c_ptr),     value :: a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5004

      integer(c_int) function f_create_from_ab_5005(vgdid, ip1, ip2, pref_8, rcoef1, &
                                     rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, &
                                     nl_m) bind(c, name='c_create_from_ab_5005')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: pref_8
         real (c_float),  value :: rcoef1, rcoef2
         type(c_ptr),     value :: a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5005

      integer(c_int) function f_create_from_ab_5100(vgdid, ip1, ip2, pref_8, rcoef1, &
                          rcoef2, rcoef3, rcoef4, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, &
                          c_t_8, ip1_m, ip1_t, nl_m) &
                          bind(c, name='c_create_from_ab_5100')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_double), value :: pref_8
         real (c_float),  value :: rcoef1, rcoef2, rcoef3, rcoef4
         type(c_ptr),     value :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, ip1_m, ip1_t
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5100

      integer(c_int) function f_create_from_ab_5999(vgdid, ip1, ip2, a_m_8, b_m_8, &
                          ip1_m, nl_m) bind(c, name='c_create_from_ab_5999')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         type(c_ptr),     value :: a_m_8, b_m_8, ip1_m
         integer (c_int), value :: nl_m
      end function f_create_from_ab_5999

      integer(c_int) function f_create_from_ab_21001(vgdid, ip1, ip2, &
			      rcoef1, rcoef2, rcoef3, rcoef4, &
			      a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, ip1_m, &
			      ip1_t, nl_m) bind(c, name='c_create_from_ab_21001')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_float),  value :: rcoef1, rcoef2, rcoef3, rcoef4
         type(c_ptr),     value :: a_m_8, b_m_8, c_m_8,a_t_8, b_t_8, c_t_8, ip1_m, ip1_t
         integer (c_int), value :: nl_m
      end function f_create_from_ab_21001

      integer(c_int) function f_create_from_ab_21002(vgdid, ip1, ip2, rcoef1, rcoef2, &
			      rcoef3, rcoef4, &
			      a_m_8, b_m_8, c_m_8, &
			      a_t_8, b_t_8, c_t_8, &
			      a_w_8, b_w_8, c_w_8, &
			      ip1_m, ip1_t, ip1_w, nl_m) &
                              bind(c, name='c_create_from_ab_21002')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid
         integer (c_int), value :: ip1, ip2
         real (c_float),  value :: rcoef1, rcoef2, rcoef3, rcoef4
         type(c_ptr),     value :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8
         type(c_ptr),     value :: a_w_8, b_w_8, c_w_8, ip1_m, ip1_t, ip1_w
         integer (c_int), value :: nl_m
      end function f_create_from_ab_21002

      integer(c_int) function f_create_from_hyb_1001(vgdid, hyb, size_hyb, ip1, ip2) &
                                                   bind(c, name='c_create_from_hyb_1001')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr),     value :: vgdid
         type(c_ptr),     value :: hyb
         integer (c_int), value :: size_hyb, ip1, ip2
      end function f_create_from_hyb_1001

      integer(c_int) function f_create_from_hyb_21001(vgdid, hyb, size_hyb, &
                              rcoef1, rcoef2, rcoef3, rcoef4, ip1, ip2, dhm, dht) &
                              bind(c, name='c_create_from_hyb_21001')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid, hyb, dhm, dht
         integer (c_int), value :: size_hyb
         integer (c_int), value :: ip1, ip2
         real (c_float),  value :: rcoef1, rcoef2, rcoef3, rcoef4
      end function f_create_from_hyb_21001

      integer(c_int) function f_create_from_hyb_21002(vgdid, hyb, size_hyb, &
                              rcoef1, rcoef2, rcoef3, rcoef4, ip1, ip2, dhm, dht, dhw) &
                              bind(c, name='c_create_from_hyb_21002')
         use iso_c_binding, only : c_ptr, c_int, c_double, c_float
         type(c_ptr),     value :: vgdid, hyb, dhm, dht, dhw
         integer (c_int), value :: size_hyb
         integer (c_int), value :: ip1, ip2
         real (c_float),  value :: rcoef1, rcoef2, rcoef3, rcoef4
      end function f_create_from_hyb_21002
      
      integer(c_int) function f_new_from_table(vgdid, table_CP, ni, nj, nk) bind(c, name='Cvgd_new_from_table')
         use iso_c_binding, only : c_ptr, c_int
         integer :: vgdid
         type(c_ptr), value :: table_CP
         integer (c_int), value :: ni, nj, nk
      end function f_new_from_table

      integer(c_int) function f_create_from_hyb(vgdid,kind,version,hyb_CP,size_hyb,rcoef1_CP,rcoef2_CP,rcoef3_CP,rcoef4_CP,ptop_8_CP,pref_8_CP,ptop_out_8_CP, &
           ip1,ip2,dhm_CP,dht_CP,dhw_CP,avg) bind(c, name='Cvgd_create_from_hyb2')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr), value :: vgdid
         integer (c_int), value :: kind, version, size_hyb
         type(c_ptr), value :: hyb_CP,rcoef1_CP,rcoef2_CP,rcoef3_CP,rcoef4_CP,ptop_8_CP,pref_8_CP,ptop_out_8_CP
         type(c_ptr), value :: dhm_CP,dht_CP,dhw_CP
         integer (c_int), value :: ip1,ip2,avg
      end function f_create_from_hyb
      
      integer(c_int) function f_create_from_ab(vgdid,kind,version,nk,ip1,ip2, &
           ptop_8_CP, pref_8_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, &
           a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, ip1_m_CP, ip1_t_CP, ip1_w_CP, nl_m, nl_t, nl_w) bind(c, name='Cvgd_create_from_ab2')
         use iso_c_binding, only : c_ptr, c_int
         integer :: vgdid
         integer (c_int), value :: kind,version,nk,ip1,ip2
         type(c_ptr), value :: ptop_8_CP, pref_8_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP
         type(c_ptr), value :: a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, ip1_m_CP, ip1_t_CP, ip1_w_CP
         integer (c_int), value :: nl_m, nl_t, nl_w
      end function f_create_from_ab
      
      subroutine f_table_shape(vgdid, tshape_CP) bind(c, name='Cvgd_table_shape')
         use iso_c_binding, only : c_ptr, c_int
         integer(c_int), value :: vgdid
         type(c_ptr) :: tshape_CP
      end subroutine f_table_shape
         
      integer(c_int) function f_write_desc(vgdid,unit) bind(c, name='Cvgd_write_desc')
         use iso_c_binding, only : c_ptr, c_int, c_char
         integer (c_int), value :: vgdid
         integer (c_int), value :: unit
      end function f_write_desc
      
      integer(c_int) function f_stda76_temp(vgdid, ip1s_CP, nl, temp_CP) bind(c, name='Cvgd_stda76_temp')
        use iso_c_binding, only : c_ptr, c_int
        integer(c_int), value :: vgdid
        type(c_ptr), value :: ip1s_CP
        integer (c_int), value :: nl
        type(c_ptr), value :: temp_CP
      end function f_stda76_temp

      integer(c_int) function f_stda76_pres(vgdid, ip1s_CP, nl, pres_CP, sfc_temp_CP, sfc_pres_CP) bind(c, name='Cvgd_stda76_pres')
        use iso_c_binding, only : c_ptr, c_int
        integer(c_int), value :: vgdid
        type(c_ptr), value :: ip1s_CP
        integer (c_int), value :: nl
        type(c_ptr), value :: pres_CP, sfc_temp_CP, sfc_pres_CP
      end function f_stda76_pres
      
      integer(c_int) function f_stda76_pres_from_hgts_list( &
           pres_CP, hgts_CP, nb) bind(c, name=&
           'Cvgd_stda76_pres_from_hgts_list')
        use iso_c_binding, only : c_ptr, c_int
        type(c_ptr), value :: pres_CP, hgts_CP
        integer (c_int), value :: nb
      end function f_stda76_pres_from_hgts_list

      integer(c_int) function f_stda76_hgts_from_pres_list( &
           hgts_CP, pres_CP, nb) bind(c, name=&
           'Cvgd_stda76_hgts_from_pres_list')
        use iso_c_binding, only : c_ptr, c_int
        type(c_ptr), value :: pres_CP, hgts_CP
        integer (c_int), value :: nb
      end function f_stda76_hgts_from_pres_list
   end interface
   
   interface vgd_new
      module procedure read_vgrid_from_file
      module procedure new_from_table
      module procedure vgd_create_from_ab
      module procedure vgd_create_from_hyb
   end interface vgd_new

   interface vgd_get
      module procedure get_int
      module procedure get_int_1d
      module procedure get_real
      module procedure get_real_1d
      module procedure get_real8
      module procedure get_real8_1d
      module procedure get_real8_3d
      module procedure get_char
      module procedure get_logical
   end interface vgd_get

   interface vgd_put
      module procedure put_int
      module procedure put_real8_3d
      module procedure put_char
   end interface vgd_put
   interface vgd_getopt
      module procedure getopt_logical
   end interface vgd_getopt
   
   interface vgd_putopt
      module procedure putopt_logical
   end interface vgd_putopt
   
   interface vgd_print
      module procedure print_desc
      module procedure print_vcode_description
   end interface vgd_print
   
   interface vgd_write
      module procedure write_desc
   end interface vgd_write

   interface vgd_levels
      module procedure levels_toplevel
      module procedure levels_readref
      module procedure levels_withref
      module procedure levels_withref_8
      module procedure levels_withref_prof
      module procedure levels_withref_prof_8
   end interface vgd_levels
   interface vgd_dpidpis
      module procedure dpidpis_withref
      module procedure dpidpis_withref_8
      module procedure dpidpis_withref_prof
      module procedure dpidpis_withref_prof_8
   end interface vgd_dpidpis
   
contains
   integer function read_vgrid_from_file(vgdid,unit,format,ip1,ip2,kind,version) result(status)
      use vgrid_utils, only: up
      ! Coordinate constructor - read from a file and initialize instance
      integer, target, intent(inout) :: vgdid     !Vertical descriptor id
      integer, intent(in) :: unit                 !File unit to read descriptor information from
      character(len=*), target, optional, intent(in) :: format !File format ('fst' or 'bin') default is 'fst'
      integer, target,optional :: ip1,ip2                 !ip1,2 values of the desired descriptors
      integer, target,optional, intent(in) :: kind        ! Level kind requested by user.
      integer, target,optional, intent(in) :: version     ! Level version requested by user.
      
      ! Local variables
      type(c_ptr) :: vgdid_p, table_8_cptr
      integer :: ni,nj,nk, istat, error, l_ip1, l_ip2, l_kind, l_version
      character(len=100) :: myformat
      real(kind=8), dimension(:,:,:), pointer :: table_8

      nullify(table_8)
      vgdid_p = c_loc(vgdid)

      status = VGD_ERROR

      myformat='FST'
      if (present(format)) myformat = trim(up(format))
      select case (trim(up(myformat)))
      case ('FST')      
         if(present(ip1))then
            l_ip1 = ip1
         else
            l_ip1 = -1
         endif
         if(present(ip2))then
            l_ip2 = ip2
         else
            l_ip2 = -1
         endif
         if(present(kind))then
            l_kind = kind
         else
            l_kind = -1
         endif
         if(present(version))then
            l_version = version
         else
            l_version = -1
         endif

         if( f_read_vgrid_from_file(vgdid_p, unit, l_ip1, l_ip2, l_kind, l_version) == VGD_ERROR )then
            print*,'(F_vgd) ERROR: In read_vgrid_from_file, problem with f_read_vgrid_from_file'
            return
         endif
      case ('BIN')
         read(unit) ni,nj,nk
         allocate(table_8(ni,nj,nk),stat=istat)
         if (istat /= 0) then
            write(for_msg,*) 'unable to allocate table_8'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
         read(unit) table_8
         print*,'table_8(1:3,1,1)',table_8(1:3,1,1)

         table_8_cptr = c_loc(table_8(1,1,1))
         error = f_new_from_table(vgdid,table_8_cptr,ni,nj,nk)
         if (error < 0) then
            write(for_msg,*) 'In new_read, problem creating record information'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
         
      case DEFAULT
         write(for_msg,*) 'invalid constructor format request ',trim(myformat)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      end select

      status = VGD_OK

    end function read_vgrid_from_file

    integer function vgd_create_from_ab_1001(vgdid, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m &
                                        ) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_1001(vgdid_p, ip1, ip2, a_m_8_p, b_m_8_p, ip1_m_p, nl_m &
                               )== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_1001'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_1001

    integer function vgd_create_from_ab_1002(vgdid, ip1, ip2, ptop_8, a_m_8, b_m_8, &
                                             ip1_m, nl_m) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: ptop_8
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_1002(vgdid_p, ip1, ip2, ptop_8, a_m_8_p, b_m_8_p, ip1_m_p, &
                               nl_m)== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_1002'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_1002

    integer function vgd_create_from_ab_1003(vgdid, ip1, ip2, ptop_8, pref_8, rcoef1, &
                                            a_m_8, b_m_8, ip1_m, nl_m) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: ptop_8, pref_8
      real :: rcoef1 
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_1003(vgdid_p, ip1, ip2, ptop_8, pref_8, rcoef1, a_m_8_p, &
                                b_m_8_p, ip1_m_p, nl_m)== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_1003'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_1003

    integer function vgd_create_from_ab_2001(vgdid, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m &
                                        ) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_2001(vgdid_p, ip1, ip2, a_m_8_p, b_m_8_p, ip1_m_p, nl_m &
                               )== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_2001'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_2001

    integer function vgd_create_from_ab_4001(vgdid, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m &
                                        ) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_4001(vgdid_p, ip1, ip2, a_m_8_p, b_m_8_p, ip1_m_p, nl_m &
                               )== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_4001'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_4001

    integer function vgd_create_from_ab_5001(vgdid, ip1, ip2, ptop_8, pref_8, rcoef1, &
                                             a_m_8, b_m_8, ip1_m, nl_m) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: ptop_8, pref_8
      real :: rcoef1 
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_5001(vgdid_p, ip1, ip2, ptop_8, pref_8, rcoef1, a_m_8_p, &
                                b_m_8_p, ip1_m_p, nl_m)== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5001'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5001

    integer function vgd_create_from_ab_5002(vgdid, ip1, ip2, ptop_8, pref_8, &
           rcoef1, rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m) &
           result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: ptop_8, pref_8
      real :: rcoef1, rcoef2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, a_t_8, b_t_8
      integer, dimension(:), pointer :: ip1_m, ip1_t
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)

      status = VGD_ERROR

      if( f_create_from_ab_5002(vgdid_p, ip1, ip2, ptop_8, pref_8, rcoef1, rcoef2, &
                                a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p, &
                                nl_m)== VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5002'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5002

    integer function vgd_create_from_ab_5003(vgdid, ip1, ip2, ptop_8, pref_8, rcoef1, &
                               rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m) &
                               result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: ptop_8, pref_8
      real :: rcoef1, rcoef2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, a_t_8, b_t_8
      
      integer, dimension(:), pointer :: ip1_m, ip1_t
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)

      status = VGD_ERROR

      if( f_create_from_ab_5003(vgdid_p, ip1, ip2, ptop_8, pref_8, rcoef1, rcoef2, &
           a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p, &
           nl_m)== VGD_ERROR &
                               )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5003'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5003

    integer function vgd_create_from_ab_5004(vgdid, ip1, ip2, ptop_8, pref_8, rcoef1, &
                               rcoef2, a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m) &
                               result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: ptop_8, pref_8
      real :: rcoef1, rcoef2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, a_t_8, b_t_8
      integer, dimension(:), pointer :: ip1_m, ip1_t
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)

      status = VGD_ERROR

      if( f_create_from_ab_5004(vgdid_p, ip1, ip2, ptop_8, pref_8, rcoef1, rcoef2, &
                                a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p, &
                                nl_m)== VGD_ERROR &
                               )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5004'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5004

    integer function vgd_create_from_ab_5005(vgdid, ip1, ip2, pref_8, rcoef1, rcoef2, &
                           a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: pref_8
      real :: rcoef1, rcoef2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, a_t_8, b_t_8
      integer, dimension(:), pointer :: ip1_m, ip1_t
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)

      status = VGD_ERROR

      if( f_create_from_ab_5005(vgdid_p, ip1, ip2, pref_8, rcoef1, rcoef2, a_m_8_p, &
                                b_m_8_p, a_t_8_p, b_t_8_p, ip1_m_p, ip1_t_p, nl_m)== VGD_ERROR &
                               )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5005'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5005

    integer function vgd_create_from_ab_5100(vgdid, ip1, ip2, pref_8, rcoef1, &
                          rcoef2, rcoef3, rcoef4, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, &
                          c_t_8, ip1_m, ip1_t, nl_m) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8) :: pref_8
      real :: rcoef1, rcoef2, rcoef3, rcoef4
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8
      integer, dimension(:), pointer :: ip1_m, ip1_t
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, c_m_8_p, a_t_8_p, b_t_8_p, c_t_8_p
      type(c_ptr) :: ip1_m_p, ip1_t_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      c_m_8_p = c_loc(c_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      c_t_8_p = c_loc(c_t_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)

      status = VGD_ERROR

      if( f_create_from_ab_5100(vgdid_p, ip1, ip2, pref_8, rcoef1, rcoef2, rcoef3, &
                                rcoef4, a_m_8_p, b_m_8_p, c_m_8_p, a_t_8_p, b_t_8_p, &
                                c_t_8_p, ip1_m_p, ip1_t_p, nl_m)== VGD_ERROR &
                               )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5100'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5100

    integer function vgd_create_from_ab_5999(vgdid, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m) &
                                        result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
      integer, dimension(:), pointer :: ip1_m
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, ip1_m_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      ip1_m_p = c_loc(ip1_m)

      status = VGD_ERROR

      if( f_create_from_ab_5999(vgdid_p, ip1, ip2, a_m_8_p, b_m_8_p, ip1_m_p, nl_m) &
                               == VGD_ERROR )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_5999'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_5999

    integer function vgd_create_from_ab_21001(vgdid, ip1, ip2, rcoef1, &
                          rcoef2, rcoef3, rcoef4, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, &
                          c_t_8, ip1_m, ip1_t, nl_m) result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real :: rcoef1, rcoef2, rcoef3, rcoef4
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8
      integer, dimension(:), pointer :: ip1_m, ip1_t
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, c_m_8_p, a_t_8_p, b_t_8_p, c_t_8_p
      type(c_ptr) :: ip1_m_p, ip1_t_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      c_m_8_p = c_loc(c_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      c_t_8_p = c_loc(c_t_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)

      status = VGD_ERROR

      if( f_create_from_ab_21001(vgdid_p, ip1, ip2, rcoef1, rcoef2, rcoef3, &
                                rcoef4, a_m_8_p, b_m_8_p, c_m_8_p, a_t_8_p, b_t_8_p, &
                                c_t_8_p, ip1_m_p, ip1_t_p, nl_m)== VGD_ERROR &
                               )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_21001'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_21001

    integer function vgd_create_from_ab_21002(vgdid, ip1, ip2, rcoef1, &
                          rcoef2, rcoef3, rcoef4, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, &
                          c_t_8, a_w_8, b_w_8, c_w_8, ip1_m, ip1_t, ip1_w, nl_m) &
                          result(status)
      integer, target :: vgdid
      integer :: ip1, ip2
      real :: rcoef1, rcoef2, rcoef3, rcoef4
      real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8
      real(kind=8), dimension(:), pointer :: a_w_8, b_w_8, c_w_8
      integer, dimension(:), pointer :: ip1_m, ip1_t, ip1_w
      integer :: nl_m

      type(c_ptr) :: vgdid_p, a_m_8_p, b_m_8_p, c_m_8_p, a_t_8_p, b_t_8_p, c_t_8_p
      type(c_ptr) :: a_w_8_p, b_w_8_p, c_w_8_p, ip1_m_p, ip1_t_p, ip1_w_p
      vgdid_p = c_loc(vgdid)
      a_m_8_p = c_loc(a_m_8)
      b_m_8_p = c_loc(b_m_8)
      c_m_8_p = c_loc(c_m_8)
      a_t_8_p = c_loc(a_t_8)
      b_t_8_p = c_loc(b_t_8)
      c_t_8_p = c_loc(c_t_8)
      a_w_8_p = c_loc(a_w_8)
      b_w_8_p = c_loc(b_w_8)
      c_w_8_p = c_loc(c_w_8)
      ip1_m_p = c_loc(ip1_m)
      ip1_t_p = c_loc(ip1_t)
      ip1_w_p = c_loc(ip1_w)

      status = VGD_ERROR

      if( f_create_from_ab_21002(vgdid_p, ip1, ip2, rcoef1, rcoef2, &
		 	         rcoef3, rcoef4, &
			         a_m_8_p, b_m_8_p, c_m_8_p, &
			         a_t_8_p, b_t_8_p, c_t_8_p, &
			         a_w_8_p, b_w_8_p, c_w_8_p, &
			         ip1_m_p, ip1_t_p, ip1_w_p, nl_m)== VGD_ERROR &
                                )then
        print*,'(F_vgd) ERROR: In vgd_create_from_ab_21002'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_ab_21002

    integer function new_from_table(vgdid,table) result(status)
       ! Coordinate constructor - build vertical descriptor from table input
       ! Set internal vcode (if all above was successful)
       integer, target, intent(inout) :: vgdid !Vertical descriptor id
       real(kind=8), dimension(:,:,:), pointer :: table   !Raw table of vgrid records

       ! Local variables
       type(c_ptr) :: table_CP
       table_CP = c_loc(table(1,1,1))
       
       status = VGD_ERROR
       
       if ( f_new_from_table(vgdid, table_CP, size(table,dim=1), size(table,dim=2), size(table,dim=3)) == VGD_ERROR )then
         print*,'(F_vgd) ERROR: in new_from_table, problem with f_new_from_table'
         return
      endif      
      
      status = VGD_OK
      
    end function new_from_table

   integer function vgd_create_from_hyb(vgdid,kind,version,hyb,rcoef1,rcoef2,rcoef3,rcoef4,ptop_8,pref_8,ptop_out_8,ip1,ip2,stdout_unit,dhm,dht,dhw,avg_L) result(status)
      implicit none

      ! Coordinate constructor - build vertical descriptor from hybrid coordinate entries
      integer,target,intent(inout) :: vgdid         !Vertical descriptor id
      integer, intent(in) :: kind,version                  !Kind,version to create
      real, target, dimension(:),intent(in) :: hyb         !List of hybrid levels
      real, target, optional, intent(in) :: rcoef1,rcoef2,rcoef3,rcoef4 !R-coefficient values for rectification
      real(kind=8), target, optional, intent(in) :: ptop_8       !Top-level pressure (Pa) inout
      real(kind=8), target, optional, intent(out):: ptop_out_8   !Top-level pressure (Pa) output if ptop_8 < 0
      real(kind=8), target, optional, intent(in) :: pref_8       !Reference-level pressure (Pa)
      integer, target, optional, intent(in) :: ip1,ip2     !IP1,2 values for FST file record [0,0]
      integer, target, optional, intent(in) :: stdout_unit !Unit number for verbose output [STDERR]
      real, target, optional, intent(in) :: dhm,dht,dhw    !Diag levels Height for Momentum/Thermo/Vertical-Velocity vaiables
      logical, optional :: avg_L                           !Obtain thermo A, B and C by averaging momentum ones (Temporary for Vcode 5100)
                                                           !   If this option becoms permenant, some code will have to be added
                                                           !   to check this option for vcode 5100 only.
      ! Local variables
      type(c_ptr) :: hyb_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, ptop_8_CP, ptop_out_8_CP, pref_8_CP
      type(c_ptr) :: stdout_unit_CP, dhm_CP, dht_CP, dhw_CP
      integer :: my_ip1, my_ip2, my_avg
      type(c_ptr) :: vgdid_ptr

      hyb_CP = c_loc(hyb)

      status = VGD_ERROR;
      ! Assign optional argument to C_NULL_PTR
      if(present(rcoef1))then
         rcoef1_CP = c_loc(rcoef1)
      else
         rcoef1_CP = C_NULL_PTR
      endif
      if(present(rcoef2))then
         rcoef2_CP = c_loc(rcoef2)
      else
         rcoef2_CP = C_NULL_PTR
      endif
      if(present(rcoef3))then
         rcoef3_CP = c_loc(rcoef3)
      else
         rcoef3_CP = C_NULL_PTR
      endif
      if(present(rcoef4))then
         rcoef4_CP = c_loc(rcoef4)
      else
         rcoef4_CP = C_NULL_PTR
      endif
      if(present(ptop_8))then
         ptop_8_CP = c_loc(ptop_8)
      else
         ptop_8_CP = C_NULL_PTR
      endif
      if(present(ptop_out_8))then
         ptop_out_8_CP = c_loc(ptop_out_8)
      else
         ptop_out_8_CP = C_NULL_PTR
      endif
      if(present(pref_8))then
         pref_8_CP = c_loc(pref_8)
      else
         pref_8_CP = C_NULL_PTR
      endif
      if(present(ip1))then
         my_ip1 = ip1
      else
         my_ip1 = -1
      endif
      if(present(ip2))then
         my_ip2 = ip2
      else
         my_ip2 = -1
      endif
      if(present(stdout_unit))then
         write(for_msg,*) 'ERROR: in vgd_create_from_hyb, implement option stdout_unit'         
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
         stdout_unit_CP = c_loc(stdout_unit)
      else
         stdout_unit_CP = C_NULL_PTR
      endif
      if(present(dhm))then
         dhm_CP = c_loc(dhm)
      else
         dhm_CP = C_NULL_PTR
      endif
      if(present(dht))then
         dht_CP = c_loc(dht)
      else
         dht_CP = C_NULL_PTR
      endif
      if(present(dhw))then
         dhw_CP = c_loc(dhw)
      else
         dhw_CP = C_NULL_PTR
      endif
      my_avg=1
      if(present(avg_L))then
         if(avg_L)then
            my_avg=1
         else
            my_avg=0
         endif
      endif

      vgdid_ptr=c_loc(vgdid)
      if(f_create_from_hyb(vgdid_ptr,kind,version,hyb_CP,size(hyb),rcoef1_CP,rcoef2_CP,rcoef3_CP,rcoef4_CP,ptop_8_CP,pref_8_CP,ptop_out_8_CP,my_ip1,my_ip2,dhm_CP,dht_CP,dhw_CP,my_avg) == VGD_ERROR)then
         print*,'(F_vgd) ERROR in vgd_create_from_hyb, problem with f_create_from_hyb'
         return
      endif
      status = VGD_OK
    end function vgd_create_from_hyb

   integer function vgd_create_from_hyb_1001(vgdid, hyb, ip1, ip2) result(status)
      implicit none

      integer,target,intent(inout) :: vgdid         !Vertical descriptor id
      real, target, dimension(:),intent(in) :: hyb  !List of hybrid levels
      integer, optional, intent(in) :: ip1, ip2

      ! Local variables
      type(c_ptr) :: hyb_CP
      type(c_ptr) :: vgdid_p
      integer l_ip1,l_ip2
   
      if(present(ip1))then
         l_ip1 = ip1
      else
         l_ip1 = -1
      endif
      if(present(ip2))then
         l_ip2 = ip2
      else
         l_ip2 = -1
      endif

      hyb_CP = c_loc(hyb)

      status = VGD_ERROR;

      vgdid_p=c_loc(vgdid)
      hyb_CP = c_loc(hyb)
      if(f_create_from_hyb_1001(vgdid_p, hyb_CP, size(hyb),l_ip1, l_ip2) &
                               == VGD_ERROR)then
         print*,'(F_vgd) ERROR in vgd_create_from_hyb_1001, problem with f_create_from_hyb'
         return
      endif
      status = VGD_OK
    end function vgd_create_from_hyb_1001

    integer function vgd_create_from_hyb_21001(vgdid, hyb, rcoef1, rcoef2, rcoef3, &
                          rcoef4, ip1, ip2, dhm, dht) &
                          result(status)
      integer, target :: vgdid
      real, target, dimension(:),intent(in) :: hyb
      real :: rcoef1, rcoef2, dhm, dht
      real, optional :: rcoef3, rcoef4
      integer, optional, intent(in) :: ip1, ip2

      real my_rcoef3, my_rcoef4
      integer l_ip1,l_ip2
      type(c_ptr) :: vgdid_p, hyb_p, dhm_p, dht_p

      if(present(rcoef3))then
         my_rcoef3 = rcoef3
      else
         my_rcoef3 = -1
      endif

      if(present(rcoef4))then
         my_rcoef4 = rcoef4
      else
         my_rcoef4 = -1
      endif

      if(present(ip1))then
         l_ip1 = ip1
      else
         l_ip1 = -1
      endif
      if(present(ip2))then
         l_ip2 = ip2
      else
         
         l_ip2 = -1
      endif

      vgdid_p = c_loc(vgdid)
      hyb_p = c_loc(hyb)
      dhm_p = c_loc(dhm)
      dht_p = c_loc(dht)

      status = VGD_ERROR

      if( f_create_from_hyb_21001(vgdid_p, hyb_p, size(hyb), &
                                  rcoef1, rcoef2, my_rcoef3, my_rcoef4, l_ip1, l_ip2, &
                                  dhm_p, dht_p)== VGD_ERROR &
                                )then
        print*,'(F_vgd) ERROR: In vgd_create_from_hyb_21001'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_hyb_21001

    integer function vgd_create_from_hyb_21002(vgdid, hyb, rcoef1, rcoef2, rcoef3, &
                          rcoef4, ip1, ip2, dhm, dht, dhw) &
                          result(status)
      integer, target :: vgdid
      real, target, dimension(:),intent(in) :: hyb
      real :: rcoef1, rcoef2, dhm, dht, dhw
      real, optional :: rcoef3, rcoef4
      integer, optional, intent(in) :: ip1, ip2

      real my_rcoef3, my_rcoef4
      integer l_ip1,l_ip2
      type(c_ptr) :: vgdid_p, hyb_p, dhm_p, dht_p, dhw_p

      if(present(rcoef3))then
         my_rcoef3 = rcoef3
      else
         my_rcoef3 = -1
      endif

      if(present(rcoef4))then
         my_rcoef4 = rcoef4
      else
         my_rcoef4 = -1
      endif

      if(present(ip1))then
         l_ip1 = ip1
      else
         l_ip1 = -1
      endif
      if(present(ip2))then
         l_ip2 = ip2
      else
         
         l_ip2 = -1
      endif

      vgdid_p = c_loc(vgdid)
      hyb_p = c_loc(hyb)
      dhm_p = c_loc(dhm)
      dht_p = c_loc(dht)
      dhw_p = c_loc(dhw)

      status = VGD_ERROR

      if( f_create_from_hyb_21002(vgdid_p, hyb_p, size(hyb), &
                                  rcoef1, rcoef2, my_rcoef3, my_rcoef4, l_ip1, l_ip2, &
                                  dhm_p, dht_p, dhw_p)== VGD_ERROR &
                                )then
        print*,'(F_vgd) ERROR: In vgd_create_from_hyb_21002'
        return
      end if

      status = VGD_OK
      return
    end function vgd_create_from_hyb_21002


   integer function vgd_create_from_ab(vgdid,kind,version,nk,ip1,ip2, &
        ptop_8,pref_8,rcoef1,rcoef2,rcoef3,rcoef4,a_m_8,b_m_8,a_t_8,b_t_8, &
        ip1_m,ip1_t,c_m_8,c_t_8,a_w_8,b_w_8,c_w_8,ip1_w) result(status)
      ! Coordinate constructor - build vertical descriptor from arguments
      integer :: vgdid                    !Vertical descriptor id
      integer, intent(in) :: kind,version               !Kind,version to create
      integer, intent(in) :: nk                         !Number of levels
      integer,target , optional, intent(in) :: ip1,ip2          !IP1,2 values for FST file record [0,0]
      real,target , optional, intent(in) :: rcoef1,rcoef2,rcoef3,rcoef4 !R-coefficient values for rectification
      real(kind=8),target , optional, intent(in) :: ptop_8            !Top-level pressure (Pa)
      real(kind=8),target , optional, intent(in) :: pref_8            !Reference-level pressure (Pa)
      real(kind=8),target , optional, dimension(:) :: a_m_8,a_t_8,a_w_8 !A-coefficients for momentum(m),thermo(t) and Vertical-Velocity levels
      real(kind=8),target , optional, dimension(:) :: b_m_8,b_t_8,b_w_8 !B-coefficients for momentum(m),thermo(t) and Vertical-Velocity levels
      real(kind=8),target , optional, dimension(:) :: c_m_8,c_t_8,c_w_8 !C-coefficients for momentum(m),thermo(t) and Vertical-Velocity levels
      integer,target , optional, dimension(:) :: ip1_m,ip1_t,ip1_w !Level ID (IP1) for momentum(m),thermo(t) and Vertical-Velocity levels

      ! Assign optional argument to C_NULL_PTR    

      ! Local variables
      type(c_ptr) :: rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, ptop_8_CP, pref_8_CP
      type(c_ptr) :: a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, ip1_m_CP, ip1_t_CP, ip1_w_CP
      integer l_ip1,l_ip2,nl_m, nl_t, nl_w

      status = VGD_ERROR

      nl_m=nk
      nl_t=-1
      nl_w=-1

      if(present(ip1))then
         l_ip1 = ip1
      else
         l_ip1 = -1
      endif
      if(present(ip2))then
         l_ip2 = ip2
      else
         l_ip2 = -1
      endif
      if(present(ptop_8))then
         ptop_8_CP = c_loc(ptop_8)
      else
         ptop_8_CP = C_NULL_PTR
      endif
      if(present(pref_8))then
         pref_8_CP = c_loc(pref_8)
      else
         pref_8_CP = C_NULL_PTR
      endif
      if(present(rcoef1))then
         rcoef1_CP = c_loc(rcoef1)
      else
         rcoef1_CP = C_NULL_PTR
      endif
      if(present(rcoef2))then
         rcoef2_CP = c_loc(rcoef2)
      else
         rcoef2_CP = C_NULL_PTR
      endif
      if(present(rcoef3))then
         rcoef3_CP = c_loc(rcoef3)
      else
         rcoef3_CP = C_NULL_PTR
      endif
      if(present(rcoef4))then
         rcoef4_CP = c_loc(rcoef4)
      else
         rcoef4_CP = C_NULL_PTR
      endif
      if(present(a_m_8))then
         a_m_8_CP = c_loc(a_m_8)
         nl_m = size(a_m_8)
      else
         a_m_8_CP = C_NULL_PTR
      endif
      if(present(b_m_8))then
         b_m_8_CP = c_loc(b_m_8)
      else
         b_m_8_CP = C_NULL_PTR
      endif
      if(present(c_m_8))then
         c_m_8_CP = c_loc(c_m_8)
      else
         c_m_8_CP = C_NULL_PTR
      endif
      if(present(a_t_8))then
         a_t_8_CP = c_loc(a_t_8)
         nl_t = size(a_t_8)
      else
         a_t_8_CP = C_NULL_PTR
      endif
      if(present(b_t_8))then
         b_t_8_CP = c_loc(b_t_8)
      else
         b_t_8_CP = C_NULL_PTR
      endif
      if(present(c_t_8))then
         c_t_8_CP = c_loc(c_t_8)
      else
         c_t_8_CP = C_NULL_PTR
      endif
      if(present(a_w_8))then
         a_w_8_CP = c_loc(a_w_8)
         nl_w = size(a_w_8)
      else
         a_w_8_CP = C_NULL_PTR
      endif
      if(present(b_w_8))then
         b_w_8_CP = c_loc(b_w_8)
      else
         b_w_8_CP = C_NULL_PTR
      endif
      if(present(c_w_8))then
         c_w_8_CP = c_loc(c_w_8)
      else
         c_w_8_CP = C_NULL_PTR
      endif
      if(present(ip1_m))then
         ip1_m_CP = c_loc(ip1_m)
      else
         ip1_m_CP = C_NULL_PTR
      endif
      if(present(ip1_t))then
         ip1_t_CP = c_loc(ip1_t)
      else
         ip1_t_CP = C_NULL_PTR
      endif
      if(present(ip1_w))then
         ip1_w_CP = c_loc(ip1_w)
      else
         ip1_w_CP = C_NULL_PTR
      endif
      if( f_create_from_ab(vgdid,kind,version,nk,l_ip1,l_ip2, &
           ptop_8_CP, pref_8_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, &
           a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, &
           ip1_m_CP, ip1_t_CP, ip1_w_CP, nl_m, nl_t, nl_w) == VGD_ERROR )then
         print*,'(F_vgd) ERROR in vgd_create_from_ab, problem with vgd_create_from_ab',VGD_ERROR
         return
      endif

      status = VGD_OK

   end function vgd_create_from_ab

   integer function getopt_logical(key,value,quiet) result(status)
      use vgrid_utils, only: up
      character(len=*), intent(in) :: key           !Descriptor key to retrieve
      logical, intent(out) :: value                 !Retrieved value
      logical, intent(in), optional :: quiet        !Do not generate messages

      ! Local variables
      integer :: level_msg, l_quiet
      integer, target :: l_value
      logical :: my_quiet
      type(c_ptr) :: l_value_CP

      ! Set error status
      status=VGD_ERROR
      
      value=.false.
      l_value_CP = c_loc(l_value)
      
      my_quiet=.false.
      if (present(quiet))my_quiet = quiet
      level_msg=MSG_ERROR
      l_quiet = 0
      if(my_quiet)then
         level_msg=MSG_QUIET
         l_quiet=1
      endif
      
      select case(trim(key))
      case ('ALLOW_RESHAPE')
         value=ALLOW_RESHAPE
      case ('ALLOW_SIGMA')
         status = f_getopt_int(key//C_NULL_CHAR,l_value_CP,l_quiet)
         if (status == VGD_ERROR) return
         value = l_value /= 0
      case DEFAULT
         write(for_msg,*) 'invalid key in call to getopt_logical: ',trim(key)
         call msg(level_msg,VGD_PRFX//for_msg)
         return
      end select
      ! Set status and return
      status = VGD_OK
      return
   end function getopt_logical
   
   integer function putopt_logical(key,value) result(status)
      character(len=*), intent(in) :: key           !Descriptor key to retrieve
      logical, intent(in) :: value                    !Retrieved value
      
      ! Local variables
      integer :: l_value

      ! Set error status
      status=VGD_ERROR
      
      select case(trim(key))
      case ('ALLOW_RESHAPE')
         ALLOW_RESHAPE=value
      case ('ALLOW_SIGMA')
         if(value)then
            l_value=1
         else
            l_value=0
         endif
         status = f_putopt_int(key//C_NULL_CHAR,l_value)
         if(status==VGD_ERROR)then
            write(for_msg,*) 'problem with f_putopt_int in putopt_logical for key ',trim(key)
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      case DEFAULT
         write(for_msg,*) 'invalid key in call to putopt_logical: ',trim(key)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      end select
      ! Set status and return
      status = VGD_OK
      return
   end function putopt_logical

   integer function print_desc(vgdid,dummy,stdout,convip_L) result(istat)      
      integer :: vgdid
      integer :: dummy ! makes print_desc distinguishable from print_vcode_description
      integer, intent(in), optional :: stdout     !Output unit to write to [6]
      logical, intent(in), optional :: convip_L

      ! Internal variables
      integer :: l_stdout, l_convip

      istat = VGD_ERROR

      l_stdout = -1
      l_convip = -1
      if(present(stdout)) l_stdout = stdout
      if(present(convip_L))then
         if(convip_L) l_convip = 1
      endif
      istat = f_print_desc(vgdid,l_stdout, l_convip)
   end function print_desc

   integer function print_vcode_description(vcode) result(status)
      ! Dump plain text vcode descriptor information to the requested file unit
      integer, intent(in) :: vcode      

      ! Set error status
      status = VGD_ERROR
      
      status = f_print_vcode_description(vcode)
    
   end function print_vcode_description
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Compute vertical levelling

  integer function levels_toplevel(unit,fstkeys,levels,in_log) result(status)
    ! Top-level interface for computing physical levelling information
    integer, intent(in) :: unit                         !File unit associated with the key
    integer, dimension(:), intent(in) :: fstkeys        !Key of prototype field
    real, dimension(:,:,:), pointer :: levels           !Physical level values
    logical, optional, intent(in) :: in_log             !Compute levels in ln() [.false.]

    ! Local variables
    real :: lev
    integer :: ip1,kind
    character(len=1) :: blk_S
    integer :: error,ig1,ig2,ig3,i
    character(len=1) :: grtyp
    logical :: multiple_grids,my_in_log
    integer :: vgdid
    type(FSTD_ext) :: var

    ! Set error status
    status = VGD_ERROR

    ! Set default values
    my_in_log = .false.
    if (present(in_log)) my_in_log = in_log

    ! Construct appropriate grid_descriptor object (rebuild if new ig1-3 values are found)
    multiple_grids = .false.
    
    grids: do i=1,size(fstkeys)
       error = my_fstprm(fstkeys(i),var)
       if (error /= VGD_OK) then
          write(for_msg,*) 'return from fstprm wrapper for fst key ',fstkeys(i)
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if (i==1) then
          ig1 = var%ig1
          ig2 = var%ig2
          ig3 = var%ig3
          grtyp = var%grtyp(1:1)
          ip1 = var%ip1
       endif
       if (var%ig1 /= ig1 .or. var%ig2 /= ig2 .or. var%ig3 /= ig3 .or. trim(var%grtyp(1:1)) /= trim(grtyp)) then
          write(for_msg,*) 'multiple grids defined in fstkeys vector'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo grids

    call convip_plus (ip1, lev, kind,-1, blk_S, .false.)
    ! Create grid descriptor instance and call level calculator
    if (any(MATCH_GRTYP == grtyp)) then
       error = read_vgrid_from_file(vgdid,unit=unit,format='fst',ip1=ig1,ip2=ig2,kind=kind)
       if(error==VGD_ERROR)then
          write(for_msg,*) 'The above error was produce with call to read_vgrid_from_file with specific ip1 and 1p2, trying with wild card -1, if there is no error below, disregard the above error.'
          call msg(MSG_WARNING,VGD_PRFX//for_msg)
          error = read_vgrid_from_file(vgdid,unit=unit,format='fst',                kind=kind)
       endif
    else
       error = read_vgrid_from_file(vgdid,unit=unit,format='fst',ip1=-1,ip2=-1,kind=kind)
    endif

    if (error /= VGD_OK) then
       write(for_msg,*) 'cannot build grid descriptor instance for fst key ',fstkeys(1)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    error = levels_readref(vgdid,vgdid,unit=unit,fstkeys=fstkeys,levels=levels,in_log=my_in_log)
    if (error /= VGD_OK) then
       write(for_msg,*) 'problem computing level information for fst key ',fstkeys(1)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Set status and return
    status = VGD_OK

    return
  end function levels_toplevel

  integer function levels_readref(vgdid,dummy,unit,fstkeys,levels,in_log) result(status)
    ! Reading referent, compute physical levelling information from the vertical description
    integer, intent(in) :: vgdid          !Vertical descriptor id
    integer :: dummy ! makes levels_readref distinguishable from levels_withref
    integer, intent(in) :: unit                         !File unit associated with the key
    integer, dimension(:), intent(in) :: fstkeys        !Key of prototype field
    real, dimension(:,:,:), pointer :: levels           !Physical level values
    logical, optional, intent(in) :: in_log             !Compute levels in ln() [.false.]

    ! Internal variables
    integer :: istat,error,i,ip1,ip2,match_ipig
    integer, dimension(size(fstkeys)) :: ip1_list
    type(FSTD_ext) prmk,prm_check
    real, dimension(:,:), pointer :: p0, p0ls
    logical :: my_in_log, relax_ipig_match_L
    character(len=VGD_LEN_RFLD) :: ref_name

    nullify(p0, p0ls)
    
    relax_ipig_match_L=.false.

    ! Set error status
    status = VGD_ERROR

    ! Set default values
    my_in_log = .false.
    if (present(in_log)) my_in_log=in_log

    ! Check input keys
    if(size(fstkeys)<1)then
       write(for_msg,*) 'fstkeys vector passed to vgd_levels is null'                             
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return                                                                
    endif
    istat=my_fstprm(fstkeys(1),prmk)
    if(istat < 0)then
       write(for_msg,*) 'problem with my_fstprm in levels_readref'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
    endif
    check_times: do i=1,size(fstkeys)
       istat=my_fstprm(fstkeys(i),prm_check)
       ip1_list(i) = prm_check%ip1
       if (prm_check%datev /= prmk%datev) then
          write(for_msg,*) 'multiple valid times given in fstkeys vector'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if (prm_check%ig1 /= prmk%ig1 .or. prm_check%ig2 /= prmk%ig2) then
          write(for_msg,*) 'multiple grids given in fstkeys vector'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo check_times
    if ( vgd_get(vgdid,"MIPG match !! IPs and IGs with records",match_ipig) == VGD_ERROR )then
       write(for_msg,*) 'ERROR in levels_readref with vgd_get on MIPG'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    if ( vgd_get(vgdid,"IP_1",ip1) == VGD_ERROR )then
       write(for_msg,*) 'ERROR in levels_readref with vgd_get on IP_1'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if ( vgd_get(vgdid,"IP_2",ip2) == VGD_ERROR )then
       write(for_msg,*) 'ERROR in levels_readref with vgd_get on IP_2'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if (match_ipig == 1) then
       if (prmk%ig1 /= ip1 .or. prmk%ig2 /= ip2) then
          write(for_msg,*) 'fstkeys do not correspond to the correct grid descriptor'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,'("   expecting (ip1,ip2)->(",i8,",",i8,"), got (ig1,ig2)->(",i8,",",i8,")")')&
          prmk%ig1,prmk%ig2,ip1,ip2
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif

    ! Check surface field if needed
    sfc_large_scale_valid: if ( is_valid(vgdid,"ref_namel_valid") ) then
       if ( vgd_get(vgdid,"RFLS",ref_name) == VGD_ERROR )then
          write(for_msg,*) 'ERROR in levels_readref with vgd_get on RFLS'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       istat=get_ref( p0ls, vgdid, ref_name, unit, prmk)
       if(istat == VGD_ERROR)then
          write(for_msg,*) 'Problem getting reference field ',trim(ref_name)
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    end if sfc_large_scale_valid

    sfc_valid: if( is_valid(vgdid,"ref_name_valid")) then
       if ( vgd_get(vgdid,"RFLD",ref_name) == VGD_ERROR )then
          write(for_msg,*) 'ERROR in levels_readref with vgd_get on RFLD'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       istat=get_ref(p0,vgdid,ref_name,unit,prmk)
       if(istat == VGD_ERROR)then
          write(for_msg,*) 'Problem getting reference field ',trim(ref_name)
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    else
       allocate(p0(1,1),stat=error)
       if (error /= 0) then
          nullify(p0)
          write(for_msg,*) 'cannot allocate space for p0 in levels_readref'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       p0 = VGD_MISSING
    endif sfc_valid
    
    ! Wrap call to level calculator
    if (is_valid(vgdid,"ref_namel_valid")) then
       error = levels_withref(vgdid,vgdid,sfc_field=p0,sfc_field_ls=p0ls,ip1_list=ip1_list,levels=levels,in_log=my_in_log)
    else
       error = levels_withref(vgdid,vgdid,sfc_field=p0,ip1_list=ip1_list,levels=levels,in_log=my_in_log)
    endif
    deallocate(p0)
    if(associated(p0ls))deallocate(p0ls)
    if (error /= VGD_OK) then
       write(for_msg,*) 'got error return from levels_withref in levels_readref'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Set status and return
    status = VGD_OK
    return
  end function levels_readref

  integer function levels_withref_prof(vgdid,ip1_list,levels,sfc_field,in_log,sfc_field_ls) result(status)
#undef REAL_8
#define REAL_KIND 4
#define PROC_SUFF ""
     integer, intent(in) :: vgdid                  !Vertical descriptor id
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real(kind=REAL_KIND), dimension(:), pointer :: levels                     !Physical level values
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field                   !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field_ls                !Surface field reference for coordinate [none]

     ! Local variables
     real(kind=REAL_KIND) :: my_sfc_field
     logical :: my_in_log

     ! Set return value
     status = VGD_ERROR
     
     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log

     ! Wrap call to level calculation
     if(present(sfc_field_ls))then
#if defined(REAL_8)
        status = diag_withref_prof_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#else
        status = diag_withref_prof(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#endif
     else
#if defined(REAL_8)
        status = diag_withref_prof_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#else
        status = diag_withref_prof(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#endif
     endif
     return
#undef REAL_KIND
#undef PROC_SUFF
  end function levels_withref_prof

  integer function levels_withref_prof_8(vgdid,ip1_list,levels,sfc_field,in_log,sfc_field_ls) result(status)
#define REAL_8 1
#define REAL_KIND 8
#define PROC_SUFF "_8"
     integer, intent(in) :: vgdid                  !Vertical descriptor id
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real(kind=REAL_KIND), dimension(:), pointer :: levels                     !Physical level values
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field                   !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field_ls                !Surface field reference for coordinate [none]

     ! Local variables
     real(kind=REAL_KIND) :: my_sfc_field
     logical :: my_in_log

     ! Set return value
     status = VGD_ERROR
     
     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log

     ! Wrap call to level calculation
     if(present(sfc_field_ls))then
#if defined(REAL_8)
        status = diag_withref_prof_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#else
        status = diag_withref_prof(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#endif
     else
#if defined(REAL_8)
        status = diag_withref_prof_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#else
        status = diag_withref_prof(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#endif
     endif
     return
#undef REAL_8
#undef REAL_KIND
#undef PROC_SUFF
  end function levels_withref_prof_8

  integer function dpidpis_withref_prof(vgdid,ip1_list,dpidpis,sfc_field) result(status)
#undef REAL_8
#define REAL_KIND 4
#define PROC_SUFF ""
     use vgrid_utils, only: up
     integer, intent(in) :: vgdid                  !Vertical descriptor id
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real(kind=REAL_KIND), dimension(:), pointer :: dpidpis                      !Derivative values
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]

     ! Local variables
     real(kind=REAL_KIND) :: my_sfc_field
     integer :: stat

     status=VGD_ERROR

     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     ! Wrap call to level calculation
#if defined(REAL_8)
     stat = diag_withref_prof_8(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#else
     stat = diag_withref_prof(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#endif
     if(stat==VGD_ERROR)then
        write(for_msg,*) 'ERROR with diag_withref_prof'//PROC_SUFF//' in dpidpis_withref_prof'//PROC_SUFF
        return
     endif
     status = VGD_OK
     return
#undef REAL_KIND
#undef PROC_SUFF
  end function dpidpis_withref_prof

  integer function dpidpis_withref_prof_8(vgdid,ip1_list,dpidpis,sfc_field) result(status)
#define REAL_8 1
#define REAL_KIND 8
#define PROC_SUFF "_8"
     use vgrid_utils, only: up
     integer, intent(in) :: vgdid                  !Vertical descriptor id
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real(kind=REAL_KIND), dimension(:), pointer :: dpidpis                      !Derivative values
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]

     ! Local variables
     real(kind=REAL_KIND) :: my_sfc_field
     integer :: stat

     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     ! Wrap call to level calculation
#if defined(REAL_8)
     stat = diag_withref_prof_8(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#else
     stat = diag_withref_prof(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#endif
     if(stat==VGD_ERROR)then
        write(for_msg,*) 'ERROR with diag_withref_prof'//PROC_SUFF//' in dpidpis_withref_prof'//PROC_SUFF
        return
     endif
     status = VGD_OK
     return
#undef REAL_8
#undef REAL_KIND
#undef PROC_SUFF
  end function dpidpis_withref_prof_8
  integer function diag_withref_prof(vgdid,ip1_list,levels,sfc_field,in_log,dpidpis,sfc_field_ls) result(status)
#undef REAL_8
#define REAL_KIND 4
#define PROC_SUFF ""
     use vgrid_utils, only: get_allocate
     integer, intent(in) :: vgdid                  !Vertical descriptor id
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real(kind=REAL_KIND), dimension(:), pointer :: levels                       !Physical level values
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field_ls                  !Surface large scale field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          
     logical, optional, intent(in) :: dpidpis                    !Compute partial derivative of hydrostatic pressure (pi) with
                                                                 !   respect to surface hydrostatic pressure(pis) [.false.]
     
     ! Local variables
     integer :: error,nk
     real(kind=REAL_KIND) :: my_sfc_field, my_sfc_field_ls
     real(kind=REAL_KIND), dimension(:,:), pointer :: sfc_field_2d, sfc_field_ls_2d
     real(kind=REAL_KIND), dimension(:,:,:), pointer :: levels_3d
     logical :: my_in_log,my_dpidpis

     ! Set error status
     status = VGD_ERROR

     nullify(sfc_field_2d,sfc_field_ls_2d,levels_3d)

     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     my_sfc_field_ls = VGD_MISSING
     if (present(sfc_field_ls)) my_sfc_field_ls = sfc_field_ls
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log
     my_dpidpis = .false.
     if (present(dpidpis)) my_dpidpis = dpidpis

     nk=size(ip1_list)

     allocate(sfc_field_2d(1,1),sfc_field_ls_2d(1,1),levels_3d(1,1,nk),stat=error)
     if (error /= 0) then
        if(associated(sfc_field_2d))deallocate(sfc_field_2d)
        if(associated(sfc_field_ls_2d))deallocate(sfc_field_ls_2d)
        if(associated(levels_3d))deallocate(levels_3d)
        write(for_msg,*) 'cannot allocate space for p0/levels in diag_withref_prof'//PROC_SUFF
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     sfc_field_2d=my_sfc_field
     ! Wrap call to level calculator
     if (present(sfc_field_ls))then
        sfc_field_ls_2d=my_sfc_field_ls
#if defined(REAL_8)
        error = diag_withref_8(vgdid,sfc_field=sfc_field_2d,sfc_field_ls=sfc_field_ls_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#else
        error = diag_withref(vgdid,sfc_field=sfc_field_2d,sfc_field_ls=sfc_field_ls_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#endif
     else
#if defined(REAL_8)
        error = diag_withref_8(vgdid,sfc_field=sfc_field_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#else
        error = diag_withref(vgdid,sfc_field=sfc_field_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#endif
     endif
     if (error /= 0) then
        deallocate(sfc_field_2d,levels_3d)
        write(for_msg,*) 'problem with diag_withref in diag_withref_prof'//PROC_SUFF
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = get_allocate('levels',levels,nk,ALLOW_RESHAPE,'(in diag_withref_prof'//PROC_SUFF//')')
     if(error/=0)return
     levels=levels_3d(1,1,1:nk)
     deallocate(sfc_field_2d,sfc_field_ls_2d,levels_3d)
     ! Set status and return
     status = VGD_OK
     return
#undef REAL_KIND
#undef PROC_SUFF
  end function diag_withref_prof

  integer function diag_withref_prof_8(vgdid,ip1_list,levels,sfc_field,in_log,dpidpis,sfc_field_ls) result(status)
#define REAL_8 1
#define REAL_KIND 8
#define PROC_SUFF "_8"
     use vgrid_utils, only: get_allocate
     integer, intent(in) :: vgdid                  !Vertical descriptor id
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real(kind=REAL_KIND), dimension(:), pointer :: levels                       !Physical level values
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]
     real(kind=REAL_KIND), optional, intent(in) :: sfc_field_ls                  !Surface large scale field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          
     logical, optional, intent(in) :: dpidpis                    !Compute partial derivative of hydrostatic pressure (pi) with
                                                                 !   respect to surface hydrostatic pressure(pis) [.false.]
     
     ! Local variables
     integer :: error,nk
     real(kind=REAL_KIND) :: my_sfc_field, my_sfc_field_ls
     real(kind=REAL_KIND), dimension(:,:), pointer :: sfc_field_2d, sfc_field_ls_2d
     real(kind=REAL_KIND), dimension(:,:,:), pointer :: levels_3d
     logical :: my_in_log,my_dpidpis

     ! Set error status
     status = VGD_ERROR

     nullify(sfc_field_2d,sfc_field_ls_2d,levels_3d)

     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     my_sfc_field_ls = VGD_MISSING
     if (present(sfc_field_ls)) my_sfc_field_ls = sfc_field_ls
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log
     my_dpidpis = .false.
     if (present(dpidpis)) my_dpidpis = dpidpis

     nk=size(ip1_list)

     allocate(sfc_field_2d(1,1),sfc_field_ls_2d(1,1),levels_3d(1,1,nk),stat=error)
     if (error /= 0) then
        if(associated(sfc_field_2d))deallocate(sfc_field_2d)
        if(associated(sfc_field_ls_2d))deallocate(sfc_field_ls_2d)
        if(associated(levels_3d))deallocate(levels_3d)
        write(for_msg,*) 'cannot allocate space for p0/levels in diag_withref_prof'//PROC_SUFF
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     sfc_field_2d=my_sfc_field
     ! Wrap call to level calculator
     if (present(sfc_field_ls))then
        sfc_field_ls_2d=my_sfc_field_ls
#if defined(REAL_8)
        error = diag_withref_8(vgdid,sfc_field=sfc_field_2d,sfc_field_ls=sfc_field_ls_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#else
        error = diag_withref(vgdid,sfc_field=sfc_field_2d,sfc_field_ls=sfc_field_ls_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#endif
     else
#if defined(REAL_8)
        error = diag_withref_8(vgdid,sfc_field=sfc_field_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#else
        error = diag_withref(vgdid,sfc_field=sfc_field_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)
#endif
     endif
     if (error /= 0) then
        deallocate(sfc_field_2d,levels_3d)
        write(for_msg,*) 'problem with diag_withref in diag_withref_prof'//PROC_SUFF
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = get_allocate('levels',levels,nk,ALLOW_RESHAPE,'(in diag_withref_prof'//PROC_SUFF//')')
     if(error/=0)return
     levels=levels_3d(1,1,1:nk)
     deallocate(sfc_field_2d,sfc_field_ls_2d,levels_3d)
     ! Set status and return
     status = VGD_OK
     return
#undef REAL_8
#undef REAL_KIND
#undef PROC_SUFF
  end function diag_withref_prof_8


  integer function levels_withref(vgdid,dummy,ip1_list,levels,sfc_field,in_log,sfc_field_ls) result(status)
#undef REAL_8
#define REAL_KIND 4
#define PROC_SUFF ""
      use vgrid_utils, only: get_allocate
      ! Given referent, compute physical levelling information from the vertical description
      integer, intent(in) :: vgdid                  !Vertical descriptor id
      integer :: dummy ! makes levels_withref distinguishable from levels_toplevel
      integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: levels                   !Physical level values
      real(kind=REAL_KIND), dimension(:,:), optional, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
      real(kind=REAL_KIND), dimension(:,:), optional, intent(in) :: sfc_field_ls  !Surface field large scale reference for coordinate [none]
      logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]

      ! Local variables
      integer :: error,ni,nj
      real(kind=REAL_KIND), dimension(:,:), pointer :: my_sfc_field
      logical :: my_in_log
      
      nullify(my_sfc_field)

      ! Set return value
      status = VGD_ERROR
      
      ! Set default values
      if (present(sfc_field)) then
         ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
      else
         ni = 1; nj = 1
      endif
      allocate(my_sfc_field(ni,nj),stat=error)
      if (error /= 0) then
         write(for_msg,*) 'cannot allocate space for sfc_field in levels_withref'//PROC_SUFF
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif
      if (present(sfc_field)) then
         my_sfc_field = sfc_field
      else
         my_sfc_field = VGD_MISSING
      endif
      if (present(sfc_field_ls)) then
         if( size(sfc_field_ls,dim=1) /= ni .or. size(sfc_field_ls,dim=2) /= nj )then
            write(for_msg,*) 'size of sfc_field_ls not the same as sfc_field in levels_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      endif
      my_in_log = .false.
      if (present(in_log)) my_in_log = in_log
      
      ! Wrap call to level calculator
      if( present(sfc_field_ls) )then
#if defined(REAL_8)
         status=diag_withref_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#else
         status=diag_withref(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#endif
      else
#if defined(REAL_8)
         status=diag_withref_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#else
         status=diag_withref(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#endif
      endif
      deallocate(my_sfc_field)
      return
#undef REAL_KIND
#undef PROC_SUFF
  end function levels_withref

  integer function levels_withref_8(vgdid,ip1_list,levels,sfc_field,in_log, &
       sfc_field_ls) result(status)
#define REAL_8 1
#define REAL_KIND 8
#define PROC_SUFF "_8"
      use vgrid_utils, only: get_allocate
      ! Given referent, compute physical levelling information from the vertical description
      integer, intent(in) :: vgdid                  !Vertical descriptor id
      integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: levels                   !Physical level values
      real(kind=REAL_KIND), dimension(:,:), optional, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
      real(kind=REAL_KIND), dimension(:,:), optional, intent(in) :: sfc_field_ls  !Surface field large scale reference for coordinate [none]
      logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]

      ! Local variables
      integer :: error,ni,nj
      real(kind=REAL_KIND), dimension(:,:), pointer :: my_sfc_field
      logical :: my_in_log
      
      nullify(my_sfc_field)

      ! Set return value
      status = VGD_ERROR
      
      ! Set default values
      if (present(sfc_field)) then
         ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
      else
         ni = 1; nj = 1
      endif
      allocate(my_sfc_field(ni,nj),stat=error)
      if (error /= 0) then
         write(for_msg,*) 'cannot allocate space for sfc_field in levels_withref'//PROC_SUFF
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif
      if (present(sfc_field)) then
         my_sfc_field = sfc_field
      else
         my_sfc_field = VGD_MISSING
      endif
      if (present(sfc_field_ls)) then
         if( size(sfc_field_ls,dim=1) /= ni .or. size(sfc_field_ls,dim=2) /= nj )then
            write(for_msg,*) 'size of sfc_field_ls not the same as sfc_field in levels_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      endif
      my_in_log = .false.
      if (present(in_log)) my_in_log = in_log
      
      ! Wrap call to level calculator
      if( present(sfc_field_ls) )then
#if defined(REAL_8)
         status=diag_withref_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#else
         status=diag_withref(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log,sfc_field_ls=sfc_field_ls)
#endif
      else
#if defined(REAL_8)
         status=diag_withref_8(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#else
         status=diag_withref(vgdid,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
#endif
      endif
      deallocate(my_sfc_field)
      return
#undef REAL_8
#undef REAL_KIND
#undef PROC_SUFF
   end function levels_withref_8

   integer function dpidpis_withref(vgdid,ip1_list,dpidpis,sfc_field) result(status)
#undef REAL_8
#define REAL_KIND 4
#define PROC_SUFF ""
      use vgrid_utils, only: get_allocate
      ! Given referent, compute physical levelling information from the vertical description
      integer, intent(in) :: vgdid                  !Vertical descriptor id
      integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: dpidpis                !pressure derivative with respect to sfc pressure
      real(kind=REAL_KIND), dimension(:,:), optional, intent(in) :: sfc_field   !Surface field reference for coordinate [none]
      
      ! Local variables 
      integer :: ni,nj,error,stat
      real(kind=REAL_KIND), dimension(:,:), pointer :: my_sfc_field
      
      nullify(my_sfc_field)
      
      ! Set return value
      status = VGD_ERROR
      
      ! Set default values
      if (present(sfc_field)) then
         ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
      else
         ni = 1; nj = 1
      endif
      allocate(my_sfc_field(ni,nj),stat=error)
      if (error /= 0) then
         nullify(my_sfc_field)
         write(for_msg,*) 'cannot allocate space for sfc_field in dpidpis_withref'//PROC_SUFF
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif
      if (present(sfc_field)) then
         my_sfc_field = sfc_field
      else
         my_sfc_field = VGD_MISSING
      endif
      ! Wrap call to level calculator
#if defined(REAL_8)
      stat=diag_withref_8(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#else
      stat=diag_withref(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#endif
      if(stat==VGD_ERROR)then
         deallocate(my_sfc_field)
         return
      endif
      deallocate(my_sfc_field)
      status=VGD_OK
      return
#undef REAL_KIND
#undef PROC_SUFF
   end function dpidpis_withref
   
   integer function dpidpis_withref_8(vgdid,ip1_list,dpidpis,sfc_field) result(status)
#define REAL_8 1
#define REAL_KIND 8
#define PROC_SUFF "_8"
      use vgrid_utils, only: get_allocate
      ! Given referent, compute physical levelling information from the vertical description
      integer, intent(in) :: vgdid                  !Vertical descriptor id
      integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: dpidpis                !pressure derivative with respect to sfc pressure
      real(kind=REAL_KIND), dimension(:,:), optional, intent(in) :: sfc_field   !Surface field reference for coordinate [none]
      
      ! Local variables 
      integer :: ni,nj,error,stat
      real(kind=REAL_KIND), dimension(:,:), pointer :: my_sfc_field
      
      nullify(my_sfc_field)
      
      ! Set return value
      status = VGD_ERROR
      
      ! Set default values
      if (present(sfc_field)) then
         ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
      else
         ni = 1; nj = 1
      endif
      allocate(my_sfc_field(ni,nj),stat=error)
      if (error /= 0) then
         nullify(my_sfc_field)
         write(for_msg,*) 'cannot allocate space for sfc_field in dpidpis_withref'//PROC_SUFF
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif
      if (present(sfc_field)) then
         my_sfc_field = sfc_field
      else
         my_sfc_field = VGD_MISSING
      endif
      ! Wrap call to level calculator
#if defined(REAL_8)
      stat=diag_withref_8(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#else
      stat=diag_withref(vgdid,ip1_list,dpidpis,sfc_field=my_sfc_field,dpidpis=.true.)
#endif
      if(stat==VGD_ERROR)then
         deallocate(my_sfc_field)
         return
      endif
      deallocate(my_sfc_field)
      status=VGD_OK
      return
#undef REAL_8
#undef REAL_KIND
#undef PROC_SUFF
   end function dpidpis_withref_8
   integer function diag_withref(vgdid,ip1_list,levels,sfc_field,in_log,dpidpis,sfc_field_ls) result(status)
#undef REAL_8
#define REAL_KIND 4
#define PROC_SUFF ""
      use vgrid_utils, only: get_allocate
      ! Given referent, compute physical levelling information from the vertical description
      integer, intent(in) :: vgdid                  !Vertical descriptor id
      integer, target, dimension(:), intent(in) :: ip1_list               !Key of prototype field
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: levels                   !Physical level values
      real(kind=REAL_KIND), dimension(:,:), optional, target, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
      real(kind=REAL_KIND), dimension(:,:), optional, target, intent(in) :: sfc_field_ls  !Surface field large scale reference for coordinate [none]
      logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]
      logical, optional, intent(in) :: dpidpis                    !Compute partial derivative of hydrostatic pressure (pi) with
      !   respect to surface hydrostatic pressure(pis) [.false.]
      
      ! Local variables
      integer istat,ni,nj,nk,error
      real(kind=REAL_KIND), dimension(:,:), pointer :: my_sfc_field, my_sfc_field_ls
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: my_levels 
      type (c_ptr) :: ip1_list_CP ,levels_CP ,sfc_field_CP, sfc_field_ls_CP
      integer :: in_log_int, dpidpis_int
      logical :: my_dpidpis, alloc_my_sfc_field_L, alloc_my_sfc_field_ls_L, alloc_my_levels_L

      ! Set error status
      status = VGD_ERROR
      
      ! Set default values      
      in_log_int = 0
      if (present(in_log))then
         if(in_log)then
            in_log_int = 1
         else
            in_log_int = 0
         endif
      endif
      my_dpidpis=.false.
      if(present(dpidpis))my_dpidpis=dpidpis
      dpidpis_int = 0
      if(my_dpidpis)then
         dpidpis_int = 1
      else
         dpidpis_int = 0
      endif
      
      alloc_my_sfc_field_L = .false.; alloc_my_sfc_field_ls_L = .false.; alloc_my_levels_L = .false.

      if (present(sfc_field)) then

         ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2); nk = size(ip1_list)

#ifdef WITH_intel
!        is_contiguous is not Fortran 2008 standard but Intel
         if(is_contiguous(sfc_field))then
            my_sfc_field => sfc_field
         else
#endif

!        Copy in and out 
            alloc_my_sfc_field_L = .true.
            allocate(my_sfc_field(ni,nj),stat=error)
            if (error /= 0) then
               write(for_msg,*) 'cannot allocate space for my_sfc_field in diag_withref'//PROC_SUFF
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            my_sfc_field(1:ni,1:nj) = sfc_field(1:ni,1:nj)

#ifdef WITH_intel
         endif 
#endif

      else
         if (is_valid(vgdid,"ref_name_valid")) then
            write(for_msg,*) 'reference field must be provided to diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         else
            ni = 1; nj = 1; nk = size(ip1_list)
         endif
      endif
      
      if (present(sfc_field_ls)) then
         if(  ni /= size(sfc_field_ls,dim=1) .or. &
              nj /= size(sfc_field_ls,dim=2) )then
            write(for_msg,*) 'reference large scale field is not of same size has reference field'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif

#ifdef WITH_intel
!        is_contiguous is not Fortran 2008 standard but Intel
         if(is_contiguous(sfc_field_ls))then
            my_sfc_field_ls => sfc_field_ls
         else
#endif
!        Copy in and out instead
            alloc_my_sfc_field_ls_L = .true.
            allocate(my_sfc_field_ls(ni,nj),stat=error)
            if (error /= 0) then
               write(for_msg,*) 'cannot allocate space for my_sfc_field_ls in diag_withref'//PROC_SUFF
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            my_sfc_field_ls(1:ni,1:nj) = sfc_field_ls(1:ni,1:nj)

#ifdef WITH_intel
         endif
#endif
      else
         if (is_valid(vgdid,"ref_namel_valid") .and. .not. my_dpidpis) then
            write(for_msg,*) 'reference large scale field must be provided to diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      endif
      if (associated(levels)) then
         if (size(levels,dim=1) /= ni .or. size(levels,dim=2) /= nj .or. size(levels,dim=3) /= nk) then
            if(ALLOW_RESHAPE)then
               write(for_msg,*) 'Levels array size error - will be reallocated'
               call msg(MSG_WARNING,VGD_PRFX//for_msg)
               deallocate(levels)
            else
               write(for_msg,*) 'Levels array size error - will not reallocate since ALLOW_RESHAPE is set to false'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
         endif
      endif
      if(.not. associated(levels) )then
         allocate(levels(ni,nj,nk),stat=error)
         if (error /= 0) then
            write(for_msg,*) 'cannot allocate space for levels in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      endif

#ifdef WITH_intel
!        is_contiguous is not Fortran 2008 standard but Intel
      if(is_contiguous(levels))then
         my_levels => levels
      else
#endif
!        Copy in and out instead
         alloc_my_levels_L = .true.
         allocate(my_levels(ni,nj,nk),stat=error)
         if (error /= 0) then
            write(for_msg,*) 'cannot allocate space for my_levels in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif

#ifdef WITH_intel
      endif
#endif
      
      ip1_list_CP  = c_loc(ip1_list)
      levels_CP    = c_loc(my_levels(1,1,1))
      sfc_field_CP = C_NULL_PTR
      if (present(sfc_field)) sfc_field_CP = c_loc(my_sfc_field(1,1))
      sfc_field_ls_CP = C_NULL_PTR
      if (present(sfc_field_ls)) sfc_field_ls_CP = c_loc(my_sfc_field_ls(1,1))
#if defined(REAL_8)
      istat = f_diag_withref_8(vgdid,ni,nj,nk,ip1_list_CP,levels_CP,sfc_field_CP,sfc_field_ls_CP,in_log_int,dpidpis_int)
#else
      istat = f_diag_withref(vgdid,ni,nj,nk,ip1_list_CP,levels_CP,sfc_field_CP,sfc_field_ls_CP,in_log_int,dpidpis_int)
#endif
      if (istat /= VGD_OK) then
         if(my_dpidpis)then
            write(for_msg,*) 'error computing dpidpis in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         else
            write(for_msg,*) 'error computing pressure in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         endif
         return
      endif
      if(alloc_my_sfc_field_L)    deallocate(my_sfc_field)
      if(alloc_my_sfc_field_ls_L) deallocate(my_sfc_field_ls)
      if(alloc_my_levels_L)then
         levels(1:ni,1:nj,1:nk) = my_levels(1:ni,1:nj,1:nk)
         deallocate(my_levels)
      end if
      
      ! Set status and return
      status = VGD_OK
      return
#undef REAL_KIND
#undef PROC_SUFF
   end function diag_withref

   integer function diag_withref_8(vgdid,ip1_list,levels,sfc_field,in_log,dpidpis,sfc_field_ls) result(status)
#define REAL_8 1
#define REAL_KIND 8
#define PROC_SUFF "_8"
      use vgrid_utils, only: get_allocate
      ! Given referent, compute physical levelling information from the vertical description
      integer, intent(in) :: vgdid                  !Vertical descriptor id
      integer, target, dimension(:), intent(in) :: ip1_list               !Key of prototype field
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: levels                   !Physical level values
      real(kind=REAL_KIND), dimension(:,:), optional, target, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
      real(kind=REAL_KIND), dimension(:,:), optional, target, intent(in) :: sfc_field_ls  !Surface field large scale reference for coordinate [none]
      logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]
      logical, optional, intent(in) :: dpidpis                    !Compute partial derivative of hydrostatic pressure (pi) with
      !   respect to surface hydrostatic pressure(pis) [.false.]
      
      ! Local variables
      integer istat,ni,nj,nk,error
      real(kind=REAL_KIND), dimension(:,:), pointer :: my_sfc_field, my_sfc_field_ls
      real(kind=REAL_KIND), dimension(:,:,:), pointer :: my_levels 
      type (c_ptr) :: ip1_list_CP ,levels_CP ,sfc_field_CP, sfc_field_ls_CP
      integer :: in_log_int, dpidpis_int
      logical :: my_dpidpis, alloc_my_sfc_field_L, alloc_my_sfc_field_ls_L, alloc_my_levels_L

      ! Set error status
      status = VGD_ERROR
      
      ! Set default values      
      in_log_int = 0
      if (present(in_log))then
         if(in_log)then
            in_log_int = 1
         else
            in_log_int = 0
         endif
      endif
      my_dpidpis=.false.
      if(present(dpidpis))my_dpidpis=dpidpis
      dpidpis_int = 0
      if(my_dpidpis)then
         dpidpis_int = 1
      else
         dpidpis_int = 0
      endif
      
      alloc_my_sfc_field_L = .false.; alloc_my_sfc_field_ls_L = .false.; alloc_my_levels_L = .false.

      if (present(sfc_field)) then

         ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2); nk = size(ip1_list)

#ifdef WITH_intel
!        is_contiguous is not Fortran 2008 standard but Intel
         if(is_contiguous(sfc_field))then
            my_sfc_field => sfc_field
         else
#endif

!        Copy in and out 
            alloc_my_sfc_field_L = .true.
            allocate(my_sfc_field(ni,nj),stat=error)
            if (error /= 0) then
               write(for_msg,*) 'cannot allocate space for my_sfc_field in diag_withref'//PROC_SUFF
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            my_sfc_field(1:ni,1:nj) = sfc_field(1:ni,1:nj)

#ifdef WITH_intel
         endif 
#endif

      else
         if (is_valid(vgdid,"ref_name_valid")) then
            write(for_msg,*) 'reference field must be provided to diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         else
            ni = 1; nj = 1; nk = size(ip1_list)
         endif
      endif
      
      if (present(sfc_field_ls)) then
         if(  ni /= size(sfc_field_ls,dim=1) .or. &
              nj /= size(sfc_field_ls,dim=2) )then
            write(for_msg,*) 'reference large scale field is not of same size has reference field'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif

#ifdef WITH_intel
!        is_contiguous is not Fortran 2008 standard but Intel
         if(is_contiguous(sfc_field_ls))then
            my_sfc_field_ls => sfc_field_ls
         else
#endif
!        Copy in and out instead
            alloc_my_sfc_field_ls_L = .true.
            allocate(my_sfc_field_ls(ni,nj),stat=error)
            if (error /= 0) then
               write(for_msg,*) 'cannot allocate space for my_sfc_field_ls in diag_withref'//PROC_SUFF
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            my_sfc_field_ls(1:ni,1:nj) = sfc_field_ls(1:ni,1:nj)

#ifdef WITH_intel
         endif
#endif
      else
         if (is_valid(vgdid,"ref_namel_valid") .and. .not. my_dpidpis) then
            write(for_msg,*) 'reference large scale field must be provided to diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      endif
      if (associated(levels)) then
         if (size(levels,dim=1) /= ni .or. size(levels,dim=2) /= nj .or. size(levels,dim=3) /= nk) then
            if(ALLOW_RESHAPE)then
               write(for_msg,*) 'Levels array size error - will be reallocated'
               call msg(MSG_WARNING,VGD_PRFX//for_msg)
               deallocate(levels)
            else
               write(for_msg,*) 'Levels array size error - will not reallocate since ALLOW_RESHAPE is set to false'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
         endif
      endif
      if(.not. associated(levels) )then
         allocate(levels(ni,nj,nk),stat=error)
         if (error /= 0) then
            write(for_msg,*) 'cannot allocate space for levels in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
      endif

#ifdef WITH_intel
!        is_contiguous is not Fortran 2008 standard but Intel
      if(is_contiguous(levels))then
         my_levels => levels
      else
#endif
!        Copy in and out instead
         alloc_my_levels_L = .true.
         allocate(my_levels(ni,nj,nk),stat=error)
         if (error /= 0) then
            write(for_msg,*) 'cannot allocate space for my_levels in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif

#ifdef WITH_intel
      endif
#endif
      
      ip1_list_CP  = c_loc(ip1_list)
      levels_CP    = c_loc(my_levels(1,1,1))
      sfc_field_CP = C_NULL_PTR
      if (present(sfc_field)) sfc_field_CP = c_loc(my_sfc_field(1,1))
      sfc_field_ls_CP = C_NULL_PTR
      if (present(sfc_field_ls)) sfc_field_ls_CP = c_loc(my_sfc_field_ls(1,1))
#if defined(REAL_8)
      istat = f_diag_withref_8(vgdid,ni,nj,nk,ip1_list_CP,levels_CP,sfc_field_CP,sfc_field_ls_CP,in_log_int,dpidpis_int)
#else
      istat = f_diag_withref(vgdid,ni,nj,nk,ip1_list_CP,levels_CP,sfc_field_CP,sfc_field_ls_CP,in_log_int,dpidpis_int)
#endif
      if (istat /= VGD_OK) then
         if(my_dpidpis)then
            write(for_msg,*) 'error computing dpidpis in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         else
            write(for_msg,*) 'error computing pressure in diag_withref'//PROC_SUFF
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         endif
         return
      endif
      if(alloc_my_sfc_field_L)    deallocate(my_sfc_field)
      if(alloc_my_sfc_field_ls_L) deallocate(my_sfc_field_ls)
      if(alloc_my_levels_L)then
         levels(1:ni,1:nj,1:nk) = my_levels(1:ni,1:nj,1:nk)
         deallocate(my_levels)
      end if
      
      ! Set status and return
      status = VGD_OK
      return
#undef REAL_8
#undef REAL_KIND
#undef PROC_SUFF
   end function diag_withref_8
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Write descriptors
   
   integer function write_desc(vgdid,unit,format) result(status)
      use vgrid_utils, only: up, get_allocate
      ! Write descriptors to the requested file
      integer, intent(in) :: vgdid                     !Vertical descriptor id
      integer, intent(in) :: unit                      !File unit to write to
      character(len=*), optional, intent(in) :: format !File format ('fst' or 'bin' ) default is 'fst'

      ! Local variables
      integer ier
      integer, target, dimension(3) :: tshape
      real(kind=8), dimension(:,:,:), pointer :: table_8
      type(c_ptr) :: tshape_CP      
      character(len=100) :: myformat

      nullify(table_8)

      ! Set error status
      status = VGD_ERROR
      myformat='FST'
      if (present(format)) myformat = trim(up(format))
      
      ! Write to the desired output file type
      select case (trim(up(myformat)))
         
         ! Write to an RPN Standard file
      case ('FST')         
         if( f_write_desc(vgdid,unit)  == VGD_ERROR )then
            print*,'(F_vgd) ERROR: In write_desc, problem with f_write_desc'
            return
         endif
      ! Write to a Fortran binary file
      case ('BIN')
         tshape_CP = c_loc(tshape)
         call f_table_shape(vgdid, tshape_CP)
         ier = get_allocate('table_8',table_8,tshape,ALLOW_RESHAPE,'(BIN) in write_desc')         
         if (ier /= 0) then
            deallocate(table_8)
            return
         endif
         if( get_real8_3d(vgdid,'VTBL',table_8) == VGD_ERROR)then
            print*,'(F_vgd) ERROR: In write_desc, problem with get_real8_3d'
            return
         endif
         write(unit) tshape
         write(unit) table_8
      ! Warn user afor unknown format
      case DEFAULT
         write(for_msg,*) 'No write done for unknown format ',trim(myformat)
         call msg(MSG_WARNING,VGD_PRFX//for_msg)
      end select      
     ! Set status and return
      status = VGD_OK
      return
   end function write_desc   

   integer function get_int(vgdid,key,value,quiet) result(status)
       use vgrid_utils, only: up
      ! Retrieve the value of the requested instance variable
      integer, intent(in) :: vgdid                        !Vertical descriptor id
      character(len=*), intent(in) :: key                 !Descriptor key to retrieve
      integer,target, intent(out) :: value                !Retrieved value
      logical, optional, intent(in) :: quiet              !Do not print massages
      
      ! Internal variables      
      type(c_ptr) :: value_CP
      integer :: l_quiet
      character(len=KEY_LENGTH) :: my_key
      
      l_quiet = 0
      if (present(quiet))then
         if(quiet) then
            l_quiet = 1
         else
            l_quiet = 0
         endif         
      endif

      value_CP = c_loc(value)
      my_key=up(key(1:KEY_LENGTH))
      status = f_get_int(vgdid,my_key//C_NULL_CHAR, value_CP, l_quiet)
      
   end function get_int
 
   integer function get_int_1d(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: get_allocate,up,get_error
      ! Retrieve the value of the requested instance variable
      integer, intent(in) :: vgdid                        !Vertical descriptor id
      character(len=*), intent(in) :: key                 !Descriptor key to retrieve
      integer, dimension(:), pointer :: value             !Retrieved value
      logical, optional, intent(in) :: quiet              !Do not print massages
      
      ! Internal variables
      integer :: nl_, istat, level_msg, error, l_quiet
      type(c_ptr) :: value_CP, nk_CP
      character(len=KEY_LENGTH) :: my_key
      integer, target :: nk
      logical :: my_quiet

      nk_CP = c_loc(nk)

      ! Set error status
      status = VGD_ERROR

      level_msg = MSG_ERROR
      l_quiet = 0
      my_quiet = .false.
      if (present(quiet))then
         my_quiet = quiet
         if(quiet) then
            level_msg=MSG_QUIET
            l_quiet = 1
         endif         
      endif
      
      my_key=up(key(1:KEY_LENGTH))

      ! Map key name to derived-type element
      select case (my_key)
      case ('VIPM','VIP1')
         if (is_valid(vgdid,"ip1_m_valid")) then
            istat = get_int(vgdid,'NL_M',nl_)
            istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(VIPM in get_int_1d)')
            if (istat /= 0) return
            value_CP = c_loc(value(1))            
            status = f_get_int_1d(vgdid,my_key//C_NULL_CHAR,value_CP,nk_CP,l_quiet)
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case ('VIPT')
         istat = get_int(vgdid,'NL_T',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(VIPT in get_int_1d)')
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_int_1d(vgdid,my_key//C_NULL_CHAR,value_CP,nk_CP,l_quiet)
      case ('VIPW')
         istat = get_int(vgdid,'NL_W',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(VIPT in get_int_1d)')
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_int_1d(vgdid,my_key//C_NULL_CHAR,value_CP,nk_CP,l_quiet)
      case DEFAULT
         write(for_msg,*) 'invalid key '//trim(key)//' given to vgd_get (int 1D)'
         call msg(level_msg,VGD_PRFX//for_msg)
         return
      end select
      
      return
      
   end function get_int_1d
   
   integer function get_real(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: up
      ! Retrieve the value of the requested instance variable
      integer, intent(in) :: vgdid                   !Vertical descriptor id
      character(len=*), intent(in) :: key            !Descriptor key to retrieve
      real, target, intent(out) :: value             !Retrieved value
      logical, target, optional, intent(in) :: quiet !Do not print massages
      
      ! Internal variables
      integer :: l_quiet
      type(c_ptr) :: value_CP
      character(len=KEY_LENGTH) :: my_key

      value = VGD_MISSING      

      l_quiet = 0
      if (present(quiet))then
         if(quiet) then
            l_quiet = 1
         else
            l_quiet = 0
         endif         
      endif
      
      value_CP = c_loc(value)
      my_key=up(key(1:KEY_LENGTH))
      status = f_get_real(vgdid,my_key//C_NULL_CHAR,value_CP,l_quiet)
      
      return
   end function get_real
  

   integer function get_real_1d(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: get_allocate,up,get_error
      ! Retrieve the value of the requested instance variable
      integer, intent(in) :: vgdid                !Vertical descriptor id
      character(len=*), intent(in) :: key         !Descriptor key to retrieve
      real, dimension(:), pointer :: value        !Retrieved value
      logical, optional, intent(in) :: quiet      !Do not print massages

      ! Internal variables
      integer :: error,istat,level_msg,nl_
      integer, dimension(:), pointer :: vipt
      integer :: l_quiet
      type(c_ptr) :: value_CP
      character(len=KEY_LENGTH) :: my_key
      logical :: my_quiet

      ! Set error status
      status = VGD_ERROR
      
      nullify(vipt)

      level_msg=MSG_ERROR
      l_quiet = 0
      my_quiet = .false.
      if (present(quiet))then
         my_quiet = quiet
         if(quiet) then            
            level_msg=MSG_QUIET
            l_quiet = 1
         endif         
      endif
      
      my_key=up(key(1:KEY_LENGTH))
      
      ! Map key name to derived-type element
      select case (my_key)
      case ('VCDM')
         if (is_valid(vgdid, "ip1_m_valid")) then
            istat = get_int(vgdid,'NL_M',nl_)
            istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(VCDM in get_real_1d)')
            if (istat /= 0) return
            value_CP = c_loc(value(1))
            istat = f_get_real_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
            if (istat == VGD_ERROR) return
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case ('VCDT')
         istat = get_int(vgdid,'NL_T',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(VCDT in get_real_1d)')
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         istat = f_get_real_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
         if (istat == VGD_ERROR) return
      case ('VCDW')
         istat = get_int(vgdid,'NL_W',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(VCDW in get_real_1d)')
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         istat = f_get_real_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
         if (istat == VGD_ERROR) return
      case ('VCRD')
         if (is_valid(vgdid,"ip1_m_valid")) then
            write(for_msg,*) 'depricated key '//trim(key)//', use VCDM instead'
            call msg(level_msg,VGD_PRFX//for_msg)
            return
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case DEFAULT
         write(for_msg,*) 'invalid key '//trim(key)//' given to vgd_get (real 1D)'
         call msg(level_msg,VGD_PRFX//for_msg)
         return
      end select
      
      ! Set status and return
      status = VGD_OK
      return
   end function get_real_1d
   
   integer function get_real8(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: up
      integer, intent(in) :: vgdid                !Vertical descriptor id
      character(len=*), intent(in) :: key         !Descriptor key to retrieve
      real(kind=8), target, intent(out) :: value  !Retrieved value
      logical, optional, intent(in) :: quiet      !Do not print massages

      ! Internal variables
      integer :: l_quiet
      type(c_ptr) :: value_CP
      character(len=KEY_LENGTH) :: my_key

      ! Set error status
      status = VGD_ERROR
      
      value = VGD_MISSING
      
      l_quiet = 0
      if (present(quiet))then
         if(quiet) then
            l_quiet = 1
         else
            l_quiet = 0
         endif         
      endif
      
      value_CP = c_loc(value)
      my_key=up(key(1:KEY_LENGTH))
      status = f_get_real8(vgdid,my_key//C_NULL_CHAR,value_CP,l_quiet)
      
   end function get_real8

   integer function get_real8_1d(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: get_allocate,up,get_error
      ! Wrapper function to C f_get_real8_1d
      integer, intent(in) :: vgdid                   !Vertical descriptor id
      character(len=*), intent(in) :: key            !Descriptor key to retrieve
      real(kind=8), dimension(:), pointer :: value   !Retrieved value
      logical, target, optional, intent(in) :: quiet !Do not print massages
      
      ! Internal variables
      integer :: nl_, istat, level_msg, error, l_quiet
      type(c_ptr) :: value_CP
      character(len=KEY_LENGTH) :: my_key
      logical :: my_quiet

      status = VGD_ERROR

      level_msg=MSG_ERROR
      l_quiet = 0
      my_quiet = .false.
      if (present(quiet))then
         my_quiet = quiet 
         if(quiet) then
            level_msg=MSG_QUIET
            l_quiet = 1
         endif         
      endif

      my_key=up(key(1:KEY_LENGTH))

      select case (my_key)
      case ('CA_M','COFA')
         if (is_valid(vgdid,"a_m_8_valid")) then
            istat = get_int(vgdid,'NL_M',nl_)
            istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CA_M in get_real8_1d)')         
            if (istat /= 0) return
            value_CP = c_loc(value(1))
            status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case ('CB_M','COFB')
         if (is_valid(vgdid,"b_m_8_valid")) then
            istat = get_int(vgdid,'NL_M',nl_)
            istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CB_M in get_real8_1d)')         
            if (istat /= 0) return
            value_CP = c_loc(value(1))
            status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case ('CC_M')
         if (is_valid(vgdid,"c_m_8_valid")) then
            istat = get_int(vgdid,'NL_M',nl_)
            istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CC_M in get_real8_1d)')         
            if (istat /= 0) return
            value_CP = c_loc(value(1))
            status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case ('CA_T')
         istat = get_int(vgdid,'NL_T',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CA_T in get_real8_1d)')         
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)            
      case ('CC_T')
         if (is_valid(vgdid,"c_t_8_valid")) then
            istat = get_int(vgdid,'NL_T',nl_)
            istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CC_T in get_real8_1d)')         
            if (istat /= 0) return
            value_CP = c_loc(value(1))
            status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
         else
            error = int(get_error(key,my_quiet))
            return
         endif
      case ('CB_T')
         istat = get_int(vgdid,'NL_T',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CB_T in get_real8_1d)')         
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
      case ('CA_W')
         istat = get_int(vgdid,'NL_W',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CA_W in get_real8_1d)')         
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
      case ('CB_W')
         istat = get_int(vgdid,'NL_W',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CB_W in get_real8_1d)')         
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
      case ('CC_W')
         istat = get_int(vgdid,'NL_W',nl_)
         istat = get_allocate(key,value,nl_,ALLOW_RESHAPE,'(CC_W in get_real8_1d)')         
         if (istat /= 0) return
         value_CP = c_loc(value(1))
         status = f_get_real8_1d(vgdid,my_key//C_NULL_CHAR,value_CP,C_NULL_PTR,l_quiet)
      case DEFAULT
         write(for_msg,*) 'invalid key '//trim(key)//' given to vgd_get (real8 1D)'
         call msg(level_msg,VGD_PRFX//for_msg)
         return
      end select
      return      
   end function get_real8_1d

   integer function get_real8_3d(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: get_allocate,up
      ! Retrieve the value of the requested instance variable
      integer, intent(in) :: vgdid                !Vertical descriptor id
      character(len=*), intent(in) :: key         !Descriptor key to retrieve
      real(kind=8), dimension(:,:,:), pointer :: value !Retrieved value
      logical, optional, intent(in) :: quiet      !Do not print massages

      ! Internal variables
      integer :: istat,level_msg
      integer, target, dimension(3) :: tshape
      integer :: l_quiet
      type(c_ptr) :: value_CP, tshape_CP
      character(len=KEY_LENGTH) :: my_key


      ! Set error status
      status = VGD_ERROR

      level_msg=MSG_ERROR
      l_quiet = 0
      if (present(quiet))then
         if(quiet) then
            level_msg=MSG_QUIET
            l_quiet = 1
         endif         
      endif
       
      my_key=up(key(1:KEY_LENGTH))
      
      ! Map key name to derived-type element
      select case (my_key)
      case ('VTBL')
         tshape_CP = c_loc(tshape)
         call f_table_shape(vgdid, tshape_CP)
         istat = get_allocate(key,value,tshape,ALLOW_RESHAPE,'(VTBL) in get_real8_3d)')         
         if (istat /= 0) return
         value_CP = c_loc(value(1,1,1))
         status = f_get_real8_3d(vgdid,"VTBL"//C_NULL_CHAR,value_CP,C_NULL_PTR,C_NULL_PTR,C_NULL_PTR,l_quiet)         
      case DEFAULT
         write(for_msg,*) 'invalid key '//trim(key)//' given to vgd_get (real8 3D)'
         call msg(level_msg,VGD_PRFX//for_msg)
         return
      end select
      
      ! Set status and return
      status = VGD_OK
      return
   end function get_real8_3d
   
   integer function get_char(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: up
      integer, intent(in) :: vgdid                !Vertical descriptor id
      character(len=*), intent(in) :: key         !Descriptor key to retrieve
      character(len=*), intent(out) :: value      !Retrieved value
      logical, optional, intent(in) :: quiet      !Do not print massages
      
      ! Internal variables
      integer :: l_quiet
      character(len=KEY_LENGTH) :: my_key
      character(kind=c_char) :: my_char(100)
      integer :: i, nchar
      logical :: my_quiet, end_L

      ! Set error status
      status = VGD_ERROR
      value="";
      do i=1,100
         my_char(i)=' '
      end do
      l_quiet = 0
      my_quiet = .false.
      if (present(quiet))then
         my_quiet = quiet
         if(quiet) then
            l_quiet = 1
         else
            l_quiet = 0
         endif         
      endif
      my_key=up(key(1:KEY_LENGTH))
      status = f_get_char(vgdid, my_key//C_NULL_CHAR, my_char, l_quiet)
      select case(trim(my_key))
      case ('ETIK')
         nchar=VGD_LEN_ETIK
      case ('NAME')
         nchar=VGD_LEN_NAME
      case ('RFLD')
         nchar=VGD_LEN_RFLD
      case ('RFLS')
         nchar=VGD_LEN_RFLS
      case DEFAULT
         if( .not. my_quiet)then
            write(for_msg,*) 'invalid key in call to get_char: ',trim(key)
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         end if
         return
      end select
      end_L=.false.
      do i=1,nchar
         if(my_char(i) == C_NULL_CHAR .or. end_L)then
            value(i:i)=' '
            end_L=.true.
            cycle
         end if
         value(i:i)=my_char(i)         
      enddo
   end function get_char
    
   integer function get_logical(vgdid,key,value,quiet) result(status)
      use vgrid_utils, only: up
      ! Retrieve the value of the requested instance variable
      integer, intent(in) :: vgdid                        !Vertical descriptor id
      character(len=*), intent(in) :: key                 !Descriptor key to retrieve
      logical, intent(out) :: value                       !Retrieved value
      logical, optional, intent(in) :: quiet              !Do not print massages    
      
      ! Local variables
      integer :: l_quiet
      type(c_ptr) :: my_value_CP
      integer, target :: my_value
      character(len=KEY_LENGTH) :: my_key
      
      ! Set error status
      status = VGD_ERROR
      value = .false.
      
      l_quiet = 0
      if (present(quiet))then
         if(quiet) then
            l_quiet = 1
         else
            l_quiet = 0
         endif         
      endif
      my_value_CP = c_loc(my_value)
      my_key=up(key(1:KEY_LENGTH))
      if(f_get_int(vgdid,my_key//C_NULL_CHAR, my_value_CP, l_quiet) == VGD_ERROR)return         
      if(my_value.eq.1)value = .true.

      status = VGD_OK

   end function get_logical

   integer function put_int(vgdid, key, value) result(status)
      use vgrid_utils, only : up
      integer, intent(inout) :: vgdid               !Vertical descriptor id
      character(len=*), intent(in) :: key           !Descriptor key to set
      integer, target, intent(in) :: value          !Value to set

      ! Internal variables
      character(len=KEY_LENGTH) :: my_key
      status = VGD_ERROR
      my_key = up(key(1:KEY_LENGTH))
      if( f_put_int(vgdid, trim(my_key)//C_NULL_CHAR, value) == VGD_ERROR ) return      
      status = VGD_OK      
   end function put_int
   
   integer function put_real8_3d(vgdid, key, value) result(status)
      use vgrid_utils, only : up
      integer, target, intent(inout) :: vgdid       !Vertical descriptor id
      character(len=*), intent(in) :: key           !Descriptor key to set
      real(kind=8), dimension(:,:,:), pointer :: value !Value to set
      type(c_ptr) :: my_value_CP
      status = VGD_ERROR
      print*,'(F_vgd) ERROR: in put_real8_3d, putint real8 3D array is not implemented in this librairy'
      print*,'(F_vgd)        if you want to put the VGTB, you may do it by reconstructing a new '
      print*,'(F_vgd)        vgrid_descriptor e.g. vgd_new(vgdid,table_8)'
      print*,'(F_vgd)        If you think you really need to vgd_put VGTB contact dev teem'
      ! To silence the compiler warning
      if(associated(value))then
      endif
      ! To silence the compiler warning
      if(key == "")then
      endif
      return     
    end function put_real8_3d

   integer function put_char(vgdid,key,value) result(status)
      use vgrid_utils, only: up,put_error
      ! Set the value of the requested instance variable
      integer, intent(in) :: vgdid                !Descriptor id
      character(len=*), intent(in) :: key         !Descriptor key to set
      character(len=*), intent(in) :: value       !Value to set
      
      ! Internal variables
      character(len=KEY_LENGTH) :: my_key
      
      ! Set error status
      status = VGD_ERROR
    
      my_key = up(key(1:KEY_LENGTH))      

      status = f_put_char(vgdid, trim(my_key)//C_NULL_CHAR, trim(value)//C_NULL_CHAR)

   end function put_char

   logical function is_valid(vgdid,element_valid_S) result(valid)
      ! Check for validity of the element
      integer :: vgdid          !Vertical descriptor id
      character(len=*) :: element_valid_S
      valid =  f_is_valid(vgdid,element_valid_S//C_NULL_CHAR) == 1
      return
   end function is_valid

   integer function my_fstprm(fstkey,record) result(status)
      ! Use fstprm function to get information about the record
      integer, intent(in) :: fstkey               !Key from FST file record
      type(FSTD_ext) :: record                    !Record information
      ! Local variables
      integer :: error
      integer, external :: fstprm,fstinf
      real(kind=8) :: nhours
      status = VGD_ERROR
      error=fstprm(fstkey,record%dateo,record%deet,record%npas, &
           record%ni,record%nj,record%nk,record%nbits,record%datyp,record%ip1,record%ip2, &
           record%ip3,record%typvar,record%nomvar,record%etiket,record%grtyp, &
           record%ig1,record%ig2,record%ig3,record%ig4,record%swa, &
           record%lng,record%dltf,record%ubc,record%extra1,record%extra2, &
           record%extra3)
      if (error < 0) then
         write(6,*) 'cannot fstprm for fstkey ',fstkey
         return
      end if
      nhours=record%deet*record%npas/3600.d0
      ! Note incdatr prints the following when dateo is zero :
      !    label 1,idate2:           0
      if (record%dateo == 0 )then
         record%datev=0
      else
         call incdatr(record%datev,record%dateo,nhours)
      endif
      status = VGD_OK
   end function my_fstprm

   integer function get_ref (F_f,vgdid,F_name_S,F_unit,prm) result(status)
    
    implicit none
    
    real, dimension(:,:), pointer, intent(inout) :: F_f
    integer, intent(in) :: vgdid !Vertical descriptor id
    character(len=*) :: F_name_S
    integer, intent(in) :: F_unit
    type(FSTD_ext), intent(in) :: prm
    
    ! Local variables
    integer :: sfc_key,fstinf,fstluk,ni,nj,nk,istat,match_ipig,ip1,ip2
    type(FSTD_ext) :: prm_p0

    ! Set error status
    status = VGD_ERROR
    
    sfc_key = fstinf(F_unit,ni,nj,nk,prm%datev,prm%etiket,-1,prm%ip2,prm%ip3,' ',F_name_S)
    if(sfc_key < 0)then
       write(for_msg,*) 'cannot find ',F_name_S,' for :'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       write(for_msg,*) 'datev=',prm%datev,' etiket=',prm%etiket,' ip2=',prm%ip2,' ip3=',prm%ip3
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    istat=my_fstprm(sfc_key,prm_p0)
    if(prm_p0%ni.ne.prm%ni.or.prm_p0%nj.ne.prm%nj)then
       write(for_msg,*) 'horizontal grid mismatch for '//trim(F_name_S),ni,nj,' vs',prm%ni,prm%nj
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if ( vgd_get(vgdid,"MIPG match !! IPs and IGs with records",match_ipig) == VGD_ERROR )then
       write(for_msg,*) 'ERROR in get_ref with vgd_get on MIPG'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if (match_ipig == 1) then
       if ( vgd_get(vgdid,"IP_1",ip1) == VGD_ERROR )then
          write(for_msg,*) 'ERROR in get_ref with vgd_get on IP_1'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if ( vgd_get(vgdid,"IP_2",ip2) == VGD_ERROR )then
          write(for_msg,*) 'ERROR in get_ref with vgd_get on IP_2'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if (prm_p0%ig1 /= ip1 .or. prm_p0%ig2 /= ip2) then
          write(for_msg,*) 'sfc_field ig1 ig2 do not correspond to the correct grid descriptor'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,'("   expecting (ip1,ip2)->(",i8,",",i8,"), got (ig1,ig2)->(",i8,",",i8,")")')&
               ip1,ip2,prm_p0%ig1,prm_p0%ig2
          call msg(MSG_ERROR,VGD_PRFX//for_msg)                            
          return
       endif
    endif
    if(associated(F_f))deallocate(F_f)
    allocate(F_f(ni,nj),stat=istat)
    if (istat /= 0) then
       nullify(F_f)
       write(for_msg,*) 'cannot allocate space in get_ref for ',trim(F_name_S) 
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    istat = fstluk(F_f,sfc_key,ni,nj,nk)
    if(istat < 0 )then
       write(for_msg,*) 'problem with fstluk '//trim(F_name_S)//' in get_ref'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       deallocate(F_f)
       return
    endif
    if ( trim(F_name_S) == 'P0' .or. &
         trim(F_name_S) == 'P0LS') F_f = F_f*100. !convert mb to Pa
    
    ! Set error status
    status = VGD_OK

 end function get_ref
 integer function vgd_standard_atmosphere_1976(vgdid, ip1s, val, var, sfc_temp, sfc_pres) result(status)
    ! Kept only for backward compatibitity with vgrid 6.3
    integer, intent(in) :: vgdid         !Vertical descriptor id
    integer, dimension(:), target, intent(in) :: ip1s  !ip1 list to get value for
    real, dimension(:), pointer, intent(inout) :: val  !Standard Atmosphere 1976 value for ip1s
    character(len=*), intent(in) :: var                !Variable name to get valid choice are "TEMPERATURE", "PRESSURE"
    real, optional, target, intent(in) :: sfc_temp     !Use this surface temperature for standard atmosphere
    real, optional, target, intent(in) :: sfc_pres     !Use this surface pressure for standard atmosphere
    ! Local variables
    if(present(sfc_temp) .and. present(sfc_pres))then
       status=vgd_stda76(vgdid, ip1s, val, var, sfc_temp=sfc_temp, sfc_pres=sfc_pres)
       return
    endif
    if(present(sfc_temp))then
       status=vgd_stda76(vgdid, ip1s, val, var, sfc_temp=sfc_temp)
       return
    endif
    if(present(sfc_pres))then
       status=vgd_stda76(vgdid, ip1s, val, var, sfc_pres=sfc_pres)
       return
    endif       
 end function vgd_standard_atmosphere_1976
 
 integer function vgd_stda76(vgdid, ip1s, val, var, sfc_temp, sfc_pres) result(status)
   use vgrid_utils, only: get_allocate, up
   integer, intent(in) :: vgdid         !Vertical descriptor id
   integer, dimension(:), target, intent(in) :: ip1s  !ip1 list to get value for
   real, dimension(:), pointer, intent(inout) :: val  !Standard Atmosphere 1976 value for ip1s
   character(len=*), intent(in) :: var                !Variable name to get valid choice are "TEMPERATURE", "PRESSURE"
   real, optional, target, intent(in) :: sfc_temp     !Use this surface temperature for standard atmosphere
   real, optional, target, intent(in) :: sfc_pres     !Use this surface pressure for standard atmosphere
   ! Local variables
   type (c_ptr) :: ip1s_CP, val_CP, sfc_pres_CP, sfc_temp_CP
   status = VGD_ERROR
   if( get_allocate('val',val,size(ip1s),ALLOW_RESHAPE,'(in stda76)') /= 0) then
      if(associated(val))deallocate(val)
      return
   endif
   ip1s_CP = c_loc(ip1s)
   val_CP = c_loc(val)
   if( present(sfc_temp) )then
      sfc_temp_CP = c_loc(sfc_temp)
   else
      sfc_temp_CP = C_NULL_PTR
   endif
   if( present(sfc_pres) )then
      sfc_pres_CP = c_loc(sfc_pres)
   else
      sfc_pres_CP = C_NULL_PTR
   endif
   select case (trim(up(var)))
   case ('TEMPERATURE')
      if (present(sfc_temp) .or. present(sfc_pres) )then
          write(for_msg,*) 'ERROR with vgd_stda76_temp, option sfc_temp and/or sfc_pres not implemented for TEMPERATURE.'&
               //'Please contact the vgrid dev team.'
          call msg(MSG_ERROR,VGD_PRFX//for_msg) 
          return
      endif
      if( f_stda76_temp(vgdid, ip1s_CP, size(val), val_CP) /= VGD_OK) then
         write(for_msg,*) 'ERROR with vgd_stda76_temp'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)       
         return
      endif
   case ('PRESSURE')
      if( f_stda76_pres(vgdid, ip1s_CP, size(val), val_CP, sfc_temp_CP, sfc_pres_CP ) /= VGD_OK) then
         write(for_msg,*) 'ERROR with vgd_stda76_pres'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)       
         return
      endif
   case DEFAULT
      write(for_msg,*) 'invalid variable name given to vgd_stda76',trim(var)
      call msg(MSG_ERROR,VGD_PRFX//for_msg)
      return
   end select
   status = VGD_OK
 end function vgd_stda76
 integer function vgd_stda76_pres_from_hgts_list(pres, hgts, nb) result(status)
   implicit none
   integer :: nb
   real, dimension(nb), target :: pres, hgts
   ! Local variables
   type (c_ptr) :: pres_CP, hgts_CP

   status = VGD_ERROR
   
   pres_CP = c_loc(pres)
   hgts_CP = c_loc(hgts)   
   if( f_stda76_pres_from_hgts_list(pres_CP, hgts_CP, nb) &
        == VGD_ERROR)return
   status = VGD_OK
   return

 end function vgd_stda76_pres_from_hgts_list
 integer function vgd_stda76_hgts_from_pres_list(hgts, pres, nb) result(status)
   implicit none
   integer :: nb
   real, dimension(nb), target :: hgts, pres
   ! Local variables
   type (c_ptr) :: hgts_CP, pres_CP

   status = VGD_ERROR
   
   hgts_CP = c_loc(hgts)
   pres_CP = c_loc(pres)   
   if( f_stda76_hgts_from_pres_list(hgts_CP, pres_CP, nb) &
        == VGD_ERROR)return
   status = VGD_OK
   return

 end function vgd_stda76_hgts_from_pres_list

end module vGrid_Descriptors
