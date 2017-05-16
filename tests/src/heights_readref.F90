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
program heights_readref

  use vGrid_Descriptors, only: vgd_putopt, VGD_OK, VGD_ERROR
  use Unit_Testing, only: ut_report
  
  implicit none
  integer :: stat, ier, chek_heights_readref
  
  stat = VGD_OK
  
  ier = vgd_putopt("ALLOW_SIGMA", .true.)

  ier = chek_heights_readref('data/dm_5005_from_model_run_with_GZ', "", "UU")
  if( ier == VGD_ERROR ) stat = VGD_ERROR
  
  call ut_report(stat,'Grid_Descriptors, vgd_new')
  
end program heights_readref
!====================================================================
!====================================================================
!====================================================================

integer function chek_heights_readref(F_fst_S, F_ips_S, F_nomvar_S) result(status)

   use vGrid_Descriptors, only: vgrid_descriptor, vgd_new, vgd_heights, vgd_putopt, VGD_ERROR, VGD_OK

   implicit none  

   character(len=*) :: F_fst_S, F_ips_S, F_nomvar_S

   ! Local variables
   integer, save :: lu=10   
   integer :: ier, fnom, fstouv, fstfrm, fstinl, fstprm, lutxt=69, count, i
   integer, dimension(:), pointer :: ip1s, fstkeys
   type(vgrid_descriptor) :: vgd
   real, dimension(:,:,:), pointer :: heights
   ! Variable for fstprm, sorry...
   integer ::dateo, datyp, deet, dltf, extra1, extra2, extra3, ig1,&
        ig2, ig3, ig4, ip1, ip2, ip3, lng, nbits,&
        ni,  nj, nk, npas, swa, ubc
   character(len=12) :: etiket
   character(len=4)  :: nomvar
   character(len=2)  :: typvar
   character(len=1)  :: grtyp

   status=VGD_ERROR   

   nullify(ip1s, fstkeys, heights)

   ier=vgd_putopt('ALLOW_RESHAPE',.true.)

   lu=lu+1

   ier = fnom(lu,F_fst_S,"RND+R/O",0)
   if(ier.lt.0)then
      print*,'ERROR with fnom on ',trim(F_fst_S)
      return
   endif
   ier = fstouv(lu,'RND')
   if(ier.le.0)then
      print*,'No record in RPN file ',trim(F_fst_S),ier
      return
   endif
   allocate(fstkeys(ier))

   if(trim(F_ips_S).eq.'')then
      ip1=-1; ip2=-1     
   else
      open(unit=lutxt,file=F_ips_S,status='OLD')
      read(lutxt,*) ip1,ip2
      close(lutxt)
   endif

   ! Get vertical grid descriptor
   ier = vgd_new(vgd,unit=lu,format="fst",ip1=ip1,ip2=ip2)
   if(ier == VGD_ERROR )then
      print*,'Problem getting vertical grid descriptor'
      return
   endif

   ! Get variable ip1 list
   ier = fstinl(lu,ni,nj,nk,-1,' ',-1,-1,-1,' ',F_nomvar_S, fstkeys, count, size(fstkeys))
   if ( count <= 0 )then
      print*,'ERROR: cannot find any ',trim(F_nomvar_S)
      return
   endif

   allocate(ip1s(count))
   
   do i=1,count
      ier = fstprm(fstkeys(i),dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1s(i), &
           ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,  &
           ubc,extra1,extra2,extra3)
   enddo   
   print*,'ip1s',ip1s

   if ( vgd_heights(vgd,lu,ip1s,heights) == VGD_ERROR )then
      print*,'ERROR with vgd_heights'
      return
   endif

   deallocate(ip1s, fstkeys, heights)
   
   ier=fstfrm(lu)

   print*,trim(F_fst_S),' is OK'

   status=VGD_OK
   
 end function chek_heights_readref
