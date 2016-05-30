!/* libdescrip - Vertical grid descriptor library for FORTRAN programming
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
program levels_withref_profile_all

  use vGrid_Descriptors, only: vgd_putopt,VGD_OK,VGD_ERROR
  use Unit_Testing, only: ut_report
  
  implicit none
  integer :: stat,ier,chek_levels_withref

  stat=VGD_OK

  ier =  vgd_putopt("ALLOW_SIGMA",.true.)
  ier=chek_levels_withref('data/dm_1001_from_model_run','','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR
  ier =  vgd_putopt("ALLOW_SIGMA",.false.)

  ier=chek_levels_withref('data/dm_1002_from_model_run','','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR

  ier=chek_levels_withref('data/dm_5001_from_model_run','','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR

  ier=chek_levels_withref('data/dm_5002_from_model_run','data/dm_5002_ips.txt','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR
  ier=chek_levels_withref('data/dm_5002_from_model_run','data/dm_5002_ips.txt','UU')
  if(ier==VGD_ERROR)stat=VGD_ERROR
  
  ier=chek_levels_withref('data/dm_5003_from_model_run','data/dm_5003_ips.txt','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR
  ier=chek_levels_withref('data/dm_5003_from_model_run','data/dm_5003_ips.txt','UU')
  if(ier==VGD_ERROR)stat=VGD_ERROR
  
  ier=chek_levels_withref('data/dm_5004_from_model_run','data/dm_5004_ips.txt','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR
  ier=chek_levels_withref('data/dm_5004_from_model_run','data/dm_5004_ips.txt','UU')
  if(ier==VGD_ERROR)stat=VGD_ERROR

  ier=chek_levels_withref('data/dm_5005_from_model_run','data/dm_5005_ips.txt','TT')
  if(ier==VGD_ERROR)stat=VGD_ERROR

  ier=chek_levels_withref('data/dm_5005_from_model_run','data/dm_5005_ips.txt','UU')
  if(ier==VGD_ERROR)stat=VGD_ERROR

  call ut_report(stat,'Grid_Descriptors, vgd_new')

end program levels_withref_profile_all
!====================================================================
!====================================================================
!====================================================================

integer function chek_levels_withref(F_fst,F_ips,F_var) result(status)

   use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_levels,VGD_ERROR,VGD_OK

   implicit none  

   character(len=*) :: F_fst,F_ips,F_var

   ! Local variables
   integer, save :: lu=10   
   integer :: fnom,fstouv,fstfrm,lutxt=69,kind
   type(vgrid_descriptor) :: d
   integer, parameter :: nmax=1000
   integer, dimension(nmax) :: liste
   integer :: ier,fstinl,fstprm,fstinf,fstluk,infon,i,j,k
   real, dimension(:), pointer :: pres
   real*8, dimension(:), pointer :: pres_8
   real, dimension(:,:), pointer :: p0,px
   real*8, dimension(:,:), pointer :: p0_8
   real :: epsilon=5.0e-6,pppp,p0_point
   real*8 :: p0_point_8
   integer, dimension(:), pointer :: ip1s
   logical :: ok
   ! Variable for fstprm, sorry...
   integer ::dateo, datev, datyp, deet, dltf, extra1, extra2, extra3, ig1,&
        ig2, ig3, ig4, ip1, ip2, ip3, iun, key, lng, nbits,&
        ni,  nj, nk, npak, npas, swa, ubc
   character(len=12) :: etiket
   character(len=4)  :: nomvar
   character(len=2)  :: typvar
   character(len=1)  :: grtyp, ctype, dummy_S
   logical :: rewrit
   
   nullify(pres,pres_8,p0,px,p0_8,ip1s)

   status=VGD_ERROR   

   print*,'================================================================'
   print*,'Testing, level: ',trim(F_var),' on file ',trim(F_fst)

   lu=lu+1

   ier=fnom(lu,F_fst,"RND",0)
   if(ier.lt.0)then
      print*,'ERROR with fnom on ',trim(F_fst)
      return
   endif
   ier=fstouv(lu,'RND')
   if(ier.le.0)then
      print*,'No record in RPN file ',trim(F_fst),ier
      return
   endif
   if(trim(F_ips).eq.'')then
      ip1=-1; ip2=-1     
   else
      open(unit=lutxt,file=F_ips,status='OLD')
      read(lutxt,*) ip1,ip2
      close(lutxt)
   endif

   ! Get vertical grid descriptor
   ier = vgd_new(d,unit=lu,format="fst",ip1=ip1,ip2=ip2)
   if(ier == VGD_ERROR )then
      print*,'Problem getting vertical grid descriptor'
      print*,'FILE: ',trim(F_fst)
      return
   endif
   
   ier = fstinl(lu,ni,nj,nk,-1,' ',-1,-1,-1,' ',F_var,liste,infon,nmax)
   if(ier>0)then
      print*,'ERROR: problem with fstinl on ',F_var
      return
   endif
   if(infon == 0 )then
      print*,'pas de record de ',F_var
      print*,'FILE: ',trim(F_fst)
      return
   endif
   allocate(ip1s(infon))
   do k=1,infon
      ier = fstprm(liste(k),dateo,deet,npas,ni,nj,nk,nbits,datyp, &
           ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,lng, &
           dltf,ubc,extra1,extra2,extra3)
      ip1s(k)=ip1
   end do
   
   key = fstinf(lu,ni,nj,nk,-1,' ',-1,-1,-1,' ','P0')
   allocate(p0(ni,nj),px(ni,nj))
   ier = fstluk(p0,key,ni,nj,nk)
   if(ier.lt.0)then
      print*,'Problem with fstluk on P0'
      print*,'FILE: ',trim(F_fst)
      return
   endif
   p0_point=p0(1,1)*100.
   allocate(p0_8(ni,nj))
   p0_point_8=p0(1,1)*100.

   ! Test 32 bits interface
   ier = vgd_levels(d,ip1s,pres,p0_point)
   if(ier == VGD_ERROR )then
      print*,'Problem with vgd_levels 32 bits'
      print*,'FILE: ',trim(F_fst)
      return
   endif
   do k=1,infon
      ier = fstprm(liste(k),dateo,deet,npas,ni,nj,nk,nbits,datyp, &
           ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,lng, &
           dltf,ubc,extra1,extra2,extra3)
      if(ier.lt.0)then
         print*,'Problem with fstprm on ',F_var,', ip1=',ip1
      print*,'FILE: ',trim(F_fst)
         return
      endif
      ! Surface pressure must be equal to P0 for surface levels
      call convip(ip1,pppp,kind,-1,dummy_S,.false.)
      if(abs(pppp-1.).lt.epsilon)then
         if(pres(k).ne.p0_point)then
            print*,'Surface pressure must be exacly equal to p0'
            print*,'k,pres(k),p0_point',k,pres(k),p0_point
            print*,'FILE: ',trim(F_fst)
            return
         endif
      endif
      call incdatr(datev,dateo,deet*npas/3600.d0)
      key=fstinf(lu,ni,nj,nk,datev,' ',ip1,ip2,-1,typvar,'PX')     
      ier = fstluk(px,key,ni,nj,nk)
      if(ier.lt.0)then
         print*,'Problem with fstinf on PX, ip1=',ip1
         print*,'FILE: ',trim(F_fst)
         return
      endif
      if(abs((px(1,1)-pres(k)/100.)/px(1,1))>epsilon)then
         print*,'32 bits: Difference in pressure is too large at'
         print*,'px(1,1),pres(k)/100.',k,px(1,1),pres(k)/100.
         print*,'File: ',F_fst
         return
      endif
   enddo
   
   ! Test 64 bits interface
   ier = vgd_levels(d,ip1s,pres_8,p0_point_8)
   if(ier == VGD_ERROR )then
      print*,'Problem with vgd_levels 64 bits'
      print*,'FILE: ',trim(F_fst)
      return
   endif 
   do k=1,infon
      ier = fstprm(liste(k),dateo,deet,npas,ni,nj,nk,nbits,datyp, &
           ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,lng, &
           dltf,ubc,extra1,extra2,extra3)
      if(ier.lt.0)then
         print*,'Problem with fstprm on ',F_var,' ip1=',ip1
         print*,'FILE: ',trim(F_fst)
         return
      endif
      ! Surface pressure must be equal to P0 for surface levels
      call convip(ip1,pppp,kind,-1,dummy_S,.false.)
      if(abs(pppp-1.).lt.epsilon)then
         if(pres_8(k).ne.p0_point_8)then
            print*,'Surface pressure must be exacly equal to p0_8'
            print*,'File:',F_fst
            print*,'k,pres_8(k),p0_point_8',k,pres_8(k),p0_point_8
            return
         endif
      endif
      call incdatr(datev,dateo,deet*npas/3600.d0)
      key=fstinf(lu,ni,nj,nk,datev,' ',ip1,ip2,-1,typvar,'PX')     
      ier = fstluk(px,key,ni,nj,nk)
      if(ier.lt.0)then
         print*,'Problem with fstinf on PX, ip1=',ip1
         print*,'FILE: ',trim(F_fst)
         return
      endif
      if(abs((px(1,1)-pres_8(k)/100.d0)/px(1,1))>epsilon)then
         print*,'64 bits: Difference in pressure is too large at'
         print*,'k,px(1,1),pres_8(k)/100.d0',k,px(1,1),pres_8(k)/100.d0
         print*,'FILE: ',trim(F_fst)
         return
      endif
   enddo
   deallocate(ip1s,px,p0,p0_8,pres,pres_8)
   
   print*,'   TEST OK!'

   ier=fstfrm(lu)

   status=VGD_OK   
   
end function chek_levels_withref
