program add_toctoc
   use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_write,vgd_print,vgd_putopt,VGD_ERROR
   implicit none
   integer, parameter :: ncle=5,lui=10,nmax=1000
   integer, dimension(nmax) :: liste
   integer :: fnom,fstouv,infon,fstinl,fstfrm,ind_to_erase
   integer :: npos,istat,luo,exdb,exfin,ni,nj,nk,kind
   character(len=256), dimension(ncle) :: cle,val,def
   logical :: samefile_L, allow_sigma_L
   character(len=12), parameter :: version='v_2.2.0'
   type(vgrid_descriptor) :: vgd
#include "vgrid_version.hf"
  
   cle=(/'s.         ','d.         ','kind       ','samefile   ','allow_sigma'/)
   val=(/'undef      ','undef      ','undef      ','NO         ','NO         '/)
   def=(/'undef      ','undef      ','undef      ','YES        ','YES        '/)

   ind_to_erase=0

   !==========================================================================

   istat=exdb('r.add_toctoc',version,'NON')
   write(6,'("   * ",a)')vgrid_descriptors_version
   write(6,'("   ******************************************************************************************************")')

   !==========================================================================
   ! Get keys
   npos=-111
   call ccard(cle,def,val,ncle,npos) 
   
   if(val(1).eq.'undef')then
      call help(version)
      stop
   endif
   
   if(val(2).eq.'undef'.and.val(3).eq.'NO')then
      call help(version)
      stop
   endif
   
   kind=-1
   if(val(3).ne.'undef')then
      read(val(3),*)kind
      print*,'Note : only adding level descriptor for kind',kind
   endif  

   samefile_L=val(4).eq.'YES'

   allow_sigma_L = trim(val(5)) == "YES"
   print*,'  Construction of sigma coordinate is allowed'

   !==========================================================================
   ! Open files
   if(samefile_L)then
      istat=fnom(lui,val(1),'RND',0)
   else
      istat=fnom(lui,val(1),'RND+R/O',0)
   endif
   if(istat.lt.0)then
      print*,'ERROR with fnom on file ',trim(val(1))
      call exit(1)
   endif
   istat=fstouv(lui,'RND')
   if(istat.le.0)then
      print*,'Error : no record in RPN file ',trim(val(1))
      istat=fstfrm(lui)
      call exit(1)
   endif

   !==========================================================================
   ! Look for !!SF if present then tell user to use convert_toctoc_5002
   istat=fstinl(lui,ni,nj,nk,-1,' ',-1,-1,-1,' ','!!SF',liste,infon,nmax)
   if(infon.gt.0)then
      print*,'ERROR record !!SF is present, use convert_toctoc_5002 instead'
      call exit(1)
   endif
   !
   istat=fstinl(lui,ni,nj,nk,-1,' ',-1,-1,-1,' ','!!',liste,infon,nmax)
   if(infon.gt.0)then
      print*,'Record !! already there, noting to do'
      goto 999
   endif
   !
   ! Open output file
   if(samefile_L)then
      luo=lui
   else
      luo=lui+1
      istat=fnom(luo,val(2),'RND',0)
      if(istat.lt.0)then
         print*,'ERROR with fnom on file ',trim(val(2))
         call exit(1)
      endif
      istat=fstouv(luo,'RND')
      if(istat.lt.0)then
         print*,'Error : problem with fstouv on ',trim(val(2))
         istat=fstfrm(luo)
         call exit(1)
      endif 
   endif

   istat = vgd_putopt("ALLOW_SIGMA",allow_sigma_L)
   if(istat.eq.VGD_ERROR)then
      print*,'Error with vgd_putopt on ALLOW_SIGMA'
      call exit(1)
   endif

   istat=vgd_new(vgd,lui,kind=kind)
   if(istat.eq.VGD_ERROR)then
      print*,'Error with vgd_new'
      call exit(1)
   endif

   istat=vgd_print(vgd)
   if(istat.eq.VGD_ERROR)then
      print*,'Error with vgd_print'
      call exit(1)
   endif

   istat=vgd_write(vgd,luo)
   if(istat.eq.VGD_ERROR)then
      print*,'Error with vgd_print'
      call exit(1)
   endif

   !==========================================================================
   ! close file(s)
   if(.not.samefile_L)istat=fstfrm(luo)
999 continue
   istat=fstfrm(lui)
   istat=exfin('r.add_toctoc',version,'NON')
end program add_toctoc
subroutine help (version)
   implicit none
   character(len=*) :: version
   integer :: istat,exfin
      print*,''
      print*,'  Usage  :  r.add_toctoc -s source_file -d destination_file'
      print*,'  Options:  -kind        narrow selection of ip1 of kind kind in reconstruction'
      print*,'                           This is handy, and some times necessairy when there'
      print*,'                           are mixed kind in the input file'
      print*,'           -samefile     put constructed !! in input file (-s input_file),'
      print*,'                         do not supply argument -d with this option'
      print*,'           -allow_sigma  allow construction of sigma levels, default to false'
      istat=exfin('r.add_toctoc',version,'NON')
end subroutine help
