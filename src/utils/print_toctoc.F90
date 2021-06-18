program print_toctoc
   !
   use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_print,vgd_get,vgd_putopt,VGD_ERROR
   !
   implicit none
   !
   type(vgrid_descriptor) :: vgd
   integer, parameter :: lu=10,ncle=7
   integer :: stat,npos,i,noptions
   integer :: fnom,fstouv,fstfrm,exdb,exfin,kind,lu_out
   integer, dimension(:), pointer :: ip1s
   character(len=12), parameter :: version='v_2.1.0'
   character(len=256), dimension(ncle) :: cle,val,def
   logical :: ip1m_only_L, ip1t_only_L, box_L, convip_L
#include "vgrid_version.hf"
   !
   !==========================================================================
   !   
   ! Get keys
   cle=(/'fst.     ','ip1m_only','ip1t_only','kind     ','no_box   ','out.     ','convip   '/)
   val=(/'undef    ','no       ','no       ','undef    ','no       ','undef    ','no       '/)
   def=(/'undef    ','yes      ','yes      ','undef    ','yes      ','undef    ','yes      '/)
   npos=-111
   call ccard(cle,def,val,ncle,npos) 
   !
   nullify(ip1s)
   !
   noptions=0
   ip1m_only_L=.false.
   ip1t_only_L=.false.
   box_L=.true.
   lu_out=6
   if(val(5).eq.'YES')box_L=.false.
   !
   if(trim(val(1)).eq.'undef')then
      stat=exdb('r.print_toctoc',version,'NON')
   write(6,'("   * ",a)')vgrid_descriptors_version
   write(6,'("   ******************************************************************************************************")')
      print*,'Usage   : r.print_toctoc -fst rpn_file'
      print*,'Options : -ip1m_only get -> ip1 list on momentum levels only'
      print*,'          -ip1T_only get -> ip1 list on thermo   levels only'
      print*,'          -kind kind     -> print !! for level kind=kind only' 
      print*,'          -no_box        -> do not print top and bottum boxes'
      print*,'          -out file_out  -> print in file file_out (works only with options -ip1m_only or -ip1T_only)'
      print*,'          -convip        -> also print real value (p) associated to ip1'
      call exit(1)
   endif
   !
   if(trim(val(2)).eq.'YES')then
      noptions=noptions+1
      ip1m_only_L=.true.
      box_L=.false.
   endif
   if(trim(val(3)).eq.'YES')then
      noptions=noptions+1
      ip1t_only_L=.true.
      box_L=.false.
   endif
   kind=-1
   if(trim(val(4)).ne.'undef')then
      read(val(4),*)kind
      print*,'Note : printing only level information for kind',kind
   endif
   !
   if(noptions.gt.1)then
      print*,'Only one of the following options can be set:'
      print*,'-ip1m_only -ip1t_only -nml'
      call exit(1)
   endif
   !
   if(trim(val(6)).ne.'undef')then
      lu_out=10
      open(unit=lu_out,file=trim(val(6)),status='unknown')
   endif
   convip_L=.false.
   if(trim(val(7)).eq.'YES')then
      convip_L=.true.
   endif
   !
   if(box_L)then
      stat=exdb('r.print_toctoc',version,'NON')
      write(6,'("   * ",a)')vgrid_descriptors_version
      write(6,'("   ******************************************************************************************************")')
   endif
   !
   !==========================================================================
   !
   stat=fnom(lu,val(1),"RND+R/O",0)
   if(stat.lt.0)then
      print*,'ERROR with fnom on',val(1)
      call exit(1)
   endif
   stat=fstouv(lu,'RND')
   if(stat.lt.0)then
      print*,'ERROR with fstouv on',val(1)
      call exit(1)
   endif
   !
   stat = vgd_putopt("ALLOW_SIGMA",.true.)
   stat = vgd_new(vgd,lu,'fst',kind=kind)
   if(stat.eq.VGD_ERROR)then
      print*,'ERROR with vgd_new on',val(1)
      call exit(1)
   endif
   if(ip1m_only_L)then
      stat=vgd_get(vgd,'VIPM - level ip1 list (m)',ip1s)
      if(stat.eq.VGD_ERROR)then
         print*,'ERROR with vgd_get on','VIPM'
         call exit(1)
      endif
      do i=1,size(ip1s)
         write(lu_out,'(i10)')ip1s(i)
      enddo
   elseif(ip1t_only_L)then      
      stat=vgd_get(vgd,'VIPT - level ip1 list (t)',ip1s)
      if(stat.eq.VGD_ERROR)then
         print*,'ERROR with vgd_get on','VIPT'
         call exit(1)
      endif
      do i=1,size(ip1s)
         write(lu_out,'(i10)')ip1s(i)
      enddo
   else
      stat = vgd_print(vgd,6,convip_L)
      if(stat.eq.VGD_ERROR)then
         print*,'ERROR with vgd_print'
         call exit(1)
      endif
      if(box_L)stat=exfin('r.print_toctoc',version,'NON')
   endif
   !
   stat=fstfrm(lu)
   close(lu_out)
   !
end program print_toctoc
