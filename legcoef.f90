!
! legcoef.f90
!
! Copyright 2018 Yutaka Masuda
!
! Redistribution and use in source and binary forms, with or without modification,
! are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, this
!    list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation and/or
!    other materials provided with the distribution.
! 3. Neither the name of the copyright holder nor the names of its contributors may
!    be used to endorse or promote products derived from this software without
!    specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
! IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
! INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
! LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
! OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
! OF THE POSSIBILITY OF SUCH DAMAGE.
!
program legcoef
   use iso_fortran_env
   implicit none
   character(len=16) :: fmt
   character(len=128) :: file
   character(len=1024) :: line,outstr
   integer :: pos,order,prec
   real(real64) :: tmin,tmax
   logical :: compat

   character(len=16),allocatable :: items(:)
   integer :: i,j,k,n,nread,unit,io
   real(real64) :: t,q
   real(real64),allocatable :: L(:,:),x(:),y(:)

   if(command_argument_count()<1) then
      call show_usage()
   end if
   call get_commandline_options(file,pos,order,tmin,tmax,prec,compat)

   if(file(1:1)=='-') then
      unit = input_unit
   else
      open(newunit=unit,file=file,status='old',iostat=io)
      if(io/=0) call error_stop('no file found: '//trim(file))
   end if

   n = order+1
   allocate(L(n,n),x(n),y(n))
   call Leg(n,L,trans=.true.)
   if(compat) L=sqrt(2.0d0)*L

   write(fmt,"(A,I0,A,I0,A)") '(*(1X,G',0,'.',prec,'))'

   allocate(items(pos))
   nread = 0
   do
      read(unit,'(A)',iostat=io) line
      if(is_iostat_end(io)) exit

      call split(line,k,items)
      if(k<pos) call error_stop('no value at the position')
      read(items(pos),*,iostat=io) t
      if(io/=0) call error_stop('error in conversion of t: '//trim(items(pos)))
      if(t<tmin .or. t>tmax) call error_stop('t out of range: '//trim(items(pos)))
      q = -1 + 2*(t-tmin)/(tmax-tmin)
      x(1:n) = [(q**i, i=0,order)]
      y = matmul(L,x)
      write(outstr,fmt,iostat=io) y
      if(io/=0) call error_stop('formatting error in converted values')
      write(output_unit,'(A,A)') trim(line),trim(outstr)
   end do

   if(unit/=input_unit) close(unit)
   deallocate(L,x,y,items)
   stop

contains

subroutine show_usage()
   print '(A)',' usage: legcoef [options] input > output'
   print '(A)',''
   print '(A)','    --file x        explicit file name; - for stdin (default:stdin)'
   print '(A)','    --position n    position of covariate in file'
   print '(A)','    --min n         the minimum value of input covariate'
   print '(A)','    --max n         the maximum value of input covariate'
   print '(A)','    --order n       order of Legendre polynomials (0=intercept; default:4)'
   print '(A)','    --precision n   decimal digits for output (default:4)'
   print '(A)','    --compatible    same values as the `legendre` program (default:off)'
   print '(A)',''
   print '(A)',' The file name can be at the beginning or at the end of the option line.'
   print '(A)',' When ommiting the file name, the program reads the data from stdin.'
   stop
end subroutine show_usage

subroutine get_commandline_options(file,pos,order,tmin,tmax,prec,compat)
   character(len=*),intent(inout) :: file
   integer,intent(inout) :: pos,order,prec
   real(real64),intent(inout) :: tmin,tmax
   logical,intent(inout) :: compat
   character(len=128) :: argv
   integer :: i,filepos,nargs,io, optmin,optmax
   file = '-'
   pos = 0
   order = 4
   tmin = 0
   tmax = 0
   prec = 4
   compat = .false.
   filepos = 0
   optmin = command_argument_count()
   optmax = 0

   call get_commandline_option('--position',i,nargs)
   if(nargs>0) then
      call get_command_argument(i+1,argv)
      read(argv,*,iostat=io) pos
      if(io/=0 .or. pos<1) call error_stop('invalid --position')
      optmin = min(optmin,i)
      optmax = max(optmax,i+1)
   else
      call error_stop('no --position')
   end if

   call get_commandline_option('--order',i,nargs)
   if(nargs>0) then
      call get_command_argument(i+1,argv)
      read(argv,*,iostat=io) order
      if(io/=0 .or. order<1) call error_stop('invalid --order')
      optmin = min(optmin,i)
      optmax = max(optmax,i+1)
   end if

   call get_commandline_option('--min',i,nargs)
   if(nargs>0) then
      call get_command_argument(i+1,argv)
      read(argv,*,iostat=io) tmin
      if(io/=0) call error_stop('invalid --min')
      optmin = min(optmin,i)
      optmax = max(optmax,i+1)
   else
      call error_stop('no --min')
   end if

   call get_commandline_option('--max',i,nargs)
   if(nargs>0) then
      call get_command_argument(i+1,argv)
      read(argv,*,iostat=io) tmax
      if(io/=0) call error_stop('invalid --max')
      optmin = min(optmin,i)
      optmax = max(optmax,i+1)
   else
      call error_stop('no --max')
   end if

   call get_commandline_option('--precision',i,nargs)
   if(nargs>0) then
      call get_command_argument(i+1,argv)
      read(argv,*,iostat=io) prec
      if(io/=0) call error_stop('invalid --precision')
      optmin = min(optmin,i)
      optmax = max(optmax,i+1)
   end if
   if(prec<1) call error_stop('invalid precision')

   call get_commandline_option('--compatible',i,nargs)
   if(i>0) then
      compat = .true.
      optmin = min(optmin,i)
      optmax = max(optmax,i)
   end if

   if(tmin>tmax) then
      call error_stop('tmin > tmax')
   end if

   call get_commandline_option('--file',i,nargs)
   if(nargs>0) then
      call get_command_argument(i+1,file)
   else if(optmin>=2) then
      call get_command_argument(optmin-1,file)
   else if(optmax<command_argument_count()) then
      call get_command_argument(optmax+1,file)
   else
      file = '-'
      !call show_usage()
   end if
end subroutine get_commandline_options

subroutine get_commandline_option(cmd,pos,nargs)
   character(len=*),intent(in) :: cmd
   integer,intent(out) :: pos
   integer,intent(out) :: nargs
   character(len=len_trim(cmd)) :: buf
   character(len=2) :: x
   integer :: i,j,n,cmdlen
   pos = 0
   nargs = 0
   cmdlen = len_trim(cmd)
   n = command_argument_count()
   do i=1,n
      call get_command_argument(i,buf)
      if(cmd(1:cmdlen)==buf(1:cmdlen)) then
         pos = i
         do j=i+1,n
            call get_command_argument(j,x)
            if(x(1:2)=='--') return
            nargs = nargs + 1
         end do
      end if
   end do
end subroutine get_commandline_option

subroutine split(str,n,items)
   character(len=*),intent(in) :: str
   integer,intent(out) :: n
   character(len=*),intent(out) :: items(:)
   character(len=len(str)) :: line
   integer :: i,p,c,limit
   n = 0
   line = str
   limit = size(items)
   do i=1,limit
      line = adjustl(line)
      p = index(line,' ')
      if(p == 1) then
         exit
      else if(p == 0) then
         c = len(line)
      else
         c = p-1
      end if
      items(i) = line(1:c)
      line(1:c) = repeat(' ',c)
      n = n + 1
   end do
end subroutine split

subroutine Leg(n,L,trans)
   integer,intent(in) :: n
   real(real64),intent(inout) :: L(n,n)
   logical,intent(in),optional :: trans
   integer :: i,j
   L = 0.0
   if(n>=1) L(1,1)=1.0
   if(n>=2) L(2,2)=1.0
   do i=3,n
      L(1:i,i) = ((2.0d0*(i-1)-1)/(i-1))*(/0.0d0, L(1:i-1,i-1)/) &
               - ((i-2.0d0)/(i-1))*L(1:i,i-2)
   end do
   do i=1,n
      L(1:i,i) = sqrt((2.0d0*(i-1)+1)/2)*L(1:i,i);
   end do
   if(present(trans)) then
      if(trans) then
         do j=1,n
            do i=j+1,n
               L(i,j) = L(j,i)
               L(j,i) = 0.0d0
            end do
         end do
      end if
   end if
end subroutine Leg

subroutine error_stop(msg)
   character(len=*),intent(in) :: msg
   write(error_unit,'(A)') trim(msg)
   stop 1
end subroutine error_stop

end program legcoef
