module netcdf_hl
use netcdf

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
integer, external :: nf_inq_varnatts

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
type :: nfhl_var
   integer :: id
   integer :: xtype
   character(len=:), allocatable :: name
   integer, allocatable, dimension(:) :: shape
   integer, allocatable, dimension(:) :: dimids
end type nfhl_var

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
type :: nfhl_dim
   integer :: id
   integer :: size
   character(len=:), allocatable :: name
end type nfhl_dim

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
type :: nfhl_att
   integer :: num
   integer :: xtype
   character(len=:), allocatable :: name
end type nfhl_att

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
type :: nfhl_dataset
   character(len=:), allocatable :: name
   integer :: id=0
   integer :: natts=0
   integer :: ndims=0
   integer :: nvars=0
   type(nfhl_att), allocatable, dimension(:) :: attributes
   type(nfhl_dim), allocatable, dimension(:) :: dimensions
   type(nfhl_var), allocatable, dimension(:) :: variables
   contains
   procedure :: close
   procedure :: open
   procedure :: print
end type nfhl_dataset

contains

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
subroutine open(ds,path,mode)
   implicit none
   class(nfhl_dataset) :: ds
   character(len=*), intent(in) :: path
   integer, intent(in), optional :: mode
   
   character(len=NF90_MAX_NAME) :: ctemp
   integer :: n,n1,n2,n3
   integer :: iret
   integer :: ndims
   integer :: nvars

   ! Open file.
   iret=nf90_open(path,mode,ds%id)
   ds%name=trim(path)

   ! Get global attributes (if any).
   iret=nf_inq_varnatts(ds%id,NF90_GLOBAL,ds%natts)
   allocate(ds%attributes(ds%natts))
   do n=1,ds%natts
      iret=nf90_inq_attname(ds%id,NF90_GLOBAL,n,ctemp)
      ds%attributes(n)%name=trim(ctemp)
      iret=nf90_inquire_attribute(ds%id,NF90_GLOBAL,ds%attributes(n)%name,ds%attributes(n)%xtype)
   end do

   ! Get dimensions.
   iret=nf90_inquire(ds%id,nDimensions=ds%ndims)
   allocate(ds%dimensions(ds%ndims))
   do n=1,ds%ndims
      iret=nf90_inquire_dimension(ds%id,n,ctemp,ds%dimensions(n)%size)
      ds%dimensions(n)%id=n
      ds%dimensions(n)%name=trim(ctemp)
   end do

   ! Get variables.
   iret=nf90_inquire(ds%id,nVariables=ds%nvars)
   allocate(ds%variables(ds%nvars))
   do n=1,ds%nvars
      iret=nf90_inquire_variable(ds%id,n,name=ctemp,xtype=ds%variables(n)%xtype,ndims=ndims)
      ds%variables(n)%id=n
      ds%variables(n)%name=trim(ctemp)
      allocate(ds%variables(n)%shape(ndims))
      allocate(ds%variables(n)%dimids(ndims))
      iret=nf90_inquire_variable(ds%id,n,dimids=ds%variables(n)%dimids)
      do n1=1,size(ds%variables(n)%dimids)
         ds%variables(n)%shape(n1)=ds%dimensions(ds%variables(n)%dimids(n1))%size
      end do
   end do

   return
end subroutine open

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
subroutine print(ds)
   implicit none
   class(nfhl_dataset) :: ds

   integer :: n,nn

   write(6,fmt='(A,A)')"Dataset: ",ds%name
   write(6,fmt='(5XA,I0.1)')"Id = ",ds%id

   write(6,fmt='(/5XA)')"Global Attributes:"
   do n=1,ds%natts
      write(6,fmt='(10XA,A)')ds%attributes(n)%name,": "
   end do

   write(6,fmt='(/5XA)')"Dimensions:"
   do n=1,ds%ndims
      write(6,fmt='(10XA,I0.1,A,A,A,I0.1)')"Id = ",ds%dimensions(n)%id,": Name = ",&
      ds%dimensions(n)%name,": Size = ",ds%dimensions(n)%size
   end do

   write(6,fmt='(/5XA)')"Variables:"
   do n=1,ds%nvars
      write(6,fmt='(10XA,I0.1,A,A,A,I0.1)',advance="no")"Id = ",ds%variables(n)%id,": Name = ",&
      ds%variables(n)%name,": Size = ("
      do nn=1,size(ds%variables(n)%shape)
         write(6,fmt='(I0.1,A2)',advance="no")ds%variables(n)%shape(nn),", "
      end do
      write(6,fmt='(A)',advance="no")")"
      write(6,fmt='(A,A)',advance="yes")": Type = ",get_var_type(ds%variables(n)%xtype)
   end do
end subroutine print

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
subroutine close(ds)
   implicit none
   class(nfhl_dataset) :: ds
   integer :: iret
   iret=0
   iret=nf90_close(ds%id)
end subroutine close

! ---------------------------------------------------------------------------------------- 
! ---------------------------------------------------------------------------------------- 
function get_var_type(xtype) result(vartype)
   implicit none
   integer, intent(in) :: xtype
   character(len=:), allocatable :: vartype
   character(len=20), dimension(0:12) :: ctype
   ctype(0)="NF90_NAT"
   ctype(1)="NF90_BYTE"
   ctype(2)="NF90_CHAR"
   ctype(3)="NF90_SHORT"
   ctype(4)="NF90_INT"
   ctype(5)="NF90_FLOAT"
   ctype(6)="NF90_DOUBLE"
   ctype(7)="NF90_UBYTE"
   ctype(8)="NF90_USHORT"
   ctype(9)="NF90_UINT"
   ctype(10)="NF90_INT64"
   ctype(11)="NF90_UINT64"
   ctype(12)="NF90_STRING"
   vartype=trim(ctype(xtype))
end function get_var_type

! ---------------------------------------------------------------------------------------- 
! END OF MODULE
! ---------------------------------------------------------------------------------------- 

end module netcdf_hl
