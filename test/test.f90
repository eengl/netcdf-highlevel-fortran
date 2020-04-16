program test
use netcdf_hl

character(len=1024) :: cfile

type(nfhl_dataset) :: dstest

call get_command_argument(1,cfile)

call dstest%open(trim(cfile),NF90_NOWRITE)

call dstest%print

call dstest%close

end program test
