module Module_HDF4
!$$$  module documentation block
!         .           .            .
!  module name: Module_HDF4
!    programmer: da,cheng        org: fsu         date: 2014-09-03
!
!  purpose:
!    For hdf4 data operation. 
!    If module_kind.f90 is not available, decomment the code in the section of the
!    kind definition.
!
!
!  revision history:
!    2014-09-03     da    - creator
!    2014-09-30     da    - kind defition from module_kind.f90
!    2014-10-01     da    - add subroutines to deal with Vdata set
!
!  dependencies:
!
!  subroutines included:
!    H4_InquireSDSInfo
!
!  functions included:
!    ***** related to the SD data *****
!
!    H4_InquireType
!    H4_CreateEmptyFile
!
!    H4_ReadSDS2d_UnsignedInteger
!    H4_ReadSDS1d_UnsignedInteger
!
!    H4_ReadSDS4d_Real8          -|-----> H4_ReadSDS4d
!    H4_ReadSDS4d_Real4           /
!
!    H4_ReadSDS3d_Real8          -|-----> H4_ReadSDS3d
!    H4_ReadSDS3d_Real4           /
!
!    H4_ReadSDS2d_Integer4       -|-----> H4_ReadSDS2d
!    H4_ReadSDS2d_Integer2       -|
!    H4_ReadSDS2d_Integer1       -|
!    H4_ReadSDS2d_Real8          -|
!    H4_ReadSDS2d_Real4           /
!
!    H4_ReadSDS1d_Real8          -|-----> H4_ReadSDS1d
!    H4_ReadSDS1d_Real4          -|
!    H4_ReadSDS1d_Integer4       -|
!    H4_ReadSDS1d_Integer2       -|
!    H4_ReadSDS1d_Integer1        /
!
!    H4_WriteSDS3d_Real8         -|-----> H4_WriteSDS3d
!    H4_WriteSDS3d_Real4         -|
!    H4_WriteSDS3d_Integer4       /
!
!    H4_WriteSDS1d_Real8         -|-----> H4_WriteSDS1d
!    H4_WriteSDS1d_Real4         -|
!    H4_WriteSDS1d_Integer4      -|
!    H4_WriteSDS1d_Integer2      -|
!    H4_WriteSDS1d_Integer1       /
!
!    H4_WriteSDS2d_Real8         -|-----> H4_WriteSDS2d
!    H4_WriteSDS2d_Real4         -|
!    H4_WriteSDS2d_Integer4      -|
!    H4_WriteSDS2d_Integer2      -|
!    H4_WriteSDS2d_Integer1       /
!
!
!    H4_ReadSDSAttr1d_Real8      -|-----> H4_ReadSDSAttr1d
!    H4_ReadSDSAttr1d_Real4      -|
!    H4_ReadSDSAttr1d_Integer4   -|
!    H4_ReadSDSAttr1d_Integer2   -|  
!    H4_ReadSDSAttr1d_Integer1   -|
!    H4_ReadSDSAttr1d_String      /
!
!    H4_WriteSDSAttr1d_Real8     -|-----> H4_WriteSDSAttr1d
!    H4_WriteSDSAttr1d_Real4     -|
!    H4_WriteSDSAttr1d_Integer4  -|
!    H4_WriteSDSAttr1d_String     /
!
!
!    ***** related to the Vdata *****
!    H4_ReadVS1d_Integer8        -|------> H4_ReadVS1d
!    H4_ReadVS1d_Integer4        -|
!    H4_ReadVS1d_Integer2        -|
!    H4_ReadVS1d_Integer1        -|
!    H4_ReadVS1d_Real8           -|
!    H4_ReadVS1d_Real4            /
!
!   
!    ***** related to the Unsigned & Signed converstion *****
!    int2uint_2_i_kind
!    int2uint_1_i_kind
!    int2uint_1_2
!
!
!  attributes: 
!    language: fortran 90
!    machine: Linux skynet2.met.fsu.edu 2.6.32-358.11.1.el6.x86_64 #1 SMP
!
!
!$$$ end documentation block

  implicit none

! set default as public
  private 

  Public :: H4_InquireSDSInfo
  Public :: H4_InquireType
  Public :: H4_CreateEmptyFile

  public :: H4_ReadSDS1d
  Public :: H4_ReadSDS2d
  Public :: H4_ReadSDS3d
  Public :: H4_ReadSDS4d
  Public :: H4_ReadSDSAttr1d
  Public :: H4_WriteSDS1d
  Public :: H4_WriteSDS2d
  Public :: H4_WriteSDS3d
  Public :: H4_WriteSDSAttr1d
  Public :: H4_ReadVS1d
  

  Public :: H4_ReadSDS1d_Integer1
  Public :: H4_ReadSDS1d_Integer2
  Public :: H4_ReadSDS1d_Integer4
  Public :: H4_ReadSDS1d_Real4
  Public :: H4_ReadSDS1d_Real8

  Public :: H4_ReadSDS2d_Integer1
  Public :: H4_ReadSDS2d_Integer2
  Public :: H4_ReadSDS2d_Integer4
  Public :: H4_ReadSDS2d_Real4
  Public :: H4_ReadSDS2d_Real8

  Public :: H4_ReadSDS3d_Real4
  Public :: H4_ReadSDS3d_Real8

  Public :: H4_ReadSDS4d_Real4
  Public :: H4_ReadSDS4d_Real8

  Public :: H4_ReadSDS1d_UnsignedInteger
  Public :: H4_ReadSDS2d_UnsignedInteger

  Public :: H4_ReadSDSAttr1d_String
  Public :: H4_ReadSDSAttr1d_Integer1
  Public :: H4_ReadSDSAttr1d_Integer2
  Public :: H4_ReadSDSAttr1d_Integer4
  Public :: H4_ReadSDSAttr1d_Real4
  Public :: H4_ReadSDSAttr1d_Real8

  Public :: H4_WriteSDS1d_Integer1
  Public :: H4_WriteSDS1d_Integer2
  Public :: H4_WriteSDS1d_Integer4
  Public :: H4_WriteSDS1d_Real4
  Public :: H4_WriteSDS1d_Real8

  Public :: H4_WriteSDS2d_Integer1
  Public :: H4_WriteSDS2d_Integer2
  Public :: H4_WriteSDS2d_Integer4
  Public :: H4_WriteSDS2d_Real4
  Public :: H4_WriteSDS2d_Real8

  Public :: H4_WriteSDS3d_Integer4
  Public :: H4_WriteSDS3d_Real4
  Public :: H4_WriteSDS3d_Real8

  Public :: H4_WriteSDSAttr1d_String
  Public :: H4_WriteSDSAttr1d_Integer4
  Public :: H4_WriteSDSAttr1d_Real4
  Public :: H4_WriteSDSAttr1d_Real8

  Public :: H4_ReadVS1d_Integer1
  Public :: H4_ReadVS1d_Integer2
  Public :: H4_ReadVS1d_Integer4
  Public :: H4_ReadVS1d_Integer8
  Public :: H4_ReadVS1d_Real4
  Public :: H4_ReadVS1d_Real8
 
!-----------------------
! procedure overloading
!-----------------------

  interface H4_ReadSDS4d
    module procedure H4_ReadSDS4d_Real4
    module procedure H4_ReadSDS4d_Real8
  endinterface

  interface H4_ReadSDS3d
    module procedure H4_ReadSDS3d_Real4
    module procedure H4_ReadSDS3d_Real8
  endinterface

  interface H4_ReadSDS2d
    module procedure H4_ReadSDS2d_Integer1
    module procedure H4_ReadSDS2d_Integer2
    module procedure H4_ReadSDS2d_Integer4
    module procedure H4_ReadSDS2d_Real4
    module procedure H4_ReadSDS2d_Real8
  endinterface

  interface H4_ReadSDS1d
    module procedure H4_ReadSDS1d_Integer1
    module procedure H4_ReadSDS1d_Integer2
    module procedure H4_ReadSDS1d_Integer4
    module procedure H4_ReadSDS1d_Real4
    module procedure H4_ReadSDS1d_Real8
  endinterface

  interface H4_WriteSDS3d
    module procedure H4_WriteSDS3d_Integer4
    module procedure H4_WriteSDS3d_Real4
    module procedure H4_WriteSDS3d_Real8
  endinterface

  interface H4_WriteSDS2d
    module procedure H4_WriteSDS2d_Integer1
    module procedure H4_WriteSDS2d_Integer2
    module procedure H4_WriteSDS2d_Integer4
    module procedure H4_WriteSDS2d_Real4
    module procedure H4_WriteSDS2d_Real8
  endinterface

  interface H4_WriteSDS1d
    module procedure H4_WriteSDS1d_Integer1
    module procedure H4_WriteSDS1d_Integer2
    module procedure H4_WriteSDS1d_Integer4
    module procedure H4_WriteSDS1d_Real4
    module procedure H4_WriteSDS1d_Real8
  endinterface


  interface H4_ReadSDSAttr1d
    module procedure H4_ReadSDSAttr1d_Integer1
    module procedure H4_ReadSDSAttr1d_Integer2
    module procedure H4_ReadSDSAttr1d_Integer4
    module procedure H4_ReadSDSAttr1d_Real4
    module procedure H4_ReadSDSAttr1d_Real8
    module procedure H4_ReadSDSAttr1d_String
  endinterface

  interface H4_WriteSDSAttr1d
    module procedure H4_WriteSDSAttr1d_Integer4
    module procedure H4_WriteSDSAttr1d_Real4
    module procedure H4_WriteSDSAttr1d_Real8
    module procedure H4_WriteSDSAttr1d_String
  endinterface

  interface H4_ReadVS1d
    module procedure H4_ReadVS1d_Integer1
    module procedure H4_ReadVS1d_Integer2
    module procedure H4_ReadVS1d_Integer4
    module procedure H4_ReadVS1d_Integer8
    module procedure H4_ReadVS1d_Real4
    module procedure H4_ReadVS1d_Real8
  endinterface
!-----------
! constants
!-----------
! kind definition   !! if module_kind is not included, then decomment this section
  integer(4),parameter :: i1 = 1
  integer(4),parameter :: i2 = 2
  integer(4),parameter :: i4 = 4
  integer(4),parameter :: i8 = 8
  integer(4),parameter :: i_kind = i4

  integer(4),parameter :: r4 = 4
  integer(4),parameter :: r8 = 8
  integer(4),parameter :: r_kind = r4

! hdf4 operation status flags
  integer(i4),parameter :: FAIL = -1
  integer(i4),parameter :: SUCCEED = 0

! hdf4 file access code flags
  integer(i4),parameter :: DFACC_READ   = 1    ! read access
  integer(i4),parameter :: DFACC_WRITE  = 2    ! read & write access
  integer(i4),parameter :: DFACC_CREATE = 4    ! create with read & write access

! hdf4 standard HDF data types and flags
  integer(i4),parameter :: DFNT_CHAR8   = 4
  integer(i4),parameter :: DFNT_UCHAR8  = 3
  integer(i4),parameter :: DFNT_INT8    = 20
  integer(i4),parameter :: DFNT_UINT8   = 21
  integer(i4),parameter :: DFNT_INT16   = 22
  integer(i4),parameter :: DFNT_UINT16  = 23
  integer(i4),parameter :: DFNT_INT32   = 24
  integer(i4),parameter :: DFNT_UINT32  = 25
  integer(i4),parameter :: DFNT_FLOAT32 = 5
  integer(i4),parameter :: DFNT_FLOAT64 = 6

! hdf4 data compression type related
  integer(i4),parameter :: COMP_CODE_NONE    = 0
  integer(i4),parameter :: COMP_CODE_RLE     = 1
  integer(i4),parameter :: COMP_CODE_NBIT    = 2
  integer(i4),parameter :: COMP_CODE_SKPHUFF = 3
  integer(i4),parameter :: COMP_CODE_DEFLATE = 4
  integer(i4),parameter :: COMP_CODE_SZIP    = 5
  integer(i4),parameter :: MAX_COMP_ELEMENTS = 5

! hdf4 GZIP compression level
  integer(i4),parameter :: GZIP_MAXIMUM_LEVEL = 9
  integer(i4),parameter :: GZIP_DEFAULT_LEVEL = 7
  integer(i4),parameter :: GZIP_MINIMUM_LEVEL = 1

! hdf4 interlace
  integer(i4),parameter :: FULL_INTERLACE = 0

! rank of a matrix
  integer(i4),parameter :: RANK_ONE        = 1
  integer(i4),parameter :: RANK_TWO        = 2
  integer(i4),parameter :: RANK_THREE      = 3
  integer(i4),parameter :: RANK_FOUR       = 4
  integer(i4),parameter :: RANK_FIVE       = 5
  integer(i4),parameter :: MAX_RANKS       = RANK_FIVE
  integer(i4),parameter :: H4_MAX_NC_NAME  = 256
  integer(i4),parameter :: MAX_CHAR_LENGTH = H4_MAX_NC_NAME
  
  

!---------------------
! external procedures 
!---------------------
  integer(i_kind), external :: sfstart,   &    ! open the SD (file) interface
                               sfn2index, &    ! get the SD index
                               sfselect,  &    ! get the SDS  index
                               sfcreate,  &    ! create the SDS index
                               sfend,     &    ! close the SD interface
                               sfendacc,  &    ! close the SDS interface
                               sfginfo,   &    ! get the info of the SDS
                               sfrdata,   &    ! read the value of the SDS
                               sfwdata,   &    ! write the value of the SDS
                               sfgainfo,  &    ! get the info of the attribute
                               sffattr,   &    ! find the index of an atrribute given its name
                               sfrnatt,   &    ! read the value of the atrribute (numerical)
                               sfrcatt,   &    ! read the value of the atrribute (character)
                               sfscompress,  & ! set data compression method and level
                               sfgcompress,  & ! get compression level
                               sfsnatt,   &    ! attach the numerical attribute
                               sfscatt,   &      ! attach the character attribute
                               vfstart, vsffnd, vsfatch, vsfsfld, vsfrd, vsfseek, &
                               vsfdtch, vfend, vsqfnelt, &
                               hclose, hopen

contains

function H4_ReadSDS2d_UnsignedInteger( fileName, varName, varType, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS2d_UnsignedInteger
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read unsigned integer (16-bit or 32-bit) 2D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varType       - type of the inquired variable ( DFNT_UINT8, or DFNT_UINT16 )
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none

  character(*),intent(in)     :: fileName
  character(*),intent(in)     :: varName
  integer(i_kind)             :: varType
  integer(i_kind),allocatable :: varValue(:,:)
  integer(i_kind)             :: errStatus
! local arguments
  integer(i4)         :: varDims(RANK_TWO)
  integer(i2),allocatable :: buffer_UINT16(:,:)
  integer(i1),allocatable :: buffer_UINT8(:,:)

  errStatus = FAIL
  if ( varType .eq. DFNT_UINT8 ) then
     errStatus = H4_ReadSDS2d_Integer1( fileName, varName, buffer_UINT8 )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to read data into buffer_UNIT8 of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
     varDims = SHAPE( buffer_UINT8 )
     allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to allocate space for the var: ", varName
        print*, "       of the file:  ", fileName
     endif
     varValue = int2uint_1_i_kind( buffer_UINT8 )
     deallocate( buffer_UINT8 )
  elseif ( varType .eq. DFNT_UINT16 ) then
     errStatus = H4_ReadSDS2d_Integer2( fileName, varName, buffer_UINT16 )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to read data into buffer_UNIT16 of the var: ", varName
        print*, "       of the file:  ", fileName
        return
     endif
     varDims = SHAPE( buffer_UINT16 )
     allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to allocate space for the var: ", varName
        print*, "       of the file:  ", fileName
     endif
     varValue = int2uint_2_i_kind( buffer_UINT16 )
     deallocate( buffer_UINT16 )
   else
     print*, "Error: Unacceptable data type!"
     print*, "       Acceptable data types are: DFNT_UINT8, DNFT_UINT16"
     return
   endif

endfunction

function H4_ReadSDS1d_UnsignedInteger( fileName, varName, varType, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS1d_UnsignedInteger
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read unsigned integer (16-bit or 32-bit) 1D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varType       - type of the inquired variable ( DFNT_UINT8, or DFNT_UINT16 )
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none

  character(*),intent(in)     :: fileName
  character(*),intent(in)     :: varName
  integer(i_kind)             :: varType
  integer(i_kind),allocatable :: varValue(:)
  integer(i_kind)             :: errStatus
! local arguments
  integer(i4)         :: varDims(RANK_ONE)
  integer(i2),allocatable :: buffer_UINT16(:)
  integer(i1),allocatable :: buffer_UINT8(:)

  errStatus = FAIL
  if ( varType .eq. DFNT_UINT8 ) then
     errStatus = H4_ReadSDS1d_Integer1( fileName, varName, buffer_UINT8 )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to read data into buffer_UNIT8 of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
     varDims = SHAPE( buffer_UINT8 )
     allocate( varValue(varDims(1)), stat=errStatus )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to allocate space for the var: ", varName
        print*, "       of the file:  ", fileName
     endif
     varValue = int2uint_1_i_kind( buffer_UINT8 )
     deallocate( buffer_UINT8 )
  elseif ( varType .eq. DFNT_UINT16 ) then
     errStatus = H4_ReadSDS1d_Integer2( fileName, varName, buffer_UINT16 )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to read data into buffer_UNIT16 of the var: ", varName
        print*, "       of the file:  ", fileName
        return
     endif
     varDims = SHAPE( buffer_UINT16 )
     allocate( varValue(varDims(1)), stat=errStatus )
     if ( errStatus .ne. SUCCEED ) then
        print*, "Error: Fail to allocate space for the var: ", varName
        print*, "       of the file:  ", fileName
     endif
     varValue = int2uint_2_i_kind( buffer_UINT16 )
     deallocate( buffer_UINT16 )
   else
     print*, "Error: Unacceptable data type!"
     print*, "       Acceptable data types are: DFNT_UINT8, DNFT_UINT16"
     return
   endif

endfunction


function H4_ReadSDS1d_Real8( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS1d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 64-bit float 1D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r8),allocatable       :: varValue(:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_FLOAT64 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction


function H4_ReadSDS1d_Real4( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 32-bit float 1D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r4),allocatable       :: varValue(:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_FLOAT32 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction


function H4_ReadSDS1d_Integer4( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS1d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 16-bit integer 1D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  integer(i4),allocatable    :: varValue(:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_INT32 .and. varType.ne.DFNT_UINT32 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction



function H4_ReadSDS1d_Integer2( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS1d_Integer2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 16-bit integer 1D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  integer(i2),allocatable    :: varValue(:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_INT16 .and. varType.ne.DFNT_UINT16 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction

function H4_ReadSDS1d_Integer1( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS1d_Integer1
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 8-bit integer 1D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  integer(i1),allocatable    :: varValue(:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_INT8 .and. varType.ne.DFNT_UINT8 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction




function H4_ReadSDS2d_Integer4( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS2d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 32-bit integer 2D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  integer(i4),allocatable    :: varValue(:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_INT32 .and. varType.ne.DFNT_UINT32 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction



function H4_ReadSDS2d_Integer2( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS2d_Integer2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 16-bit integer 2D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  integer(i2),allocatable    :: varValue(:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrCounts
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_INT16 .and. varType.ne.DFNT_UINT16 ) then
     print*, "Error: the type of the var:  ", varName, " is ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif

  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if (errStatus .ne. SUCCEED) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction


function H4_ReadSDS2d_Integer1( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS2d_Integer1
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read signed 8-bit integer 2D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  integer(i1),allocatable    :: varValue(:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType.ne.DFNT_INT8 .and. varType.ne. DFNT_UINT8) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction


function H4_ReadSDS4d_Real8( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS4d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read 64-bit real 4D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 4d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r8),allocatable       :: varValue(:,:,:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_FOUR), varStart(RANK_FOUR), varStride(RANK_FOUR)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_FOUR ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType .ne. DFNT_FLOAT64 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2),varDims(3),varDims(4)), &
            stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction

function H4_ReadSDS4d_Real4( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS4d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read 32-bit real 4D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 4d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r4),allocatable       :: varValue(:,:,:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_FOUR), varStart(RANK_FOUR), varStride(RANK_FOUR)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_FOUR ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType .ne. DFNT_FLOAT32 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2),varDims(3),varDims(4)), &
            stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction



function H4_ReadSDS3d_Real8( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS3d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read 64-bit real 3D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 3d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r8),allocatable       :: varValue(:,:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_THREE), varStart(RANK_THREE), varStride(RANK_THREE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_THREE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType .ne. DFNT_FLOAT64 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2),varDims(3)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction



function H4_ReadSDS3d_Real4( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS3d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read 32-bit real 3D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 3d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r4),allocatable       :: varValue(:,:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_THREE), varStart(RANK_THREE), varStride(RANK_THREE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_THREE ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType .ne. DFNT_FLOAT32 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2),varDims(3)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction








function H4_ReadSDS2d_Real8( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS2d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read 64-bit real 2D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r8),allocatable       :: varValue(:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType .ne. DFNT_FLOAT64 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction


function H4_ReadSDS2d_Real4( fileName, varName, varValue ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDS2d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read 32-bit real 2D array from the HDF4 SDS.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r4),allocatable       :: varValue(:,:)
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  character(MAX_CHAR_LENGTH) :: varSdsName
  integer(i4) :: varRank, varType, varAttrsCounts
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the dimensions of the var
  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrsCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get dimension of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var:  ", varName, "   is   ", varRank
     errStatus = FAIL
     return
  endif
  if ( varType .ne. DFNT_FLOAT32 ) then
     print*, "Error: the type of the var:  ", varName, "   is   ", &
             TRIM( H4_InquireType( varType ) )
     errStatus = FAIL
     return
  endif


  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if (errStatus .ne. SUCCEED ) then
     print*,  "Error: failure to allocate space for the var:  ", varName
     return
  endif
  varStart = 0
  varStride = 1
! read data
  errStatus = sfrdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get value of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )
endfunction


function H4_InquireType( varType ) result( varTypeName )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_InquireType
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Return the corresponding character name of the var type according
!    to the numerical var type
!
!  revision history:
!
!  input arguments:
!    varType       - numerical value of the var type
!
!  output arguments:
!    varTypeName   - character name of the var type
!$$$  
  implicit none

  integer(i4),intent(in)     :: varType
  character(MAX_CHAR_LENGTH) :: varTypeName
! local arguments
  integer(i4) :: i

! reset value of varTypeName
  do i = 1, MAX_CHAR_LENGTH
     varTypeName(i:i) = " "
  enddo

  selectcase (varType)
        case( DFNT_CHAR8   )
          varTypeName = "DFNT_CHAR8"
        case( DFNT_UCHAR8  )
          varTypeName = "DFNT_UCHAR8"
        case( DFNT_INT8    )
          varTypeName = "DFNT_INT8"
        case( DFNT_UINT8   )
          varTypeName = "DFNT_UINT8"
        case( DFNT_INT16   )
          varTypeName = "DFNT_INT16"
        case( DFNT_UINT16  )
          varTypeName = "DFNT_UINT16"
        case( DFNT_INT32   )
          varTypeName = "DFNT_INT32"
        case( DFNT_UINT32   )
          varTypeName = "DFNT_UINT32"
        case( DFNT_FLOAT32 )
          varTypeName = "DFNT_FLOAT32"
        case( DFNT_FLOAT64 )
          varTypeName = "DFNT_FLOAT64"
        case default
          varTypeName = "Unknown type"
        endselect
endfunction

subroutine H4_InquireSDSInfo( fileName, varName ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_InquireSDSInfo
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    get the info of one variable.
!      | var_sds_id | var_sds_name | var_rank | var_dims | var_type |
!      | var_attribute_info |
!
!  revision history:
!
!  input arguments:
!    fileName       - name of the HDF4 file
!    varName        - name of the inquired variable
!
!  output arguments:
!    None
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
! local vars
  integer(i4) :: errStatus
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(MAX_RANKS) 
  integer(i4) :: varRank, varType, varAttrCounts
  character(MAX_CHAR_LENGTH) :: varSdsName
  character(MAX_CHAR_LENGTH),allocatable :: varAttrName(:)
  integer(i4),allocatable :: varAttrType(:), varAttrDims(:) 
  integer(i4) :: i


  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id .eq. FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif

! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id .eq. FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 

! retrieve the sds index of the var
  var_sds_id = sfselect( sd_id, var_id )
  if ( var_sds_id .eq. FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

  errStatus = sfginfo( var_sds_id, varSdsName, varRank, varDims, &
                       varType, varAttrCounts )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: failure to get info of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif

! retrieve the attribute info
  if ( varAttrCounts >0 ) then
     allocate( varAttrName(varAttrCounts) )
     allocate( varAttrType(varAttrCounts) )
     allocate( varAttrDims(varAttrCounts) )
     do i = 1, varAttrCounts
        errStatus = sfgainfo( var_sds_id, i-1, varAttrName(i), varAttrType(i), &
                           varAttrDims(i) )
     enddo 
  endif
  write(*,'(1x,a,a)'  ) "Inquired variable name: ", varName
  write(*,'(3x,a,i10)') "VAR_ID                = ", var_id
  write(*,'(3x,a,i10)') "VAR_SDS_ID            = ", var_sds_id
  write(*,'(3x,a,a)')   "VAR_SDS_NAME          = ", TRIM( varSdsName )
  write(*,'(3x,a,i10)') "VAR_RANK              = ", varRank
  do i = 1, varRank
    write(*,'(4x,a,i5,a,i10)') "|-> VAR_DIM(", i, "   ) = ", varDims(i)
  enddo
  write(*,'(3x,a,a)') "VAR_TYPE              = ", TRIM( H4_InquireType( varType ) )
  write(*,'(3x,a,i10)') "VAR_ATRRIBUTE_COUNTS = ", varAttrCounts
  do i = 1, varAttrCounts
    write(*,'(4x,a,i5,2a)') "|-> ATTRIBUTE(", i,"  )"
    write(*,'(4x,5(a15),i10,a)') " || Name:  ", TRIM( varAttrName(i) ) , &
                " | Type: ", TRIM( H4_InquireType( varAttrType(i) ) ), &
                " | Dims: ", varAttrDims(i), "        ||"
  enddo

  if ( allocated( varAttrName ) ) deallocate( varAttrName )
  if ( allocated( varAttrType ) ) deallocate( varAttrType )
  if ( allocated( varAttrDims ) ) deallocate( varAttrDims )

! close SDS
  errStatus = sfendacc( var_sds_id )

! close SD
  errStatus = sfend( sd_id )

endsubroutine


elemental function int2uint_1_2( integer1 ) result (integer2)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: int2uint_1_2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Convert the value of 8-bit unsigned integer to the value of 8-bit
!    signed integer. The result is stored in a 16-bit integer.
!
!  revision history:
!
!  input arguments:
!    integer1       - value if stored as the 8-bit signed integer.
!                     
!
!  output arguments:
!    integer2       - value if stored as the 8-bit unsigned integer.
!$$$  
  implicit none

  integer(i1),intent(in) :: integer1
  integer(i2)            :: integer2
! local arguments
  integer(i1)        :: i, weight
  integer(i8)        :: base

  integer2 = 0
  base = 2**(BIT_SIZE(integer1) - 1)
  do i = 1, BIT_SIZE(integer1)
     weight = IBITS( integer1, BIT_SIZE(integer1)-i, 1 )
     !write(*,'(I1\)') weight  !for check
     integer2 = integer2 + weight*base 
     base = base/2
  enddo

endfunction


elemental function int2uint_1_i_kind( integer1 ) result (integer_default)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: int2uint_1_i_kind
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Convert the value of 8-bit unsigned integer to the value of 8-bit
!    signed integer. The result is stored in a (8*(i_kind))-bit integer.
!
!  revision history:
!
!  input arguments:
!    integer1       - value if stored as the 8-bit signed integer.
!                     
!
!  output arguments:
!    integer2       - value if stored as the 8-bit unsigned integer.
!$$$  
  implicit none

  integer(i1),intent(in) :: integer1
  integer(i_kind)        :: integer_default
! local arguments
  integer(i1)        :: i, weight
  integer(i8)        :: base

  integer_default = 0
  base = 2**(BIT_SIZE(integer1) - 1)
  do i = 1, BIT_SIZE(integer1)
     weight = IBITS( integer1, BIT_SIZE(integer1)-i, 1 )
     !write(*,'(I1\)') weight  !for check
     integer_default = integer_default + weight*base 
     base = base/2
  enddo

endfunction


elemental function int2uint_2_i_kind( integer2 ) result (integer_default)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: int2uint_1_2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Convert the value of 16-bit unsigned integer to the value of 16-bit
!    signed integer. The result is stored in a (8*i_kind)-bit integer.
!
!  revision history:
!
!  input arguments:
!    integer1       - value if stored as the 16-bit signed integer.
!                     
!
!  output arguments:
!    integer2       - value if stored as the 16-bit unsigned integer.
!$$$  
  implicit none

  integer(i2),intent(in) :: integer2
  integer(i_kind)        :: integer_default
! local arguments
  integer(i1)        :: i, weight  ! max{i}=64, weight=1 or 0
  integer(i8)        :: base

  integer_default = 0
  base = 2**(BIT_SIZE(integer2) - 1)
  do i = 1, BIT_SIZE(integer2)
     weight = IBITS( integer2, BIT_SIZE(integer2)-i, 1 )
     !write(*,'(I1\)') weight  !for check
     integer_default = integer_default + weight*base 
     base = base/2
  enddo

endfunction

function H4_ReadSDSAttr1d_Real8( fileName, varName, varAttrName, varAttrValue ) &
                      result (errStatus) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDSAttr1d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read the 64-bit float 1d array of the attribute
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    varName         - name of the variables that contains the inquired attribute     
!    varAttrName     - name of the inquired attribtue
!    varAttrValue    - value of the inquired attribute
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),intent(in)  :: fileName
  character(*),intent(in)  :: varName
  character(*),intent(in)  :: varAttrName
  real(r8),allocatable     :: varAttrValue(:)
  integer(i_kind)          :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id, var_attr_id
  character(MAX_CHAR_LENGTH) :: bufferAttrName
  integer(i4) :: bufferAttrType, bufferAttrCounts

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! retrieva the index of the attribute
  var_attr_id = sffattr( var_sds_id, varAttrName ) 
  if ( var_attr_id .eq. FAIL ) then
     print*, "Error: failure to get attr_id of the attribute:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! check the properties of the inquired atrribute 
  errStatus = sfgainfo( var_sds_id, var_attr_id, bufferAttrName, &
                  bufferAttrType, bufferAttrCounts )
  if ( TRIM(varAttrName).ne.TRIM(bufferAttrName) ) then
     print*, "Error: the attribute name differs."
     print*, "       Inquired_Attribute_Name =  ", TRIM(varAttrName)
     print*, "       Existing_Attribute_Name =  ", TRIM(bufferAttrName)
     errStatus = FAIL
     return
  endif
  if ( bufferAttrType .ne. DFNT_FLOAT64 ) then
     print*, "Error: the attribute type is not DFNT_FLOAT64, but ", &
             TRIM(H4_InquireType(bufferAttrType))
     errStatus = FAIL
     return
  endif
! allocate space for attribute value
  allocate( varAttrValue(bufferAttrCounts), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the attribute: ", varAttrName
     return
  endif
! get the value of the attribute
  errStatus = sfrnatt( var_sds_id, var_attr_id, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to read the value of the attribtue:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file: ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_ReadSDSAttr1d_Real4( fileName, varName, varAttrName, varAttrValue ) &
                      result (errStatus) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDSAttr1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read the 32-bit float 1d array of the attribute
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    varName         - name of the variables that contains the inquired attribute     
!    varAttrName     - name of the inquired attribtue
!    varAttrValue    - value of the inquired attribute
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),intent(in)  :: fileName
  character(*),intent(in)  :: varName
  character(*),intent(in)  :: varAttrName
  real(r4),allocatable     :: varAttrValue(:)
  integer(i_kind)          :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id, var_attr_id
  character(MAX_CHAR_LENGTH) :: bufferAttrName
  integer(i4) :: bufferAttrType, bufferAttrCounts

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! retrieva the index of the attribute
  var_attr_id = sffattr( var_sds_id, varAttrName ) 
  if ( var_attr_id .eq. FAIL ) then
     print*, "Error: failure to get attr_id of the attribute:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! check the properties of the inquired atrribute 
  errStatus = sfgainfo( var_sds_id, var_attr_id, bufferAttrName, &
                  bufferAttrType, bufferAttrCounts )
  if ( TRIM(varAttrName).ne.TRIM(bufferAttrName) ) then
     print*, "Error: the attribute name differs."
     print*, "       Inquired_Attribute_Name =  ", TRIM(varAttrName)
     print*, "       Existing_Attribute_Name =  ", TRIM(bufferAttrName)
     errStatus = FAIL
     return
  endif
  if ( bufferAttrType .ne. DFNT_FLOAT32 ) then
     print*, "Error: the attribute type is not DFNT_FLOAT32, but ", &
             TRIM(H4_InquireType(bufferAttrType))
     errStatus = FAIL
     return
  endif
! allocate space for attribute value
  allocate( varAttrValue(bufferAttrCounts), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the attribute: ", varAttrName
     return
  endif
! get the value of the attribute
  errStatus = sfrnatt( var_sds_id, var_attr_id, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to read the value of the attribtue:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file: ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction



function H4_ReadSDSAttr1d_String( fileName, varName, varAttrName, varAttrValue ) &
                      result (errStatus) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDSAttr1d_String
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read the string value of the attribute
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    varName         - name of the variables that contains the inquired attribute     
!    varAttrName     - name of the inquired attribtue
!    varAttrValue    - string value of the inquired attribute
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),              intent(in)    :: fileName
  character(*),              intent(in)    :: varName
  character(*),              intent(in)    :: varAttrName
  character(MAX_CHAR_LENGTH),intent(inout) :: varAttrValue
  integer(i_kind)            :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id, var_attr_id
  character(MAX_CHAR_LENGTH) :: bufferAttrName
  integer(i4) :: bufferAttrType, bufferAttrLength
  integer(i4) :: i

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! retrieva the index of the attribute
  var_attr_id = sffattr( var_sds_id, varAttrName ) 
  if ( var_attr_id .eq. FAIL ) then
     print*, "Error: failure to get attr_id of the attribute:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! check the properties of the inquired atrribute 
  errStatus = sfgainfo( var_sds_id, var_attr_id, bufferAttrName, &
                  bufferAttrType, bufferAttrLength )
  if ( TRIM(varAttrName).ne.TRIM(bufferAttrName) ) then
     print*, "Error: the attribute name differs."
     print*, "       Inquired_Attribute_Name =  ", TRIM(varAttrName)
     print*, "       Existing_Attribute_Name =  ", TRIM(bufferAttrName)
     errStatus = FAIL
     return
  endif
  if ( bufferAttrType.ne.DFNT_CHAR8 .and. bufferAttrType.ne.DFNT_UCHAR8 ) then
     print*, "Error: the attribute type is not DFNT_CHAR8 nor DFNT_UCHAR8, but ", &
             TRIM(H4_InquireType(bufferAttrType))
     errStatus = FAIL
     return
  endif
  if ( bufferAttrLength > LEN(varAttrValue) ) then
     print*, "Error: the length of input character var is not long enough."
     print*, "       LEN(Input_Character_Var) =  ", varAttrValue
     print*, "       LEN(Attribute)           =  ", bufferAttrLength
     errStatus = FAIL
     return
  endif
! reset the string
  do i = 1, LEN(varAttrValue)
     varAttrValue(i:i)=" "
  enddo
! get the value of the attribute
  errStatus = sfrnatt( var_sds_id, var_attr_id, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to read the value of the attribtue:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file: ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_CreateEmptyFile( fileName, replace ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_CreateEmptyFile
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Create an empty HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    replace         - .true. if delete the exisitng HDF4 file with the name fileName
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),     intent(in) :: fileName
  logical,optional, intent(in) :: replace
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4) :: sd_id

  errStatus = FAIL
  sd_id = sfstart( fileName, DFACC_READ )
  errStatus = sfend( sd_id )
  errStatus = FAIL
  if ( sd_id .ne. FAIL ) then
     print*, "Warning: The file ( ", fileName, " ) already exists."
     if (present(replace) .and. replace ) then
        sd_id = sfstart( fileName, DFACC_CREATE )
        print*, "Warning: replace the existing HDF4 file:  ", fileName
        errStatus = sfend( sd_id )
     endif
  else
     sd_id = sfstart( fileName, DFACC_CREATE )
     if ( sd_id .eq. FAIL ) then
        print*, "Error: fail to create the HDF4 file:  ", fileName
        return
     endif
     errStatus = sfend( sd_id )
  endif

endfunction


function H4_WriteSDS2d_Real4( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS2d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 32-bit real 2D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  real(r4),            intent(in) :: varValue(:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_FLOAT32, RANK_TWO, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  ! if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exists."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDS2d_Real8( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS2d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 64-bit real 2D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  real(r8),            intent(in) :: varValue(:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_FLOAT64, RANK_TWO, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDS2d_Integer4( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS2d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 32-bit signed integer 2D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i4),         intent(in) :: varValue(:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT32, RANK_TWO, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDS2d_Integer2( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS2d_Integer2
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 16-bit signed integer 2D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i2),         intent(in) :: varValue(:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT16, RANK_TWO, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction



function H4_WriteSDS2d_Integer1( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS2d_Integer1
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 8-bit signed integer 2D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i1),         intent(in) :: varValue(:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_TWO), varStart(RANK_TWO), varStride(RANK_TWO)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT8, RANK_TWO, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_ReadSDSAttr1d_Integer4( fileName, varName, varAttrName, varAttrValue ) &
                      result (errStatus) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDSAttr1d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read the 32-bit signed integer 1d array of the attribute
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    varName         - name of the variables that contains the inquired attribute     
!    varAttrName     - name of the inquired attribtue
!    varAttrValue    - value of the inquired attribute
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),intent(in)  :: fileName
  character(*),intent(in)  :: varName
  character(*),intent(in)  :: varAttrName
  integer(i4),allocatable  :: varAttrValue(:)
  integer(i_kind)          :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id, var_attr_id
  character(MAX_CHAR_LENGTH) :: bufferAttrName
  integer(i4) :: bufferAttrType, bufferAttrCounts

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! retrieva the index of the attribute
  var_attr_id = sffattr( var_sds_id, varAttrName ) 
  if ( var_attr_id .eq. FAIL ) then
     print*, "Error: failure to get attr_id of the attribute:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! check the properties of the inquired atrribute 
  errStatus = sfgainfo( var_sds_id, var_attr_id, bufferAttrName, &
                  bufferAttrType, bufferAttrCounts )
  if ( TRIM(varAttrName).ne.TRIM(bufferAttrName) ) then
     print*, "Error: the attribute name differs."
     print*, "       Inquired_Attribute_Name =  ", TRIM(varAttrName)
     print*, "       Existing_Attribute_Name =  ", TRIM(bufferAttrName)
     errStatus = FAIL
     return
  endif
  if ( bufferAttrType .ne. DFNT_INT32 ) then
     print*, "Error: the attribute type is not DFNT_INT32, but ", &
             TRIM(H4_InquireType(bufferAttrType))
     errStatus = FAIL
     return
  endif
! allocate space for attribute value
  allocate( varAttrValue(bufferAttrCounts), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the attribute: ", varAttrName
     return
  endif
! get the value of the attribute
  errStatus = sfrnatt( var_sds_id, var_attr_id, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to read the value of the attribtue:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file: ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_ReadSDSAttr1d_Integer2( fileName, varName, varAttrName, varAttrValue ) &
                      result (errStatus) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadSDSAttr1d_Integer2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read the 16-bit signed integer 1d array of the attribute
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    varName         - name of the variables that contains the inquired attribute     
!    varAttrName     - name of the inquired attribtue
!    varAttrValue    - value of the inquired attribute
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),intent(in)  :: fileName
  character(*),intent(in)  :: varName
  character(*),intent(in)  :: varAttrName
  integer(i2),allocatable  :: varAttrValue(:)
  integer(i_kind)          :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id, var_attr_id
  character(MAX_CHAR_LENGTH) :: bufferAttrName
  integer(i4) :: bufferAttrType, bufferAttrCounts

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! retrieva the index of the attribute
  var_attr_id = sffattr( var_sds_id, varAttrName ) 
  if ( var_attr_id .eq. FAIL ) then
     print*, "Error: failure to get attr_id of the attribute:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! check the properties of the inquired atrribute 
  errStatus = sfgainfo( var_sds_id, var_attr_id, bufferAttrName, &
                  bufferAttrType, bufferAttrCounts )
  if ( TRIM(varAttrName).ne.TRIM(bufferAttrName) ) then
     print*, "Error: the attribute name differs."
     print*, "       Inquired_Attribute_Name =  ", TRIM(varAttrName)
     print*, "       Existing_Attribute_Name =  ", TRIM(bufferAttrName)
     errStatus = FAIL
     return
  endif
  if ( bufferAttrType .ne. DFNT_INT16 ) then
     print*, "Error: the attribute type is not DFNT_INT16, but ", &
             TRIM(H4_InquireType(bufferAttrType))
     errStatus = FAIL
     return
  endif
! allocate space for attribute value
  allocate( varAttrValue(bufferAttrCounts), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the attribute: ", varAttrName
     return
  endif
! get the value of the attribute
  errStatus = sfrnatt( var_sds_id, var_attr_id, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to read the value of the attribtue:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file: ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_ReadSDSAttr1d_Integer1( fileName, varName, varAttrName, varAttrValue ) &
                      result (errStatus) 
!$$$ subprogram documentation block
!       .               .                  .
!  subprogram: H4_ReadSDSAttr1d_Integer1
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Read the 8-bit signed integer 1d array of the attribute
!
!  revision history:
!
!  input arguments:
!    fileName        - name of the HDF4 file
!    varName         - name of the variables that contains the inquired attribute     
!    varAttrName     - name of the inquired attribtue
!    varAttrValue    - value of the inquired attribute
!
!  output arguments:
!    errStatus       - succeed to get the attribute value if return SUCCEED(0),
!                      otherwise return FAIL (-1)
!$$$  
  implicit none

  character(*),intent(in)  :: fileName
  character(*),intent(in)  :: varName
  character(*),intent(in)  :: varAttrName
  integer(i1),allocatable  :: varAttrValue(:)
  integer(i_kind)          :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id, var_attr_id
  character(MAX_CHAR_LENGTH) :: bufferAttrName
  integer(i4) :: bufferAttrType, bufferAttrCounts

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_READ )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! retrieva the index of the attribute
  var_attr_id = sffattr( var_sds_id, varAttrName ) 
  if ( var_attr_id .eq. FAIL ) then
     print*, "Error: failure to get attr_id of the attribute:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! check the properties of the inquired atrribute 
  errStatus = sfgainfo( var_sds_id, var_attr_id, bufferAttrName, &
                  bufferAttrType, bufferAttrCounts )
  if ( TRIM(varAttrName).ne.TRIM(bufferAttrName) ) then
     print*, "Error: the attribute name differs."
     print*, "       Inquired_Attribute_Name =  ", TRIM(varAttrName)
     print*, "       Existing_Attribute_Name =  ", TRIM(bufferAttrName)
     errStatus = FAIL
     return
  endif
  if ( bufferAttrType .ne. DFNT_INT8 ) then
     print*, "Error: the attribute type is not DFNT_INT8, but ", &
             TRIM(H4_InquireType(bufferAttrType))
     errStatus = FAIL
     return
  endif
! allocate space for attribute value
  allocate( varAttrValue(bufferAttrCounts), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the attribute: ", varAttrName
     return
  endif
! get the value of the attribute
  errStatus = sfrnatt( var_sds_id, var_attr_id, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to read the value of the attribtue:  ", varAttrName
     print*, "       of the var:  ", varName
     print*, "       of the file: ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_WriteSDS3d_Real8( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS3d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 64-bit real 3D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  real(r8),            intent(in) :: varValue(:,:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_THREE), varStart(RANK_THREE), varStride(RANK_THREE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_FLOAT64, RANK_THREE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif
! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDS3d_Real4( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS3d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 32-bit real 3D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  real(r4),            intent(in) :: varValue(:,:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_THREE), varStart(RANK_THREE), varStride(RANK_THREE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_FLOAT32, RANK_THREE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif
! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_WriteSDS3d_Integer4( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS3d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 32-bit signed integer 3D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i4),         intent(in) :: varValue(:,:,:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_THREE), varStart(RANK_THREE), varStride(RANK_THREE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT32, RANK_THREE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  !  if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif
! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exist."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_WriteSDSAttr1d_String( fileName, varName, varAttrName, varAttrValue) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDSAttr1d_String
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write character string into one SDS attribute 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varAttrName   - name of the attribute
!    varAttrValue  - value of the attribute
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  character(*),intent(in) :: varAttrName
  character(*),intent(in) :: varAttrValue
  integer(i_kind)         :: errStatus
! local vars
  integer(i4) :: sd_id, var_id, var_sds_id 
  integer(i4) :: varAttrLength

  errStatus = FAIL
  varAttrLength = LEN_TRIM(varAttrValue)
  if ( varAttrLength > MAX_CHAR_LENGTH ) then
     print*, "Error: Length( ", varAttrName, " ) is greater than ", MAX_CHAR_LENGTH
     return
  endif
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! set attribute value
  errStatus = sfscatt( var_sds_id, varAttrName, DFNT_CHAR8, &
                       varAttrLength, varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to set the value of the attribute: ", varAttrName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_WriteSDSAttr1d_Real8( fileName, varName, varAttrName, varAttrValue) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDSAttr1d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write character string into one SDS attribute 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varAttrName   - name of the attribute
!    varAttrValue  - value of the attribute
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  character(*),intent(in) :: varAttrName
  real(r8),intent(in)     :: varAttrValue(:)
  integer(i_kind)         :: errStatus
! local vars
  integer(i4) :: sd_id, var_id, var_sds_id 
  integer(i4) :: varAttrDims(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! set attribute value
  varAttrDims = SHAPE( varAttrValue )
  errStatus = sfsnatt( var_sds_id, varAttrName, DFNT_FLOAT64, &
                       varAttrDims(1), varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to set the value of the attribute: ", varAttrName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDSAttr1d_Real4( fileName, varName, varAttrName, varAttrValue) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDSAttr1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write character string into one SDS attribute 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varAttrName   - name of the attribute
!    varAttrValue  - value of the attribute
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  character(*),intent(in) :: varAttrName
  real(r4),intent(in)     :: varAttrValue(:)
  integer(i_kind)         :: errStatus
! local vars
  integer(i4) :: sd_id, var_id, var_sds_id 
  integer(i4) :: varAttrDims(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! set attribute value
  varAttrDims = SHAPE( varAttrValue )
  errStatus = sfsnatt( var_sds_id, varAttrName, DFNT_FLOAT32, &
                       varAttrDims(1), varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to set the value of the attribute: ", varAttrName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_WriteSDSAttr1d_Integer4( fileName, varName, varAttrName, varAttrValue) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDSAttr1d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write character string into one SDS attribute 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varAttrName   - name of the attribute
!    varAttrValue  - value of the attribute
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  character(*),intent(in) :: varAttrName
  integer(i4),intent(in)  :: varAttrValue(:)
  integer(i_kind)         :: errStatus
! local vars
  integer(i4) :: sd_id, var_id, var_sds_id 
  integer(i4) :: varAttrDims(RANK_ONE)

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
     print*, "Error: failure to get var_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif 
! retrieve the sds index of the var
  var_sds_id =sfselect( sd_id, var_id )
  if ( var_sds_id == FAIL ) then
     print*, "Error: failure to get sds_id of the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! set attribute value
  varAttrDims = SHAPE( varAttrValue )
  errStatus = sfsnatt( var_sds_id, varAttrName, DFNT_INT32, &
                       varAttrDims(1), varAttrValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to set the value of the attribute: ", varAttrName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction

function H4_ReadVS1d_Real4( fileName, VDataName, fieldName, fieldValue) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadVS1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-10-01
!
!  purpose:
!    Read 32-bit float Vdata 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    VDataName     - name of the Vdata
!    fieldName     - name of the inquired field
!    fieldValue    - 1d array of the field value
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: VDataName
  character(*),intent(in) :: fieldName
  real(r4),allocatable    :: fieldValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4),parameter   :: recordIndex = 0                ! always read from the first element
  integer(i4)             :: file_id, vdata_ref, vdata_id, rec_pos
  integer(i4)             :: nRecords, nRecordsOut


  errStatus = FAIL
! open the HDF file for reading
  file_id = hopen( TRIM(fileName), DFACC_READ, 0 )
  if ( file_id .eq. FAIL ) then
     print*, "Error: fail to get the file_id of the file: ", TRIM(fileName)
     return 
  endif

! initialize the VS interface
  errStatus = vfstart( file_id )

! get the reference number of the vdata, whoes name is specified by the 
! vDataName
  vdata_ref = vsffnd( file_id, VDataName )
  if ( vdata_ref .eq. FAIL ) then
     print*, "Error: fail to get the vdata_ref of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! attach to the vdata for reading if it is found
  vdata_id = vsfatch( file_id, vdata_ref, 'r' )
  if ( vdata_id .eq. FAIL ) then
     print*, "Error: fail to get the vdata_id of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  
! retrieve the number of the records
  errStatus = vsqfnelt( vdata_id, nRecords )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to get the number of records of the VData:", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

  allocate( fieldValue(nRecords), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the fieldName: ", TRIM(fieldName)
     return
  endif

! Specify the fields that will be read
  errStatus = vsfsfld(vdata_id, fieldName )

! place the current point to the position specfied in recordIndex 
  rec_pos = vsfseek( vdata_id, recordIndex )

! read the next nRecords fromt the VData
  nRecordsOut = vsfrd( vdata_id, fieldValue, nRecords, FULL_INTERLACE )
  if ( nRecordsOut /= nRecordsOut ) then
     print*, "Error: nRecords /= nRecords of the fieldName:", TRIM(fieldName)  
     errStatus = FAIL
     return
  elseif ( nRecordsOut == 0 ) then
     print*, "Error: fail to read the value of the fieldName:", TRIM(fieldName)
     errStatus = FAIL
     return
  endif

! terminate access to the Vdata and VSinterface, and close the HDF file
  errStatus = vsfdtch( vdata_id )
  errStatus = vfend( file_id )
  errStatus = hclose( file_id )
 
endfunction

function H4_ReadVS1d_Real8( fileName, VDataName, fieldName, fieldValue) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadVS1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-10-01
!
!  purpose:
!    Read 64-bit float Vdata 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    VDataName     - name of the Vdata
!    fieldName     - name of the inquired field
!    fieldValue    - 1d array of the field value
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: VDataName
  character(*),intent(in) :: fieldName
  real(r8),allocatable    :: fieldValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4),parameter   :: recordIndex = 0                ! always read from the first element
  integer(i4)             :: file_id, vdata_ref, vdata_id, rec_pos
  integer(i4)             :: nRecords, nRecordsOut


  errStatus = FAIL
! open the HDF file for reading
  file_id = hopen( TRIM(fileName), DFACC_READ, 0 )
  if ( file_id .eq. FAIL ) then
     print*, "Error: fail to get the file_id of the file: ", TRIM(fileName)
     return 
  endif

! initialize the VS interface
  errStatus = vfstart( file_id )

! get the reference number of the vdata, whoes name is specified by the 
! vDataName
  vdata_ref = vsffnd( file_id, VDataName )
  if ( vdata_ref .eq. FAIL ) then
     print*, "Error: fail to get the vdata_ref of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! attach to the vdata for reading if it is found
  vdata_id = vsfatch( file_id, vdata_ref, 'r' )
  if ( vdata_id .eq. FAIL ) then
     print*, "Error: fail to get the vdata_id of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  
! retrieve the number of the records
  errStatus = vsqfnelt( vdata_id, nRecords )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to get the number of records of the VData:", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

  allocate( fieldValue(nRecords), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the fieldName: ", TRIM(fieldName)
     return
  endif

! Specify the fields that will be read
  errStatus = vsfsfld(vdata_id, fieldName )

! place the current point to the position specfied in recordIndex 
  rec_pos = vsfseek( vdata_id, recordIndex )

! read the next nRecords fromt the VData
  nRecordsOut = vsfrd( vdata_id, fieldValue, nRecords, FULL_INTERLACE )
  if ( nRecordsOut /= nRecordsOut ) then
     print*, "Error: nRecords /= nRecords of the fieldName:", TRIM(fieldName)  
     errStatus = FAIL
     return
  elseif ( nRecordsOut == 0 ) then
     print*, "Error: fail to read the value of the fieldName:", TRIM(fieldName)
     errStatus = FAIL
     return
  endif

! terminate access to the Vdata and VSinterface, and close the HDF file
  errStatus = vsfdtch( vdata_id )
  errStatus = vfend( file_id )
  errStatus = hclose( file_id )
 
endfunction


function H4_ReadVS1d_Integer1( fileName, VDataName, fieldName, fieldValue ) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadVS1d_Integer1
!    programmer: Cheng Da            org: fsu          date: 2014-10-01
!
!  purpose:
!    Read 8-bit signed integer Vdata 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    VDataName     - name of the Vdata
!    fieldName     - name of the inquired field
!    fieldValue    - 1d array of the field value
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: VDataName
  character(*),intent(in) :: fieldName
  integer(i1),allocatable :: fieldValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4),parameter   :: recordIndex = 0                ! always read from the first element
  integer(i4)             :: file_id, vdata_ref, vdata_id, rec_pos
  integer(i4)             :: nRecords, nRecordsOut


  errStatus = FAIL
! open the HDF file for reading
  file_id = hopen( TRIM(fileName), DFACC_READ, 0 )
  if ( file_id .eq. FAIL ) then
     print*, "Error: fail to get the file_id of the file: ", TRIM(fileName)
     return 
  endif

! initialize the VS interface
  errStatus = vfstart( file_id )

! get the reference number of the vdata, whoes name is specified by the 
! vDataName
  vdata_ref = vsffnd( file_id, VDataName )
  if ( vdata_ref .eq. FAIL ) then
     print*, "Error: fail to get the vdata_ref of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! attach to the vdata for reading if it is found
  vdata_id = vsfatch( file_id, vdata_ref, 'r' )
  if ( vdata_id .eq. FAIL ) then
     print*, "Error: fail to get the vdata_id of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  
! retrieve the number of the records
  errStatus = vsqfnelt( vdata_id, nRecords )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to get the number of records of the VData:", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

  allocate( fieldValue(nRecords), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the fieldName: ", TRIM(fieldName)
     return
  endif

! Specify the fields that will be read
  errStatus = vsfsfld(vdata_id, fieldName )

! place the current point to the position specfied in recordIndex 
  rec_pos = vsfseek( vdata_id, recordIndex )

! read the next nRecords fromt the VData
  nRecordsOut = vsfrd( vdata_id, fieldValue, nRecords, FULL_INTERLACE )
  if ( nRecordsOut /= nRecordsOut ) then
     print*, "Error: nRecords /= nRecords of the fieldName:", TRIM(fieldName)  
     errStatus = FAIL
     return
  elseif ( nRecordsOut == 0 ) then
     print*, "Error: fail to read the value of the fieldName:", TRIM(fieldName)
     errStatus = FAIL
     return
  endif

! terminate access to the Vdata and VSinterface, and close the HDF file
  errStatus = vsfdtch( vdata_id )
  errStatus = vfend( file_id )
  errStatus = hclose( file_id )
 
endfunction


function H4_ReadVS1d_Integer2( fileName, VDataName, fieldName, fieldValue ) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadVS1d_Integer2
!    programmer: Cheng Da            org: fsu          date: 2014-10-01
!
!  purpose:
!    Read 16-bit signed integer Vdata 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    VDataName     - name of the Vdata
!    fieldName     - name of the inquired field
!    fieldValue    - 1d array of the field value
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: VDataName
  character(*),intent(in) :: fieldName
  integer(i2),allocatable :: fieldValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4),parameter   :: recordIndex = 0                ! always read from the first element
  integer(i4)             :: file_id, vdata_ref, vdata_id, rec_pos
  integer(i4)             :: nRecords, nRecordsOut


  errStatus = FAIL
! open the HDF file for reading
  file_id = hopen( TRIM(fileName), DFACC_READ, 0 )
  if ( file_id .eq. FAIL ) then
     print*, "Error: fail to get the file_id of the file: ", TRIM(fileName)
     return 
  endif

! initialize the VS interface
  errStatus = vfstart( file_id )

! get the reference number of the vdata, whoes name is specified by the 
! vDataName
  vdata_ref = vsffnd( file_id, VDataName )
  if ( vdata_ref .eq. FAIL ) then
     print*, "Error: fail to get the vdata_ref of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! attach to the vdata for reading if it is found
  vdata_id = vsfatch( file_id, vdata_ref, 'r' )
  if ( vdata_id .eq. FAIL ) then
     print*, "Error: fail to get the vdata_id of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  
! retrieve the number of the records
  errStatus = vsqfnelt( vdata_id, nRecords )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to get the number of records of the VData:", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

  allocate( fieldValue(nRecords), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the fieldName: ", TRIM(fieldName)
     return
  endif

! Specify the fields that will be read
  errStatus = vsfsfld(vdata_id, fieldName )

! place the current point to the position specfied in recordIndex 
  rec_pos = vsfseek( vdata_id, recordIndex )

! read the next nRecords fromt the VData
  nRecordsOut = vsfrd( vdata_id, fieldValue, nRecords, FULL_INTERLACE )
  if ( nRecordsOut /= nRecordsOut ) then
     print*, "Error: nRecords /= nRecords of the fieldName:", TRIM(fieldName)  
     errStatus = FAIL
     return
  elseif ( nRecordsOut == 0 ) then
     print*, "Error: fail to read the value of the fieldName:", TRIM(fieldName)
     errStatus = FAIL
     return
  endif

! terminate access to the Vdata and VSinterface, and close the HDF file
  errStatus = vsfdtch( vdata_id )
  errStatus = vfend( file_id )
  errStatus = hclose( file_id )
 
endfunction


function H4_ReadVS1d_Integer4( fileName, VDataName, fieldName, fieldValue ) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadVS1d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-10-01
!
!  purpose:
!    Read 32-bit signed integer Vdata 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    VDataName     - name of the Vdata
!    fieldName     - name of the inquired field
!    fieldValue    - 1d array of the field value
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: VDataName
  character(*),intent(in) :: fieldName
  integer(i4),allocatable :: fieldValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4),parameter   :: recordIndex = 0                ! always read from the first element
  integer(i4)             :: file_id, vdata_ref, vdata_id, rec_pos
  integer(i4)             :: nRecords, nRecordsOut


  errStatus = FAIL
! open the HDF file for reading
  file_id = hopen( TRIM(fileName), DFACC_READ, 0 )
  if ( file_id .eq. FAIL ) then
     print*, "Error: fail to get the file_id of the file: ", TRIM(fileName)
     return 
  endif

! initialize the VS interface
  errStatus = vfstart( file_id )

! get the reference number of the vdata, whoes name is specified by the 
! vDataName
  vdata_ref = vsffnd( file_id, VDataName )
  if ( vdata_ref .eq. FAIL ) then
     print*, "Error: fail to get the vdata_ref of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! attach to the vdata for reading if it is found
  vdata_id = vsfatch( file_id, vdata_ref, 'r' )
  if ( vdata_id .eq. FAIL ) then
     print*, "Error: fail to get the vdata_id of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  
! retrieve the number of the records
  errStatus = vsqfnelt( vdata_id, nRecords )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to get the number of records of the VData:", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

  allocate( fieldValue(nRecords), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the fieldName: ", TRIM(fieldName)
     return
  endif

! Specify the fields that will be read
  errStatus = vsfsfld(vdata_id, fieldName )

! place the current point to the position specfied in recordIndex 
  rec_pos = vsfseek( vdata_id, recordIndex )

! read the next nRecords fromt the VData
  nRecordsOut = vsfrd( vdata_id, fieldValue, nRecords, FULL_INTERLACE )
  if ( nRecordsOut /= nRecordsOut ) then
     print*, "Error: nRecords /= nRecords of the fieldName:", TRIM(fieldName)  
     errStatus = FAIL
     return
  elseif ( nRecordsOut == 0 ) then
     print*, "Error: fail to read the value of the fieldName:", TRIM(fieldName)
     errStatus = FAIL
     return
  endif

! terminate access to the Vdata and VSinterface, and close the HDF file
  errStatus = vsfdtch( vdata_id )
  errStatus = vfend( file_id )
  errStatus = hclose( file_id )
 
endfunction


function H4_ReadVS1d_Integer8( fileName, VDataName, fieldName, fieldValue ) &
result ( errStatus ) 
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_ReadVS1d_Integer8
!    programmer: Cheng Da            org: fsu          date: 2014-10-01
!
!  purpose:
!    Read 64-bit signed integer Vdata 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    VDataName     - name of the Vdata
!    fieldName     - name of the inquired field
!    fieldValue    - 1d array of the field value
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: VDataName
  character(*),intent(in) :: fieldName
  integer(i8),allocatable :: fieldValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(i4),parameter   :: recordIndex = 0                ! always read from the first element
  integer(i4)             :: file_id, vdata_ref, vdata_id, rec_pos
  integer(i4)             :: nRecords, nRecordsOut


  errStatus = FAIL
! open the HDF file for reading
  file_id = hopen( TRIM(fileName), DFACC_READ, 0 )
  if ( file_id .eq. FAIL ) then
     print*, "Error: fail to get the file_id of the file: ", TRIM(fileName)
     return 
  endif

! initialize the VS interface
  errStatus = vfstart( file_id )

! get the reference number of the vdata, whoes name is specified by the 
! vDataName
  vdata_ref = vsffnd( file_id, VDataName )
  if ( vdata_ref .eq. FAIL ) then
     print*, "Error: fail to get the vdata_ref of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! attach to the vdata for reading if it is found
  vdata_id = vsfatch( file_id, vdata_ref, 'r' )
  if ( vdata_id .eq. FAIL ) then
     print*, "Error: fail to get the vdata_id of the VData: ", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  
! retrieve the number of the records
  errStatus = vsqfnelt( vdata_id, nRecords )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to get the number of records of the VData:", TRIM(VDataName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

  allocate( fieldValue(nRecords), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the fieldName: ", TRIM(fieldName)
     return
  endif

! Specify the fields that will be read
  errStatus = vsfsfld(vdata_id, fieldName )

! place the current point to the position specfied in recordIndex 
  rec_pos = vsfseek( vdata_id, recordIndex )

! read the next nRecords fromt the VData
  nRecordsOut = vsfrd( vdata_id, fieldValue, nRecords, FULL_INTERLACE )
  if ( nRecordsOut /= nRecordsOut ) then
     print*, "Error: nRecords /= nRecords of the fieldName:", TRIM(fieldName)  
     errStatus = FAIL
     return
  elseif ( nRecordsOut == 0 ) then
     print*, "Error: fail to read the value of the fieldName:", TRIM(fieldName)
     errStatus = FAIL
     return
  endif

! terminate access to the Vdata and VSinterface, and close the HDF file
  errStatus = vsfdtch( vdata_id )
  errStatus = vfend( file_id )
  errStatus = hclose( file_id )
 
endfunction


function H4_WriteSDS1d_Real8( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS1d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 64-bit real 1D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  real(r8),            intent(in) :: varValue(:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_FLOAT64, RANK_ONE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  ! if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exists."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction



function H4_WriteSDS1d_Real4( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 32-bit real 1D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  real(r4),            intent(in) :: varValue(:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_FLOAT32, RANK_ONE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  ! if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exists."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction



function H4_WriteSDS1d_Integer4( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS1d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 32-bit integer 1D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i4),         intent(in) :: varValue(:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT32, RANK_ONE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  ! if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exists."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDS1d_Integer2( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS1d_Integer2
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 16-bit integer 1D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i2),         intent(in) :: varValue(:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT16, RANK_ONE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  ! if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exists."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


function H4_WriteSDS1d_Integer1( fileName, varName, varValue, gzipLevel ) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H4_WriteSDS1d_Integer1
!    programmer: Cheng Da            org: fsu          date: 2014-09-04
!
!  purpose:
!    Write 8-bit integer 1D array into HDF4 file.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF4 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!    gzipLevel     - the compression level of the data using GZIP
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$  
  implicit none

  character(*),        intent(in) :: fileName
  character(*),        intent(in) :: varName
  integer(i1),         intent(in) :: varValue(:)
  integer(i4),optional,intent(in) :: gzipLevel
  integer(i_kind)                 :: errStatus
! local arguments
  integer(i4) :: sd_id, var_id, var_sds_id
  integer(i4) :: varDims(RANK_ONE), varStart(RANK_ONE), varStride(RANK_ONE)
  integer(i4) :: comp_prm(MAX_COMP_ELEMENTS), compMethod
  logical     :: is_new_var

  errStatus = FAIL
! open SD interface
  sd_id = sfstart( fileName, DFACC_WRITE )
  if ( sd_id == FAIL ) then
     print*, "Error: failure to get sd_id of the file:  ", fileName
     return
  endif
! retrieve index of the var
  varDims = SHAPE( varValue )
  var_id = sfn2index( sd_id, varName )
  if ( var_id == FAIL ) then
  ! if the var does not exist, then create it
     is_new_var = .true.
     print*, "Warning: var ( ", varName, " ) does not exist, now creating it."
     var_sds_id = sfcreate( sd_id, varName, DFNT_INT8, RANK_ONE, &
                        varDims )
     if (var_sds_id == FAIL ) then
        print*, "Error: fail to create the var:  ", varName
        errStatus = FAIL
        return
     endif
  else
  ! if the var exists, retrieve the sds index of the var
     is_new_var = .false.
     print*, "Warning: updating the var : ", varName
     var_sds_id =sfselect( sd_id, var_id )
     if ( var_sds_id == FAIL ) then
        print*, "Error: failure to get sds_id of the var:  ", varName
        print*, "       of the file:  ", fileName
        return
     endif
  endif

! set the compression level
  if ( present( gzipLevel ) ) then
     ! check whether the data is already compressed
     errStatus = sfgcompress( var_sds_id, compMethod, comp_prm ) 
     if ( compMethod == COMP_CODE_NONE .and. is_new_var ) then 
        compMethod = COMP_CODE_DEFLATE
        comp_prm(1) = gzipLevel
        errStatus = sfscompress( var_sds_id, compMethod, comp_prm(1) )    
     else
        print*, "Warning: the var (", varName, ") already exists."
        print*, "         Neglect the GZIP compression flag."
     endif
   endif
 
! write data
  varStart = 0
  varStride = 1
  errStatus = sfwdata( var_sds_id, varStart, varStride, varDims, varValue )
  if ( errStatus .eq. FAIL ) then
     print*, "Error: fail to write the var:  ", varName
     print*, "       of the file:  ", fileName
     return
  endif
! close SDS
  errStatus = sfendacc( var_sds_id )
! close SD
  errStatus = sfend( sd_id )

endfunction


endmodule
