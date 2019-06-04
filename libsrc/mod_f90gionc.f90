Module mod_f90gionc
!$$$  program documentation block
!         .           .            .
!  program name: mod_f90gionc
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  purpose:
!    Includes subroutines & functions related to the NetCDF files.
!
!  revision history:
!    2014-Nov-30     Da,Cheng    - creator
!    2015-Jan-20     Da,Cheng    - add 4d data read routine
!
!  file dependencies:
!
!  attributes: 
!    language: fortran 90
!    machine : 
!
!
!$$$ end documentation block
  Use NetCDF, Only: NF90_NOWRITE, NF90_WRITE, NF90_NOERR, NF90_CLOBBER, &
                    NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_SHORT, NF90_UNLIMITED, &
                    NF90_Open, NF90_Close, &
                    NF90_inquire, NF90_Inq_Varid, NF90_Inquire_Variable, NF90_Inquire_Dimension, &
                    NF90_Get_Var, NF90_Put_Var, &
                    NF90_Create, NF90_Def_dim, NF90_Def_Var, NF90_Enddef, NF90_reDef
  Implicit None

  Private
  !
  !
  !
  Public :: NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_SHORT
  !
  ! internal vars
  !
  Public :: NC_Check
  !
  ! read utility
  !
  Public :: NC_ReadVar1d
  Public :: NC_ReadVar2d
  Public :: NC_ReadVar3d
  Public :: NC_ReadVar4d

  Public :: NC_ReadVar1d_Integer2
  Public :: NC_ReadVar1d_Integer4
  Public :: NC_ReadVar1d_Real4
  Public :: NC_ReadVar1d_Real8

  Public :: NC_ReadVar2d_Integer2
  Public :: NC_ReadVar2d_Integer4
  Public :: NC_ReadVar2d_Real4
  Public :: NC_ReadVar2d_Real8

  Public :: NC_ReadVar3d_Integer2
  Public :: NC_ReadVar3d_Integer4
  Public :: NC_ReadVar3d_Real4
  Public :: NC_ReadVar3d_Real8

  Public :: NC_ReadVar4d_Integer2
  Public :: NC_ReadVar4d_Integer4
  Public :: NC_ReadVar4d_Real4
  Public :: NC_ReadVar4d_Real8

  !
  ! write utility
  !
  Public :: NC_CreateFile
  Public :: NC_CreateDims
  Public :: NC_CreateVars  ! also used to append new vars

  Public :: NC_AddDim

  Public :: NC_UpdateVar1d
  Public :: NC_UpdateVar2d
  Public :: NC_UpdateVar3d
  Public :: NC_UpdateVar4d

  Public :: NC_UpdateVar1d_Integer2
  Public :: NC_UpdateVar1d_Integer4
  Public :: NC_UpdateVar1d_Real4
  Public :: NC_UpdateVar1d_Real8

  Public :: NC_UpdateVar2d_Integer2
  Public :: NC_UpdateVar2d_Integer4
  Public :: NC_UpdateVar2d_Real4
  Public :: NC_UpdateVar2d_Real8

  Public :: NC_UpdateVar3d_Integer2
  Public :: NC_UpdateVar3d_Integer4
  Public :: NC_UpdateVar3d_Real4
  Public :: NC_UpdateVar3d_Real8

  Public :: NC_UpdateVar4d_Integer2
  Public :: NC_UpdateVar4d_Integer4
  Public :: NC_UpdateVar4d_Real4
  Public :: NC_UpdateVar4d_Real8




!-----------------------
! Procedure overloading
!-----------------------

  Interface NC_ReadVar1d
    Module Procedure NC_ReadVar1d_Integer2
    Module Procedure NC_ReadVar1d_Integer4
    Module Procedure NC_ReadVar1d_Real4
    Module Procedure NC_ReadVar1d_Real8
  EndInterface

  Interface NC_ReadVar2d
    Module Procedure NC_ReadVar2d_Integer2
    Module Procedure NC_ReadVar2d_Integer4
    Module Procedure NC_ReadVar2d_Real4
    Module Procedure NC_ReadVar2d_Real8
  EndInterface

  Interface NC_ReadVar3d
    Module Procedure NC_ReadVar3d_Integer2
    Module Procedure NC_ReadVar3d_Integer4
    Module Procedure NC_ReadVar3d_Real4
    Module Procedure NC_ReadVar3d_Real8
  EndInterface

  Interface NC_ReadVar4d
    Module Procedure NC_ReadVar4d_Integer2
    Module Procedure NC_ReadVar4d_Integer4
    Module Procedure NC_ReadVar4d_Real4
    Module Procedure NC_ReadVar4d_Real8
  EndInterface

  Interface NC_UpdateVar1d
    Module Procedure NC_UpdateVar1d_Integer2
    Module Procedure NC_UpdateVar1d_Integer4
    Module Procedure NC_UpdateVar1d_Real4
    Module Procedure NC_UpdateVar1d_Real8
  EndInterface

  Interface NC_UpdateVar2d
    Module Procedure NC_UpdateVar2d_Integer2
    Module Procedure NC_UpdateVar2d_Integer4
    Module Procedure NC_UpdateVar2d_Real4
    Module Procedure NC_UpdateVar2d_Real8
  EndInterface

  Interface NC_UpdateVar3d
    Module Procedure NC_UpdateVar3d_Integer2
    Module Procedure NC_UpdateVar3d_Integer4
    Module Procedure NC_UpdateVar3d_Real4
    Module Procedure NC_UpdateVar3d_Real8
  EndInterface

  Interface NC_UpdateVar4d
    Module Procedure NC_UpdateVar4d_Integer2
    Module Procedure NC_UpdateVar4d_Integer4
    Module Procedure NC_UpdateVar4d_Real4
    Module Procedure NC_UpdateVar4d_Real8
  EndInterface



!----------
! Constant
!----------

  Integer,Parameter :: i2 = 2
  Integer,Parameter :: i4 = 4
  Integer,Parameter :: i8 = 8
  Integer,Parameter :: r4 = 4
  Integer,Parameter :: r8 = 8

  Integer(i4),Parameter :: RANK_ONE   = 1
  Integer(i4),Parameter :: RANK_TWO   = 2
  Integer(i4),Parameter :: RANK_THREE = 3
  Integer(i4),Parameter :: RANK_FOUR  = 4

  
  Type :: t_NC_Dims
       integer :: nDims
       character(80),allocatable :: dimNames(:)
       logical,      allocatable :: ldimUnlimited(:)
       integer,      allocatable :: dimLength(:)
       integer,      allocatable :: dimIds(:) ! private 
  Endtype

  Type :: t_NC_Vars
       integer :: nVars
       character(80),allocatable :: varNames(:)
       integer,      allocatable :: varTypes(:)
       integer,      allocatable :: varMaxDims(:)
       character(80),allocatable :: varDimNames(:,:) ! 4*nVars

       integer,      allocatable :: varIds(:)
       integer,      allocatable :: varDimIds(:,:) ! 4*nVars
  Endtype

  Type :: t_NC_filedef
       character(80) :: fileName

       type(t_NC_Dims) :: dims
       type(t_NC_Vars) :: vars

  Endtype



!-------------------------
! Functions & Subroutines
!-------------------------
Contains

Subroutine NC_CreateFile(fileName)
  implicit none

  character(*),intent(in) :: fileName

  integer :: ierr, nc_fid

  ierr=NF90_Create(trim(fileName),NF90_CLOBBER,nc_fid)
  call NC_Check(ierr,"NC_CreateFiles: create file "//TRIM(fileName))
  ierr=NF90_Close(nc_fid)
  call NC_Check(ierr,"NC_CreateFiles: close file "//TRIM(fileName))

Endsubroutine 


Subroutine NC_AddDim(fileName,dimName,lunlimited,dimLength)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: dimName
  logical,     intent(in) :: lunlimited
  integer,     intent(in) :: dimLength

  integer :: ierr, ncId, i, ndims, dimId
  character(80),allocatable :: dimNames(:)


  ierr = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(ierr,"NC_AddDim: open file "//trim(fileName)) 
  !
  ! get the existing dims
  !
  ierr = NF90_inquire(ncid, nDimensions=ndims)
  call NC_Check(ierr,"NC_AddDim: inquire nDimensions "//trim(fileName)) 
 
  if (ndims>0) then
     allocate(dimNames(ndims))
     do i = 1, ndims
        ierr = nf90_inquire_dimension(ncid, dimid=i, name=dimNames(i))
        call NC_Check(ierr,"NC_AddDim: inquire dimid")
        if (trim(dimName)==trim(dimNames(i))) then
           write(*,*) "[err] NC_AddDim: dim (", trim(dimNames(i)), ") already exists"
           stop 114
        endif
     enddo
  endif
  !
  ! add new dims
  !
  ierr = NF90_reDef(ncId)
  call NC_Check(ierr,"NC_AddDim: redef") 
  if (lUnlimited) then
     ierr = NF90_Def_dim(ncId,trim(dimName),NF90_Unlimited,dimId)
  else
     ierr = NF90_Def_dim(ncId,trim(dimName),dimLength,dimId)
  endif
  call NC_Check(ierr,"NC_AddDim: define dim: "//trim(dimName))

  ierr=NF90_enddef(ncId)
  call NC_Check(ierr,"NC_AddDim: enddef")
  ierr=NF90_Close(ncId)
  call NC_Check(ierr,"NC_AddDim: close file "//TRIM(fileName))

  if(allocated(dimNames)) deallocate(dimNames)


Endsubroutine 



Subroutine NC_CreateDims(fileName,nDims,dimNames,ldimUnlimited,dimLengths)
  implicit none

  character(*),intent(in) :: fileName
  integer,     intent(in) :: nDims
  character(*),intent(in) :: dimNames(nDims)
  logical,     intent(in) :: ldimUnlimited(nDims)
  integer,     intent(in) :: dimLengths(nDims)

  integer :: dim_ids(nDims)
  integer :: n, ierr, nUnlimited, ncId

  nUnlimited = 0
  do n = 1, nDims
     if (ldimUnlimited(n)) nUnlimited = nUnlimited + 1
  enddo
  if (nUnlimited>1) then
     print*, "[err] NC_createDims: nUnlimited>1"
     do n = 1, nDims
        if (ldimUnlimited(n)) print*, "      dim: ", trim(dimNames(n))
     enddo
     stop 111
  endif

  ierr = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(ierr,"NC_CreateDims: open file "//trim(fileName)) 
  ierr = NF90_reDef(ncId)
  call NC_Check(ierr,"NC_CreateDims: redef") 
  do n = 1, nDims
     if (ldimUnlimited(n)) then
        ierr = NF90_Def_dim(ncId,trim(dimNames(n)),NF90_Unlimited,dim_ids(n))
     else
        ierr = NF90_Def_dim(ncId,trim(dimNames(n)),dimLengths(n),dim_ids(n))
     endif
     call NC_Check(ierr,"NC_CreateDims: define dim: "//trim(dimNames(n)))
  enddo
  ierr=NF90_enddef(ncId)
  call NC_Check(ierr,"NC_CreateDims: enddef")
  ierr=NF90_Close(ncId)
  call NC_Check(ierr,"NC_CreateDims: close file "//TRIM(fileName))

endsubroutine


Subroutine NC_CreateVars(fileName,nVars,varNames,varTypes,varMaxDims,varDimNames)
  Implicit none
  
  character(*),intent(in) :: fileName
  integer,     intent(in) :: nVars
  character(*),intent(in) :: varNames(nVars)
  integer,     intent(in) :: varTypes(nVars)
  integer,     intent(in) :: varMaxDims(nVars)
  character(*),intent(in) :: varDimNames(4,nVars) !

  integer :: ndims
  character(80),allocatable :: dimNames(:)
  integer,      allocatable :: dimIds(:)
  integer :: varIds(nVars), varDimIds(4,nVars)
  integer :: varDimIds1d(1), varDimIds2d(2), varDimIds3d(3), varDimIds4d(4)
  integer :: i, j, n, ierr, ncId
  logical :: lfound

  if (maxval(varMaxDims)>4) then
     write(*,*) "[err] NC_createVars: varMaxdims>5"
     stop 110
  endif

  ierr = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(ierr,"NC_CreateVars: open file "//trim(fileName)) 

  ierr = NF90_inquire(ncid, nDimensions=ndims)
  call NC_Check(ierr,"NC_CreateVars: inquire nDimensions "//trim(fileName)) 
  if (ndims<=0) then
     write(*,*) "[err] NC_createVars: nDims<=0"
     stop 112
  endif
  
  allocate(dimNames(ndims),dimIds(ndims))
  do i = 1, ndims
     dimIds(i) = i
     ierr = nf90_inquire_dimension(ncid, dimid=dimIds(i), name=dimNames(i))
     call NC_Check(ierr,"NC_CreateVars: inquire dimid")
  enddo

  do n = 1, nVars
     do j = 1, varMaxDims(n)
        lfound = .false.
        find_dimid: do i = 1, nDims
           if (trim(varDimNames(j,n))==trim(dimNames(i))) then
              varDimIds(j,n) = i
              lfound = .true.
              exit find_dimid
           endif
        enddo find_dimid
        if (.not. lfound) then
           write(*,*) "[err] NC_createVars: cannot find dimid of dim(",trim(varDimNames(j,n)),") for var (",trim(varNames(n)), ")"
           stop 113
        endif
     enddo
  enddo
  deallocate(dimNames,dimIds)

  ierr = NF90_reDef(ncId)
  call NC_Check(ierr,"NC_CreateVars: redef") 

  do n = 1, nVars
     if (varMaxDims(n)==RANK_ONE) then
        varDimIds1d(:) = varDimIds(1,n)
        ierr=nf90_def_var(ncid,trim(varNames(n)), varTypes(n), varDimIds1d, varIds(n))
     elseif (varMaxDims(n)==RANK_TWO) then
        varDimIds2d(:) = varDimIds(1:2,n)
        ierr=nf90_def_var(ncid,trim(varNames(n)), varTypes(n), varDimIds2d, varIds(n))
     elseif (varMaxDims(n)==RANK_THREE) then
        varDimIds3d(:) = varDImIds(1:3,n)
        ierr=nf90_def_var(ncid,trim(varNames(n)), varTypes(n), varDimIds3d, varIds(n))
     elseif (varMaxDims(n)==RANK_FOUR) then
        varDimIds4d(:) = varDimIds(1:4,n)
        ierr=nf90_def_var(ncid,trim(varNames(n)), varTypes(n), varDimIds4d, varIds(n))
     endif
     call NC_Check(ierr,"NC_CreateVars: define vars"//trim(varNames(n)))
  enddo
 
  ierr=NF90_enddef(ncId)
  call NC_Check(ierr,"NC_CreateVars: enddef")
  ierr=NF90_Close(ncId)
  call NC_Check(ierr,"NC_CreateVars: close file "//TRIM(fileName))


Endsubroutine 


Subroutine NC_Check(ios,cinfo)
  implicit none

  integer,intent(in) :: ios
  character(*),intent(in) :: cinfo

  if (ios/=NF90_NOERR) then
     write(*,*) "[err] ", trim(cinfo)
     stop 111
  endif

Endsubroutine NC_Check



Subroutine NC_ReadVar1d_Integer2( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar1d_Integer2
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 1d integer(kind=2) arrays
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues    - 1d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i2), Intent(inout) :: varValues(:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_ONE)
  Integer(i4) :: varSize(RANK_ONE)
  Integer(i4) :: varType
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf
    
EndSubroutine


Subroutine NC_ReadVar1d_Integer4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar1d_Integer4
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 1d integer(kind=4) arrays
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues    - 1d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i4), Intent(inout) :: varValues(:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_ONE)
  Integer(i4) :: varSize(RANK_ONE)
  Integer(i4) :: varType
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf
    
EndSubroutine


Subroutine NC_ReadVar1d_Real4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar1d_Real4
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 1d real(kind=4) arrays
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues    - 1d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r4),    Intent(inout) :: varValues(:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_ONE)
  Integer(i4) :: varSize(RANK_ONE)
  Integer(i4) :: varType
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "[err]: var type mismatches."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf
    
EndSubroutine


Subroutine NC_ReadVar1d_Real8( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar1d_Real8
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 1d real(kind=8) arrays
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues    - 1d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r8),    Intent(inout) :: varValues(:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_ONE)
  Integer(i4) :: varSize(RANK_ONE)
  Integer(i4) :: varType
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "[err]: var type mismatches."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf
    
EndSubroutine


Subroutine NC_ReadVar2d_Integer2( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar2d_Integer2
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 2d integer(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 2d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i2), Intent(inout) :: varValues(:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_TWO)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_TWO)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar2d_Integer4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar2d_Integer4
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 2d integer(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 2d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i4), Intent(inout) :: varValues(:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_TWO)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_TWO)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar2d_Real4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar2d_Real4
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 2d real(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 2d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r4),    Intent(inout) :: varValues(:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_TWO)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_TWO)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar2d_Real8( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar2d_Real8
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 2d real(kind=8) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 2d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r8),    Intent(inout) :: varValues(:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_TWO)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_TWO)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar3d_Integer2( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar3d_Integer2
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 3d integer(kind=2) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i2), Intent(inout) :: varValues(:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_THREE)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_THREE)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar3d_Integer4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar3d_Integer4
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 3d integer(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i4), Intent(inout) :: varValues(:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_THREE)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_THREE)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar3d_Real4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar3d_Real4
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 3d real(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r4),    Intent(inout) :: varValues(:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_THREE)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_THREE)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine


Subroutine NC_ReadVar3d_Real8( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar3d_Real8
!    programmer: da,cheng        org: fsu         date: 2014-Nov-30
!
!  Purpose:
!    Read 3d real(kind=8) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2014-Nov-30     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r8),    Intent(inout) :: varValues(:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_THREE)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_THREE)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine



Subroutine NC_ReadVar4d_Integer2( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar4d_Integer2
!    programmer: da,cheng        org: fsu         date: 2015-Jan-20
!
!  Purpose:
!    Read 4d integer(kind=2) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2015-Jan-20     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i2), Intent(inout) :: varValues(:,:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_FOUR)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_FOUR)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(4), len=varSize(4) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) .or. &
       varSize(4) /= SIZE(varValues,4) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine




Subroutine NC_ReadVar4d_Integer4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar4d_Integer4
!    programmer: da,cheng        org: fsu         date: 2015-Jan-20
!
!  Purpose:
!    Read 4d integer(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2015-Jan-20     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Integer(i4), Intent(inout) :: varValues(:,:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_FOUR)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_FOUR)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(4), len=varSize(4) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) .or. &
       varSize(4) /= SIZE(varValues,4) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine



Subroutine NC_ReadVar4d_Real4( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar4d_Real4
!    programmer: da,cheng        org: fsu         date: 2015-Jan-20
!
!  Purpose:
!    Read 4d real(kind=4) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2015-Jan-20     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r4),    Intent(inout) :: varValues(:,:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_FOUR)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_FOUR)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(4), len=varSize(4) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) .or. &
       varSize(4) /= SIZE(varValues,4) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine



Subroutine NC_ReadVar4d_Real8( fileName, varName, varValues )
!$$$  subprogram documentation block
!         .           .            .
!  Program name: NC_ReadVar4d_Real8
!    programmer: da,cheng        org: fsu         date: 2015-Jan-20
!
!  Purpose:
!    Read 4d real(kind=8) vars
!  
!  Input args:
!    fileName    - Name of the inquired file
!    varName     - Name of the inquired var
!    varValues   - 3d array to hold the var value
!  
!  Output args:
!    errStatus   - status flag
!  
!  Revision history:
!    2015-Jan-20     da    - creator
!
!$$$ end documentation block
  Implicit None
! Passed vars
  Character(*),Intent(in   ) :: fileName
  Character(*),Intent(in   ) :: varName
  Real(r8),    Intent(inout) :: varValues(:,:,:,:)
! Local vars
  Integer(i4) :: ncId, varId, dimIds(RANK_FOUR)
  Integer(i4) :: varType
  Integer(i4) :: varSize(RANK_FOUR)
  Integer(i4) :: nDims
  Integer(i4) :: errStatus

! Open NetCDf file 
  errStatus = NF90_Open( TRIM(fileName), NF90_NOWRITE, ncId)
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "[err]: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "[err]: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(4), len=varSize(4) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) .or. &
       varSize(4) /= SIZE(varValues,4) ) Then
     Print*, "[err]: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "[err]: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine

!-------------------------------------------------------------------------------
! write utility
!-------------------------------------------------------------------------------

!
! update1d
!
subroutine NC_UpdateVar1d_Real8(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r8),    intent(in) :: varValue(:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine 

subroutine NC_UpdateVar1d_Real4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r4),    intent(in) :: varValue(:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine


subroutine NC_UpdateVar1d_Integer4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),    intent(in) :: varValue(:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine

subroutine NC_UpdateVar1d_Integer2(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i2),    intent(in) :: varValue(:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine 
!
! update2d
!
subroutine NC_UpdateVar2d_Real8(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r8),    intent(in) :: varValue(:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine 

subroutine NC_UpdateVar2d_Real4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r4),    intent(in) :: varValue(:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine


subroutine NC_UpdateVar2d_Integer4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),    intent(in) :: varValue(:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine

subroutine NC_UpdateVar2d_Integer2(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i2),    intent(in) :: varValue(:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine 
!
! update3d
!
subroutine NC_UpdateVar3d_Real8(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r8),    intent(in) :: varValue(:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine 

subroutine NC_UpdateVar3d_Real4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r4),    intent(in) :: varValue(:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine


subroutine NC_UpdateVar3d_Integer4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),    intent(in) :: varValue(:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine

subroutine NC_UpdateVar3d_Integer2(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i2),    intent(in) :: varValue(:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine 
!
! update4d
!
subroutine NC_UpdateVar4d_Real8(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r8),    intent(in) :: varValue(:,:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine 

subroutine NC_UpdateVar4d_Real4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r4),    intent(in) :: varValue(:,:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))

endsubroutine


subroutine NC_UpdateVar4d_Integer4(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),    intent(in) :: varValue(:,:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine

subroutine NC_UpdateVar4d_Integer2(fileName,varName,varValue)
  implicit none

  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i2),    intent(in) :: varValue(:,:,:,:)

  integer :: ncId, varId
  integer :: errStatus

  errStatus = NF90_Open( TRIM(fileName), NF90_WRITE, ncId)
  call NC_Check(errStatus,"open file: "//trim(fileName))

  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  call NC_Check(errStatus,"get var id: "//trim(varName))

  errStatus = NF90_Put_Var( ncId, varId, varValue)
  call NC_Check(errStatus,"put var value: "//trim(varName))

  errStatus = NF90_Close(ncId)
  call NC_Check(errStatus,"close file: "//trim(fileName))


endsubroutine 










EndModule

