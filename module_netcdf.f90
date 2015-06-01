Module Module_NetCDF
!$$$  program documentation block
!         .           .            .
!  program name: Module_NetCDF
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
  Use NetCDF, Only: NF90_NOWRITE, NF90_NOERR, &
                    NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_SHORT, &
                    NF90_Open, NF90_Close, &
                    NF90_Inq_Varid, NF90_Inquire_Variable, NF90_Inquire_Dimension, &
                    NF90_Get_Var
  Implicit None

  Private
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


!-------------------------
! Functions & Subroutines
!-------------------------
Contains

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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "Error: var type mismatches."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  Endif

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  Endif 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "Error: var type mismatches."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_ONE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_ONE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  If ( varSize(1) /= SIZE(varValues,1) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  Endif
 ! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_TWO ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_TWO"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_THREE ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_THREE"
     Stop
  EndIf
! Check var sizes  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(1), len=varSize(1) )
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(2), len=varSize(2) )  
  errStatus = NF90_Inquire_Dimension( ncId, dimIds(3), len=varSize(3) )  
  If ( varSize(1) /= SIZE(varValues,1) .or. &
       varSize(2) /= SIZE(varValues,2) .or. &
       varSize(3) /= SIZE(varValues,3) ) Then
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_SHORT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
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
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_INT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
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
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_FLOAT ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
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
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
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
     Print*, "Error: fail to get the file id:",TRIM(fileName)
     Stop
  EndIf

! Inquire varId
  errStatus = NF90_Inq_Varid( ncId, TRIM(varName), varId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var id: ", TRIM(varName)
     Stop
  EndIf 

! Retreive infos of the inquired var
  errStatus = NF90_Inquire_Variable( ncId, varId, ndims = nDims, &
                                     xtype=varType, dimids = dimIds )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to retrieve info of the var: ", TRIM(varName)
     Stop
  EndIf

! Check var type
  If ( varType /= NF90_DOUBLE ) Then
     Print*, "Error: var type mismatch."
     Print*, "       the type of var ( ", TRIM(varName), " ) is ", varType
     Stop
  EndIf
! Check var dimension
  If ( nDims /= RANK_FOUR ) Then
     Print*, "Error: var ( ", TRIM(varName), " ) is not of RANK_FOUR"
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
     Print*, "Error: var size mismatches"
     Write(*,"(2(A,I8))") "dim(1)=",varSize(1), ", dim_input(1)=", SIZE(varValues,1)
     Write(*,"(2(A,I8))") "dim(2)=",varSize(2), ", dim_input(2)=", SIZE(varValues,2)
     Write(*,"(2(A,I8))") "dim(3)=",varSize(3), ", dim_input(3)=", SIZE(varValues,3)
     Write(*,"(2(A,I8))") "dim(4)=",varSize(4), ", dim_input(4)=", SIZE(varValues,4)
     Stop
  Endif

! Get var value
  errStatus = NF90_Get_Var( ncId, varId, varValues )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to get the var value: ", TRIM(varName)
     Stop
  EndIf

! Close the NetCDF file
  errStatus = NF90_Close( ncId )
  If ( errStatus /= NF90_NOERR ) Then
     Print*, "Error: fail to the close the var: ", TRIM(fileName)
     Stop
  EndIf

EndSubroutine





EndModule

