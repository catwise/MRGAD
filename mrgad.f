c-----------------------------------------------------------------------
c
c       MrgAD  -  Merge Ascending & Descending mdex files for a tile
c                 (some cloning from tftool.f)
c
c  version 1.0  B70818: initial version
c
c-----------------------------------------------------------------------
c
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      Character*5000 Line
      Character*500  InFNam, OutFNam, InFNamA, InFNamD
      Character*141  EclData
      Character*25   Field(MaxFld)      
      Character*11   Vsn, Fmt, IFmt
      Character*8    CDate, CTime
      Character*7    ChTmp1, ChTmp2
      Character*3    Flag, Flag0
      Integer*4      IArgC, LNBlnk, NArgs, NArg, nSrc, Access, nEpA,
     +               nEpD, IFa(MaxFld), IFb(MaxFld), NF, k, k1, k2, n,
     +               nRow, nBadAst1, nBadAst2, nBadW1Phot1, nBadW1Phot2,
     +               nBadAst, nBadW1Phot, KodeAst, KodePhot1, KodePhot2,
     +               KodePM, nBadPM1, nBadPM2, nBadPM, Itmp1, Itmp2,
     +               nBadW2Phot, nBadW2Phot1, nBadW2Phot2, NmdetIDerr
      Logical*4      dbg, GotIn, GotOut, GotInA, GotInD, Good1, Good2
      Real*8         ra,  dec,  sigra,  sigdec,  sigradec,
     +               ra2, dec2, sigra2, sigdec2, sigraded,
     +               EcLong, EcLat, EcLong2, EcLat2, EcLongSig,
     +               EcLongSig2, EcLatSig, EcLatSig2, R8tmp1, R8tmp2,
     +               d2r, AngDiff, dEcLong, dEcLat, dEcLongSig,
     +               dEcLatSig, sig1, sig2,dEcLongSNR, dEcLatSNR,
     +               chi2pmra, chi2pmdec, wad11, wad12, wad22,
     +               oma11, oma12, oma22, omd11, omd12, omd22,
     +               wa11,  wa12,  wa22,  wd11,  wd12,  wd22,
     +               deta, detd, detad, v11, v12, v22, v1, v2
     
c
      Data Vsn/'1.0  B70818'/, nSrc/0/, nRow/0/, d2r/1.745329252d-2/,
     +     dbg,GotIn,GotOut,GotInA,GotInD/5*.false./,
     +     nBadAst1,nBadAst2,nBadW1Phot1,nBadW1Phot2,nBadAst,
     +     nBadW1Phot,nBadW2Phot1,nBadW2Phot2,nBadW2Phot/9*0/,
     +     KodeAst,KodePhot1,KodePhot2,KodePM/4*0/,
     +     nBadPM1,nBadPM2,nBadPM/3*0/, NmdetIDerr/0/
c
      Common / VDT / CDate, CTime, Vsn
c
c-----------------------------------------------------------------------
c
      NArgs = IArgC()
      If (NArgs .lt. 8) then
        print *,'MrgAD vsn ', Vsn
        print *
        print *,'Usage: mrgad <flags specifications>'
        print *
        print *,'Where the REQUIRED flags and specifications are:'
        print *,'    -i  name of the ascending+descending gsa file'
        print *,'    -o  name of the merged output file'
        print *,'    -ia name of the ascending stf file'
        print *,'    -id name of the descending stf file'
        print *
        print *,'The OPTIONAL flag is:'
        print *,'    -d  turn on debug prints'
        stop
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      call signon('mrgad')
c
      NArg = 0
5     NArg = NArg + 1
      call GetArg(NArg,Flag)
      Flag0 = Flag
      call UpCase(Flag)
c                                      ! input gsa file
      If (Flag .eq. '-I') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,InFNam)
        if (Access(InFNam(1:lnblnk(InFNam)),' ') .ne. 0) then
          print *,'File not found: ',InFNam(1:lnblnk(InFNam))
          call exit(64)
        end if
        GotIn = .true.
c                                      ! Turn debug prints on
      else if (Flag .eq. '-D') then
        dbg = .true.
        print *,'Debug prints enabled'
c                                      ! input ascending stf file
      else if (Flag .eq. '-IA') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,InFNamA)
        if (Access(InFNamA(1:lnblnk(InFNamA)),' ') .ne. 0) then
          print *,'File not found: ',InFNamA(1:lnblnk(InFNamA))
          call exit(64)
        end if
        GotInA = .true.
c                                      ! input ascending stf file
      else if (Flag .eq. '-ID') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,InFNamD)
        if (Access(InFNamD(1:lnblnk(InFNamD)),' ') .ne. 0) then
          print *,'File not found: ',InFNamD(1:lnblnk(InFNamD))
          call exit(64)
        end if
        GotInD = .true.
c                                      ! output   file
      else if (Flag .eq. '-O') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,OutFNam)
        GotOut = .true.
      Else
        print *,'ERROR: unrecognized command-line specification: '
     +          //Flag0
      end if
c 
      If (NArg .lt. NArgs) Go to 5
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Verify input
      If (.not.GotIn) then
        print *,'ERROR: Input gsa file not specified'
        call exit(64)
      end if
c
      If (.not.GotOut) then
        print *,'ERROR: Output filename stem not specified'
        call exit(64)
      end if
c
      If (.not.GotInA) then
        print *,'ERROR: Ascending stf filename not specified'
        call exit(64)
      end if
c
      If (.not.GotInD) then
        print *,'ERROR: Descending stf filename not specified'
        call exit(64)
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Create output table header
      open (10, file = InFNam)
10    read (10, '(a)', end=3000) Line
      if ((Line(1:1) .eq. '\') .or. Line(1:1) .eq. '|') go to 10
      nSrc = 1
20    read (10, '(a)', end=30) Line
      nSrc = nSrc + 1
      go to 20
30    if (dbg) print *,'No. of sources: ', nSrc      
      rewind(10)
c
      open (12, file = InFNamA)
      open (14, file = InFNamD)
      open (20, file = OutFNam)
      write (20,'(''\Nsrc ='',I8)') nSrc
      read (12, '(a)', end=3001) Line
      read (12, '(a)', end=3001) Line
      read (Line(22:23), *, err=3003) nEpA
      read (14, '(a)', end=3002) Line
      read (14, '(a)', end=3002) Line
      read (Line(22:23), *, err=3004) nEpD
      write (20,'(''\ number of unWISE epochs engaged:'',I2,
     +      '' ascending,'',I2,'' descending'')') nEpA, nEpD
40    read (12, '(a)', end=3001) Line
      if (Line(1:1) .eq. '\') then
        if ((Line(1:26) .ne. '\  Subset generated by STF') .and.
     +      (Line(1:26) .ne. '\  Fields retained by STF:'))   
     +   write(20,'(a)') Line(1:lnblnk(Line))
        go to 40
      end if
      close(12)
      close(14)
c
50    read (10, '(a)', end=3000) Line
      if (Line(1:1) .eq. '\') go to 50
c
      call GetFlds(Line,Field,IFa,IFb,NF)
      if (dbg) print *,'No. fields returned by GetFlds:', NF
c                                          ! verify fields in gsa file
      call ChkFld(Field(3),'ra',3)
      call ChkFld(Field(4),'dec',4)
      call ChkFld(Field(5),'sigra',5)
      call ChkFld(Field(6),'sigdec',6)
      call ChkFld(Field(7),'sigradec',7)
      call ChkFld(Field(169),'ra2',169)
      call ChkFld(Field(170),'dec2',170)
      call ChkFld(Field(171),'sigra2',171)
      call ChkFld(Field(172),'sigdec2',172)
      call ChkFld(Field(173),'sigraded',173)
c
      write (20, '(a)') Line(1:1556)
     +       //'  EcLong  | EcLongSig|  EcLat   | EcLatSig|'
     +       //'  dEcLong |dEcLongSig|  dEcLat  |dEcLatSig|'
     +       //'dEcLongSNR| dEcLatSNR| chi2pmra|chi2pmdec|'
     +       //'ka|k1|k2|km|m|'
      read (10, '(a)', end=3000) Line
      write (20, '(a)') Line(1:1556)
     +       //'    r     |     r    |     r    |    r    |'
     +       //'    r     |     r    |     r    |    r    |'
     +       //'    r     |     r    |    r    |     r   |'
     +       //' i| i| i| i|c|'
      read (10, '(a)', end=3000) Line
      write (20, '(a)') Line(1:1556)
     +       //'   deg    |   asec   |    deg   |   asec  |'
     +       //'   asec   |   asec   |   asec   |   asec  |'
     +       //'     -    |     -    |    -    |    -    |'
     +       //' -| -| -| -|-|'
      read (10, '(a)', end=3000) Line
      write (20, '(a)') Line(1:1556)
     +       //'   null   |   null   |   null   |   null  |'
     +       //'   null   |   null   |   null   |   null  |'
     +       //'   null   |   null   |   null  |   null  |'
     +       //' n| n| n| n|x|'
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Process each data line
1000  nRow = nRow + 1
      read (10, '(a)', end=3005) Line
c                                      ! Check astrometric parameters
      Good1 = index(Line(IFA(3):IFB(7)),    'null') .eq. 0
      Good2 = index(Line(IFA(169):IFB(173)),'null') .eq. 0
c
      if (Good1 .and. Good2) go to 1040
      nBadAst = nBadAst + 1
      If (Good2) then
        nBadAst1 = nBadAst1 + 1
        KodeAst  = 2
        EclData = '   null        null      null       null     '
     +        //' null       null       null       null   '
     +        //'   null       null       null      null    2  n  n  n x'
        if (dbg) print *,'Bad  ascending Astrometry on source'
     +                //Line(IFA(1):IFB(1))
      else If (Good1) then
        nBadAst2 = nBadAst2 + 1
        KodeAst  = 1
        EclData = '   null        null      null       null     '
     +        //' null       null       null       null   '
     +        //'   null       null       null      null    1  n  n  n x'
        if (dbg) print *,'Bad descending Astrometry on source'
     +                //Line(IFA(1):IFB(1))
      else
        nBadAst1 = nBadAst1 + 1
        nBadAst2 = nBadAst2 + 1
        KodeAst  = 0     
        EclData = '   null        null      null       null     '
     +        //' null       null       null       null   '
     +        //'   null       null       null      null    0  n  n  n x'
        if (dbg) print *,
     +     'Bad ascending AND descending Astrometry on source '
     +                //Line(IFA(1):IFB(1))
      end if
      if (Good2) Line(IFA(3):IFB(7)) = Line(IFA(169):IFB(173))
      go to 1200
c   
1040  KodeAst = 3                      ! Both good; read positions
      k = 3
      Read(Line(IFA(k):IFB(k)), *, err = 3006) ra
      k = 4
      Read(Line(IFA(k):IFB(k)), *, err = 3006) dec
      k = 5
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigra
      k = 6
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigdec
      k = 7
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigradec
      k = 169
      Read(Line(IFA(k):IFB(k)), *, err = 3006) ra2
      k = 170
      Read(Line(IFA(k):IFB(k)), *, err = 3006) dec2
      k = 171
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigra2
      k = 172
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigdec2
      k = 173
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigraded
c                                      ! Check for zero-point straddle
      if (dabs(ra-ra2) .gt. 180.0d0) then
        if (ra .gt. ra2) then
          ra = ra - 360.0d0
        else
          ra2 = ra2 - 360.0d0        
        end if
      end if      
c                                      ! Get Ecliptic positions
      call Cel2Ec(ra,  dec,  EcLong,  EcLat)
      call Cel2Ec(ra2, dec2, EcLong2, EcLat2)
c                                      ! Get Ecliptic position uncertainties
      sig1 = sigra/(3600.0d0*dcos(d2r*EcLat))
      sig2 = sigdec/3600.0d0
      call Cel2Ec(ra+sig1,  dec+sig2,  R8tmp1,  R8tmp2)
      EcLongSig = AngDiff(EcLong, R8tmp1)
      EcLatSig  = AngDiff(EcLat,  R8tmp2) 
      call Cel2Ec(ra+sig1,  dec-sig2,  R8tmp1,  R8tmp2)
      EcLongSig = EcLongSig + AngDiff(EcLong, R8tmp1)
      EcLatSig  = EcLatSig  + AngDiff(EcLat,  R8tmp2) 
      call Cel2Ec(ra-sig1,  dec+sig2,  R8tmp1,  R8tmp2)
      EcLongSig = EcLongSig + AngDiff(EcLong, R8tmp1)
      EcLatSig  = EcLatSig  + AngDiff(EcLat,  R8tmp2) 
      call Cel2Ec(ra-sig1,  dec-sig2,  R8tmp1,  R8tmp2)
      EcLongSig = 900.0d0*(EcLongSig + AngDiff(EcLong, R8tmp1))
     +          * dcos(d2r*EcLat) 
      EcLatSig  = 900.0d0*(EcLatSig  + AngDiff(EcLat,  R8tmp2))
c      
      sig1 = sigra2/(3600.0d0*dcos(d2r*EcLat2))
      sig2 = sigdec2/3600.0d0
      call Cel2Ec(ra2+sig1,  dec2+sig2,  R8tmp1,  R8tmp2)
      EcLongSig2 = AngDiff(EcLong2, R8tmp1)
      EcLatSig2  = AngDiff(EcLat2,  R8tmp2) 
      call Cel2Ec(ra2+sig1,  dec2-sig2,  R8tmp1,  R8tmp2)
      EcLongSig2 = EcLongSig2 + AngDiff(EcLong2, R8tmp1)
      EcLatSig2  = EcLatSig2  + AngDiff(EcLat2,  R8tmp2) 
      call Cel2Ec(ra2-sig1,  dec2+sig2,  R8tmp1,  R8tmp2)
      EcLongSig2 = EcLongSig2 + AngDiff(EcLong2, R8tmp1)
      EcLatSig2  = EcLatSig2  + AngDiff(EcLat2,  R8tmp2) 
      call Cel2Ec(ra2-sig1,  dec2-sig2,  R8tmp1,  R8tmp2)
      EcLongSig2 = 900.0d0*(EcLongSig2 + AngDiff(EcLong2, R8tmp1))
     +           * dcos(d2r*EcLat2) 
      EcLatSig2  = 900.0d0*(EcLatSig2  + AngDiff(EcLat2,  R8tmp2))
c                                      ! Get ecliptic position offsets      
      dEcLong = AngDiff(EcLong, EcLong2)
      if (EcLong .gt. EcLong2) dEcLong = -dEcLong
      dEcLong = 3600.0d0*dEcLong*dcos(EcLat)
      dEcLat = AngDiff(EcLat, EcLat2)
      if (EcLat .gt. EcLat2) dEcLat = -dEcLat
      dEcLat = 3600.0d0*dEcLat
      dEcLongSig = dsqrt(EcLongSig**2 + EcLongSig2**2)
      dEcLatSig  = dsqrt(EcLatSig**2  + EcLatSig2**2)
      dEcLongSNR = dEcLong/dEcLongSig
      dEcLatSNR  = dEcLat/dEcLatSig
c                                      ! Get Average Ecliptic positions     
      EcLong = (EcLong*EcLongSig2**2 + EcLong2*EcLongSig**2)
     +       / (EcLongSig**2 + EcLongSig2**2) 
      EcLat  = (EcLat*EcLatSig2**2 + EcLat2*EcLatSig**2)
     +       / (EcLatSig**2 + EcLatSig2**2) 
      EcLongSig = sqrt(EcLongSig**2 * EcLongSig2**2
     +          /     (EcLongSig**2 + EcLongSig2**2))
      EcLatSig  = sqrt(EcLatSig**2 * EcLatSig2**2
     +          /     (EcLatSig**2 + EcLatSig2**2))
     
c                                      ! Get averaged RA & Dec
      oma11 = sigra**2                 ! A & D error covariance matrices
      oma12 = sigradec*dabs(sigradec)
      oma22 = sigdec**2
      deta  = oma11*oma22 - oma12**2
      omd11 = sigra2**2
      omd12 = sigraded*dabs(sigraded)
      omd22 = sigdec2**2
      detd  = omd11*omd22 - omd12**2
c                                      ! A & D weight (inverse) matrices
      wa11 =  oma22/deta
      wa12 = -oma12/deta
      wa22 =  oma11/deta
      wd11 =  omd22/detd
      wd12 = -omd12/detd
      wd22 =  omd11/detd
c                                      ! Sum of the weight matrices    
      wad11 = wa11 + wd11
      wad12 = wa12 + wd12
      wad22 = wa22 + wd22
      detad = wad11*wad22 - wad12**2
c                                      ! Inverse of the weight sum matrix
      v11 =  wad22/detad
      v12 = -wad12/detad
      v22 =  wad11/detad
c      
      R8tmp1 = wd11*ra  + wd12*dec + wa11*ra2 + wa12*dec2
      R8tmp2 = wd12*ra  + wd22*dec + wa12*ra2 + wa22*dec2
c      
      ra  = v11*R8tmp1 + v12*R8tmp2
      dec = v12*R8tmp1 + v22*R8tmp2
      if (ra .lt. 0.0d0) ra = ra + 360.0d0
c     
      sigra    = dsqrt(v11)
      sigdec   = dsqrt(v22)
      sigradec = dsqrt(dabs(v12))
      if (v12 .lt. 0.0d0) sigradec = -sigradec
c
      write (Line(IFA(3):IFB(7)), '(2F12.7,3F9.4)')
     +       ra, dec, sigra, sigdec, sigradec     
c      
c  EcLong  | EcLongSig|  EcLat   | EcLatSig|  dEcLong |dEcLongSig|  dEcLat  |dEcLatSig|dEcLongSNR| dEcLatSNR| chi2pmra|chi2pmdec|ka|k1|k2|km|m|
c1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
c         1         2         3          4         5        6         7         8         9        10       11         12        13        14
      write (EclData,'(F10.6,F11.3,F11.6,F10.3,3F11.3,F10.3,2F11.3,2F10.3,4I3)') 
     +       EcLong,  EcLongSig,  EcLat,  EcLatSig,
     +      dEcLong, dEcLongSig, dEcLat, dEcLatSig,
     +      dEcLongSNR, dEcLatSNR, chi2pmra, chi2pmdec,
     +      KodeAst, KodePhot1, KodePhot2, KodePM   
c     
      Good1 = index(Line(IFA(164):IFB(165)),'null') .eq. 0
      Good2 = index(Line(IFA(330):IFB(331)),'null') .eq. 0
      if (Good1 .and. Good2) then
        read (Line(IFA(164):IFB(164)), *, err = 1050) Itmp1 ! nIters_pm
        read (Line(IFA(330):IFB(330)), *, err = 1050) Itmp2 ! nIters_pm
        if (Itmp2 .gt. Itmp1)
     +      Line(IFA(164):IFB(164)) = Line(IFA(330):IFB(330))
        read (Line(IFA(165):IFB(165)), *, err = 1050) Itmp1 ! nSteps_pm
        read (Line(IFA(331):IFB(331)), *, err = 1050) Itmp2 ! nSteps_pn
        if (Itmp2 .gt. Itmp1)
     +      Line(IFA(165):IFB(165)) = Line(IFA(331):IFB(331))
      else if (Good2) then
        Line(IFA(164):IFB(165)) = Line(IFA(330):IFB(331))
      end if
      go to 1200
1050  print *,
     +'ERROR reading nIters/nItert/nSteps/nStept on source',
     +       Line(IFA(1):IFB(1))
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Process photometric parameters
c                                      ! w1sky - w2conf
1200  Good1 = index(Line(IFA(12):IFB(17)),  'null') .eq. 0 ! Note: these tend to be all
      Good2 = index(Line(IFA(178):IFB(183)),'null') .eq. 0 !       good or all null
c
      if (Good1 .and. Good2) go to 1230
      if (Good2) Line(IFA(12):IFB(17)) = Line(IFA(178):IFB(183))
      go to 1240
c
1230  k = 178
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1sky2
      k = 12
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1sky
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
      k = 179
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1sigsl
      k = 13
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1sigsk
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
      k = 180
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1conf2
      k = 14
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1conf
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
      k = 181
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2sky2
      k = 15
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2sky
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
      k = 182
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2sigsl
      k = 16
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2sigsk
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
      k = 183
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2conf2
      k = 17
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2conf
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
c
1240  Good1 = index(Line(IFA(20):IFB(20)),  'null') .eq. 0
      Good2 = index(Line(IFA(186):IFB(186)),'null') .eq. 0
      if (Good1 .and. Good2) then
        k = 20
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1snr
        k = 186
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1snr2
        R8tmp1 = dsqrt(R8tmp1**2 + R8tmp2**2)
        write (Line(IFA(20):IFB(20)), '(F7.1)') R8tmp1
      else if (Good2) then
        Line(IFA(20):IFB(20)) = Line(IFA(186):IFB(186))
      end if
c
      Good1 = index(Line(IFA(21):IFB(21)),  'null') .eq. 0
      Good2 = index(Line(IFA(187):IFB(187)),'null') .eq. 0
      if (Good1 .and. Good2) then
        k = 21
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2snr
        k = 187
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2snr2
        R8tmp1 = dsqrt(R8tmp1**2 + R8tmp2**2)
        write (Line(IFA(21):IFB(21)), '(F7.1)') R8tmp1
      else if (Good2) then
        Line(IFA(21):IFB(21)) = Line(IFA(187):IFB(187))
      end if
c
      Good1 = index(Line(IFA(22):IFB(23)),  'null') .eq. 0
      Good2 = index(Line(IFA(188):IFB(189)),'null') .eq. 0
      if (Good1 .and. Good2) then
        k = 23
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1sigflux
        k = 189
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1sigflux2
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 22
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1flux
        k = 188
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1flux2
        R8tmp1 = (v2*R8tmp1 + v1*R8tmp2)/(v1+v2)
        write (Line(IFA(22):IFB(22)), '(1pe12.4)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        write (Line(IFA(23):IFB(23)), '(1pe14.4)') R8tmp1
      else if (Good2) then
        Line(IFA(22):IFB(23)) = Line(IFA(188):IFB(189))
      end if
c
      Good1 = index(Line(IFA(24):IFB(25)),  'null') .eq. 0
      Good2 = index(Line(IFA(190):IFB(191)),'null') .eq. 0
      if (Good1 .and. Good2) then
        k = 25
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2sigflux
        k = 191
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2sigflux2
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 24
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2flux
        k = 190
        Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2flux2
        R8tmp1 = (v2*R8tmp1 + v1*R8tmp2)/(v1+v2)
        write (Line(IFA(24):IFB(24)), '(1pe12.4)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        write (Line(IFA(25):IFB(25)), '(1pe14.4)') R8tmp1
      else if (Good2) then
        Line(IFA(24):IFB(25)) = Line(IFA(190):IFB(191))
      end if
c
c                                      ! WPRO Photometry
c
      Good1 = index(Line(IFA(26):IFB(28)),  'null') .eq. 0
      Good1 = Good1 .and. index(Line(IFA(98):IFB(99)),  'null') .eq. 0
      Good1 = Good1 .and. index(Line(IFA(99):IFB(99)),  '0') .eq. 0
      Good2 = index(Line(IFA(192):IFB(194)),'null') .eq. 0
      Good2 = Good2 .and. index(Line(IFA(264):IFB(265)),  'null') .eq. 0
      Good2 = Good2 .and. index(Line(IFA(265):IFB(265)),  '0') .eq. 0
      if (Good1 .and. Good2) go to 1250
      nBadW1Phot = nBadW1Phot + 1
      If (Good2) then
        nBadW1Phot1 = nBadW1Phot1 + 1
        KodePhot1 = 2
        if (dbg) print *,'Bad  ascending W1 Photrometry on source'
     +                //Line(IFA(1):IFB(1))
      else If (Good1) then
        nBadW1Phot2 = nBadW1Phot2 + 1
        KodePhot1 = 1
        if (dbg) print *,'Bad descending W1 Photrometry on source'
     +                //Line(IFA(1):IFB(1))
      else
        nBadW1Phot1 = nBadW1Phot1 + 1
        nBadW1Phot2 = nBadW1Phot2 + 1
        KodePhot1 = 0     
        if (dbg) print *,
     +     'Bad ascending AND descending W1 Photrometry on source '
     +                //Line(IFA(1):IFB(1))
      end if
      if (Good2) then
        Line(IFA(26):IFB(28)) = Line(IFA(192):IFB(194))
        Line(IFA(98):IFB(99)) = Line(IFA(264):IFB(265))
      end if
      go to 1260
c
1250  KodePhot1 = 3
      k = 27
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1sigmpr0
      k = 193
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1sigmprp
      v1 = R8tmp1**2
      v2 = R8tmp2**2
      k = 192
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1mprp
      k = 26
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1mpro
      R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
      write (Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
      R8tmp1 = dsqrt(v1*v2/(v1+v2))
      k = 27
      write (Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
c      
      k = 28
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1rchi2
      k = 194
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1rchi22
      k = 99
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1    ! w1M
      k = 265
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2    ! w1M2
      R8tmp1 = (dfloat(Itmp1)*R8tmp1 + dfloat(Itmp2)*R8tmp2)
     +       /  dfloat(Itmp1+Itmp2)
      Itmp1 = Itmp1 + Itmp2
      k = 28
      write (Line(IFA(k):IFB(k)),'(1pe11.3)') R8tmp1
      k = 99
      write (Line(IFA(k):IFB(k)),'(I6)') Itmp1
      k = 264
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2    ! w1NM2
      k = 98
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1    ! w1NM
      Itmp1 = Itmp1 + Itmp2
      write (Line(IFA(k):IFB(k)),'(I7)') Itmp1

1260  Good1 = index(Line(IFA(29):IFB(31)),  'null') .eq. 0
      Good1 = Good1 .and. index(Line(IFA(109):IFB(110)),  'null') .eq. 0
      Good1 = Good1 .and. index(Line(IFA(110):IFB(110)),  '0') .eq. 0
      Good2 = index(Line(IFA(195):IFB(197)),'null') .eq. 0
      Good2 = Good2 .and. index(Line(IFA(275):IFB(276)),  'null') .eq. 0
      Good2 = Good2 .and. index(Line(IFA(276):IFB(276)),  '0') .eq. 0
      if (Good1 .and. Good2) go to 1270
      nBadW2Phot = nBadW2Phot + 1
      If (Good2) then
        nBadW2Phot1 = nBadW2Phot1 + 1
        KodePhot2 = 2
        if (dbg) print *,'Bad  ascending W2 Photrometry on source'
     +                //Line(IFA(1):IFB(1))
      else If (Good1) then
        nBadW2Phot2 = nBadW2Phot2 + 1
        KodePhot2 = 1
        if (dbg) print *,'Bad descending W2 Photrometry on source'
     +                //Line(IFA(1):IFB(1))
      else
        nBadW2Phot1 = nBadW2Phot1 + 1
        nBadW2Phot2 = nBadW2Phot2 + 1
        KodePhot2 = 0     
        if (dbg) print *,
     +     'Bad ascending AND descending W2 Photrometry on source '
     +                //Line(IFA(1):IFB(1))
      end if
      if (Good2) then
        Line(IFA(29):IFB(31))   = Line(IFA(195):IFB(197))
        Line(IFA(109):IFB(110)) = Line(IFA(275):IFB(276))
      end if
      go to 1280
c
1270  KodePhot2 = 3
      k = 30
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2sigmpr0
      k = 196
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2sigmprp
      v1 = R8tmp1**2
      v2 = R8tmp2**2
      k = 195
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2mprp
      k = 29
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2mpro
      R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
      write (Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
      R8tmp1 = dsqrt(v1*v2/(v1+v2))
      k = 30
      write (Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
c      
      k = 31
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w2rchi2
      k = 197
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w2rchi22
      k = 110
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1    ! w2M
      k = 276
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2    ! w2M2
      R8tmp1 = (dfloat(Itmp1)*R8tmp1 + dfloat(Itmp2)*R8tmp2)
     +       /  dfloat(Itmp1+Itmp2)
      Itmp1 = Itmp1 + Itmp2
      k = 31
      write (Line(IFA(k):IFB(k)),'(1pe11.3)') R8tmp1
      k = 110
      write (Line(IFA(k):IFB(k)),'(I6)') Itmp1
      k = 275
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2    ! w2NM2
      k = 109
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1    ! w2NM
      Itmp1 = Itmp1 + Itmp2
      write (Line(IFA(k):IFB(k)),'(I7)') Itmp1
c                                      ! update EclData for KodePhot
1280  write (EclData(132:133),'(I2)') KodePhot1
      write (EclData(135:136),'(I2)') KodePhot2
 
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                            ! Process remaining photometric parameters
      k = 198
      read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2 ! rchi22
      k = 32
      read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1 ! rchi2
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0         ! assume equal Ndf
      write(Line(IFA(k):IFB(k)),'(1pE11.3)') R8tmp1
c     
      k = 199
      read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp2 ! nb2
      k = 33
      read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp1 ! nb
      if (Itmp2 .gt. Itmp1) Itmp1 = Itmp2
      write(Line(IFA(k):IFB(k)),'(I4)') Itmp1
c
      k = 200     
      read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp2 ! na2
      k = 34
      read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp1 ! na
      if (Itmp2 .gt. Itmp1) Itmp1 = Itmp2
      write(Line(IFA(k):IFB(k)),'(I4)') Itmp1
c
      Good1 = index(Line(IFA(40):IFB(44)),'null')   .eq. 0 ! w1mag-w1mcor
      Good2 = index(Line(IFA(206):IFB(210)),'null') .eq. 0 ! w1mag2-w1mcos
      if (Good1 .and. Good2) then
        k = 41
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1sigm
        k = 207
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1sign
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 206
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mag2
        k = 40
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mag
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 41
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
        k = 208
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2  !  w1flh
        k = 42
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1  !  w1flg
        Itmp1 = IOR(Itmp1,Itmp2)
        write(Line(IFA(k):IFB(k)),'(I6)') Itmp1
        k = 209
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1Cov2
        k = 43
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1Cov
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        k = 210
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mcos
        k = 44
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mcor
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
      else if (Good2) then
        Line(IFA(40):IFB(44)) = Line(IFA(206):IFB(210))
      end if
c
      Good1 = index(Line(IFA(45):IFB(49)),'null')   .eq. 0 ! w2mag-w2mcor
      Good2 = index(Line(IFA(211):IFB(215)),'null') .eq. 0 ! w2mag2-w2mcos
      if (Good1 .and. Good2) then
        k = 46
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2sigm
        k = 212
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2sign
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 211
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mag2
        k = 45
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mag
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 46
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
        k = 213
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2  !  w2flh
        k = 47
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1  !  w2flg
        Itmp1 = IOR(Itmp1,Itmp2)
        write(Line(IFA(k):IFB(k)),'(I6)') Itmp1
        k = 214
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2Cov2
        k = 48
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2Cov
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        k = 215
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mcos
        k = 49
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mcor
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
      else if (Good2) then
        Line(IFA(45):IFB(49)) = Line(IFA(211):IFB(215))
      end if
c      
      do 1400 n = 1, 16                ! 8 mag sets for 2 bands each
        k1 = 47  + 3*n
        k2 = 213 + 3*n
        Good1 = index(Line(IFA(k1):IFB(k1+2)),'null') .eq. 0 ! w?mag_?-w?flg_?
        Good2 = index(Line(IFA(k2):IFB(k2+2)),'null') .eq. 0 ! w?mag_?2-w?flg_??
        if (Good1 .and. Good2) then
          k = k1 + 1
          read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w?sigm_?
          k = k2 + 1
          read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w?sigm_?2
          v1 = R8tmp1**2
          v2 = R8tmp2**2
          k = k2
          read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w?mag_?2
          k = k1
          read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w?mag_?
          R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
          write(Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
          R8tmp1 = dsqrt(v1*v2/(v1+v2))
          k = k1 + 1
          write(Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
          k = k1 + 2
          read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1  !  w?flg_?
          k = k2 + 2
          read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2  !  w?flg_?2
          Itmp1 = IOR(Itmp1,Itmp2)
          write(Line(IFA(k):IFB(k)),'(I8)') Itmp1
        else if (Good2) then
          Line(IFA(k1):IFB(k1+2)) = Line(IFA(k2):IFB(k2+2))
        end if
1400  continue
c
      Good1 = index(Line(IFA(100):IFB(102)),'null') .eq. 0 ! w1magP-w1sigP2
      Good2 = index(Line(IFA(266):IFB(268)),'null') .eq. 0 ! w1magP2-w1sigP4
      if (Good1 .and. Good2) then
        k = 102
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1sigP2
        k = 268
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1sigP4
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 266
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1magP2
        k = 100
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1magP
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 102
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        k = 267
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1sigP3
        k = 101
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1sigP1
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0        
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
      else if (Good2) then
        Line(IFA(100):IFB(102)) = Line(IFA(266):IFB(268))
      end if
c
      Good1 = index(Line(IFA(111):IFB(113)),'null') .eq. 0 ! w2magP-w2sigP2
      Good2 = index(Line(IFA(277):IFB(279)),'null') .eq. 0 ! w2magP2-w2sigP4
      if (Good1 .and. Good2) then
        k = 113
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2sigP2
        k = 279
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2sigP4
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 277
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2magP2
        k = 111
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2magP
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 113
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        k = 278
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2sigP3
        k = 112
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2sigP1
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0        
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
      else if (Good2) then
        Line(IFA(111):IFB(113)) = Line(IFA(277):IFB(279))
      end if
c
      Good1 = index(Line(IFA(103):IFB(103)),'null') .eq. 0 ! w1k
      Good2 = index(Line(IFA(269):IFB(269)),'null') .eq. 0 ! w1k2
      if (Good1 .and. Good2) then
        k = 269
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1k2
        k = 103
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1k
        if (R8tmp2 .gt. R8tmp1)
     +      Line(IFA(103):IFB(103)) = Line(IFA(269):IFB(269))
      else if (Good2) then
        Line(IFA(103):IFB(103)) = Line(IFA(269):IFB(269))
      end if
c
      Good1 = index(Line(IFA(114):IFB(114)),'null') .eq. 0 ! w2k
      Good2 = index(Line(IFA(280):IFB(280)),'null') .eq. 0 ! w2k2
      if (Good1 .and. Good2) then
        k = 280
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2k2
        k = 114
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2k
        if (R8tmp2 .gt. R8tmp1)
     +      Line(IFA(103):IFB(103)) = Line(IFA(269):IFB(269))
      else if (Good2) then
        Line(IFA(103):IFB(103)) = Line(IFA(269):IFB(269))
      end if
c
      Good1 = index(Line(IFA(104):IFB(104)),'null') .eq. 0 ! w1Ndf
      Good1 = Good1 .and. (Line(IFA(104):IFB(104)) .ne. '     0')
      Good2 = index(Line(IFA(270):IFB(270)),'null') .eq. 0 ! w1Ndg
      Good2 = Good2 .and. (Line(IFA(270):IFB(270)) .ne. '     0')
      if (Good1 .and. Good2) then
        k = 270
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2   !  w1Ndg
        k = 104
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1   !  w1Ndf
        Itmp1 = Itmp1+Itmp2
        write(Line(IFA(k):IFB(k)),'(I6)') Itmp1
      else if (Good2) then
        Line(IFA(104):IFB(104)) = Line(IFA(270):IFB(270))
      end if
c
      Good1 = index(Line(IFA(115):IFB(115)),'null') .eq. 0 ! w2Ndf
      Good1 = Good1 .and. (Line(IFA(115):IFB(115)) .ne. '     0')
      Good2 = index(Line(IFA(281):IFB(281)),'null') .eq. 0 ! w2Ndg
      Good2 = Good2 .and. (Line(IFA(281):IFB(281)) .ne. '     0')
      if (Good1 .and. Good2) then
        k = 281
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2   !  w2Ndg
        k = 115
        read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1   !  w2Ndf
        Itmp1 = Itmp1+Itmp2
        write(Line(IFA(k):IFB(k)),'(I6)') Itmp1
      else if (Good2) then
        Line(IFA(115):IFB(115)) = Line(IFA(281):IFB(281))
      end if
c
      Good1 = index(Line(IFA(105):IFB(105)),'null') .eq. 0 ! w1mLQ
      Good2 = index(Line(IFA(271):IFB(271)),'null') .eq. 0 ! w1mLa
      if (Good1 .and. Good2) then
        k = 271
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mLa
        k = 105
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mLQ
        if (R8tmp2 .gt. R8tmp1) 
     +      Line(IFA(105):IFB(105)) = Line(IFA(271):IFB(271))
      else if (Good2) then
        Line(IFA(105):IFB(105)) = Line(IFA(271):IFB(271))
      end if
c
      Good1 = index(Line(IFA(116):IFB(116)),'null') .eq. 0 ! w2mLQ
      Good2 = index(Line(IFA(282):IFB(282)),'null') .eq. 0 ! w2mLa
      if (Good1 .and. Good2) then
        k = 282
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mLa
        k = 116
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mLQ
        if (R8tmp2 .gt. R8tmp1) 
     +      Line(IFA(116):IFB(116)) = Line(IFA(282):IFB(282))
      else if (Good2) then
        Line(IFA(116):IFB(116)) = Line(IFA(282):IFB(282))
      end if
c
      Good1 = index(Line(IFA(120):IFB(121)),'null') .eq. 0 ! rho12-q12
      Good2 = index(Line(IFA(286):IFB(287)),'null') .eq. 0 ! rho13-q122
      if (Good1 .and. Good2) then
        k = 286
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  rho13
        k = 120
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  rho12
        Itmp1 = NInt((R8tmp1+R8tmp2)/2.0d0)
        write(Line(IFA(k):IFB(k)),'(I6)') Itmp1
        k = 287
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  q122
        k = 121
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  q12
        Itmp1 = NInt((R8tmp1+R8tmp2)/2.0d0)
        write(Line(IFA(k):IFB(k)),'(I6)') Itmp1
      else if (Good2) then
        Line(IFA(120):IFB(121)) = Line(IFA(286):IFB(287))
      end if
c
      Good1 = index(Line(IFA(106):IFB(108)),'null') .eq. 0 ! w1mJDmin-w1mJDmean
      Good2 = index(Line(IFA(272):IFB(274)),'null') .eq. 0 ! w1mJDmin2-w1mJDmean2
      if (Good1 .and. Good2) then
        k = 272
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mJDmin2
        k = 106
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mJDmin
        if (R8tmp2 .lt. R8tmp1) 
     +      Line(IFA(106):IFB(106)) = Line(IFA(272):IFB(272))
        k = 273
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mJDmax2
        k = 107
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mJDmax
        if (R8tmp2 .gt. R8tmp1) 
     +      Line(IFA(107):IFB(107)) = Line(IFA(273):IFB(273))
        k = 274
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mJDmean2
        k = 108
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mJDmean
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0        
        write(Line(IFA(k):IFB(k)),'(F18.8)') R8tmp1
      else if (Good2) then
        Line(IFA(106):IFB(108)) = Line(IFA(272):IFB(274))
      end if
c
      Good1 = index(Line(IFA(117):IFB(119)),'null') .eq. 0 ! w2mJDmin-w2mJDmean
      Good2 = index(Line(IFA(283):IFB(285)),'null') .eq. 0 ! w2mJDmin2-w2mJDmean2
      if (Good1 .and. Good2) then
        k = 283
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mJDmin2
        k = 117
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mJDmin
        if (R8tmp2 .lt. R8tmp1) 
     +      Line(IFA(117):IFB(117)) = Line(IFA(283):IFB(283))
        k = 284
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mJDmax2
        k = 118
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mJDmax
        if (R8tmp2 .gt. R8tmp1) 
     +      Line(IFA(118):IFB(118)) = Line(IFA(284):IFB(284))
        k = 285
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mJDmean2
        k = 119
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mJDmean
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0        
        write(Line(IFA(k):IFB(k)),'(F18.8)') R8tmp1
      else if (Good2) then
        Line(IFA(117):IFB(119)) = Line(IFA(283):IFB(285))
      end if
c      
      Good1 = index(Line(IFA(137):IFB(137)),'null') .eq. 0 ! mdetID
      Good2 = index(Line(IFA(303):IFB(303)),'null') .eq. 0 ! mdetIa
      if (Good1 .and. Good2) then
        if (Line(IFA(137):IFB(137)) .eq. Line(IFA(303):IFB(303))) then
          EclData(141:141) = 'y'
        else
          EclData(141:141) = 'n'
          NmdetIDerr = NmdetIDerr + 1
          if (dbg) print *,'WARNING: mdetID mismatch on source',
     +                      Line(IFA(1):IFB(1))
        end if
      else
        EclData(141:141) = 'x'
        if (dbg) print *,'WARNING: missing mdetID on source',
     +                    Line(IFA(1):IFB(1))
      end if
c      
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Process proper motion parameters
c                                      ! MeanObsMJD - sigPMDec
1500  Good1 = index(Line(IFA(140):IFB(149)),'null') .eq. 0
      Good2 = index(Line(IFA(306):IFB(315)),'null') .eq. 0
      if (Good1 .and. Good2) go to 1510
      nBadPM = nBadPM + 1
      If (Good2) then
        nBadPM1 = nBadPM1 + 1
        KodePM = 2
        if (dbg) print *,'Bad  ascending PM on source'
     +                //Line(IFA(1):IFB(1))
      else If (Good1) then
        nBadPM2 = nBadPM2 + 1
        KodePM = 1
        if (dbg) print *,'Bad descending PM on source'
     +                //Line(IFA(1):IFB(1))
      else
        nBadPM1 = nBadPM1 + 1
        nBadPM2 = nBadPM2 + 1
        KodePM = 0     
        if (dbg) print *,
     +     'Bad ascending AND descending PM on source '
     +                //Line(IFA(1):IFB(1))
      end if
      if (Good2) then
        Line(IFA(140):IFB(149))   = Line(IFA(306):IFB(315))
      end if
      EclData(108:128) = '    null      null   '
      go to 1550
c
1510  KodePM = 3      
      k = 306
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! MeanObsMJD2
      k = 140
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! MeanObsMJD
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      write (Line(IFA(k):IFB(k)),'(F13.6)') R8tmp1
c                                      ! NOTE: using stationary-solution position
c                                      !       variables for motion-solution 
c                                      !       positions; code cloned from above
      k = 141
      Read(Line(IFA(k):IFB(k)), *, err = 3006) ra
      k = 142
      Read(Line(IFA(k):IFB(k)), *, err = 3006) dec
      k = 143
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigra
      k = 144
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigdec
      k = 145
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigradec
      k = 307
      Read(Line(IFA(k):IFB(k)), *, err = 3006) ra2
      k = 308
      Read(Line(IFA(k):IFB(k)), *, err = 3006) dec2
      k = 309
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigra2
      k = 310
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigdec2
      k = 311
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigraded
c      
      oma11 = sigra**2                 ! A & D error covariance matrices
      oma12 = sigradec*dabs(sigradec)
      oma22 = sigdec**2
      deta  = oma11*oma22 - oma12**2
      omd11 = sigra2**2
      omd12 = sigraded*dabs(sigraded)
      omd22 = sigdec2**2
      detd  = omd11*omd22 - omd12**2
c                                      ! A & D weight (inverse) matrices
      wa11 =  oma22/deta
      wa12 = -oma12/deta
      wa22 =  oma11/deta
      wd11 =  omd22/detd
      wd12 = -omd12/detd
      wd22 =  omd11/detd
c                                      ! Sum of the weight matrices    
      wad11 = wa11 + wd11
      wad12 = wa12 + wd12
      wad22 = wa22 + wd22
      detad = wad11*wad22 - wad12**2
c                                      ! Inverse of the weight sum matrix
      v11 =  wad22/detad
      v12 = -wad12/detad
      v22 =  wad11/detad
c      
      R8tmp1 = wd11*ra  + wd12*dec + wa11*ra2 + wa12*dec2
      R8tmp2 = wd12*ra  + wd22*dec + wa12*ra2 + wa22*dec2
c      
      ra  = v11*R8tmp1 + v12*R8tmp2
      dec = v12*R8tmp1 + v22*R8tmp2
      if (ra .lt. 0.0d0) ra = ra + 360.0d0
c     
      sigra    = dsqrt(v11)
      sigdec   = dsqrt(v22)
      sigradec = dsqrt(dabs(v12))
      if (v12 .lt. 0.0d0) sigradec = -sigradec
c
      write (Line(IFA(141):IFB(145)), '(2F12.7,F9.4,F10.4,F12.4)')
     +       ra, dec, sigra, sigdec, sigradec
c                                      ! NOTE: using stationary-solution position
c                                      !       variables for motion-solution 
c                                      !       motions; code cloned from above
      k = 146
      Read(Line(IFA(k):IFB(k)), *, err = 3006) ra
      k = 147
      Read(Line(IFA(k):IFB(k)), *, err = 3006) dec
      k = 148
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigra
      k = 149
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigdec
      k = 312
      Read(Line(IFA(k):IFB(k)), *, err = 3006) ra2
      k = 313
      Read(Line(IFA(k):IFB(k)), *, err = 3006) dec2
      k = 314
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigra2
      k = 315
      Read(Line(IFA(k):IFB(k)), *, err = 3006) sigdec2
c
      v1 = sigra**2
      v2 = sigra2**2
      R8tmp1 = (ra - ra2)**2/(v1 + v2)  ! chi2pmra
      write(EclData(108:117),'(1pE10.3)') R8tmp1
      R8tmp1 = (v2*ra+v1*ra2)/(v1+v2)
      R8tmp2 = dsqrt(v1*v2/(v1+v2))
      k = 146
      if ((abs(R8tmp1) .ge. 0.01) .and. (abs(R8tmp1) .le. 999.9999))
     + then
        write(Line(IFA(k):IFB(k)),'(F10.4)')   R8tmp1
      else
        write(Line(IFA(k):IFB(k)),'(1pE10.2)') R8tmp1
      end if
      k = 148
      write(Line(IFA(k):IFB(k)),'(F9.4)')   R8tmp2
c
      v1 = sigdec**2
      v2 = sigdec2**2
      R8tmp1 = (dec - dec2)**2/(v1 + v2)  ! chi2pmdec
      write(EclData(118:127),'(1pE10.3)') R8tmp1
      R8tmp1 = (v2*dec+v1*dec2)/(v1+v2)
      R8tmp2 = dsqrt(v1*v2/(v1+v2))
      k = 147
      if ((abs(R8tmp1) .ge. 0.01) .and. (abs(R8tmp1) .le. 999.9999))
     + then
        write(Line(IFA(k):IFB(k)),'(F10.4)')   R8tmp1
      else
        write(Line(IFA(k):IFB(k)),'(1pE10.2)') R8tmp1
      end if
      k = 149
      write(Line(IFA(k):IFB(k)),'(F9.4)')   R8tmp2
c
      Good1 = index(Line(IFA(163):IFB(163)),'null') .eq. 0  ! pmcode
      Good2 = index(Line(IFA(329):IFB(329)),'null') .eq. 0  ! pmcodf
c  1N000
c1234567      
      if (Good1 .and. Good2) then
        ChTmp1 = Line(IFA(163):IFB(163))
        ChTmp2 = Line(IFA(329):IFB(329))
        read (ChTmp1(3:3), *, err = 1520) Itmp1
        read (ChTmp2(3:3), *, err = 1520) Itmp2
        if (Itmp2 .gt. Itmp1) ChTmp1(3:3) = ChTmp2(3:3)
        if (ChTmp2(4:4) .eq. 'Y') ChTmp1(4:4) = ChTmp2(4:4)
        if (ChTmp2(5:7) .gt. ChTmp1(5:7)) ChTmp1(5:7) = ChTmp2(5:7)
        Line(IFA(163):IFB(163)) = ChTmp1
      else if (Good2) then
        Line(IFA(163):IFB(163)) = Line(IFA(329):IFB(329))
      end if
      go to 1530
1520  print *,'ERROR reading pmcode or pmcodf on source',
     +         Line(IFA(1):IFB(1))
c     
1530  Good1 = index(Line(IFA(164):IFB(165)),'null') .eq. 0
      Good2 = index(Line(IFA(330):IFB(331)),'null') .eq. 0
      if (Good1 .and. Good2) then
        read (Line(IFA(164):IFB(164)), *, err = 1540) Itmp1 ! nIters_pm
        read (Line(IFA(330):IFB(330)), *, err = 1540) Itmp1 ! nIters_pn
        if (Itmp2 .gt. Itmp1)
     +      Line(IFA(164):IFB(164)) = Line(IFA(330):IFB(330))
        read (Line(IFA(165):IFB(165)), *, err = 1540) Itmp1 ! nSteps_pm
        read (Line(IFA(331):IFB(331)), *, err = 1540) Itmp1 ! nSteps_pn
        if (Itmp2 .gt. Itmp1)
     +      Line(IFA(165):IFB(165)) = Line(IFA(331):IFB(331))
      else if (Good2) then
        Line(IFA(164):IFB(165)) = Line(IFA(330):IFB(331))
      end if
      go to 1550
1540  print *,
     +'ERROR reading nIters_pm/nIters_pn/nSteps_pm/nSteps_pm on source',
     +       Line(IFA(1):IFB(1))
c     
1550  Good1 = index(Line(IFA(150):IFB(150)),'null') .eq. 0  ! w1snr_pm
      Good2 = index(Line(IFA(316):IFB(316)),'null') .eq. 0  ! w1snr_pn
      if (Good1 .and. Good2) then
        k = 316
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w1snr_pn
        k = 150
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w1snr_pm
        R8tmp1 = dsqrt(R8tmp1**2 + R8tmp2**2)
        write (Line(IFA(k):IFB(k)), '(F9.1)') R8tmp1
      else if (Good2) then
        Line(IFA(150):IFB(150)) = Line(IFA(316):IFB(316))
      end if
c      
      Good1 = index(Line(IFA(151):IFB(151)),'null') .eq. 0  ! w2snr_pm
      Good2 = index(Line(IFA(317):IFB(317)),'null') .eq. 0  ! w2snr_pn
      if (Good1 .and. Good2) then
        k = 317
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w2snr_pn
        k = 151
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w2snr_pm
        R8tmp1 = dsqrt(R8tmp1**2 + R8tmp2**2)
        write (Line(IFA(k):IFB(k)), '(F9.1)') R8tmp1
      else if (Good2) then
        Line(IFA(151):IFB(151)) = Line(IFA(317):IFB(317))
      end if
c      
      Good1 = index(Line(IFA(138):IFB(139)),'null') .eq. 0  ! p1-p2
      Good2 = index(Line(IFA(304):IFB(305)),'null') .eq. 0  ! p12-p22
      if (Good1 .and. Good2) then
        k = 304
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! p12
        k = 138
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! p1
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write (Line(IFA(k):IFB(k)), '(F10.5)') R8tmp1
        k = 305
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! p22
        k = 139
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! p2
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write (Line(IFA(k):IFB(k)), '(F10.5)') R8tmp1
      else if (Good2) then
        Line(IFA(138):IFB(139)) = Line(IFA(304):IFB(305))
      end if
c      
      Good1 = index(Line(IFA(152):IFB(153)),'null') .eq. 0  ! w1flux_pm-w1sigflux_pm
      Good2 = index(Line(IFA(318):IFB(319)),'null') .eq. 0  ! w1flux_pm2-w1sigflux_pm2
      if (Good1 .and. Good2) then
        k = 319
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w1sigflux_pm2
        k = 153
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w1sigflux_pm
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 318
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w1flux_pm2
        k = 152
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w1flux_pm
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write (Line(IFA(k):IFB(k)), '(1pE12.4)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 153
        write (Line(IFA(k):IFB(k)), '(1pE14.4)') R8tmp1
      else if (Good2) then
        Line(IFA(152):IFB(153)) = Line(IFA(318):IFB(319))
      end if
c      
      Good1 = index(Line(IFA(154):IFB(155)),'null') .eq. 0  ! w2flux_pm-w2sigflux_pm
      Good2 = index(Line(IFA(320):IFB(321)),'null') .eq. 0  ! w2flux_pm2-w2sigflux_pm2
      if (Good1 .and. Good2) then
        k = 321
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w2sigflux_pm2
        k = 155
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w2sigflux_pm
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 320
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w2flux_pm2
        k = 154
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w2flux_pm
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write (Line(IFA(k):IFB(k)), '(1pE12.4)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 155
        write (Line(IFA(k):IFB(k)), '(1pE14.4)') R8tmp1
      else if (Good2) then
        Line(IFA(152):IFB(153)) = Line(IFA(318):IFB(319))
      end if
c      
      Good1 = index(Line(IFA(156):IFB(158)),'null') .eq. 0  ! w1mpro_pm-w1rchi2_pm
      Good2 = index(Line(IFA(322):IFB(324)),'null') .eq. 0  ! w1mpro_pn-w1rchi2_pn
      if (Good1 .and. Good2) then
        k = 323
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w1sigmpro_pn
        k = 157
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w1sigmpro_pm
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 322
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w1mpro_pn
        k = 156
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w1mpro_pm
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write (Line(IFA(k):IFB(k)), '(F10.3)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 157
        write (Line(IFA(k):IFB(k)), '(F13.3)') R8tmp1
        k = 324
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w1rchi2_pn
        k = 158
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w1rchi2_pm
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write (Line(IFA(k):IFB(k)), '(1pE11.3)') R8tmp1
      else if (Good2) then
        Line(IFA(156):IFB(158)) = Line(IFA(322):IFB(324))
      end if
c      
      Good1 = index(Line(IFA(159):IFB(161)),'null') .eq. 0  ! w2mpro_pm-w2rchi2_pm
      Good2 = index(Line(IFA(325):IFB(327)),'null') .eq. 0  ! w2mpro_pn-w2rchi2_pn
      if (Good1 .and. Good2) then
        k = 326
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w2sigmpro_pn
        k = 160
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w2sigmpro_pm
        v1 = R8tmp1**2
        v2 = R8tmp2**2
        k = 325
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w2mpro_pn
        k = 159
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w2mpro_pm
        R8tmp1 = (v2*R8tmp1+v1*R8tmp2)/(v1+v2)
        write (Line(IFA(k):IFB(k)), '(F10.3)') R8tmp1
        R8tmp1 = dsqrt(v1*v2/(v1+v2))
        k = 160
        write (Line(IFA(k):IFB(k)), '(F13.3)') R8tmp1
        k = 327
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! w2rchi2_pn
        k = 161
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! w2rchi2_pm
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write (Line(IFA(k):IFB(k)), '(1pE11.3)') R8tmp1
      else if (Good2) then
        Line(IFA(159):IFB(161)) = Line(IFA(325):IFB(327))
      end if
c      
      Good1 = index(Line(IFA(162):IFB(162)),'null') .eq. 0  ! rchi2_pm
      Good2 = index(Line(IFA(328):IFB(328)),'null') .eq. 0  ! rchi2_pm2
      if (Good1 .and. Good2) then
        k = 328
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! rchi2_pm2
        k = 162
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! rchi2_pm
        R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
        write (Line(IFA(k):IFB(k)), '(1pE11.3)') R8tmp1
      else if (Good2) then
        Line(IFA(162):IFB(162)) = Line(IFA(328):IFB(328))
      end if
c
      write(EclData(138:139),'(I2)') KodePM
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Output data row
c
      write (20,'(a)') Line(1:1556)//EclData
c     
      if (nRow .lt. nSrc) go to 1000
c
      print *,'No. data rows with bad astrometry passed through:   ',
     +         nBadAst
      if (nBadAst .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadAst1
        print *,'No. of bad descending rows:',nBadAst2
        if (nBadAst .ne. nBadAst1+nBadAst2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadAst1+nBadAst2-nBadAst
      end if
      print *,'No. data rows with bad W1 photometry passed through:   ',
     +         nBadW1Phot
      if (nBadW1Phot .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadW1Phot1
        print *,'No. of bad descending rows:',nBadW1Phot2
        if (nBadW1Phot .ne. nBadW1Phot1+nBadW1Phot2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadW1Phot1+nBadW1Phot2-nBadW1Phot
      end if
      print *,'No. data rows with bad W2 photometry passed through:   ',
     +         nBadW2Phot
      if (nBadW2Phot .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadW2Phot1
        print *,'No. of bad descending rows:',nBadW2Phot2
        if (nBadW2Phot .ne. nBadW2Phot1+nBadW2Phot2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadW2Phot1+nBadW2Phot2-nBadW2Phot
      end if
      print *,'No. data rows with bad proper motion passed through:',
     +         nBadPM
      if (nBadPM .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadPM1
        print *,'No. of bad descending rows:',nBadPM2
        if (nBadPM .ne. nBadPM1+nBadPM2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadPM1+nBadPM2-nBadPM
      end if
      print *,'No. data rows with mdetID mismatch:', NmdetIDerr
c
      call signoff('mrgad')
      stop
c
3000  print *,'ERROR: Eof-of-file encountered in gsa input file '
     +      //'before data found'
      call exit(64)
      stop
c
3001  print *,'ERROR: Eof-of-file encountered in ascending stf '
     +      //'file before data found'
      call exit(64)
      stop
c
3002  print *,'ERROR: Eof-of-file encountered in descending stf '
     +      //'file before data found'
      call exit(64)
      stop
c
3003  print *,'ERROR: read error encountered in ascending stf '
     +      //'file line:'
      print *,Line
      call exit(64)
      stop
c
3004  print *,'ERROR: read error encountered in descending stf '
     +      //'file line:'
      print *,Line
      call exit(64)
      stop
c
3005  print *,'ERROR: unexpected Eof-of-file encountered in gsa input file '
     +      //'while reading data'
      print *,'       Attempted to read data for source no.',nRow
      print *,'       Expected',nSrc,' sources'
      call exit(64)
      stop
c
3006  print *,'ERROR: read error encountered in gsa input file '
     +      //'on data line no.',nRow
      print *,'       Data field no.',k,', column name "',
     +                Field(k)(1:lnblnk(Field(k))),'"'
      print *,'        Numeric field: "',Line(IFA(k):IFB(k)),'"'
      call exit(64)
      stop
c      
      end
c
c-----------------------------------------------------------------------
c
      subroutine Cel2Ec(RA, Dec, Long, Lat)
c
      real*8 RA, Dec, Long, Lat, SOb, Cob, X, Y, Z, d2r,
     +       cRA, cDec, sRA, sDec, X2, Y2
c
c   Obliquity(2015) in J2000: 23.43734105     
c
      data d2r/1.745329252d-2/, cOb, sOb/0.9174956d0, 0.39777459d0/
c
c-----------------------------------------------------------------------
c
      cRA   = cos(d2r*RA)
      cDec  = cos(d2r*Dec)
      sRA   = sin(d2r*RA)
      sDec  = sin(d2r*Dec)
c
      X =  sDec
      Y = -cDec*sRA
      Z =  cDec*cRA
c
      X2 =  X*Cob + Y*Sob
      Y2 = -X*Sob + Y*Cob
c     Z2 =  Z
c
      Lat  = asin(X2)/d2r
      Long = atan2(-Y2,Z)/d2r
      if (Long .lt. 0.0) Long = Long + 360.0
c
      return
      end
c      
c=======================================================================
c
      Function AngDiff(Ang1, Ang2)
c
      Real*8 AngDiff, Ang1, Ang2, R8tmp
c
      R8tmp = Ang1 - Ang2
      if (R8tmp .gt.  180.0d0) AngDiff = AngDiff - 360.0d0
      if (R8tmp .lt. -180.0d0) AngDiff = AngDiff + 360.0d0
      AngDiff = dabs(R8tmp)
c      
      return
      end
c      
c=======================================================================
c
      subroutine SignOn(pgmnam)
c
c *** signon- routine which provides sign-on and sign-off messages
c             (orig by John Fowler- mod by Howard McCallon-041214-SIRTF)
c
c     inputs:  pgmnam = program name                                 [call arg]
c
c     outputs: message to stdout
c
      character*(*) pgmnam
      character vsn*11,cdate*8,ctime*8,Fmt*11,FLen*4
      integer*4 onoff,jdate(3),jtime(3),lnblnk
      real*4    dummyt,second(2),etime
c
      common /vdt/ cdate,ctime,vsn
c##
      onoff = 1
c
c         i. obtain date
c
100   cdate = '00-00-00'
      call idate(jdate)    ! Linux call
c
      jdate(3) = mod(jdate(3), 100)
      write(cdate(1:2), '(i2)') jdate(2)
      write(cdate(4:5), '(i2)') jdate(1)
      write(cdate(7:8), '(i2)') jdate(3)
c
      if(cdate(4:4) .eq. ' ') cdate(4:4) = '0'
      if(cdate(7:7) .eq. ' ') cdate(7:7) = '0'
c
c         ii. obtain time
c
      ctime = '00:00:00'
      call itime(jtime)
      write(ctime(1:2), '(i2)') jtime(1)
      write(ctime(4:5), '(i2)') jtime(2)
      write(ctime(7:8), '(i2)') jtime(3)
c
      if(ctime(4:4) .eq. ' ') ctime(4:4) = '0'
      if(ctime(7:7) .eq. ' ') ctime(7:7) = '0'
c
c         iii. set up format for pgmnam
c
      write(Flen,'(I4)') lnblnk(pgmnam)
      Fmt = '(A'//Flen//'$)'
c
c         iv. write out results
c
      write(*,Fmt) pgmnam
      if(onoff .eq. 1) then                      ! sign on
        write(*,301) vsn,cdate,ctime
      else                                       ! sign off
        dummyt = etime(second)
        write(*,302) vsn,cdate,ctime,second
      endif
  301 format(' version: ',a11,' - execution begun on ',a8,' at ',a8)
  302 format(' version: ',a11,' - execution ended on ',a8,' at ',a8
     *    /1x,f9.2,' cpu seconds used;',f8.2,' system seconds used.')
c
      return
c
      entry SignOff(pgmnam)
      OnOff = 2
      go to 100
c
      end
c      
c=======================================================================
c
      subroutine GetFlds(ColNam,Field,IFa,IFb,NF)
c-----------------------------------------------------------------------
c
c  Get fields in a table-file header line
c
c-----------------------------------------------------------------------
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      character*5000 ColNam
      Character*300  Line
      character*25   Field(MaxFld)
      integer*4      IFa(MaxFld), IFb(MaxFld), NF, N, M, L, K, LNBlnk,
     +               LastErr
c
c-----------------------------------------------------------------------
c
      N = 0
      K = 0
      LastErr = 0
      do 100 M = 1, LNBlnk(ColNam)
        if (ColNam(M:M) .eq. '|') then
          N = N + 1
          NF = N - 1
          if (N .gt. 1) IFb(N-1) = M-1
          if (N .gt. MaxFld) return
          IFa(N) = M
          do 10 L = 1, 25
            Field(N)(L:L) = ' '
10        continue
          K = 0
        else
          if (ColNam(M:M) .ne. ' ') then
            K = K + 1
            if (K .le. 25) then
              Field(N)(K:K) = ColNam(M:M)
            else
              if (LastErr .ne. N) then
                write(Line,*) N
                Line = 'GetFlds - Table column name no. '
     +               //Line(1:lnblnk(Line))//' longer than 25 '
     +               //'characters: '//Field(N)//'....; excess ignored'
                print *,Line(1:lnblnk(line))
                LastErr = N
              end if
            end if
          end if
        end if
100   continue
c
      return
      end
c
c=======================================================================
c
      Subroutine NextNarg(NArg,NArgs)
c
      integer NArg, NArgs
c
c-----------------------------------------------------------------------
c
      if (NArg .lt. NArgs) then
        NArg = NArg + 1
        return
      else
        print *,'ERROR: expected another argument but none found'
        call exit(64)
      end if
      return
      end
c
c=======================================================================
c
      subroutine upcase(string)
      character*(*) string
      integer*4 j, lnblnk
c
      do 10 j = 1,lnblnk(string)
         if(string(j:j) .ge. "a" .and. string(j:j) .le. "z") then
            string(j:j) = achar(iachar(string(j:j)) - 32)
         end if
10    continue
      return
      end
c
c=======================================================================
c
      subroutine ChkFld(Fld1, Fld2, k)
c      
      character*(*) Fld1, Fld2
      integer*4     k, lnblnk      
c
      if (Fld1 .ne. Fld2) then
        print *,'ERROR: input field no.',k,' expected to be ',
     +           Fld2(1:lnblnk(Fld2)),'; got ',Fld1(1:lnblnk(Fld2))
        call exit(64)
      end if
c      
      return
c      
      end
      
