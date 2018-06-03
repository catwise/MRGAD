c-----------------------------------------------------------------------
c
c       MrgAD  -  Merge Ascending & Descending mdex files for a tile
c                 (some cloning from tftool.f)
c
c  version 1.0  B70818: initial version
c          1.1  B71221: changed handling of aperture photometry; Tom
c                       says Asce and Desc solutions use same data; use
c                       simple average (errors ~100% correlated)
c          1.2  B71225: corrected computation of MJDmean variables;
c                       removed output header references to band 3;
c          1.3  B71225: fixed negative S/N for dEcLong & dEcLat
c          1.4  B80127: made dEcLong Desc-Asce to conform to parallax
c          1.4  B80130: added calculation of median differences in
c                       ecliptic long/lat and RA/Dec, parallax bias
c          1.5  B80202: added photometric discrepancy columns to output
c          1.6  B80207: bulletproofed rchi2, na, nb
c          1.7  B80209: added outlier rejection based on chi-squares
c          1.7  B80210: fixed bug in w1rchi2 outlier rejection
c          1.71 B80213: included the motion-solution photometry under
c                       the rchi2_pm filter for outlier rejection
c          1.72 B80215: fixed uninitialized character in dMagData string
c          1.8  B80222: added w?rchi2 & rchi2 histograms as functions of
c                       w?mpro
c          1.81 B80228: added Tom's mean & sigma dwmag? and
c                       dwmag?**2/(avg(w?sig_a)**2) + avg(w?sig_d)**2))
c                       for sources with s?snr = 20 ± 0.5; added
c                       PM-based estimate of parallax dEcLong_pm
c          1.82 B80302: added Tom's median dwmag? & histograms; added
c                       error covariance matrix rotation for ecliptic
c                       uncertainties; added dEcLong_pm &c.
c          1.83 B80307: added avg & sigma over MeanObsMJD
c          1.84 B80314: fixed field overflow for dwmpro rchi2's
c          1.85 B80320: set pBias default to 0.1; added mean & sigma 
c                       Asce-Desc w?mJDmean difference and used to 
c                       propagate Asce RA/Dec to Desc for sole purpose
c                       of stationary-based parallax estimate; added
c                       latter to mdex table
c          1.86 B80329: added test for singular error covariance
c                       matrices & fix if found
c          1.86 B80415: switch TJ rchi2 from avg to median
c          1.86 B80505: remove avg chisq from TJ stats; extreme
c                       asymmetry --> too misleading; median better
c          1.87 B80514: added galactic & ecliptic coordinates to "hists"
c          1.88 B80518: added galactic & ecliptic coordinates to "hists"
c                       headers
c          1.88 B80520: added window options for TJ Statistics
c          1.88 B80601: changed "error covariance" to "errcov" in
c                       summary to avoid false alarms in searches for
c                       "error" in stdouts
c          1.88 B80603: don't call GetMed for empty array
c
c-----------------------------------------------------------------------
c
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      Character*5000 Line
      Character*500  InFNam, OutFNam, InFNamA, InFNamD, HistNam, TJnam
      Character*141  EclData
      Character*145  ChiSqData
      Character*44   ParData
      Character*29   dMagData
      Character*25   Field(MaxFld), NumStr
      Character*11   Vsn, Fmt, IFmt
      Character*8    CDate, CTime
      Character*7    ChTmp1, ChTmp2
      Character*3    Flag, Flag0
      Character*1    SlashChar
      Integer*4      IArgC, LNBlnk, NArgs, NArg, nSrc, Access, nEpA,
     +               nEpD, IFa(MaxFld), IFb(MaxFld), NF, k, k1, k2, n,
     +               nRow, nBadAst1, nBadAst2, nBadW1Phot1, nBadW1Phot2,
     +               nBadAst, nBadW1Phot, KodeAst, KodePhot1, KodePhot2,
     +               KodePM, nBadPM1, nBadPM2, nBadPM, Itmp1, Itmp2,
     +               nBadW2Phot, nBadW2Phot1, nBadW2Phot2, NmdetIDerr,
     +               w1M, w1M2, w2M, w2M2, nBadPMCh1, nBadPMCh2,
     +               Nw1rchi2asce(20), Nw1rchi2desc(20),
     +               Nw1rchi2mrg(20),  Nw2rchi2asce(20),
     +               Nw2rchi2desc(20), Nw2rchi2mrg(20),
     +               Nrchi2asce(20),   Nrchi2desc(20),
     +               Nrchi2mrg(20), nMedDiff, nTJw1, nTJw2,
     +               TJdw1Hist(51), TJdw2Hist(51), nMJD, ndwMJD,
     +               nBadCovMatA, nBadCovMatD, nBadCovMat,
     +               nBadCovMatPA, nBadCovMatPD, nBadCovMatP,
     +               nAlpha1, nAlpha2, tw
      Logical*4      dbg, GotIn, GotOut, GotInA, GotInD, Good1, Good2,
     +               GoodCh1, GoodCh2, doTJw1, doTJw2, doTJhist,
     +               GoodDeterm1, GoodDeterm2
      Real*8         ra,  dec,  sigra,  sigdec,  sigradec,
     +               ra2, dec2, sigra2, sigdec2, sigraded, pa,
     +               EcLong, EcLat, EcLong2, EcLat2, EcLongSig,
     +               EcLongSig2, EcLatSig, EcLatSig2, R8tmp1, R8tmp2,
     +               d2r, AngDiff, dEcLong, dEcLat, dEcLongSig,
     +               dEcLatSig, dEcLongSNR, dEcLatSNR, dRA, dDec,
     +               chi2pmra, chi2pmdec, wad11, wad12, wad22,
     +               oma11, oma12, oma22, omd11, omd12, omd22,
     +               wa11,  wa12,  wa22,  wd11,  wd12,  wd22,
     +               deta, detd, detad, v11, v12, v22, v1, v2, pBias,
     +               dwmpro, ChiSqRat, wrchi2a, wrchi2d, wmpro,
     +               rchi2a, rchi2d, rchi2, TJw1Sum, TJw1SumSq,
     +               TJw2Sum, TJw2SumSq, TJw1SumSigA, TJw2SumSigA,
     +               TJw1SumSigD, TJw2SumSigD, sumAlpha1, sumAlpha2,
     +               sumMJD, sumMJDsq, rchi2dwm, rastat, decstat, 
     +               sigrastat, sigdecstat, sigradecstat, dw1MJD,
     +               dw2MJD, sumdw1MJD, sumdw2MJD, sum2dw1MJD,
     +               sum2dw2MJD, PMRA, sigPMRA, PMDec, sigPMDec,
     +               Alpha1, sumDec1, sumDec2
      Real*4, allocatable :: MedEclong(:), MedEcLat(:), MedRA(:),
     +               MedDec(:), w1rchi2asce(:,:), w2rchi2asce(:,:),
     +               w1rchi2desc(:,:), w2rchi2desc(:,:),
     +               w1rchi2mrg(:,:), w2rchi2mrg(:,:), rchi2asce(:,:),
     +               rchi2desc(:,:), rchi2mrg(:,:), TJdw1(:), TJdw2(:),
     +               dw1ChSq(:), dw2ChSq(:)
      Real*4         MedDiff(4), MedRchi2(9,20), TrFrac, TJsnr1, TJsnr2,
     +               rchisq, GaLong, GaLat 
c
      Data Vsn/'1.88 B80603'/, nSrc/0/, nRow/0/, d2r/1.745329252d-2/,
     +     dbg,GotIn,GotOut,GotInA,GotInD/5*.false./, doTJhist/.false./,
     +     nBadAst1,nBadAst2,nBadW1Phot1,nBadW1Phot2,nBadAst,
     +     nBadW1Phot,nBadW2Phot1,nBadW2Phot2,nBadW2Phot/9*0/,
     +     KodeAst,KodePhot1,KodePhot2,KodePM/4*0/, pBias/0.09d0/,
     +     nBadPM1,nBadPM2,nBadPM/3*0/, NmdetIDerr/0/, ChiSqRat/3.0d0/,
     +     nBadPMCh1,nBadPMCh2/2*0/, nMedDiff/0/, SlashChar/'/'/,
     +     dMagData/'  null   null   null   null  '/, Nw1rchi2asce,
     +     Nw1rchi2desc,Nw1rchi2mrg,Nw2rchi2asce, Nw2rchi2desc,
     +     Nw2rchi2mrg,Nrchi2asce,Nrchi2desc,Nrchi2mrg/180*0/,
     +     TJw1Sum,TJw1SumSq,TJw2Sum,TJw2SumSq/4*0.0d0/, TrFrac/0.1/,
     +     TJw1SumSigA,TJw2SumSigA,TJw1SumSigD,TJw2SumSigD/4*0.0d0/,
     +     nTJw1, nTJw2/2*0/, Alpha1/0.0d0/, tw/1/,
     +     TJdw1Hist,TJdw2Hist/102*0/, TJsnr1/9.98/, TJsnr2/10.02/,
     +     sumMJD,sumMJDsq/2*0.0d0/, nMJD/0/, ndwMJD/0/,
     +     sumdw1MJD,sumdw2MJD,sum2dw1MJD,sum2dw2MJD/4*0.0d0/,
     +     rastat,decstat/2*0.0d0/, nAlpha1,nAlpha2/2*0/,
     +     nBadCovMatA,nBadCovMatD,nBadCovMat/3*0/,
     +     nBadCovMatPA,nBadCovMatPD,nBadCovMatP/3*0/,
     +     sumAlpha1,sumAlpha2,sumDec1, sumDec2/4*0.0d0/

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
        print *,'The OPTIONAL flags are:'
        print *,'    -cr maximum chi-square ratio for averaging (3.0)'
        print *,'    -pb parallax bias correction (asec; 0.09)'
        print *,'    -t1 lower S/N limit for TJ statistics (9.98)'
        print *,'    -t2 upper S/N limit for TJ statistics (10.02)'
        print *,'    -tf trim fraction for TJ statistics (0.1)'
        print *,'    -th generate TJ histogram table file'
        print *,'    -tw S/N window type for TJ statistics, 0-2 (1)'
        print *,'    -w  testing on Windows machine'
        print *,'    -d  turn on debug prints'
        call exit(32)
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
        if (dbg) print *,'input gsa file: ',InFNam(1:lnblnk(InFNam))
c                                      ! Turn debug prints on
      else if (Flag .eq. '-D') then
        dbg = .true.
        print *,'Debug prints enabled'
c                                      ! Testing on Windows machine
      else if (Flag .eq. '-W') then
        SlashChar = '\'
        if (dbg) print *,'SlashChar = "\"'
c                                      ! input ascending stf file
      else if (Flag .eq. '-IA') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,InFNamA)
        if (Access(InFNamA(1:lnblnk(InFNamA)),' ') .ne. 0) then
          print *,'File not found: ',InFNamA(1:lnblnk(InFNamA))
          call exit(64)
        end if
        GotInA = .true.
        if (dbg) print *,'input ascending stf file: ',
     +           InFNamA(1:lnblnk(InFNamA))
c                                      ! input descending stf file
      else if (Flag .eq. '-ID') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,InFNamD)
        if (Access(InFNamD(1:lnblnk(InFNamD)),' ') .ne. 0) then
          print *,'File not found: ',InFNamD(1:lnblnk(InFNamD))
          call exit(64)
        end if
        GotInD = .true.
        if (dbg) print *,'input descending stf file: ',
     +           InFNamD(1:lnblnk(InFNamD))
c                                      ! output   file
      else if (Flag .eq. '-O') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,OutFNam)
        GotOut = .true.
c                                      ! parallax bias
      else if (Flag .eq. '-PB') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3007) pBias
        if (dbg) print *, 'parallax bias:', pBias
c                                      ! max chisq ratio
      else if (Flag .eq. '-CR') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3008) ChiSqRat
        if (dbg) print *, 'max chi-square ratio:', ChiSqRat
c                                      ! trim fraction
      else if (Flag .eq. '-TF') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3009) TrFrac
        if (dbg) print *, 'trim fraction:', TrFrac
c                                      ! trim fraction
      else if (Flag .eq. '-TW') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3012) tw
        if ((tw .lt. 0) .or. (tw .gt. 2)) go to 3012
        if (dbg) print *, 'TJ window type', tw
c                                      ! lower S/N limit
      else if (Flag .eq. '-T1') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3010) TJsnr1
        if (dbg) print *, 'TJsnr1:', TJsnr1
c                                      ! upper S/N limit
      else if (Flag .eq. '-T2') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3011) TJsnr2
        if (dbg) print *, 'TJsnr2:', TJsnr2
c                                      ! generate TJ hist file
      else if (Flag .eq. '-TH') then
        doTJhist = .true.
        if (dbg) print *,'TJ hist file will be generated'
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
c                                      ! Allocate arrays for median diffs
      allocate(MedEcLong(nSrc))
      if (.not.allocated(MedEcLong)) then
        print *,'ERROR: allocation of MedEcLong failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
      allocate(MedEcLat(nSrc))
      if (.not.allocated(MedEcLat)) then
        print *,'ERROR: allocation of MedEcLat failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
      allocate(MedRA(nSrc))
      if (.not.allocated(MedRA)) then
        print *,'ERROR: allocation of MedRA failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
      allocate(MedDec(nSrc))
      if (.not.allocated(MedDec)) then
        print *,'ERROR: allocation of MedDec failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c                                      ! Allocate arrays for chi-squares
      allocate(w1rchi2asce(nSrc,20))
      if (.not.allocated(w1rchi2asce)) then
        print *,'ERROR: allocation of w1rchi2asce failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(w1rchi2desc(nSrc,20))
      if (.not.allocated(w1rchi2desc)) then
        print *,'ERROR: allocation of w1rchi2desc failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(w1rchi2mrg(nSrc,20))
      if (.not.allocated(w1rchi2mrg)) then
        print *,'ERROR: allocation of w1rchi2mrg failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(w2rchi2asce(nSrc,20))
      if (.not.allocated(w2rchi2asce)) then
        print *,'ERROR: allocation of w2rchi2asce failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(w2rchi2desc(nSrc,20))
      if (.not.allocated(w1rchi2desc)) then
        print *,'ERROR: allocation of w2rchi2desc failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(w2rchi2mrg(nSrc,20))
      if (.not.allocated(w2rchi2mrg)) then
        print *,'ERROR: allocation of w2rchi2mrg failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(rchi2asce(nSrc,20))
      if (.not.allocated(rchi2asce)) then
        print *,'ERROR: allocation of rchi2asce failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(rchi2desc(nSrc,20))
      if (.not.allocated(rchi2desc)) then
        print *,'ERROR: allocation of rchi2desc failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(rchi2mrg(nSrc,20))
      if (.not.allocated(rchi2mrg)) then
        print *,'ERROR: allocation of rchi2mrg failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c                                      ! Allocate arrays for TJ Statistics
      allocate(TJdw1(nSrc))
      if (.not.allocated(TJdw1)) then
        print *,'ERROR: allocation of TJdw1 failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(dw1ChSq(nSrc))
      if (.not.allocated(dw1ChSq)) then
        print *,'ERROR: allocation of dw1ChSq failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(TJdw2(nSrc))
      if (.not.allocated(TJdw2)) then
        print *,'ERROR: allocation of TJdw2 failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
      allocate(dw2ChSq(nSrc))
      if (.not.allocated(dw2ChSq)) then
        print *,'ERROR: allocation of dw2ChSq failed'
        print *,'       no. elements =',nSrc
        call exit(64)
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Read and process table files
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
        if ((Line(1:26) .eq. '\  Subset generated by STF') .or.
     +      (Line(1:26) .eq. '\  Fields retained by STF:') .or.
     +      (Line(1:11) .eq. '\ band =  3')) go to 40
        if  (Line(1:16) .eq. '\ bands engaged:')
     +  Line = '\ bands engaged:   1  1  0  0'   
        write(20,'(a)') Line(1:lnblnk(Line))
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
c                                      ! append to input header lines
      write (20, '(a)') Line(1:1556)//'dw1mag|rch2w1|dw2mag|rch2w2|'
     +       //'   elon   |  elonSig |   elat   | elatSig |'
     +       //'   Delon  | DelonSig |   Delat  | DelatSig|'
     +       //' DelonSNR | DelatSNR | chi2pmra|chi2pmdec|ka|k1|k2|'
     +       //'km|m|  par_pm  | par_pmSig| par_stat | par_sigma|'
      read (10, '(a)', end=3000) Line
      write (20, '(a)') Line(1:1556)//'   r  |   r  |   r  |   r  |'
     +       //'    r     |     r    |     r    |    r    |'
     +       //'    r     |     r    |     r    |    r    |'
     +       //'    r     |     r    |    r    |     r   | i| i| i|'
     +       //' i|c|    r     |     r    |    r     |     r    |'
      read (10, '(a)', end=3000) Line
      write (20, '(a)') Line(1:1556)//'  mag |   -  |  mag |   -  |'
     +       //'   deg    |   asec   |    deg   |   asec  |'
     +       //'   asec   |   asec   |   asec   |   asec  |'
     +       //'     -    |     -    |    -    |    -    | -| -| -|'
     +       //' -|-|   asec   |   asec   |   asec   |   asec   |'
      read (10, '(a)', end=3000) Line
      write (20, '(a)') Line(1:1556)//' null | null | null | null |'
     +       //'   null   |   null   |   null   |   null  |'
     +       //'   null   |   null   |   null   |   null  |'
     +       //'   null   |   null   |   null  |   null  | n| n| n|'
     +       //' n|x|   null   |   null   |   null   |   null   |'
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Process each data line
1000  nRow = nRow + 1
      doTJw1 = .false.
      doTJw2 = .false.
      read (10, '(a)', end=3005) Line
      ParData = '    null       null       null       null   '
c                                      ! Check astrometric parameters
      GoodCh1 = index(Line(IFA(32):IFB(32)),  'null') .eq. 0
      GoodCh2 = index(Line(IFA(198):IFB(198)),'null') .eq. 0
      if (GoodCh1 .and. GoodCh2) then
        k = 198
        read (Line(IFA(k):IFB(k)), *, err = 3006) rchi2d ! rchi22
        k = 32
        read (Line(IFA(k):IFB(k)), *, err = 3006) rchi2a ! rchi2
        if ((rchi2a/rchi2d .gt. ChiSqRat) .or. 
     +      (rchi2d/rchi2a .gt. ChiSqRat)) then
          GoodCh1 = (rchi2d/rchi2a .gt. ChiSqRat)
          GoodCh2 = .not.GoodCh1
        else
          rchi2 = (rchi2a+rchi2d)/2.0d0         ! assume equal Ndf
          write(Line(IFA(k):IFB(k)),'(1pE11.3)') rchi2
        end if
      else if (GoodCh2) then
        Line(IFA(32):IFB(32)) = Line(IFA(198):IFB(198))
      end if
c
      Good1 = GoodCh1 .and. index(Line(IFA(3):IFB(7)),    'null') .eq. 0
      Good2 = GoodCh2 .and. index(Line(IFA(169):IFB(173)),'null') .eq. 0
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
     +     'Bad ascending AND descending Astrometry on source'
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
      if (sigra   .lt. 0.0001) sigra   = 0.0001
      if (sigra2  .lt. 0.0001) sigra2  = 0.0001
      if (sigdec  .lt. 0.0001) sigdec  = 0.0001
      if (sigdec2 .lt. 0.0001) sigdec2 = 0.0001
c                                      ! save for later parallax estimate
      rastat       = ra
      decstat      = dec
      sigrastat    = sigra
      sigdecstat   = sigdec
      sigradecstat = sigradec
c                                      ! Get Ecliptic positions
      call Cel2Ec(ra,  dec,  EcLong,  EcLat)
      call Cel2Ec(ra2, dec2, EcLong2, EcLat2)
      call cel2pa(ra,  dec, pa)
c                                      ! Get Ecliptic position uncertainties
      call r8covmat(sigra,sigdec,sigradec,-pa,
     +              EcLongSig,EcLatSig,R8tmp1)    ! ignore Ecl off-diagonal
      call r8covmat(sigra2,sigdec2,sigraded,-pa,
     +              EcLongSig2,EcLatSig2,R8tmp2)
c      
      dEcLong = EcLong2 - EcLong       ! use Desc-Asce for parallax
      if (dEcLong .gt.  180.0d0) dEcLong = dEcLong - 360.0d0
      if (dEcLong .lt. -180.0d0) dEcLong = dEcLong + 360.0d0
      dEcLong = 3600.0d0*dEcLong*dcos(d2r*EcLat) - pBias
      nMedDiff = nMedDiff + 1
      MedEcLong(nMedDiff) = dEcLong
      dEcLat = 3600.0d0*(EcLat2 - EcLat)
      MedEcLat(nMedDiff) = dEcLat
      dEcLongSig = dsqrt(EcLongSig**2 + EcLongSig2**2)
      dEcLatSig  = dsqrt(EcLatSig**2  + EcLatSig2**2)
      dEcLongSNR = dabs(dEcLong)/dEcLongSig
      dEcLatSNR  = dabs(dEcLat)/dEcLatSig
c                                      ! Get Average Ecliptic positions     
      EcLong = (EcLong*EcLongSig2**2 + EcLong2*EcLongSig**2)
     +       / (EcLongSig**2 + EcLongSig2**2) 
      EcLat  = (EcLat*EcLatSig2**2 + EcLat2*EcLatSig**2)
     +       / (EcLatSig**2 + EcLatSig2**2) 
      EcLongSig = sqrt(EcLongSig**2 * EcLongSig2**2
     +          /     (EcLongSig**2 + EcLongSig2**2))
      EcLatSig  = sqrt(EcLatSig**2 * EcLatSig2**2
     +          /     (EcLatSig**2 + EcLatSig2**2))
c     
      R8tmp1 = ra2 - ra
      if (R8tmp1 .gt.  180.0d0) R8tmp1 = R8tmp1 - 360.0d0
      if (R8tmp1 .lt. -180.0d0) R8tmp1 = R8tmp1 + 360.0d0
      MedRA(nMedDiff) = 3600.0d0*R8tmp1*dcos(d2r*dec)
      MedDec(nMedDiff) = 3600.0d0*(dec2 - dec)    
c                                      ! Get averaged RA & Dec
      oma11 = sigra**2                 ! A & D error covariance matrices
      oma12 = sigradec*dabs(sigradec)
      oma22 = sigdec**2
      deta  = oma11*oma22 - oma12**2
      omd11 = sigra2**2
      omd12 = sigraded*dabs(sigraded)
      omd22 = sigdec2**2
      detd  = omd11*omd22 - omd12**2
c
      GoodDeterm1 = deta .gt. 0.0d0    ! check for singular error covariance matrices
      GoodDeterm2 = detd .gt. 0.0d0
      if (.not.(GoodDeterm1.and.GoodDeterm2))
     +           nBadCovMat = nBadCovMat + 1
      if (.not.GoodDeterm1) then
        oma11 = sigra**2                 ! A & D error covariance matrices
        oma22 = sigdec**2
        deta  = oma11*oma22
        nBadCovMatA = nBadCovMatA + 1
        if (dbg) print *, 'Bad Asce covariance matrix on source'
     +                   //Line(IFA(1):IFB(1))
      end if
      if (.not.GoodDeterm2) then
        omd11 = sigra**2                 ! A & D error covariance matrices
        omd22 = sigdec**2
        detd  = omd11*omd22
        nBadCovMatD = nBadCovMatD + 1
        if (dbg) print *, 'Bad Desc covariance matrix on source'
     +                   //Line(IFA(1):IFB(1))
      end if
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
      if (nAlPha1 .eq. 0) then
        nAlpha1   = 1
        sumAlpha1 = ra
        sumDec1   = dec
        Alpha1    = ra
      end if
      if (nAlpha1 .gt. 0) then
        if (dabs(ra-Alpha1) .lt. 180.0d0) then
          nAlpha1 = nAlpha1 + 1
          sumAlpha1 = sumAlpha1 + ra
          sumDec1   = sumDec1 + dec
        else
          nAlpha2 = nAlpha2 + 1
          sumAlpha2 = sumAlpha2 + ra        
          sumDec2   = sumDec2 + dec
        end if
      end if
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
        read (Line(IFA(330):IFB(330)), *, err = 1050) Itmp2 ! nIters_pn
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
     +'ERROR reading nIters/nItert/nSteps/nStept on source'
     +       //Line(IFA(1):IFB(1))
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
        doTJw1 = ((R8tmp1 .ge. TJsnr1) .or. (R8tmp2 .ge. TJsnr1)) .and.
     +           ((R8tmp1 .le. TJsnr2) .or. (R8tmp2 .le. TJsnr2))
        if (doTJw1 .and. (tw .ne. 0)) then
          if (tw .eq. 1) then
            doTJw1 = ((R8tmp1 .ge. TJsnr1) .and. (R8tmp1 .le. TJsnr2))
     +          .or. ((R8tmp2 .ge. TJsnr1) .and. (R8tmp2 .le. TJsnr2))
          else
            doTJw1 = ((R8tmp1 .ge. TJsnr1) .and. (R8tmp1 .le. TJsnr2))
     +         .and. ((R8tmp2 .ge. TJsnr1) .and. (R8tmp2 .le. TJsnr2))
          end if
        end if
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
        doTJw2 = ((R8tmp1 .ge. TJsnr1) .or. (R8tmp2 .ge. TJsnr1)) .and.
     +           ((R8tmp1 .le. TJsnr2) .or. (R8tmp2 .le. TJsnr2))
        if (doTJw2 .and. (tw .ne. 0)) then
          if (tw .eq. 1) then
            doTJw2 = ((R8tmp1 .ge. TJsnr1) .and. (R8tmp1 .le. TJsnr2))
     +          .or. ((R8tmp2 .ge. TJsnr1) .and. (R8tmp2 .le. TJsnr2))
          else
            doTJw2 = ((R8tmp1 .ge. TJsnr1) .and. (R8tmp1 .le. TJsnr2))
     +         .and. ((R8tmp2 .ge. TJsnr1) .and. (R8tmp2 .le. TJsnr2))
          end if
        end if
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
      GoodCh1 = index(Line(IFA(28):IFB(28)),  'null') .eq. 0  ! w1rchi2
      GoodCh2 = index(Line(IFA(194):IFB(194)),'null') .eq. 0  ! w1rchi22
1245  if (GoodCh1 .and. GoodCh2) then
        k = 28
        Read(Line(IFA(k):IFB(k)), *, err = 3006) wrchi2a   ! w1rchi2
        k = 194
        Read(Line(IFA(k):IFB(k)), *, err = 3006) wrchi2d   ! w1rchi22
        if ((wrchi2a/wrchi2d .gt. ChiSqRat) .or. 
     +      (wrchi2d/wrchi2a .gt. ChiSqRat)) then
          GoodCh1 = (wrchi2d/wrchi2a .gt. ChiSqRat)
          GoodCh2 = .not.GoodCh1
        end if
      end if
c
      Good1 = GoodCh1 .and. index(Line(IFA(26):IFB(27)),  'null') .eq. 0
      Good1 = Good1   .and. index(Line(IFA(98):IFB(99)),  'null') .eq. 0
      Good1 = Good1   .and. index(Line(IFA(99):IFB(99)),     '0') .eq. 0
      Good2 = GoodCh2 .and. index(Line(IFA(192):IFB(193)),'null') .eq. 0
      Good2 = Good2   .and. index(Line(IFA(264):IFB(265)),'null') .eq. 0
      Good2 = Good2   .and. index(Line(IFA(265):IFB(265)),   '0') .eq. 0
      if (Good1 .and. Good2) go to 1250
      dMagData = '  null   null   null   null  '
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
     +     'Bad ascending AND descending W1 Photrometry on source'
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
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1sigmpro
      k = 193
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1sigmprp
      v1 = R8tmp1**2
      v2 = R8tmp2**2
      k = 192
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! w1mprp
      k = 26
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! w1mpro
      dwmpro = R8tmp1 - R8tmp2
      wmpro = (v2*R8tmp1+v1*R8tmp2)/(v1+v2) ! refined w1mpro
      write (Line(IFA(k):IFB(k)),'(F7.3)') wmpro
      k = R8tmp1 + 1
      if (k .lt.  1) k = 1
      if (k .gt. 20) k = 20
      Nw1rchi2asce(k) = Nw1rchi2asce(k) + 1
      w1rchi2asce(Nw1rchi2asce(k),k) = wrchi2a
      R8tmp1 = dsqrt(v1*v2/(v1+v2))
      k = 27
      write (Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
      rchi2dwm = dwmpro**2/(v1+v2)
      if (rchi2dwm .gt. 99.999) rchi2dwm = 99.999
      write (dMagData(1:14),'(2F7.3)') dwmpro, rchi2dwm
c                                      ! TJ Statistics
      if (doTJw1) then
        nTJw1          = nTJw1 + 1
        TJdw1(nTJw1)   = dwmpro
        TJw1SumSigA    = TJw1SumSigA + dsqrt(v1)
        TJw1SumSigD    = TJw1SumSigD + dsqrt(v2)
        dw1ChSq(nTJw1) = dwmpro**2/(v1+v2)
        k = (50.5*(dwmpro + 0.5)) + 1
        if (k .lt. 1)  k = 1
        if (k .gt. 51) k = 51
        TJdw1Hist(k) = TJdw1Hist(k) + 1
      end if
c
      k = wmpro + 1
      if (k .lt.  1) k = 1
      if (k .gt. 20) k = 20
      Nrchi2mrg(k) = Nrchi2mrg(k) + 1
      rchi2mrg(Nrchi2mrg(k),k) = rchi2
      Nrchi2asce(k) = Nrchi2asce(k) + 1
      rchi2asce(Nrchi2asce(k),k) = rchi2a
      k = R8tmp2 + 1
      if (k .lt.  1) k = 1
      if (k .gt. 20) k = 20
      Nw1rchi2desc(k) = Nw1rchi2desc(k) + 1
      w1rchi2desc(Nw1rchi2desc(k),k) = wrchi2d
      Nrchi2desc(k) = Nrchi2desc(k) + 1
      rchi2desc(Nrchi2desc(k),k) = rchi2d
c      
      k = 99
      Read(Line(IFA(k):IFB(k)), *, err = 3006) w1M      ! w1M
      k = 265
      Read(Line(IFA(k):IFB(k)), *, err = 3006) w1M2     ! w1M2
      Itmp1 = w1M + w1M2
      if (Itmp1 .gt. 0) then
        wrchi2a = (dfloat(w1M)*wrchi2a + dfloat(w1M2)*wrchi2d)
     +          /  dfloat(w1M+w1M2)
        k = 28
        write (Line(IFA(k):IFB(k)),'(1pe11.3)') wrchi2a
        k = wmpro + 1
        if (k .lt.  1) k = 1
        if (k .gt. 20) k = 20
        Nw1rchi2mrg(k) = Nw1rchi2mrg(k) + 1
        w1rchi2mrg(Nw1rchi2mrg(k),k) = wrchi2a
      end if                           ! else leave "null"
      k = 99
      write (Line(IFA(k):IFB(k)),'(I6)') Itmp1
      k = 264
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp2    ! w1NM2
      k = 98
      Read(Line(IFA(k):IFB(k)), *, err = 3006) Itmp1    ! w1NM
      Itmp1 = Itmp1 + Itmp2
      write (Line(IFA(k):IFB(k)),'(I7)') Itmp1

1260  GoodCh1 = index(Line(IFA(31):IFB(31)),  'null') .eq. 0  ! w2rchi2
      GoodCh2 = index(Line(IFA(197):IFB(197)),'null') .eq. 0  ! w2rchi22
      if (GoodCh1 .and. GoodCh2) then     
        k = 31
        Read(Line(IFA(k):IFB(k)), *, err = 3006) wrchi2a   ! w2rchi2
        k = 197
        Read(Line(IFA(k):IFB(k)), *, err = 3006) wrchi2d   ! w2rchi22
        if ((wrchi2a/wrchi2d .gt. ChiSqRat) .or. 
     +      (wrchi2d/wrchi2a .gt. ChiSqRat)) then
          GoodCh1 = (wrchi2d/wrchi2a .gt. ChiSqRat)
          GoodCh2 = .not.GoodCh1
        end if
      end if
c
      Good1 = GoodCh1 .and. index(Line(IFA(29):IFB(31)),  'null') .eq. 0
      Good1 = Good1   .and. index(Line(IFA(109):IFB(110)),'null') .eq. 0
      Good1 = Good1   .and. index(Line(IFA(110):IFB(110)),   '0') .eq. 0
      Good2 = GoodCh2 .and. index(Line(IFA(195):IFB(197)),'null') .eq. 0
      Good2 = Good2   .and. index(Line(IFA(275):IFB(276)),'null') .eq. 0
      Good2 = Good2   .and. index(Line(IFA(276):IFB(276)),   '0') .eq. 0
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
     +     'Bad ascending AND descending W2 Photrometry on source'
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
c
      k = R8tmp1 + 1
      if (k .lt.  1) k = 1
      if (k .gt. 20) k = 20
      Nw2rchi2asce(k) = Nw2rchi2asce(k) + 1
      w2rchi2asce(Nw2rchi2asce(k),k) = wrchi2a
      k = R8tmp2 + 1
      if (k .lt.  1) k = 1
      if (k .gt. 20) k = 20
      Nw2rchi2desc(k) = Nw2rchi2desc(k) + 1
      w2rchi2desc(Nw2rchi2desc(k),k) = wrchi2d
c      
      dwmpro = R8tmp1 - R8tmp2
      wmpro = (v2*R8tmp1+v1*R8tmp2)/(v1+v2) ! refined w2mpro
      k = 29
      write (Line(IFA(k):IFB(k)),'(F7.3)') wmpro
      R8tmp1 = dsqrt(v1*v2/(v1+v2))
      k = 30
      write (Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
      rchi2dwm = dwmpro**2/(v1+v2)
      if (rchi2dwm .gt. 99.999) rchi2dwm = 99.999
      write (dMagData(15:28),'(2F7.3)') dwmpro, rchi2dwm
c                                      ! TJ Statistics
      if (doTJw2) then
        nTJw2        = nTJw2 + 1
        TJdw2(nTJw2) = dwmpro
        dw2ChSq(nTJw2) = dwmpro**2/(v1+v2)
        TJw2SumSigA  = TJw2SumSigA + dsqrt(v1)
        TJw2SumSigD  = TJw2SumSigD + dsqrt(v2)
        k = (50.5*(dwmpro + 0.5)) + 1
        if (k .lt. 1)  k = 1
        if (k .gt. 51) k = 51
        TJdw2Hist(k) = TJdw2Hist(k) + 1
      end if
c      
      k = 110
      Read(Line(IFA(k):IFB(k)), *, err = 3006) w2M      ! w2M
      k = 276
      Read(Line(IFA(k):IFB(k)), *, err = 3006) w2M2     ! w2M2
      Itmp1 = w2M + w2M2
      if (Itmp1 .gt. 0) then
        wrchi2a = (dfloat(w2M)*wrchi2a + dfloat(w2M2)*wrchi2d)
     +          /  dfloat(Itmp1)
        k = 31
        write (Line(IFA(k):IFB(k)),'(1pe11.3)') wrchi2a
        k = wmpro + 1
        if (k .lt.  1) k = 1
        if (k .gt. 20) k = 20
        Nw2rchi2mrg(k) = Nw2rchi2mrg(k) + 1
        w2rchi2mrg(Nw2rchi2mrg(k),k) = wrchi2a
      end if                           ! else leave "null"
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
c     
      Good1 = index(Line(IFA(33):IFB(33)),  'null') .eq. 0
      Good2 = index(Line(IFA(199):IFB(199)),'null') .eq. 0
      if (Good1 .and. Good2) then
        k = 199
        read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp2 ! nb2
        k = 33
        read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp1 ! nb
        if (Itmp2 .gt. Itmp1) Itmp1 = Itmp2
        write(Line(IFA(k):IFB(k)),'(I4)') Itmp1
      else if (Good2) then
        Line(IFA(33):IFB(33)) = Line(IFA(199):IFB(199))
      end if
c
      Good1 = index(Line(IFA(34):IFB(34)),  'null') .eq. 0
      Good2 = index(Line(IFA(200):IFB(200)),'null') .eq. 0
      if (Good1 .and. Good2) then
        k = 200     
        read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp2 ! na2
        k = 34
        read (Line(IFA(k):IFB(k)), *, err = 3006) Itmp1 ! na
        if (Itmp2 .gt. Itmp1) Itmp1 = Itmp2
        write(Line(IFA(k):IFB(k)),'(I4)') Itmp1
      else if (Good2) then
        Line(IFA(34):IFB(34)) = Line(IFA(200):IFB(200))
      end if
c
      Good1 = index(Line(IFA(40):IFB(44)),'null')   .eq. 0 ! w1mag-w1mcor
      Good2 = index(Line(IFA(206):IFB(210)),'null') .eq. 0 ! w1mag2-w1mcos
      if (Good1 .and. Good2) then
        k = 41
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1sigm
        k = 207
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1sign
        v1 = (R8tmp1 + R8tmp2)/2.0
        k = 206
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mag2
        k = 40
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mag
        R8tmp1 = (R8tmp1+R8tmp2)/2.0
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
        k = 41
        write(Line(IFA(k):IFB(k)),'(F7.3)') v1
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
        v1 = (R8tmp1 + R8tmp2)/2.0
        k = 211
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mag2
        k = 45
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mag
        R8tmp1 = (R8tmp1+R8tmp2)/2.0
        write(Line(IFA(k):IFB(k)),'(F7.3)') R8tmp1
        k = 46
        write(Line(IFA(k):IFB(k)),'(F7.3)') v1
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
          v1 = (R8tmp1 + R8tmp2)/2.0
          k = k2
          read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w?mag_?2
          k = k1
          read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w?mag_?
          R8tmp1 = (R8tmp1+R8tmp2)/2.0
          write(Line(IFA(k):IFB(k)),'(F10.3)') R8tmp1
           k = k1 + 1
          write(Line(IFA(k):IFB(k)),'(F10.3)') v1
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
        v1 = (R8tmp1 + R8tmp2)/2.0
        k = 266
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1magP2
        k = 100
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1magP
        R8tmp1 = (R8tmp1+R8tmp2)/2.0
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        k = 102
        write(Line(IFA(k):IFB(k)),'(F8.3)') v1
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
        v1 = (R8tmp1 + R8tmp2)/2.0
        k = 277
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2magP2
        k = 111
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2magP
        R8tmp1 = (R8tmp1+R8tmp2)/2.0
        write(Line(IFA(k):IFB(k)),'(F8.3)') R8tmp1
        k = 113
        write(Line(IFA(k):IFB(k)),'(F8.3)') v1
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
      dw1MJD = -9.9d9
      Good1 = index(Line(IFA(106):IFB(108)),'null') .eq. 0 ! w1mJDmin-w1mJDmean
      Good2 = index(Line(IFA(272):IFB(274)),'null') .eq. 0 ! w1mJDmin2-w1mJDmean2
      if (Good1 .and. Good2) then
        k = 274
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mJDmean2
        k = 108
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mJDmean
        dw1MJD = r8tmp1 - r8tmp2                         !  Asce-Dsec for PM later
        k = 272
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mJDmin2
        k = 106
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mJDmin
        if (R8tmp2 .lt. R8tmp1) then
          Line(IFA(106):IFB(106)) = Line(IFA(272):IFB(272))
          v1 = R8tmp2
        else
          v1 = R8tmp1
        end if
        k = 273
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w1mJDmax2
        k = 107
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w1mJDmax
        if (R8tmp2 .gt. R8tmp1) then
          Line(IFA(107):IFB(107)) = Line(IFA(273):IFB(273))
          v2 = R8tmp2
        else
          v2 = R8tmp1
        end if
        k = 108
        R8tmp1 = (v1+v2)/2.0d0        
        write(Line(IFA(k):IFB(k)),'(F18.8)') R8tmp1
      else if (Good2) then
        Line(IFA(106):IFB(108)) = Line(IFA(272):IFB(274))
      end if
c
      dw2MJD = -9.9d9
      Good1 = index(Line(IFA(117):IFB(119)),'null') .eq. 0 ! w2mJDmin-w2mJDmean
      Good2 = index(Line(IFA(283):IFB(285)),'null') .eq. 0 ! w2mJDmin2-w2mJDmean2
      if (Good1 .and. Good2) then
        k = 285
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mJDmean2
        k = 119
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mJDmean
        dw2MJD = r8tmp1 - r8tmp2                         !  Asce-Dsec for PM later
        k = 283
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mJDmin2
        k = 117
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mJDmin
        if (R8tmp2 .lt. R8tmp1) then
          Line(IFA(117):IFB(117)) = Line(IFA(283):IFB(283))
          v1 = R8tmp2
        else
          v2 = R8tmp1
        end if      
        k = 284
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  !  w2mJDmax2
        k = 118
        read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  !  w2mJDmax
        if (R8tmp2 .gt. R8tmp1) then
          Line(IFA(118):IFB(118)) = Line(IFA(284):IFB(284))
          v2 = R8tmp2
        else
          v2 = R8tmp1
        end if
        k = 119
        R8tmp1 = (v1+v2)/2.0d0        
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
c      
      GoodCh1 = index(Line(IFA(162):IFB(162)),'null') .eq. 0  ! rchi2_pm
      GoodCh2 = index(Line(IFA(328):IFB(328)),'null') .eq. 0  ! rchi2_pm2
1500  if (GoodCh1 .and. GoodCh2) then
        k = 328
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2  ! rchi2_pm2
        k = 162
        read (Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1  ! rchi2_pm
        if ((R8tmp1/R8tmp2 .gt. ChiSqRat) .or. 
     +      (R8tmp2/R8tmp1 .gt. ChiSqRat)) then
          GoodCh1 = (R8tmp2/R8tmp1 .gt. ChiSqRat)
          GoodCh2 = .not.GoodCh1
          if (GoodCh1) then
            nBadPMCh2 = nBadPMCh2 + 1
          else
            nBadPMCh1 = nBadPMCh1 + 1
          end if
          go to 1500
        end if
        if (w1M+w2M+w2M2+w2M2 .gt. 0) then
          R8tmp1 = (dfloat(w1M+w2M)*R8tmp1 + dfloat(w1M2+w2M2)*R8tmp2)
     +           /  dfloat(w1M+w2M+w2M2+w2M2)
          write (Line(IFA(k):IFB(k)), '(1pE11.3)') R8tmp1
        end if                         ! else leave "null"
      else if (GoodCh2) then
        Line(IFA(162):IFB(162)) = Line(IFA(328):IFB(328))
      end if

      Good1 = GoodCh1 .and. index(Line(IFA(140):IFB(149)),'null') .eq. 0
      Good2 = GoodCh2 .and. index(Line(IFA(306):IFB(315)),'null') .eq. 0
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
     +     'Bad ascending AND descending PM on source'
     +                //Line(IFA(1):IFB(1))
      end if
      if (Good2) then
        Line(IFA(140):IFB(149))   = Line(IFA(306):IFB(315))
      end if
      EclData(108:128) = '    null      null   '
      go to 1512
c
1510  KodePM = 3      
      k = 306
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp2   ! MeanObsMJD2
      k = 140
      Read(Line(IFA(k):IFB(k)), *, err = 3006) R8tmp1   ! MeanObsMJD
      R8tmp1 = (R8tmp1+R8tmp2)/2.0d0
      sumMJD   = sumMJD   + R8tmp1
      sumMJDsq = sumMJDsq + R8tmp1**2
      nMJD     = nMJD + 1
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
      if (sigra   .lt. 0.0001) sigra   = 0.0001
      if (sigra2  .lt. 0.0001) sigra2  = 0.0001
      if (sigdec  .lt. 0.0001) sigdec  = 0.0001
      if (sigdec2 .lt. 0.0001) sigdec2 = 0.0001
c
      call Cel2Ec(ra,  dec,  EcLong,  EcLat)
      call Cel2Ec(ra2, dec2, EcLong2, EcLat2)
c                                      ! Get Ecliptic position uncertainties
      call r8covmat(sigra,sigdec,sigradec,-pa,
     +              EcLongSig,EcLatSig,R8tmp1)
      call r8covmat(sigra2,sigdec2,sigraded,-pa,
     +              EcLongSig2,EcLatSig2,R8tmp2)
c      
      dEcLong = EcLong2 - EcLong       ! use Desc-Asce for parallax
      if (dEcLong .gt.  180.0d0) dEcLong = dEcLong - 360.0d0
      if (dEcLong .lt. -180.0d0) dEcLong = dEcLong + 360.0d0
      dEcLong = (3600.0d0*dEcLong*dcos(d2r*EcLat) - pBias)/2.0d0
      dEcLongSig = dsqrt(EcLongSig**2 + EcLongSig2**2)/2.0d0
      write(ParData(1:22),'(2f11.3)') dEcLong, dEcLongSig     
c                                      ! Check for zero-point straddle
      if (dabs(ra-ra2) .gt. 180.0d0) then
        if (ra .gt. ra2) then
          ra = ra - 360.0d0
        else
          ra2 = ra2 - 360.0d0        
        end if
      end if      
c      
      oma11 = sigra**2                 ! A & D error covariance matrices
      oma12 = sigradec*dabs(sigradec)
      oma22 = sigdec**2
      deta  = oma11*oma22 - oma12**2
      omd11 = sigra2**2
      omd12 = sigraded*dabs(sigraded)
      omd22 = sigdec2**2
      detd  = omd11*omd22 - omd12**2
c
      GoodDeterm1 = deta .gt. 0.0d0    ! check for singular error covariance matrices
      GoodDeterm2 = detd .gt. 0.0d0
      if (.not.(GoodDeterm1.and.GoodDeterm2))
     +          nBadCovMatP = nBadCovMatP + 1
      if (.not.GoodDeterm1) then
        oma11 = sigra**2                 ! A & D error covariance matrices
        oma22 = sigdec**2
        deta  = oma11*oma22
        nBadCovMatPA = nBadCovMatPA + 1
        if (dbg) print *, 'Bad Asce motion-position covariance matrix'
     +                   //' on source'//Line(IFA(1):IFB(1))
      end if
      if (.not.GoodDeterm2) then
        omd11 = sigra**2                 ! A & D error covariance matrices
        omd22 = sigdec**2
        detd  = omd11*omd22
        nBadCovMatPD = nBadCovMatPD + 1
        if (dbg) print *, 'Bad Desc motion-position covariance matrix'
     +                   //' on source'//Line(IFA(1):IFB(1))
      end if
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
      if (sigra   .lt. 0.0001) sigra   = 0.0001
      if (sigra2  .lt. 0.0001) sigra2  = 0.0001
      if (sigdec  .lt. 0.0001) sigdec  = 0.0001
      if (sigdec2 .lt. 0.0001) sigdec2 = 0.0001
c
      v1 = sigra**2
      v2 = sigra2**2
      R8tmp1 = (ra - ra2)**2/(v1 + v2)  ! chi2pmra
      write(EclData(108:117),'(1pE10.3)') R8tmp1
      R8tmp1  = (v2*ra+v1*ra2)/(v1+v2)
      R8tmp2  = dsqrt(v1*v2/(v1+v2))
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
1512  Good1 = GoodCh1 .and. index(Line(IFA(163):IFB(163)),'null') .eq. 0  ! pmcode
      Good2 = GoodCh2 .and. index(Line(IFA(329):IFB(329)),'null') .eq. 0  ! pmcodf
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
1530  Good1 = GoodCh1 .and. index(Line(IFA(164):IFB(165)),'null') .eq. 0
      Good2 = GoodCh2 .and. index(Line(IFA(330):IFB(331)),'null') .eq. 0
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
1550  Good1 = GoodCh1 .and. index(Line(IFA(150):IFB(150)),'null') .eq. 0  ! w1snr_pm
      Good2 = GoodCh2 .and. index(Line(IFA(316):IFB(316)),'null') .eq. 0  ! w1snr_pn
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
      Good1 = GoodCh1 .and. index(Line(IFA(151):IFB(151)),'null') .eq. 0  ! w2snr_pm
      Good2 = GoodCh2 .and. index(Line(IFA(317):IFB(317)),'null') .eq. 0  ! w2snr_pn
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
      Good1 = GoodCh1 .and. index(Line(IFA(138):IFB(139)),'null') .eq. 0  ! p1-p2
      Good2 = GoodCh2 .and. index(Line(IFA(304):IFB(305)),'null') .eq. 0  ! p12-p22
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
      Good1 = GoodCh1 .and. index(Line(IFA(152):IFB(153)),'null') .eq. 0  ! w1flux_pm-w1sigflux_pm
      Good2 = GoodCh2 .and. index(Line(IFA(318):IFB(319)),'null') .eq. 0  ! w1flux_pm2-w1sigflux_pm2
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
      Good1 = GoodCh1 .and. index(Line(IFA(154):IFB(155)),'null') .eq. 0  ! w2flux_pm-w2sigflux_pm
      Good2 = GoodCh2 .and. index(Line(IFA(320):IFB(321)),'null') .eq. 0  ! w2flux_pm2-w2sigflux_pm2
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
        Line(IFA(154):IFB(155)) = Line(IFA(320):IFB(321))
      end if
c      
      Good1 = GoodCh1 .and. index(Line(IFA(156):IFB(158)),'null') .eq. 0  ! w1mpro_pm-w1rchi2_pm
      Good2 = GoodCh2 .and. index(Line(IFA(322):IFB(324)),'null') .eq. 0  ! w1mpro_pn-w1rchi2_pn
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
        if (w1M+w1M2 .gt. 0) then
          R8tmp1 = (dfloat(w1M)*R8tmp1 + dfloat(w1M2)*R8tmp2)
     +           /  dfloat(w1M+w1M2)
          write (Line(IFA(k):IFB(k)), '(1pE11.3)') R8tmp1
        end if                         ! else leave "null"
      else if (Good2) then
        Line(IFA(156):IFB(158)) = Line(IFA(322):IFB(324))
      end if
c      
      Good1 = GoodCh1 .and. index(Line(IFA(159):IFB(161)),'null') .eq. 0  ! w2mpro_pm-w2rchi2_pm
      Good2 = GoodCh2 .and. index(Line(IFA(325):IFB(327)),'null') .eq. 0  ! w2mpro_pn-w2rchi2_pn
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
        if (w2M+w2M2 .gt. 0) then
          R8tmp1 = (dfloat(w2M)*R8tmp1 + dfloat(w2M2)*R8tmp2)
     +           /  dfloat(w2M+w2M2)
          write (Line(IFA(k):IFB(k)), '(1pE11.3)') R8tmp1
        end if                         ! else leave "null"
      else if (Good2) then
        Line(IFA(159):IFB(161)) = Line(IFA(325):IFB(327))
      end if
c
      write(EclData(138:139),'(I2)') KodePM
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Stationary Solution Parallax
c                                      ! translate ra, dec by PM
      if ((KodeAst .lt. 3) .or. (dw1MJD .lt. -1.0e9) .or.
     +    (KodePM  .eq. 0) .or. (dw2MJD .lt. -1.0e9)) go to 1900
      ndwMJD     = ndwMJD + 1
      sumdw1MJD  = sumdw1MJD + dw1MJD
      sum2dw1MJD = sum2dw1MJD + dw1MJD**2
      sumdw2MJD  = sumdw2MJD + dw2MJD
      sum2dw2MJD = sum2dw2MJD + dw2MJD**2
c
      dw1MJD = (dw1MJD + dw2MJD)/2.0
      k = 146
      Read(Line(IFA(k):IFB(k)), *, err = 1600) pmra
      pmra = pmra/1314900.0            ! convert asec/yr to deg/day
      if (dabs(decstat) .ne. 90.0d0) then
        dRA  = pmra*dw1MJD/dcos(d2r*decstat)
      else
        dRA = 0.0d0
      end if
      ra = rastat + dRA
	  if (ra .gt. 360.0d0) then
		ra = ra - 360.0d0
	  else if (ra .lt. 0.0d0) then
		ra = 360.0d0 + ra
      end if
      k = 147
      Read(Line(IFA(k):IFB(k)), *, err = 1600) pmdec
      pmdec = pmdec/1314900.0
      dDec = pmdec*dw1mjd
      dec  = decstat + dDec
c
      k = 148
      Read(Line(IFA(k):IFB(k)), *, err = 1600) sigpmra
      k = 149
      Read(Line(IFA(k):IFB(k)), *, err = 1600) sigpmdec
      sigrastat  = dsqrt(sigrastat**2  + (dw1MJD*sigpmra/365.25)**2)
      sigdecstat = dsqrt(sigdecstat**2 + (dw1MJD*sigpmdec/365.25)**2)
c                                      ! Get Ecliptic positions
      call Cel2Ec(ra,  dec,  EcLong,  EcLat)
c                                      ! Get Ecliptic position uncertainties
      call r8covmat(sigrastat,sigdecstat,sigradecstat,-pa,
     +              EcLongSig,EcLatSig,R8tmp1)    ! ignore Ecl off-diagonal
c      
      dEcLong = EcLong2 - EcLong       ! use Desc-Asce for parallax
      if (dEcLong .gt.  180.0d0) dEcLong = dEcLong - 360.0d0
      if (dEcLong .lt. -180.0d0) dEcLong = dEcLong + 360.0d0
      dEcLong = (3600.0d0*dEcLong*dcos(d2r*EcLat) - pBias)/2.0d0
      dEcLongSig = dsqrt(EcLongSig**2 + EcLongSig2**2)/2.0d0
      write (ParData(23:44),'(2f11.3)') dEcLong, dEcLongSig
      go to 1900
c
1600  if (dbg) then
        print *,'WARNING: parallax computation failed for source ',
     +           Line(IFA(1):IFB(2))
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Output data row
c
1900  write (20,'(a)') Line(1:1555)//dMagData//EclData//ParData
c     
      if (nRow .lt. nSrc) go to 1000
      close (20)
c
      print *,'No. data rows passed through to the output file:       ',
     +         nRow
      print *,'No. data rows with bad astrometry passed through:      ',
     +         nBadAst
      if (nBadAst .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadAst1
        print *,'No. of bad descending rows:',nBadAst2
        if (nBadAst .ne. nBadAst1+nBadAst2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadAst1+nBadAst2-nBadAst
      end if
      print *,'No. data rows with bad stationary-position'
      print *,'    errcov matrices passed '
     +         //'through:                    ', nBadCovMat
      if (nBadCovMat .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadCovMatA
        print *,'No. of bad descending rows:',nBadCovMatD
        if (nBadCovMat .ne. nBadCovMatA+nBadCovMatD)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadCovMatA+nBadCovMatD-nBadCovMat
      end if
      print *,'No. data rows with bad motion-solution position'
      print *,'    errcov matrices passed '
     +         //'through:                    ', nBadCovMatP
      if (nBadCovMatP .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadCovMatPA
        print *,'No. of bad descending rows:',nBadCovMatPD
        if (nBadCovMatP .ne. nBadCovMatPA+nBadCovMatPD)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadCovMatPA+nBadCovMatPD-nBadCovMatP
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
      print *,
     + 'No. data rows with bad W2 photometry passed through:   ',
     +         nBadW2Phot
      if (nBadW2Phot .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadW2Phot1
        print *,'No. of bad descending rows:',nBadW2Phot2
        if (nBadW2Phot .ne. nBadW2Phot1+nBadW2Phot2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadW2Phot1+nBadW2Phot2-nBadW2Phot
      end if
      print *,
     + 'No. data rows with bad proper motion passed through:   ',
     +         nBadPM
      if (nBadPM .gt. 0) then
        print *,'No. of bad ascending rows: ',nBadPM1
        print *,'No. of bad descending rows:',nBadPM2
        if (nBadPM .ne. nBadPM1+nBadPM2)
     +  print *,'No. of bad ascending AND descending rows:',
     +  nBadPM1+nBadPM2-nBadPM
      end if
      print *,
     + 'No. data rows with bad proper motion chi-squares:      ',
     +         nBadPMCh1+nBadPMCh2
      print *,'No. of bad ascending rows: ',nBadPMCh1
      print *,'No. of bad descending rows:',nBadPMCh2
      print *,'No. data rows with mdetID mismatch:      ',
     +         NmdetIDerr
      if (nMJD .gt. 0) then
        R8tmp1 = sumMJD/dfloat(nMJD)
        R8tmp2 = dsqrt(dabs(sumMJDsq/dfloat(nMJD) - R8tmp1**2))
        print *
        write(6,
     + '('' Motion Solution Epoch Mean & Sigma:'',2f10.2,''; N ='',I6)')
     +       R8tmp1, R8tmp2, nMJD
      end if
      if (ndwMJD .gt. 0) then
        R8tmp1 = sumdw1MJD/dfloat(ndwMJD)
        R8tmp2 = dsqrt(dabs(sum2dw1MJD/dfloat(ndwMJD) - R8tmp1**2))
        print *
        print *, 'Asce-Desc MJDmean Differences, no. pts. =',ndwMJD
        write(6,'('' W1 Difference Mean & Sigma:'',2f10.2)')
     +               R8tmp1, R8tmp2
        R8tmp1 = sumdw2MJD/dfloat(ndwMJD)
        R8tmp2 = dsqrt(dabs(sum2dw2MJD/dfloat(ndwMJD) - R8tmp1**2))
        write(6,'('' W2 Difference Mean & Sigma:'',2f10.2)')
     +               R8tmp1, R8tmp2
      end if
c                                      TJ Statistics
      if (nTJw1 .gt. 1) then
        print *
        write(6,'(a)')
     +   '------------------ TJ Statistics for W1 -----------------'
        write(6,'(''w1snr range:'',f6.2,'' to '',f6.2)') TJsnr1, TJsnr2
        call TJsort(nTJw1,TJdw1)
        call TJsort(nTJw1,dw1ChSq)
        k = nTJw1/2
        if (mod(nTJw1,2) .eq. 0) then
          dwmpro = (TJdw1(k)   +   TJdw1(k+1))/2.0
          rchisq = (dw1ChSq(k) + dw1ChSq(k+1))/2.0
        else
          dwmpro = TJdw1(k+1)
          rchisq = dw1ChSq(k+1)
        end if
        write(6,
     +  '(''median dw1mpro ='',f8.4,''; median rchisq ='',f8.4)')
     +      dwmpro, rchisq
        k1 = NInt(TrFrac*(float(nTJw1)))
        k2 = NInt((1.0 - TrFrac)*(float(nTJw1)))
        if (k1 .lt. 1) k1 = 1
        if (k1 .ge. k2) then
          k1 = nTJw1/2 - 1
          k2 = nTJw1/2 + 1
          if (k1 .lt. 1)     k1 = 1
          if (k2 .gt. nTJw1) k2 = nTJw1
        end if
        if (dbg) print *,'W1 trim range:',k1,' to ',k2,' in ',nTJw1
        do 2000 k = k1, k2
          TJw1Sum      = TJw1Sum   + TJdw1(k)
          TJw1SumSq    = TJw1SumSq + TJdw1(k)**2
2000    continue
        dwmpro = TJw1Sum/dfloat(k2-k1+1)
        R8tmp1 = dsqrt(dabs(TJw1SumSq/dfloat(k2-k1+1) - dwmpro**2))
        write(6,'(''mean dw1mpro ='',f8.4,''; sigma ='',f8.4)')
     +              dwmpro, R8tmp1
        v1 = TJw1SumSigA/dfloat(nTJw1)
        v2 = TJw1SumSigD/dfloat(nTJw1)
        write(6,'(''Mean w1sigmpro ascending:'',f8.4,'
     +  //'''; descending:'',f8.4)') v1, v2
        R8tmp2 = R8tmp1/dsqrt(v1**2 + v2**2)
        write(6,'(''TJ Statistic:'',f8.4,''; squared:'',f8.4)')
     +        R8tmp2, R8tmp2**2
        write(6,
     +  '(''No. points for TJ W1 Statistics ='',I6,''; trimmed ='',I6)')
     +       nTJw1, k2-k1+1
      else
        print *,'No. points for TJ W1 Statistics =', nTJw1
      end if
c
      if (nTJw2 .gt. 1) then
        print *
        write(6,'(a)')
     +   '------------------ TJ Statistics for W2 -----------------'
        write(6,'(''w2snr range:'',f6.2,'' to '',f6.2)') TJsnr1, TJsnr2
        call TJsort(nTJw2,TJdw2)
        call TJsort(nTJw2,dw2ChSq)
        k = nTJw2/2
        if (mod(nTJw2,2) .eq. 0) then
          dwmpro = (TJdw2(k)   +   TJdw2(k+1))/2.0
          rchisq = (dw2ChSq(k) + dw2ChSq(k+1))/2.0
        else
          dwmpro = TJdw2(k+1)
          rchisq = dw2ChSq(k+1)
        end if
        write(6,
     +  '(''median dw2mpro ='',f8.4,''; median rchisq ='',f8.4)')
     +      dwmpro, rchisq
        k1 = NInt(TrFrac*(float(nTJw2)))
        k2 = NInt((1.0 - TrFrac)*(float(nTJw2)))
        if (k1 .lt. 1) k1 = 1
        if (k1 .ge. k2) then
          k1 = nTJw2/2 - 1
          k2 = nTJw2/2 + 1
          if (k1 .lt. 1)     k1 = 1
          if (k2 .gt. nTJw2) k2 = nTJw2
        end if
        if (dbg) print *,'W2 trim range:',k1,' to ',k2,' in ',nTJw1
        do 2010 k = k1, k2
          TJw2Sum      = TJw2Sum   + TJdw2(k)
          TJw2SumSq    = TJw2SumSq + TJdw2(k)**2
2010    continue
        dwmpro = TJw2Sum/dfloat(k2-k1+1)
        R8tmp1 = dsqrt(dabs(TJw2SumSq/dfloat(k2-k1+1) - dwmpro**2))
        write(6,'(''mean dw1mpro ='',f8.4,''; sigma ='',f8.4)')
     +              dwmpro, R8tmp1
        v1 = TJw2SumSigA/dfloat(nTJw2)
        v2 = TJw2SumSigD/dfloat(nTJw2)
        write(6,'(''Mean w2sigmpro ascending:'',f8.4,'
     +  //'''; descending:'',f8.4)') v1, v2
        R8tmp2 = R8tmp1/dsqrt(v1**2 + v2**2)
        write(6,'(''TJ Statistic:'',f8.4,''; squared:'',f8.4)')
     +        R8tmp2, R8tmp2**2
        write(6,
     +  '(''No. points for TJ W2 Statistics ='',I6,''; trimmed ='',I6)')
     +       nTJw2, k2-k1+1
      else
        print *,'No. points for TJ W2 Statistics =', nTJw2
      end if        
      write(6,'(a)')
     + '---------------------------------------------------------'
c
      call TJsort(nMedDiff, MedEcLong)      
      call TJsort(nMedDiff, MedEcLat)      
      call TJsort(nMedDiff, MedRA)      
      call TJsort(nMedDiff, MedDec)
      k = nMedDiff/2
      if (mod(nMedDiff,2) .eq. 1) then
        MedDiff(1) = MedEcLong(k+1) 
        MedDiff(2) = MedEcLat(k+1) 
        MedDiff(3) = MedRA(k+1) 
        MedDiff(4) = MedDec(k+1) 
      else
        MedDiff(1) = (MedEcLong(k)+MedEcLong(k+1) )/2.0
        MedDiff(2) = (MedEcLat(k)+MedEcLat(k+1) )/2.0
        MedDiff(3) = (MedRA(k)+MedRA(k+1) )/2.0
        MedDiff(4) = (MedDec(k)+MedDec(k+1) )/2.0
      end if
      print *
      print *,'Median position offsets (descending-ascending, asec)'
      write(6,'('' Ecliptic Longitude:'',F10.5$)') MedDiff(1)+pBias
      if (pBias .ne. 0.0d0) then
        write(6,'(''; after bias correction:'',F9.5)') MedDiff(1)
      else
        print *
      end if
      write(6,'('' Ecliptic Latitude: '',F10.5)') MedDiff(2)
      write(6,'('' Right Ascension:   '',F10.5)') MedDiff(3)
      write(6,'('' Declination:       '',F10.5)') MedDiff(4)
      write(6,'('' No. of samples:    '',I10)') nMedDiff
      print *
      if (dbg) print *,'corresponding radial offsets:',
     + sqrt(MedDiff(1)**2+MedDiff(2)**2),
     + sqrt(MedDiff(3)**2+MedDiff(4)**2)
c
c-----------------------------------------------------------------------     
c                                      ! Process chi-square histograms
      do 2020 k = 1, 20
        call TJsort(Nw1rchi2asce(k), w1rchi2asce(1,k)) 
        call TJsort(Nw1rchi2desc(k), w1rchi2desc(1,k)) 
        call TJsort(Nw1rchi2mrg(k),   w1rchi2mrg(1,k)) 
        call TJsort(Nw2rchi2asce(k), w2rchi2asce(1,k)) 
        call TJsort(Nw2rchi2desc(k), w2rchi2desc(1,k)) 
        call TJsort(Nw2rchi2mrg(k),   w2rchi2mrg(1,k)) 
        call TJsort(Nrchi2asce(k),     rchi2asce(1,k)) 
        call TJsort(Nrchi2desc(k),     rchi2desc(1,k)) 
        call TJsort(Nrchi2mrg(k),       rchi2mrg(1,k)) 
        call GetMed(Nw1rchi2asce(k), w1rchi2asce(1,k), MedRchi2(1,k))
        call GetMed(Nw1rchi2desc(k), w1rchi2desc(1,k), MedRchi2(2,k)) 
        call GetMed(Nw1rchi2mrg(k),   w1rchi2mrg(1,k), MedRchi2(3,k)) 
        call GetMed(Nw2rchi2asce(k), w2rchi2asce(1,k), MedRchi2(4,k)) 
        call GetMed(Nw2rchi2desc(k), w2rchi2desc(1,k), MedRchi2(5,k)) 
        call GetMed(Nw2rchi2mrg(k),   w2rchi2mrg(1,k), MedRchi2(6,k)) 
        call GetMed(Nrchi2asce(k),     rchi2asce(1,k), MedRchi2(7,k)) 
        call GetMed(Nrchi2desc(k),     rchi2desc(1,k), MedRchi2(8,k)) 
        call GetMed(Nrchi2mrg(k),       rchi2mrg(1,k), MedRchi2(9,k)) 
2020  continue        
c
      if (index(OutFNam,SlashChar) .gt. 0) then
        k = 0
        do 2030 n = 1, lnblnk(OutFNam)
          if (OutFNam(n:n) .eq. SlashChar) k = n
2030    continue
        HistNam = OutFNam(1:k)//'hists-'//OutFNam(k+1:lnblnk(OutFNam))
        TJnam   = OutFNam(1:k)//'tjhist-'//OutFNam(k+1:lnblnk(OutFNam))
        if (dbg) print *,'HistNam = ', HistNam(1:lnblnk(HistNam))
      else
        HistNam = 'hists-'//OutFNam
        TJnam   = 'tjhist-'//OutFNam
      end if
c
      if (nAlpha1 .gt. nAlpha2) then
        ra  = sumAlpha1/dfloat(nAlpha1)
        dec = sumDec1/dfloat(nAlpha1)
      else
        ra  = sumAlpha2/dfloat(nAlpha2)
        dec = sumDec2/dfloat(nAlpha2)
      end if
      call cel2gal(sngl(ra), sngl(dec), GaLong, GaLat)
      call Cel2Ec(ra, dec, EcLong, EcLat)
c
      open (24, file = HistNam)
      write (24,'(''\ Mean Galactic Longitude and Latitude: '',2f10.4)')
     +       GaLong, GaLat
      write (6,'('' Mean Galactic Longitude and Latitude: '',2f10.4)')
     +       GaLong, GaLat
      write (24,'(''\ Mean Ecliptic Longitude and Latitude: '',2f10.4)')
     +       EcLong, EcLat
      write (6,'('' Mean Ecliptic Longitude and Latitude: '',2f10.4)')
     +       EcLong, EcLat
      write (24,'(''\ Mean Right Ascension and Declination: '',2f10.4)')
     +       ra, dec
      write (6,'('' Mean Right Ascension and Declination: '',2f10.4)')
     +       ra, dec
      write (24,'(a)') '|bin|wmpro|w1rchi2a| nw1a|w1rchi2d| nw1d|'
     + //'w1rchi2m| nw1m|w2rchi2a| nw2a|w2rchi2d| nw2d|w2rchi2m| nw2m|'
     + //' rchi2a |nasce| rchi2d |ndesc| rchi2m | nmrg|'
      write (24,'(a)') '| i | real|  real  |  i  |  real  |  i  |'
     + //'  real  |  i  |  real  |  i  |  real  |  i  |  real  |  i  |'
     + //'  real  |  i  |  real  |  i  |  real  |  i  |'
      write (24,'(a)') '| - | mag |    -   |  -  |    -   |  -  |'
     + //'    -   |  -  |    -   |  -  |    -   |  -  |    -   |  -  |'
     + //'    -   |  -  |    -   |  -  |    -   |  -  |'
      write (24,'(a)') '| n |  n  |  null  |  n  |  null  |  n  |'
     + //'  null  |  n  |  null  |  n  |  null  |  n  |  null  |  n  |'
     + //'  null  |  n  |  null  |  n  |  null  |  n  |'
c
      do 2100 k = 1, 20
        if (Nw1rchi2asce(k)+Nw1rchi2desc(k)+Nw1rchi2mrg(k)
     +     +Nw2rchi2asce(k)+Nw2rchi2desc(k)+Nw2rchi2mrg(k)
     +     +Nrchi2asce(k)  +Nrchi2desc(k)  +Nrchi2mrg(k)  .eq. 0)
     +      go to 2100
        ChiSqData = '             null       0   null       0 '
     +  //'  null       0   null       0   null       0   null       0 '
     +  //'  null       0   null       0   null       0'
        write (ChiSqData(1:10),'(i4,f6.1)') k, float(k)
        if ((Nw1rchi2asce(k) .gt. 0) .and. (MedRchi2(1,k) .lt. 999.9))
     +  write(ChiSqData(11:25), '(f9.4,i6)')
     +  MedRchi2(1,k), Nw1rchi2asce(k)
        if ((Nw1rchi2desc(k) .gt. 0) .and. (MedRchi2(2,k) .lt. 999.9))
     +  write(ChiSqData(26:40), '(f9.4,i6)')
     +  MedRchi2(2,k), Nw1rchi2desc(k)
        if ((Nw1rchi2mrg(k) .gt. 0) .and.  (MedRchi2(3,k) .lt. 999.9)) 
     +  write(ChiSqData(41:55), '(f9.4,i6)')
     +  MedRchi2(3,k), Nw1rchi2mrg(k)
        if ((Nw2rchi2asce(k) .gt. 0) .and. (MedRchi2(4,k) .lt. 999.9))
     +  write(ChiSqData(56:70), '(f9.4,i6)')
     +  MedRchi2(4,k), Nw2rchi2asce(k)
        if ((Nw2rchi2desc(k) .gt. 0) .and. (MedRchi2(5,k) .lt. 999.9))
     +  write(ChiSqData(71:85), '(f9.4,i6)')
     +  MedRchi2(5,k), Nw2rchi2desc(k)
        if ((Nw2rchi2mrg(k) .gt. 0) .and.  (MedRchi2(6,k) .lt. 999.9))
     +  write(ChiSqData(86:100),'(f9.4,i6)')
     +  MedRchi2(6,k), Nw2rchi2mrg(k)
        if ((Nrchi2asce(k) .gt. 0) .and.   (MedRchi2(7,k) .lt. 999.9))
     +  write(ChiSqData(101:115),'(f9.4,i6)')
     +  MedRchi2(7,k), Nrchi2asce(k)
        if ((Nrchi2desc(k) .gt. 0) .and.   (MedRchi2(8,k) .lt. 999.9))
     +  write(ChiSqData(116:130),'(f9.4,i6)')
     +  MedRchi2(8,k), Nrchi2desc(k)
        if ((Nrchi2mrg(k) .gt. 0) .and.    (MedRchi2(9,k) .lt. 999.9))
     +  write(ChiSqData(131:145),'(f9.4,i6)')
     +  MedRchi2(9,k), Nrchi2mrg(k)
        write(24,'(a)') ChiSqData
2100  continue
c
      if (doTJhist) then
        open (26, file = TJnam)
        write (26,'(''\ S/N range: '',f6.2,'' to '',f6.2)')
     +                  TJsnr1, TJsnr2
        write (26,
     +  '(''\ No. points for W1: '',i6,''; no. points for W2:'',i6)')
     +                  nTJw1, nTJw2
        write (26,'(a)') '|bin|dwmpro|  nW1 |  nW2 |'
        write (26,'(a)') '| i | real |  int |  int |'
        write (26,'(a)') '| - |  mag |  -   |  -   |'
        write (26,'(a)') '| n |   n  | null | null |'
        do 2200 k = 1, 51
          write(26,'(i4,f7.2,2i7)') k, float(k-26)/50.0,
     +                           TJdw1Hist(k), TJdw2Hist(k)
2200    continue
      end if
c
      call signoff('mrgad')
      stop
c
3000  print *,'ERROR: End-of-file encountered in gsa input file '
     +      //'before data found'
      go to 3030
c
3001  print *,'ERROR: End-of-of-file encountered in ascending stf '
     +      //'file before data found'
      go to 3030
c
3002  print *,'ERROR: End-of-file encountered in descending stf '
     +      //'file before data found'
      go to 3030
c
3003  print *,'ERROR: read error encountered in ascending stf '
     +      //'file line:'
      print *,Line
      go to 3030
c
3004  print *,'ERROR: read error encountered in descending stf '
     +      //'file line:'
      print *,Line
      go to 3030
c
3005  print *,'ERROR: unexpected End-of-file encountered in gsa input file '
     +      //'while reading data'
      print *,'       Attempted to read data for source no. ',nRow
      print *,'       Expected ',nSrc,' sources'
      go to 3030
c
3006  print *,'ERROR: read error encountered in gsa input file '
     +      //'on data line no. ',nRow
      print *,'       Data field no. ',k,', column name "',
     +                Field(k)(1:lnblnk(Field(k))),'"'
      print *,'        Numeric field: "',Line(IFA(k):IFB(k)),'"'
      go to 3030
c
3007  print *,'ERROR: bad specification for "-pb": ', NumStr
      call exit(64)
      stop
c
3008  print *,'ERROR: bad specification for "-cr": ', NumStr
      call exit(64)
      stop
c
3009  print *,'ERROR: bad specification for "-tf": ', NumStr
      call exit(64)
      stop
c
3010  print *,'ERROR: bad specification for "-t1": ', NumStr
      call exit(64)
      stop
c
3011  print *,'ERROR: bad specification for "-t2" :', NumStr
      call exit(64)
      stop
c
3012  print *,'ERROR: bad specification for "-tw": ', NumStr
      print *,'       must be 0, 1, or 2'
      call exit(64)
      stop
c
3030  continue
c TBD  signal error to cognizant folks
      call exit(64)
      stop      
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
      cRA   = dcos(d2r*RA)
      cDec  = dcos(d2r*Dec)
      sRA   = dsin(d2r*RA)
      sDec  = dsin(d2r*Dec)
c
      X =  sDec
      Y = -cDec*sRA
      Z =  cDec*cRA
c
      X2 =  X*Cob + Y*Sob
      Y2 = -X*Sob + Y*Cob
c     Z2 =  Z
c
      Lat  = dasin(X2)/d2r
      Long = datan2(-Y2,Z)/d2r
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
c
C=======================================================================
c                                  Sort for real*4 array
c                                  from Numerical Recipes via T. Jarrett
      SUBROUTINE TJSORT(N,RA)
c
      Integer*4 N,L,IR,J,I
      Real*4 RA(N),RRA
c
      if (n .lt. 2) return
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
c
C=======================================================================
c                                  Get median real*4 array
      subroutine GetMed(N, array, med)
c
      integer*4 N, k
      real*4    array(N), med
c
c     print *,'GetMed: N = ',N
      if (N .lt. 1) return
      k = N/2
      if (mod(N,2) .eq. 1) then
        med = array(k+1) 
      else
        med = (array(k)+array(k+1) )/2.0
      end if
c
      return
      end
c      
c=======================================================================
c
      subroutine Cel2pa(RA, Dec, pa)
c
      real*8 RA, Dec, pa, SOb, Cob, d2r, r2d
c
c   Obliquity(2015) in J2000: 23.43734105     
c
      data d2r/1.745329252d-2/, cOb, sOb/0.9174956, -0.39777459/,
     +     r2d/57.29578/ 
c
c-----------------------------------------------------------------------
c
      pa = 90.0 - r2d*atan2(-sOb*sin(d2r*Dec)*sin(d2r*RA)
     +                      +cOb*cos(d2r*Dec), sOb*cos(d2r*RA))
      if (pa .gt. 360.0) pa = pa - 360.0
c     
      return
      end
c      
c=======================================================================
c     
      subroutine r8covmat(sigX1,sigY1,sigX1Y1,ang,
     +                    sigX2,sigY2,sigX2Y2)
c
      real*8 sigX1,sigY1,sigX1Y1,ang,sigX2,sigY2,sigX2Y2,
     +       d2r, cang, sang, v11, v12, v22, t11, t12, t21, t22,
     +       u11, u12, u21, u22
      data d2r/1.745329252d-2/     
c
c-----------------------------------------------------------------------
c
      v11 = sigX1**2
      v12 = sigX1Y1*dabs(sigX1Y1)
      v22 = sigY1**2
c
      cang =  dcos(d2r*ang)
      sang =  dsin(d2r*ang)
c
      t11 =  v11*cang + v12*sang
      t12 = -v11*sang + v12*cang
      t21 =  v12*cang + v22*sang
      t22 = -v12*sang + v22*cang
c      
      u11 =  cang*t11 + sang*t21
      u12 =  cang*t12 + sang*t22
      u21 = -sang*t11 + cang*t21
      u22 = -sang*t12 + cang*t22
c
      sigX2   = dsqrt(dabs(u11))
      sigY2   = dsqrt(dabs(u22))
      sigX2Y2 = dsqrt(dabs(u12))
      if (t12 .lt. 0.0d0) sigX2Y2 = -sigX2Y2
c
      return
      end
c
c-----------------------------------------------------------------------
c
      subroutine Cel2Gal(RA, Dec, Long, Lat)
c
      real*4 RA, Dec, Long, Lat, RA0, cDec0, sDec0, Long0, d2r,
     +       cRA, cDec, sRA, sDec
c
      data d2r/1.745329252e-2/, RA0/192.8595/, sDec0/0.4559861/,
     +     cDec0/0.889987/, Long0/122.9320/
c
c-----------------------------------------------------------------------
c
      cRA   = cos(d2r*(RA-RA0))
      cDec  = cos(d2r*Dec)
      sRA   = sin(d2r*(RA-RA0))
      sDec  = sin(d2r*Dec)
c      
      Lat  = asin(sDec0*sDec + cDec0*cDec*cRA)/d2r
      Long = Long0 - atan2(cDec*sRA,cDec0*sDec - sDec0*cDec*cRA)/d2r
      
      if (Long .lt. 0.0) Long = Long + 360.0
c
      return
      end
