;;;; cwms-mappings.lisp - DSL file for CWMS/PlaceNames

;;; GNIS feature classes

(provenance (name GNIS))
(concept GNIS::Feature
  (inherit ONT::geographic-region)
  (concept GNIS::Airport	(inherit ONT::airport))
  (concept GNIS::Arch		(inherit ONT::geo-formation))
  (concept GNIS::Area)
  (concept GNIS::Arroyo		(inherit ONT::body-of-water))
  (concept GNIS::Bar		(inherit ONT::geo-formation)) ; NOTE: not ONT::drinking-establishment
  (concept GNIS::Basin		(inherit ONT::valley))
  (concept GNIS::Bay		(inherit ONT::shore))
  (concept GNIS::Beach		(inherit ONT::shore))
  (concept GNIS::Bench		(inherit ONT::geo-formation))
  (concept GNIS::Bend		(inherit ONT::geo-formation)) ; ?
  (concept GNIS::Bridge		(inherit ONT::bridge))
  (concept GNIS::Building	(inherit ONT::building))
  (concept GNIS::Canal		(inherit ONT::route)) ; maybe ONT::body-of-water?
  (concept GNIS::Cape		(inherit ONT::shore))
  (concept GNIS::Cemetery	(inherit ONT::region-for-activity))
  (concept GNIS::Census)
  (concept GNIS::Channel	(inherit ONT::body-of-water)) ; maybe ONT::route?
  (concept GNIS::Church		(inherit ONT::religious-facility))
  (concept GNIS::Civil		(inherit ONT::political-region))
  (concept GNIS::Cliff		(inherit ONT::geo-formation))
  (concept GNIS::Crater		(inherit ONT::geo-formation))
  (concept GNIS::Crossing	(inherit ONT::route))
  (concept GNIS::Dam		(inherit ONT::facility))
  (concept GNIS::Falls		(inherit ONT::body-of-water))
  (concept GNIS::Flat		(inherit ONT::geo-formation))
  (concept GNIS::Forest		(inherit ONT::geo-formation)) ; ?
  (concept GNIS::Gap		(inherit ONT::route))
  (concept GNIS::Glacier	(inherit ONT::geo-formation))
  (concept GNIS::Gut		(inherit ONT::body-of-water)) ; NOTE: not ONT::body-part
  (concept GNIS::Harbor		(inherit ONT::shore)) ; maybe ONT::transportation-facility? ONT::production-facility? ONT::workplace? ONT::body-of-water?
  (concept GNIS::Hospital	(inherit ONT::health-care-facility))
  (concept GNIS::Island		(inherit ONT::geo-formation))
  (concept GNIS::Isthmus	(inherit ONT::geo-formation))
  (concept GNIS::Lake		(inherit ONT::body-of-water))
  (concept GNIS::Lava		(inherit ONT::geo-formation))
  (concept GNIS::Levee		(inherit ONT::man-made-structure))
  (concept GNIS::Locale		(inherit ONT::location))
  (concept GNIS::Military	(inherit ONT::facility))
  (concept GNIS::Mine		(inherit ONT::mine))
  (concept GNIS::Oilfield	(inherit ONT::production-facility))
  (concept GNIS::Park		(inherit ONT::region-for-activity))
  (concept GNIS::Pillar		(inherit ONT::geo-formation))
  (concept GNIS::Plain		(inherit ONT::geo-formation))
  (concept GNIS::Populated_Place (inherit ONT::city))
  (concept GNIS::Post_Office	(inherit ONT::public-service-facility))
  (concept GNIS::Range		(inherit ONT::mountain))
  (concept GNIS::Rapids		(inherit ONT::body-of-water))
  (concept GNIS::Reserve	(inherit ONT::region-for-activity))
  (concept GNIS::Reservoir	(inherit ONT::body-of-water))
  (concept GNIS::Ridge		(inherit ONT::mountain))
  (concept GNIS::School		(inherit ONT::education-facility))
  (concept GNIS::Sea		(inherit ONT::body-of-water))
  (concept GNIS::Slope		(inherit ONT::geo-formation))
  (concept GNIS::Spring		(inherit ONT::body-of-water)) ; maybe ONT::geo-formation? ONT::startpoint?
  (concept GNIS::Stream		(inherit ONT::body-of-water))
  (concept GNIS::Summit		(inherit ONT::mountain))
  (concept GNIS::Swamp		(inherit ONT::geo-formation)) ; is it ONT::land or ONT::body-of-water?
  (concept GNIS::Tower		(inherit ONT::tower))
  (concept GNIS::Trail		(inherit ONT::route))
  (concept GNIS::Tunnel		(inherit ONT::route))
  (concept GNIS::Unknown)
  (concept GNIS::Valley		(inherit ONT::valley))
  (concept GNIS::Well		(inherit ONT::man-made-structure)) ; maybe ONT::body-of-water?
  (concept GNIS::Woods) ; ?
  )

;;;
;;; GeoNames.org feature classes and codes, and mappings from them to the TRIPS
;;; ontology
;;;
(provenance
  (name GNO)
  (filename "https://www.geonames.org/export/codes.html")
  )
(concept GNO::A
  (comment "country, state, region,...")
  (inherit ONT::political-region)
  (concept GNO::ADM1	(inherit ONT::state))
  (concept GNO::ADM1H	(inherit ONT::state))
  (concept GNO::ADM2	(inherit ONT::county))
  (concept GNO::ADM2H	(inherit ONT::county))
  (concept GNO::ADM3	(inherit ONT::district))
  (concept GNO::ADM3H	(inherit ONT::district))
  (concept GNO::ADM4)
  (concept GNO::ADM4H)
  (concept GNO::ADM5)
  (concept GNO::ADM5H)
  (concept GNO::ADMD)
  (concept GNO::ADMDH)
  (concept GNO::LTER)
  (concept GNO::PCL) ;; FIXME some of these might also be ONT::country?
  (concept GNO::PCLD)
  (concept GNO::PCLF)
  (concept GNO::PCLH)
  (concept GNO::PCLI	(inherit ONT::country))
  (concept GNO::PCLIX)
  (concept GNO::PCLS)
  (concept GNO::PRSH)
  (concept GNO::TERR)
  (concept GNO::ZN)
  (concept GNO::ZNB)
  )
(concept GNO::H
  (comment "stream, lake, ...")
  (inherit ONT::body-of-water)
  (concept GNO::AIRS)
  (concept GNO::ANCH)
  (concept GNO::BAY)
  (concept GNO::BAYS)
  (concept GNO::BGHT)
  (concept GNO::BNK)
  (concept GNO::BNKR)
  (concept GNO::BNKX)
  (concept GNO::BOG)
  (concept GNO::CAPG)
  (concept GNO::CHN)
  (concept GNO::CHNL)
  (concept GNO::CHNM)
  (concept GNO::CHNN)
  (concept GNO::CNFL)
  (concept GNO::CNL)
  (concept GNO::CNLA)
  (concept GNO::CNLB)
  (concept GNO::CNLD)
  (concept GNO::CNLI)
  (concept GNO::CNLN)
  (concept GNO::CNLQ)
  (concept GNO::CNLSB)
  (concept GNO::CNLX)
  (concept GNO::COVE)
  (concept GNO::CRKT)
  (concept GNO::CRNT)
  (concept GNO::CUTF)
  (concept GNO::DCK)
  (concept GNO::DCKB)
  (concept GNO::DOMG)
  (concept GNO::DPRG)
  (concept GNO::DTCH)
  (concept GNO::DTCHD)
  (concept GNO::DTCHI)
  (concept GNO::DTCHM)
  (concept GNO::ESTY)
  (concept GNO::FISH)
  (concept GNO::FJD)
  (concept GNO::FJDS)
  (concept GNO::FLLS)
  (concept GNO::FLLSX)
  (concept GNO::FLTM)
  (concept GNO::FLTT)
  (concept GNO::GLCR)
  (concept GNO::GULF)
  (concept GNO::GYSR)
  (concept GNO::HBR)
  (concept GNO::HBRX)
  (concept GNO::INLT)
  (concept GNO::INLTQ)
  (concept GNO::LBED)
  (concept GNO::LGN)
  (concept GNO::LGNS)
  (concept GNO::LGNX)
  (concept GNO::LK)
  (concept GNO::LKC)
  (concept GNO::LKI)
  (concept GNO::LKN)
  (concept GNO::LKNI)
  (concept GNO::LKO)
  (concept GNO::LKOI)
  (concept GNO::LKS)
  (concept GNO::LKSB)
  (concept GNO::LKSC)
  (concept GNO::LKSI)
  (concept GNO::LKSN)
  (concept GNO::LKSNI)
  (concept GNO::LKX)
  (concept GNO::MFGN)
  (concept GNO::MGV)
  (concept GNO::MOOR)
  (concept GNO::MRSH)
  (concept GNO::MRSHN)
  (concept GNO::NRWS)
  (concept GNO::OCN)
  (concept GNO::OVF)
  (concept GNO::PND)
  (concept GNO::PNDI)
  (concept GNO::PNDN)
  (concept GNO::PNDNI)
  (concept GNO::PNDS)
  (concept GNO::PNDSF)
  (concept GNO::PNDSI)
  (concept GNO::PNDSN)
  (concept GNO::POOL)
  (concept GNO::POOLI)
  (concept GNO::RCH)
  (concept GNO::RDGG)
  (concept GNO::RDST)
  (concept GNO::RF)
  (concept GNO::RFC)
  (concept GNO::RFX)
  (concept GNO::RPDS)
  (concept GNO::RSV)
  (concept GNO::RSVI)
  (concept GNO::RSVT)
  (concept GNO::RVN)
  (concept GNO::SBKH)
  (concept GNO::SD)
  (concept GNO::SEA)
  (concept GNO::SHOL)
  (concept GNO::SILL)
  (concept GNO::SPNG)
  (concept GNO::SPNS)
  (concept GNO::SPNT)
  (concept GNO::STM)
  (concept GNO::STMA)
  (concept GNO::STMB)
  (concept GNO::STMC)
  (concept GNO::STMD)
  (concept GNO::STMH)
  (concept GNO::STMI)
  (concept GNO::STMIX)
  (concept GNO::STMM)
  (concept GNO::STMQ)
  (concept GNO::STMS)
  (concept GNO::STMSB)
  (concept GNO::STMX)
  (concept GNO::STRT)
  (concept GNO::SWMP)
  (concept GNO::SYSI)
  (concept GNO::TNLC)
  (concept GNO::WAD)
  (concept GNO::WADB)
  (concept GNO::WADJ)
  (concept GNO::WADM)
  (concept GNO::WADS)
  (concept GNO::WADX)
  (concept GNO::WHRL)
  (concept GNO::WLL)
  (concept GNO::WLLQ)
  (concept GNO::WLLS)
  (concept GNO::WTLD)
  (concept GNO::WTLDI)
  (concept GNO::WTRC)
  (concept GNO::WTRH)
  )
(concept GNO::L
  (comment "parks,area, ...")
  (inherit ONT::geo-object)
  (concept GNO::AGRC)
  (concept GNO::AMUS)
  (concept GNO::AREA)
  (concept GNO::BSND)
  (concept GNO::BSNP)
  (concept GNO::BTL)
  (concept GNO::CLG)
  (concept GNO::CMN)
  (concept GNO::CNS)
  (concept GNO::COLF)
  (concept GNO::CONT)
  (concept GNO::CST)
  (concept GNO::CTRB)
  (concept GNO::DEVH)
  (concept GNO::FLD)
  (concept GNO::FLDI)
  (concept GNO::GASF)
  (concept GNO::GRAZ)
  (concept GNO::GVL)
  (concept GNO::INDS)
  (concept GNO::LAND)
  (concept GNO::LCTY	(inherit ONT::location))
  (concept GNO::MILB)
  (concept GNO::MNA)
  (concept GNO::MVA)
  (concept GNO::NVB)
  (concept GNO::OAS)
  (concept GNO::OILF)
  (concept GNO::PEAT)
  (concept GNO::PRK)
  (concept GNO::PRT)
  (concept GNO::QCKS)
  (concept GNO::RES	(inherit ONT::district))
  (concept GNO::RESA	(inherit ONT::district))
  (concept GNO::RESF	(inherit ONT::district))
  (concept GNO::RESH	(inherit ONT::district))
  (concept GNO::RESN	(inherit ONT::district))
  (concept GNO::RESP	(inherit ONT::district))
  (concept GNO::RESV	(inherit ONT::district))
  (concept GNO::RESW	(inherit ONT::district))
  (concept GNO::RGN	(inherit ONT::geographic-region))
  (concept GNO::RGNE)
  (concept GNO::RGNH)
  (concept GNO::RGNL)
  (concept GNO::RNGA)
  (concept GNO::SALT)
  (concept GNO::SNOW)
  (concept GNO::TRB	(inherit ONT::political-region))
  )
(concept GNO::P
  (comment "city, village,...")
  (inherit ONT::city)
  (concept GNO::PPL)
  (concept GNO::PPLA)
  (concept GNO::PPLA2)
  (concept GNO::PPLA3)
  (concept GNO::PPLA4)
  (concept GNO::PPLA5)
  (concept GNO::PPLC)
  (concept GNO::PPLCH)
  (concept GNO::PPLF	(inherit ONT::district))
  (concept GNO::PPLG)
  (concept GNO::PPLH)
  (concept GNO::PPLL	(inherit ONT::district))
  (concept GNO::PPLQ	(inherit ONT::district))
  (concept GNO::PPLR	(inherit ONT::district))
  (concept GNO::PPLS)
  (concept GNO::PPLW	(inherit ONT::district))
  (concept GNO::PPLX	(inherit ONT::district))
  (concept GNO::STLMT	(inherit ONT::district))
  )
(concept GNO::R
  (comment "road, railroad")
  (inherit ONT::route)
  (concept GNO::CSWY)
  (concept GNO::OILP)
  (concept GNO::PRMN)
  (concept GNO::PTGE)
  (concept GNO::RD)
  (concept GNO::RDA)
  (concept GNO::RDB)
  (concept GNO::RDCUT)
  (concept GNO::RDJCT)
  (concept GNO::RJCT)
  (concept GNO::RR)
  (concept GNO::RRQ)
  (concept GNO::RTE)
  (concept GNO::RYD)
  (concept GNO::ST)
  (concept GNO::STKR)
  (concept GNO::TNL)
  (concept GNO::TNLN)
  (concept GNO::TNLRD)
  (concept GNO::TNLRR)
  (concept GNO::TNLS)
  (concept GNO::TRL)
  )
(concept GNO::S
  (comment "spot, building, farm")
  (inherit ONT::man-made-structure)
  (concept GNO::ADMF	(inherit ONT::public-service-facility))
  (concept GNO::AGRF	(inherit ONT::production-facility))
  (concept GNO::AIRB	(inherit ONT::airport))
  (concept GNO::AIRF	(inherit ONT::airport))
  (concept GNO::AIRH	(inherit ONT::airport))
  (concept GNO::AIRP	(inherit ONT::airport))
  (concept GNO::AIRQ	(inherit ONT::airport))
  (concept GNO::AIRT)
  (concept GNO::AMTH	(inherit ONT::entertainment-establishment))
  (concept GNO::ANS)
  (concept GNO::AQC	(inherit ONT::production-facility))
  (concept GNO::ARCH)
  (concept GNO::ARCHV)
  (concept GNO::ART)
  (concept GNO::ASTR)
  (concept GNO::ASYL	(inherit ONT::health-care-facility))
  (concept GNO::ATHF	(inherit ONT::athletic-facility))
  (concept GNO::ATM)
  (concept GNO::BANK)
  (concept GNO::BCN)
  (concept GNO::BDG	(inherit ONT::bridge))
  (concept GNO::BDGQ)
  (concept GNO::BLDA)
  (concept GNO::BLDG	(inherit ONT::building))
  (concept GNO::BLDO)
  (concept GNO::BP)
  (concept GNO::BRKS)
  (concept GNO::BRKW)
  (concept GNO::BSTN)
  (concept GNO::BTYD)
  (concept GNO::BUR)
  (concept GNO::BUSTN)
  (concept GNO::BUSTP)
  (concept GNO::CARN)
  (concept GNO::CAVE	(inherit ONT::geo-formation))
  (concept GNO::CH	(inherit ONT::religious-facility))
  (concept GNO::CMP	(inherit ONT::lodging))
  (concept GNO::CMPL)
  (concept GNO::CMPLA)
  (concept GNO::CMPMN)
  (concept GNO::CMPO)
  (concept GNO::CMPQ)
  (concept GNO::CMPRF)
  (concept GNO::CMTY)
  (concept GNO::COMC	(inherit ONT::facility))
  (concept GNO::CRRL	(inherit ONT::storage-facility))
  (concept GNO::CSNO)
  (concept GNO::CSTL)
  (concept GNO::CSTM	(inherit ONT::transportation-facility))
  (concept GNO::CTHSE	(inherit ONT::public-service-facility))
  (concept GNO::CTRA)
  (concept GNO::CTRCM)
  (concept GNO::CTRF)
  (concept GNO::CTRM	(inherit ONT::health-care-facility))
  (concept GNO::CTRR	(inherit ONT::religious-facility))
  (concept GNO::CTRS)
  (concept GNO::CVNT)
  (concept GNO::DAM)
  (concept GNO::DAMQ)
  (concept GNO::DAMSB)
  (concept GNO::DARY)
  (concept GNO::DCKD)
  (concept GNO::DCKY)
  (concept GNO::DIKE)
  (concept GNO::DIP)
  (concept GNO::DPOF)
  (concept GNO::EST	(inherit ONT::farm))
  (concept GNO::ESTO)
  (concept GNO::ESTR)
  (concept GNO::ESTSG)
  (concept GNO::ESTT)
  (concept GNO::ESTX)
  (concept GNO::FCL)
  (concept GNO::FNDY)
  (concept GNO::FRM	(inherit ONT::farm))
  (concept GNO::FRMQ	(inherit ONT::farm))
  (concept GNO::FRMS	(inherit ONT::farm))
  (concept GNO::FRMT	(inherit ONT::farm))
  (concept GNO::FT)
  (concept GNO::FY)
  (concept GNO::FYT)
  (concept GNO::GATE)
  (concept GNO::GDN)
  (concept GNO::GHAT)
  (concept GNO::GHSE)
  (concept GNO::GOSP)
  (concept GNO::GOVL)
  (concept GNO::GRVE)
  (concept GNO::HERM)
  (concept GNO::HLT)
  (concept GNO::HMSD)
  (concept GNO::HSE)
  (concept GNO::HSEC)
  (concept GNO::HSP	(inherit ONT::health-care-facility))
  (concept GNO::HSPC	(inherit ONT::health-care-facility))
  (concept GNO::HSPD)
  (concept GNO::HSPL	(inherit ONT::health-care-facility))
  (concept GNO::HSTS)
  (concept GNO::HTL	(inherit ONT::accommodation))
  (concept GNO::HUT)
  (concept GNO::HUTS)
  (concept GNO::INSM)
  (concept GNO::ITTR	(inherit ONT::research-facililty))
  (concept GNO::JTY)
  (concept GNO::LDNG	(inherit ONT::transportation-facility))
  (concept GNO::LEPC)
  (concept GNO::LIBR)
  (concept GNO::LNDF)
  (concept GNO::LOCK)
  (concept GNO::LTHSE)
  (concept GNO::MALL)
  (concept GNO::MAR)
  (concept GNO::MFG	(inherit ONT::production-facility))
  (concept GNO::MFGB)
  (concept GNO::MFGC)
  (concept GNO::MFGCU)
  (concept GNO::MFGLM)
  (concept GNO::MFGM)
  (concept GNO::MFGPH)
  (concept GNO::MFGQ)
  (concept GNO::MFGSG)
  (concept GNO::MKT	(inherit ONT::commercial-facility))
  (concept GNO::ML)
  (concept GNO::MLM)
  (concept GNO::MLO)
  (concept GNO::MLSG)
  (concept GNO::MLSGQ)
  (concept GNO::MLSW	(inherit ONT::production-facility))
  (concept GNO::MLWND)
  (concept GNO::MLWTR)
  (concept GNO::MN	(inherit ONT::production-facility))
  (concept GNO::MNAU)
  (concept GNO::MNC)
  (concept GNO::MNCR)
  (concept GNO::MNCU)
  (concept GNO::MNFE)
  (concept GNO::MNMT)
  (concept GNO::MNN)
  (concept GNO::MNQ)
  (concept GNO::MNQR)
  (concept GNO::MOLE)
  (concept GNO::MSQE	(inherit ONT::religious-facility))
  (concept GNO::MSSN	(inherit ONT::religious-facility))
  (concept GNO::MSSNQ)
  (concept GNO::MSTY	(inherit ONT::religious-facility))
  (concept GNO::MTRO)
  (concept GNO::MUS)
  (concept GNO::NOV)
  (concept GNO::NSY)
  (concept GNO::OBPT)
  (concept GNO::OBS)
  (concept GNO::OBSR)
  (concept GNO::OILJ)
  (concept GNO::OILQ)
  (concept GNO::OILR)
  (concept GNO::OILT)
  (concept GNO::OILW)
  (concept GNO::OPRA)
  (concept GNO::PAL	(inherit ONT::lodging))
  (concept GNO::PGDA)
  (concept GNO::PIER)
  (concept GNO::PKLT)
  (concept GNO::PMPO)
  (concept GNO::PMPW)
  (concept GNO::PO	(inherit ONT::public-service-facility))
  (concept GNO::PP	(inherit ONT::public-service-facility))
  (concept GNO::PPQ)
  (concept GNO::PRKGT)
  (concept GNO::PRKHQ)
  (concept GNO::PRN)
  (concept GNO::PRNJ)
  (concept GNO::PRNQ)
  (concept GNO::PS	(inherit ONT::production-facility))
  (concept GNO::PSH)
  (concept GNO::PSN)
  (concept GNO::PSTB	(inherit ONT::transportation-facility))
  (concept GNO::PSTC)
  (concept GNO::PSTP)
  (concept GNO::PYR)
  (concept GNO::PYRS)
  (concept GNO::QUAY)
  (concept GNO::RDCR)
  (concept GNO::RDIN)
  (concept GNO::RECG)
  (concept GNO::RECR)
  (concept GNO::REST)
  (concept GNO::RET)
  (concept GNO::RHSE	(inherit ONT::transportation-facility))
  (concept GNO::RKRY)
  (concept GNO::RLG)
  (concept GNO::RLGR)
  (concept GNO::RNCH)
  (concept GNO::RSD)
  (concept GNO::RSGNL)
  (concept GNO::RSRT)
  (concept GNO::RSTN	(inherit ONT::transportation-facility))
  (concept GNO::RSTNQ)
  (concept GNO::RSTP	(inherit ONT::transportation-facility))
  (concept GNO::RSTPQ)
  (concept GNO::RUIN)
  (concept GNO::SCH	(inherit ONT::education-facility))
  (concept GNO::SCHA)
  (concept GNO::SCHC	(inherit ONT::education-facility))
  (concept GNO::SCHL)
  (concept GNO::SCHM)
  (concept GNO::SCHN)
  (concept GNO::SCHT)
  (concept GNO::SECP)
  (concept GNO::SHPF)
  (concept GNO::SHRN)
  (concept GNO::SHSE)
  (concept GNO::SLCE)
  (concept GNO::SNTR)
  (concept GNO::SPA)
  (concept GNO::SPLY)
  (concept GNO::SQR)
  (concept GNO::STBL)
  (concept GNO::STDM	(inherit ONT::athletic-facility))
  (concept GNO::STNB)
  (concept GNO::STNC)
  (concept GNO::STNE)
  (concept GNO::STNF)
  (concept GNO::STNI)
  (concept GNO::STNM)
  (concept GNO::STNR)
  (concept GNO::STNS)
  (concept GNO::STNW)
  (concept GNO::STPS)
  (concept GNO::SWT)
  (concept GNO::SYG)
  (concept GNO::THTR)
  (concept GNO::TMB)
  (concept GNO::TMPL)
  (concept GNO::TNKD)
  (concept GNO::TOLL)
  (concept GNO::TOWR)
  (concept GNO::TRAM)
  (concept GNO::TRANT)
  (concept GNO::TRIG)
  (concept GNO::TRMO)
  (concept GNO::TWO)
  (concept GNO::UNIP)
  (concept GNO::UNIV)
  (concept GNO::USGE)
  (concept GNO::VETF)
  (concept GNO::WALL)
  (concept GNO::WALLA)
  (concept GNO::WEIR)
  (concept GNO::WHRF)
  (concept GNO::WRCK)
  (concept GNO::WTRW)
  (concept GNO::ZNF)
  (concept GNO::ZOO)
  )
(concept GNO::T
  (comment "mountain,hill,rock,...")
  (inherit ONT::geo-formation)
  (concept GNO::ASPH)
  (concept GNO::ATOL)
  (concept GNO::BAR)
  (concept GNO::BCH	(inherit ONT::shore))
  (concept GNO::BCHS	(inherit ONT::shore))
  (concept GNO::BDLD)
  (concept GNO::BLDR)
  (concept GNO::BLHL)
  (concept GNO::BLOW)
  (concept GNO::BNCH)
  (concept GNO::BUTE	(inherit ONT::mountain))
  (concept GNO::CAPE	(inherit ONT::shore))
  (concept GNO::CFT	(inherit ONT::shore))
  (concept GNO::CLDA)
  (concept GNO::CLF)
  (concept GNO::CNYN	(inherit ONT::valley))
  (concept GNO::CONE	(inherit ONT::mountain))
  (concept GNO::CRDR	(inherit ONT::route))
  (concept GNO::CRQ	(inherit ONT::valley))
  (concept GNO::CRQS	(inherit ONT::valley))
  (concept GNO::CRTR)
  (concept GNO::CUET	(inherit ONT::mountain))
  (concept GNO::DLTA)
  (concept GNO::DPR)
  (concept GNO::DSRT)
  (concept GNO::DUNE)
  (concept GNO::DVD)
  (concept GNO::ERG)
  (concept GNO::FAN)
  (concept GNO::FORD	(inherit ONT::route))
  (concept GNO::FSR	(inherit ONT::valley))
  (concept GNO::GAP)
  (concept GNO::GRGE	(inherit ONT::valley))
  (concept GNO::HDLD	(inherit ONT::shore))
  (concept GNO::HLL	(inherit ONT::mountain))
  (concept GNO::HLLS	(inherit ONT::mountain))
  (concept GNO::HMCK)
  (concept GNO::HMDA)
  (concept GNO::INTF)
  (concept GNO::ISL)
  (concept GNO::ISLET)
  (concept GNO::ISLF)
  (concept GNO::ISLM)
  (concept GNO::ISLS)
  (concept GNO::ISLT)
  (concept GNO::ISLX)
  (concept GNO::ISTH)
  (concept GNO::KRST)
  (concept GNO::LAVA)
  (concept GNO::LEV)
  (concept GNO::MESA	(inherit ONT::mountain))
  (concept GNO::MND	(inherit ONT::mountain))
  (concept GNO::MRN	(inherit ONT::mountain))
  (concept GNO::MT 	(inherit ONT::mountain))
  (concept GNO::MTS	(inherit ONT::mountain))
  (concept GNO::NKM)
  (concept GNO::NTK	(inherit ONT::mountain))
  (concept GNO::NTKS	(inherit ONT::mountain))
  (concept GNO::PAN)
  (concept GNO::PANS)
  (concept GNO::PASS	(inherit ONT::route))
  (concept GNO::PEN	(inherit ONT::shore))
  (concept GNO::PENX	(inherit ONT::shore))
  (concept GNO::PK	(inherit ONT::mountain))
  (concept GNO::PKS	(inherit ONT::mountain))
  (concept GNO::PLAT)
  (concept GNO::PLATX)
  (concept GNO::PLDR)
  (concept GNO::PLN)
  (concept GNO::PLNX)
  (concept GNO::PROM	(inherit ONT::mountain))
  (concept GNO::PT	(inherit ONT::shore))
  (concept GNO::PTS	(inherit ONT::shore))
  (concept GNO::RDGB)
  (concept GNO::RDGE)
  (concept GNO::REG)
  (concept GNO::RK)
  (concept GNO::RKFL)
  (concept GNO::RKS)
  (concept GNO::SAND)
  (concept GNO::SBED)
  (concept GNO::SCRP)
  (concept GNO::SDL)
  (concept GNO::SHOR)
  (concept GNO::SINK)
  (concept GNO::SLID)
  (concept GNO::SLP)
  (concept GNO::SPIT)
  (concept GNO::SPUR	(inherit ONT::mountain))
  (concept GNO::TAL)
  (concept GNO::TRGD)
  (concept GNO::TRR)
  (concept GNO::UPLD)
  (concept GNO::VAL	(inherit ONT::valley))
  (concept GNO::VALG	(inherit ONT::valley))
  (concept GNO::VALS	(inherit ONT::valley))
  (concept GNO::VALX	(inherit ONT::valley))
  (concept GNO::VLC	(inherit ONT::mountain))
  )
(concept GNO::U
  (comment "undersea")
  (inherit ONT::geo-formation)
  (concept GNO::APNU)
  (concept GNO::ARCU)
  (concept GNO::ARRU)
  (concept GNO::BDLU)
  (concept GNO::BKSU)
  (concept GNO::BNKU)
  (concept GNO::BSNU)
  (concept GNO::CDAU)
  (concept GNO::CNSU)
  (concept GNO::CNYU)
  (concept GNO::CRSU)
  (concept GNO::DEPU)
  (concept GNO::EDGU)
  (concept GNO::ESCU)
  (concept GNO::FANU)
  (concept GNO::FLTU)
  (concept GNO::FRZU)
  (concept GNO::FURU)
  (concept GNO::GAPU)
  (concept GNO::GLYU)
  (concept GNO::HLLU)
  (concept GNO::HLSU)
  (concept GNO::HOLU)
  (concept GNO::KNLU)
  (concept GNO::KNSU)
  (concept GNO::LDGU)
  (concept GNO::LEVU)
  (concept GNO::MESU)
  (concept GNO::MNDU)
  (concept GNO::MOTU)
  (concept GNO::MTU)
  (concept GNO::PKSU)
  (concept GNO::PKU)
  (concept GNO::PLNU)
  (concept GNO::PLTU)
  (concept GNO::PNLU)
  (concept GNO::PRVU)
  (concept GNO::RDGU)
  (concept GNO::RDSU)
  (concept GNO::RFSU)
  (concept GNO::RFU)
  (concept GNO::RISU)
  (concept GNO::SCNU)
  (concept GNO::SCSU)
  (concept GNO::SDLU)
  (concept GNO::SHFU)
  (concept GNO::SHLU)
  (concept GNO::SHSU)
  (concept GNO::SHVU)
  (concept GNO::SILU)
  (concept GNO::SLPU)
  (concept GNO::SMSU)
  (concept GNO::SMU)
  (concept GNO::SPRU)
  (concept GNO::TERU)
  (concept GNO::TMSU)
  (concept GNO::TMTU)
  (concept GNO::TNGU)
  (concept GNO::TRGU)
  (concept GNO::TRNU)
  (concept GNO::VALU)
  (concept GNO::VLSU)
  )
(concept GNO::V
  (comment "forest,heath,...")
  (inherit ONT::geo-formation)
  (concept GNO::BUSH)
  (concept GNO::CULT)
  (concept GNO::FRST)
  (concept GNO::FRSTF)
  (concept GNO::GROVE)
  (concept GNO::GRSLD)
  (concept GNO::GRVC)
  (concept GNO::GRVO)
  (concept GNO::GRVP)
  (concept GNO::GRVPN)
  (concept GNO::HTH)
  (concept GNO::MDW)
  (concept GNO::OCH)
  (concept GNO::SCRB)
  (concept GNO::TREE)
  (concept GNO::TUND)
  (concept GNO::VIN)
  (concept GNO::VINS)
  )
(concept GNO::ll
  (comment "not available")
  (inherit ONT::geo-object)
  )

