module ExtensionFieldBenchmarks where

import Protolude

import Criterion.Main
import ExtensionField

import GaloisFieldBenchmarks
import PrimeFieldBenchmarks

data Pu
instance IrreducibleMonic Fq Pu where
  split _ = X2 + 1
type Fq2 = ExtensionField Fq Pu

data Pv
instance IrreducibleMonic Fq2 Pv where
  split _ = X3 - 9 - Y X
type Fq6 = ExtensionField Fq2 Pv

data Pw
instance IrreducibleMonic Fq6 Pw where
  split _ = X2 - Y X
type Fq12 = ExtensionField Fq6 Pw

fq12 :: Fq12
fq12 = toField
  [ toField
    [ toField
      [ 4025484419428246835913352650763180341703148406593523188761836807196412398582
      , 5087667423921547416057913184603782240965080921431854177822601074227980319916
      ]
    , toField
      [ 8868355606921194740459469119392835913522089996670570126495590065213716724895
      , 12102922015173003259571598121107256676524158824223867520503152166796819430680
      ]
    , toField
      [ 92336131326695228787620679552727214674825150151172467042221065081506740785
      , 5482141053831906120660063289735740072497978400199436576451083698548025220729
      ]
    ]
  , toField
    [ toField
      [ 7642691434343136168639899684817459509291669149586986497725240920715691142493
      , 1211355239100959901694672926661748059183573115580181831221700974591509515378
      ]
    , toField
      [ 20725578899076721876257429467489710434807801418821512117896292558010284413176
      , 17642016461759614884877567642064231230128683506116557502360384546280794322728
      ]
    , toField
      [ 17449282511578147452934743657918270744212677919657988500433959352763226500950
      , 1205855382909824928004884982625565310515751070464736233368671939944606335817
      ]
    ]
  ]

fq12' :: Fq12
fq12' = toField
  [ toField
    [ toField
      [ 495492586688946756331205475947141303903957329539236899715542920513774223311
      , 9283314577619389303419433707421707208215462819919253486023883680690371740600
      ]
    , toField
      [ 11142072730721162663710262820927009044232748085260948776285443777221023820448
      , 1275691922864139043351956162286567343365697673070760209966772441869205291758
      ]
    , toField
      [ 20007029371545157738471875537558122753684185825574273033359718514421878893242
      , 9839139739201376418106411333971304469387172772449235880774992683057627654905
      ]
    ]
  , toField
    [ toField
      [ 9503058454919356208294350412959497499007919434690988218543143506584310390240
      , 19236630380322614936323642336645412102299542253751028194541390082750834966816
      ]
    , toField
      [ 18019769232924676175188431592335242333439728011993142930089933693043738917983
      , 11549213142100201239212924317641009159759841794532519457441596987622070613872
      ]
    , toField
      [ 9656683724785441232932664175488314398614795173462019188529258009817332577664
      , 20666848762667934776817320505559846916719041700736383328805334359135638079015
      ]
    ]
  ]

benchmarkExtensionField :: Benchmark
benchmarkExtensionField = benchmark "ExtensionField Fq12" fq12 fq12'
