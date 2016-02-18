unit CodePageConvertor;

interface

uses
  SysUtils, Classes;

type
  TCodePage=(cpALT,cpISO,cpKOI,cpMAC,cpWIN);
  TCodePageConvertor = class(TComponent)
  private
    { Private declarations }
    FInputCodePage : TCodePage;
    FOutputCodePage: TCodePage;
    FInputString   : string;
    FOutputString  : string;

    procedure FSetInputCodePage(InputCodePage: TCodePage);
    procedure FSetOutputCodePage(OutputCodePage: TCodePage);
    procedure FSetInputString(InputString: string);
    procedure FSetOutputString(OutputString: string);
    function ConvertString(InputString: string; InputCodePage, OutputCodepage: TCodePage): string;

  protected
    { Protected declarations }
  public
    { Public declarations }
  published
   property InputCodePage : TCodePage read FInputCodePage write FSetInputCodePage default cpALT;
   property OutputCodePage: TCodePage read FOutputCodePage write FSetOutputCodePage default cpWIN;

   property InputString: String read FInputString write FSetInputString;
   property OutputString: String read FOutputString write FSetOutputString;
  end;

procedure Register;

implementation

Type
 TLittleTable=array [1..2] of word;
Const
 alt2isotable: array[1..66] of TLittleTable =(($80,$b0),($81,$b1),($82,$b2),($83,$b3),($84,$b4),($85,$b5),
                                             ($86,$b6),($87,$b7),($88,$b8),($89,$b9),($8a,$ba),($8b,$bb),
                                             ($8c,$bc),($8d,$bd),($8e,$be),($8f,$bf),($90,$c0),($91,$c1),
                                             ($92,$c2),($93,$c3),($94,$c4),($95,$c5),($96,$c6),($97,$c7),
                                             ($98,$c8),($99,$c9),($9a,$ca),($9b,$cb),($9c,$cc),($9d,$cd),
                                             ($9e,$ce),($9f,$cf),($a0,$d0),($a1,$d1),($a2,$d2),($a3,$d3),
                                             ($a4,$d4),($a5,$d5),($a6,$d6),($a7,$d7),($a8,$d8),($a9,$d9),
                                             ($aa,$da),($ab,$db),($ac,$dc),($ad,$dd),($ae,$de),($af,$df),
                                             ($e0,$e0),($e1,$e1),($e2,$e2),($e3,$e3),($e4,$e4),($e5,$e5),
                                             ($e6,$e6),($e7,$e7),($e8,$e8),($e9,$e9),($ea,$ea),($eb,$eb),
                                             ($ec,$ec),($ed,$ed),($ee,$ee),($ef,$ef),($f0,$a1),($f1,$f1)
                                             );
 alt2koitable: array[1..66] of TLittleTable =(($80,$e1),($81,$e2),($82,$f7),($83,$e7),($84,$e4),($85,$e5),
                                             ($86,$f6),($87,$fa),($88,$e9),($89,$ea),($8a,$eb),($8b,$ec),
                                             ($8c,$ed),($8d,$ee),($8e,$ef),($8f,$f0),($90,$f2),($91,$f3),
                                             ($92,$f4),($93,$f5),($94,$e6),($95,$e8),($96,$e3),($97,$fe),
                                             ($98,$fb),($99,$fd),($9a,$ff),($9b,$f9),($9c,$f8),($9d,$fc),
                                             ($9e,$e0),($9f,$f1),($a0,$c1),($a1,$c2),($a2,$d7),($a3,$c7),
                                             ($a4,$c4),($a5,$c5),($a6,$d6),($a7,$da),($a8,$c9),($a9,$ca),
                                             ($aa,$cb),($ab,$cc),($ac,$cd),($ad,$ce),($ae,$cf),($af,$d0),
                                             ($e0,$d2),($e1,$d3),($e2,$d4),($e3,$d5),($e4,$c6),($e5,$c8),
                                             ($e6,$c3),($e7,$de),($e8,$db),($e9,$dd),($ea,$df),($eb,$d9),
                                             ($ec,$d8),($ed,$dc),($ee,$c0),($ef,$d1),($f0,$b3),($f1,$a3)
                                             );
 alt2mactable: array[1..34] of TLittleTable =(($a0,$e0),($a1,$e1),($a2,$e2),($a3,$e3),($a4,$e4),($a5,$e5),
                                             ($a6,$e6),($a7,$e7),($a8,$e8),($a9,$e9),($aa,$ea),($ab,$eb),
                                             ($ac,$ec),($ad,$ed),($ae,$ee),($af,$ef),($e0,$f0),($e1,$f1),
                                             ($e2,$f2),($e3,$f3),($e4,$f4),($e5,$f5),($e6,$f6),($e7,$f7),
                                             ($e8,$f8),($e9,$f9),($ea,$fa),($eb,$fb),($ec,$fc),($ed,$fd),
                                             ($ee,$fe),($ef,$df),($f0,$dd),($f1,$de));
 alt2wintable: array[1..66] of TLittleTable =(($80,$c0),($81,$c1),($82,$c2),($83,$c3),($84,$c4),($85,$c5),
                                             ($86,$c6),($87,$c7),($88,$c8),($89,$c9),($8a,$ca),($8b,$cb),
                                             ($8c,$cc),($8d,$cd),($8e,$ce),($8f,$cf),($90,$d0),($91,$d1),
                                             ($92,$d2),($93,$d3),($94,$d4),($95,$d5),($96,$d6),($97,$d7),
                                             ($98,$d8),($99,$d9),($9a,$da),($9b,$db),($9c,$dc),($9d,$dd),
                                             ($9e,$de),($9f,$df),($a0,$e0),($a1,$e1),($a2,$e2),($a3,$e3),
                                             ($a4,$e4),($a5,$e5),($a6,$e6),($a7,$e7),($a8,$e8),($a9,$e9),
                                             ($aa,$ea),($ab,$eb),($ac,$ec),($ad,$ed),($ae,$ee),($af,$ef),
                                             ($e0,$f0),($e1,$f1),($e2,$f2),($e3,$f3),($e4,$f4),($e5,$f5),
                                             ($e6,$f6),($e7,$f7),($e8,$f8),($e9,$f9),($ea,$fa),($eb,$fb),
                                             ($ec,$fc),($ed,$fd),($ee,$fe),($ef,$ff),($f0,$a8),($f1,$b8)
                                             );
 iso2alttable: array[1..50] of TLittleTable =(($b0,$80),($b1,$81),($b2,$82),($b3,$83),($b4,$84),($b5,$85),
                                             ($b6,$86),($b7,$87),($b8,$88),($b9,$89),($ba,$8a),($bb,$8b),
                                             ($bc,$8c),($bd,$8d),($be,$8e),($bf,$8f),($c0,$90),($c1,$91),
                                             ($c2,$92),($c3,$93),($c4,$94),($c5,$95),($c6,$96),($c7,$97),
                                             ($c8,$98),($c9,$99),($ca,$9a),($cb,$9b),($cc,$9c),($cd,$9d),
                                             ($ce,$9e),($cf,$9f),($d0,$a0),($d1,$a1),($d2,$a2),($d3,$a3),
                                             ($d4,$a4),($d5,$a5),($d6,$a6),($d7,$a7),($d8,$a8),($d9,$a9),
                                             ($da,$aa),($db,$ab),($dc,$ac),($dd,$ad),($de,$ae),($df,$af),
                                             ($a1,$f0),($f1,$f1));
 iso2koitable: array[1..66] of TLittleTable =(($b0,$e1),($b1,$e2),($b2,$f7),($b3,$e7),($b4,$e4),($b5,$e5),
                                             ($b6,$f6),($b7,$fa),($b8,$e9),($b9,$ea),($ba,$eb),($bb,$ec),
                                             ($bc,$ed),($bd,$ee),($be,$ef),($bf,$f0),($c0,$f2),($c1,$f3),
                                             ($c2,$f4),($c3,$f5),($c4,$e6),($c5,$e8),($c6,$e3),($c7,$fe),
                                             ($c8,$fb),($c9,$fd),($ca,$ff),($cb,$f9),($cc,$f8),($cd,$fc),
                                             ($ce,$e0),($cf,$f1),($d0,$c1),($d1,$c2),($d2,$d7),($d3,$c7),
                                             ($d4,$c4),($d5,$c5),($d6,$d6),($d7,$da),($d8,$c9),($d9,$ca),
                                             ($da,$cb),($db,$cc),($dc,$cd),($dd,$ce),($de,$cf),($df,$d0),
                                             ($e0,$d2),($e1,$d3),($e2,$d4),($e3,$d5),($e4,$c6),($e5,$c8),
                                             ($e6,$c3),($e7,$de),($e8,$db),($e9,$dd),($ea,$df),($eb,$d9),
                                             ($ec,$d8),($ed,$dc),($ee,$c0),($ef,$d1),($a1,$b3),($f1,$a3)
                                             );
 iso2mactable: array[1..66] of TLittleTable =(($b0,$80),($b1,$81),($b2,$82),($b3,$83),($b4,$84),($b5,$85),
                                             ($b6,$86),($b7,$87),($b8,$88),($b9,$89),($ba,$8a),($bb,$8b),
                                             ($bc,$8c),($bd,$8d),($be,$8e),($bf,$8f),($c0,$90),($c1,$91),
                                             ($c2,$92),($c3,$93),($c4,$94),($c5,$95),($c6,$96),($c7,$97),
                                             ($c8,$98),($c9,$99),($ca,$9a),($cb,$9b),($cc,$9c),($cd,$9d),
                                             ($ce,$9e),($cf,$9f),($d0,$e0),($d1,$e1),($d2,$e2),($d3,$e3),
                                             ($d4,$e4),($d5,$e5),($d6,$e6),($d7,$e7),($d8,$e8),($d9,$e9),
                                             ($da,$ea),($db,$eb),($dc,$ec),($dd,$ed),($de,$ee),($df,$ef),
                                             ($e0,$f0),($e1,$f1),($e2,$f2),($e3,$f3),($e4,$f4),($e5,$f5),
                                             ($e6,$f6),($e7,$f7),($e8,$f8),($e9,$f9),($ea,$fa),($eb,$fb),
                                             ($ec,$fc),($ed,$fd),($ee,$fe),($ef,$df),($a1,$dd),($f1,$de)
                                             );
 iso2wintable: array[1..66] of TLittleTable =(($b0,$c0),($b1,$c1),($b2,$c2),($b3,$c3),($b4,$c4),($b5,$c5),
                                             ($b6,$c6),($b7,$c7),($b8,$c8),($b9,$c9),($ba,$ca),($bb,$cb),
                                             ($bc,$cc),($bd,$cd),($be,$ce),($bf,$cf),($c0,$d0),($c1,$d1),
                                             ($c2,$d2),($c3,$d3),($c4,$d4),($c5,$d5),($c6,$d6),($c7,$d7),
                                             ($c8,$d8),($c9,$d9),($ca,$da),($cb,$db),($cc,$dc),($cd,$dd),
                                             ($ce,$de),($cf,$df),($d0,$e0),($d1,$e1),($d2,$e2),($d3,$e3),
                                             ($d4,$e4),($d5,$e5),($d6,$e6),($d7,$e7),($d8,$e8),($d9,$e9),
                                             ($da,$ea),($db,$eb),($dc,$ec),($dd,$ed),($de,$ee),($df,$ef),
                                             ($e0,$f0),($e1,$f1),($e2,$f2),($e3,$f3),($e4,$f4),($e5,$f5),
                                             ($e6,$f6),($e7,$f7),($e8,$f8),($e9,$f9),($ea,$fa),($eb,$fb),
                                             ($ec,$fc),($ed,$fd),($ee,$fe),($ef,$ff),($a1,$a8),($f1,$b8)
                                             );
 koi2alttable: array[1..66] of TLittleTable =(($e1,$80),($e2,$81),($f7,$82),($e7,$83),($e4,$84),($e5,$85),
                                             ($f6,$86),($fa,$87),($e9,$88),($ea,$89),($eb,$8a),($ec,$8b),
                                             ($ed,$8c),($ee,$8d),($ef,$8e),($f0,$8f),($f2,$90),($f3,$91),
                                             ($f4,$92),($f5,$93),($e6,$94),($e8,$95),($e3,$96),($fe,$97),
                                             ($fb,$98),($fd,$99),($ff,$9a),($f9,$9b),($f8,$9c),($fc,$9d),
                                             ($e0,$9e),($f1,$9f),($c1,$a0),($c2,$a1),($d7,$a2),($c7,$a3),
                                             ($c4,$a4),($c5,$a5),($d6,$a6),($da,$a7),($c9,$a8),($ca,$a9),
                                             ($cb,$aa),($cc,$ab),($cd,$ac),($ce,$ad),($cf,$ae),($d0,$af),
                                             ($d2,$e0),($d3,$e1),($d4,$e2),($d5,$e3),($c6,$e4),($c8,$e5),
                                             ($c3,$e6),($de,$e7),($db,$e8),($dd,$e9),($df,$ea),($d9,$eb),
                                             ($d8,$ec),($dc,$ed),($c0,$ee),($d1,$ef),($b3,$f0),($a3,$f1)
                                             );
 koi2isotable: array[1..66] of TLittleTable =(($e1,$b0),($e2,$b1),($f7,$b2),($e7,$b3),($e4,$b4),($e5,$b5),
                                             ($f6,$b6),($fa,$b7),($e9,$b8),($ea,$b9),($eb,$ba),($ec,$bb),
                                             ($ed,$bc),($ee,$bd),($ef,$be),($f0,$bf),($f2,$c0),($f3,$c1),
                                             ($f4,$c2),($f5,$c3),($e6,$c4),($e8,$c5),($e3,$c6),($fe,$c7),
                                             ($fb,$c8),($fd,$c9),($ff,$ca),($f9,$cb),($f8,$cc),($fc,$cd),
                                             ($e0,$ce),($f1,$cf),($c1,$d0),($c2,$d1),($d7,$d2),($c7,$d3),
                                             ($c4,$d4),($c5,$d5),($d6,$d6),($da,$d7),($c9,$d8),($ca,$d9),
                                             ($cb,$da),($cc,$db),($cd,$dc),($ce,$dd),($cf,$de),($d0,$df),
                                             ($d2,$e0),($d3,$e1),($d4,$e2),($d5,$e3),($c6,$e4),($c8,$e5),
                                             ($c3,$e6),($de,$e7),($db,$e8),($dd,$e9),($df,$ea),($d9,$eb),
                                             ($d8,$ec),($dc,$ed),($c0,$ee),($d1,$ef),($b3,$a1),($a3,$f1)
                                             );
 koi2mactable: array[1..66] of TLittleTable =(($e1,$80),($e2,$81),($f7,$82),($e7,$83),($e4,$84),($e5,$85),
                                             ($f6,$86),($fa,$87),($e9,$88),($ea,$89),($eb,$8a),($ec,$8b),
                                             ($ed,$8c),($ee,$8d),($ef,$8e),($f0,$8f),($f2,$90),($f3,$91),
                                             ($f4,$92),($f5,$93),($e6,$94),($e8,$95),($e3,$96),($fe,$97),
                                             ($fb,$98),($fd,$99),($ff,$9a),($f9,$9b),($f8,$9c),($fc,$9d),
                                             ($e0,$9e),($f1,$9f),($c1,$e0),($c2,$e1),($d7,$e2),($c7,$e3),
                                             ($c4,$e4),($c5,$e5),($d6,$e6),($da,$e7),($c9,$e8),($ca,$e9),
                                             ($cb,$ea),($cc,$eb),($cd,$ec),($ce,$ed),($cf,$ee),($d0,$ef),
                                             ($d2,$f0),($d3,$f1),($d4,$f2),($d5,$f3),($c6,$f4),($c8,$f5),
                                             ($c3,$f6),($de,$f7),($db,$f8),($dd,$f9),($df,$fa),($d9,$fb),
                                             ($d8,$fc),($dc,$fd),($c0,$fe),($d1,$df),($b3,$dd),($a3,$de)
                                             );
 koi2wintable: array[1..66] of TLittleTable =(($e1,$c0),($e2,$c1),($f7,$c2),($e7,$c3),($e4,$c4),($e5,$c5),
                                             ($f6,$c6),($fa,$c7),($e9,$c8),($ea,$c9),($eb,$ca),($ec,$cb),
                                             ($ed,$cc),($ee,$cd),($ef,$ce),($f0,$cf),($f2,$d0),($f3,$d1),
                                             ($f4,$d2),($f5,$d3),($e6,$d4),($e8,$d5),($e3,$d6),($fe,$d7),
                                             ($fb,$d8),($fd,$d9),($ff,$da),($f9,$db),($f8,$dc),($fc,$dd),
                                             ($e0,$de),($f1,$df),($c1,$e0),($c2,$e1),($d7,$e2),($c7,$e3),
                                             ($c4,$e4),($c5,$e5),($d6,$e6),($da,$e7),($c9,$e8),($ca,$e9),
                                             ($cb,$ea),($cc,$eb),($cd,$ec),($ce,$ed),($cf,$ee),($d0,$ef),
                                             ($d2,$f0),($d3,$f1),($d4,$f2),($d5,$f3),($c6,$f4),($c8,$f5),
                                             ($c3,$f6),($de,$f7),($db,$f8),($dd,$f9),($df,$fa),($d9,$fb),
                                             ($d8,$fc),($dc,$fd),($c0,$fe),($d1,$ff),($b3,$a8),($a3,$b8)
                                             );
 mac2alttable: array[1..34] of TLittleTable =(($e0,$a0),($e1,$a1),($e2,$a2),($e3,$a3),($e4,$a4),($e5,$a5),
                                             ($e6,$a6),($e7,$a7),($e8,$a8),($e9,$a9),($ea,$aa),($eb,$ab),
                                             ($ec,$ac),($ed,$ad),($ee,$ae),($ef,$af),($f0,$e0),($f1,$e1),
                                             ($f2,$e2),($f3,$e3),($f4,$e4),($f5,$e5),($f6,$e6),($f7,$e7),
                                             ($f8,$e8),($f9,$e9),($fa,$ea),($fb,$eb),($fc,$ec),($fd,$ed),
                                             ($fe,$ee),($df,$ef),($dd,$f0),($de,$f1));
 mac2isotable: array[1..66] of TLittleTable =(($80,$b0),($81,$b1),($82,$b2),($83,$b3),($84,$b4),($85,$b5),
                                             ($86,$b6),($87,$b7),($88,$b8),($89,$b9),($8a,$ba),($8b,$bb),
                                             ($8c,$bc),($8d,$bd),($8e,$be),($8f,$bf),($90,$c0),($91,$c1),
                                             ($92,$c2),($93,$c3),($94,$c4),($95,$c5),($96,$c6),($97,$c7),
                                             ($98,$c8),($99,$c9),($9a,$ca),($9b,$cb),($9c,$cc),($9d,$cd),
                                             ($9e,$ce),($9f,$cf),($e0,$d0),($e1,$d1),($e2,$d2),($e3,$d3),
                                             ($e4,$d4),($e5,$d5),($e6,$d6),($e7,$d7),($e8,$d8),($e9,$d9),
                                             ($ea,$da),($eb,$db),($ec,$dc),($ed,$dd),($ee,$de),($ef,$df),
                                             ($f0,$e0),($f1,$e1),($f2,$e2),($f3,$e3),($f4,$e4),($f5,$e5),
                                             ($f6,$e6),($f7,$e7),($f8,$e8),($f9,$e9),($fa,$ea),($fb,$eb),
                                             ($fc,$ec),($fd,$ed),($fe,$ee),($df,$ef),($dd,$a1),($de,$f1)
                                             );
 mac2koitable: array[1..66] of TLittleTable =(($80,$e1),($81,$e2),($82,$f7),($83,$e7),($84,$e4),($85,$e5),
                                             ($86,$f6),($87,$fa),($88,$e9),($89,$ea),($8a,$eb),($8b,$ec),
                                             ($8c,$ed),($8d,$ee),($8e,$ef),($8f,$f0),($90,$f2),($91,$f3),
                                             ($92,$f4),($93,$f5),($94,$e6),($95,$e8),($96,$e3),($97,$fe),
                                             ($98,$fb),($99,$fd),($9a,$ff),($9b,$f9),($9c,$f8),($9d,$fc),
                                             ($9e,$e0),($9f,$f1),($e0,$c1),($e1,$c2),($e2,$d7),($e3,$c7),
                                             ($e4,$c4),($e5,$c5),($e6,$d6),($e7,$da),($e8,$c9),($e9,$ca),
                                             ($ea,$cb),($eb,$cc),($ec,$cd),($ed,$ce),($ee,$cf),($ef,$d0),
                                             ($f0,$d2),($f1,$d3),($f2,$d4),($f3,$d5),($f4,$c6),($f5,$c8),
                                             ($f6,$c3),($f7,$de),($f8,$db),($f9,$dd),($fa,$df),($fb,$d9),
                                             ($fc,$d8),($fd,$dc),($fe,$c0),($df,$d1),($dd,$b3),($de,$a3)
                                             );
 mac2wintable: array[1..43] of TLittleTable =(($80,$c0),($81,$c1),($82,$c2),($83,$c3),($84,$c4),($85,$c5),
                                             ($86,$c6),($87,$c7),($88,$c8),($89,$c9),($8a,$ca),($8b,$cb),
                                             ($8c,$cc),($8d,$cd),($8e,$ce),($8f,$cf),($90,$d0),($91,$d1),
                                             ($92,$d2),($93,$d3),($94,$d4),($95,$d5),($96,$d6),($97,$d7),
                                             ($98,$d8),($99,$d9),($9a,$da),($9b,$db),($9c,$dc),($9d,$dd),
                                             ($9e,$de),($9f,$df),($df,$ff),($dd,$a8),($de,$b8),($ae,$a5),
                                             ($af,$b4),($b8,$aa),($b9,$ba),($a7,$b2),($b4,$b3),($ba,$af),
                                             ($bb,$bf));
 win2alttable: array[1..66] of TLittleTable =(($c0,$80),($c1,$81),($c2,$82),($c3,$83),($c4,$84),($c5,$85),
                                             ($c6,$86),($c7,$87),($c8,$88),($c9,$89),($ca,$8a),($cb,$8b),
                                             ($cc,$8c),($cd,$8d),($ce,$8e),($cf,$8f),($d0,$90),($d1,$91),
                                             ($d2,$92),($d3,$93),($d4,$94),($d5,$95),($d6,$96),($d7,$97),
                                             ($d8,$98),($d9,$99),($da,$9a),($db,$9b),($dc,$9c),($dd,$9d),
                                             ($de,$9e),($df,$9f),($e0,$a0),($e1,$a1),($e2,$a2),($e3,$a3),
                                             ($e4,$a4),($e5,$a5),($e6,$a6),($e7,$a7),($e8,$a8),($e9,$a9),
                                             ($ea,$aa),($eb,$ab),($ec,$ac),($ed,$ad),($ee,$ae),($ef,$af),
                                             ($f0,$e0),($f1,$e1),($f2,$e2),($f3,$e3),($f4,$e4),($f5,$e5),
                                             ($f6,$e6),($f7,$e7),($f8,$e8),($f9,$e9),($fa,$ea),($fb,$eb),
                                             ($fc,$ec),($fd,$ed),($fe,$ee),($ff,$ef),($a8,$f0),($b8,$f1)
                                             );
 win2isotable: array[1..66] of TLittleTable =(($c0,$b0),($c1,$b1),($c2,$b2),($c3,$b3),($c4,$b4),($c5,$b5),
                                             ($c6,$b6),($c7,$b7),($c8,$b8),($c9,$b9),($ca,$ba),($cb,$bb),
                                             ($cc,$bc),($cd,$bd),($ce,$be),($cf,$bf),($d0,$c0),($d1,$c1),
                                             ($d2,$c2),($d3,$c3),($d4,$c4),($d5,$c5),($d6,$c6),($d7,$c7),
                                             ($d8,$c8),($d9,$c9),($da,$ca),($db,$cb),($dc,$cc),($dd,$cd),
                                             ($de,$ce),($df,$cf),($e0,$d0),($e1,$d1),($e2,$d2),($e3,$d3),
                                             ($e4,$d4),($e5,$d5),($e6,$d6),($e7,$d7),($e8,$d8),($e9,$d9),
                                             ($ea,$da),($eb,$db),($ec,$dc),($ed,$dd),($ee,$de),($ef,$df),
                                             ($f0,$e0),($f1,$e1),($f2,$e2),($f3,$e3),($f4,$e4),($f5,$e5),
                                             ($f6,$e6),($f7,$e7),($f8,$e8),($f9,$e9),($fa,$ea),($fb,$eb),
                                             ($fc,$ec),($fd,$ed),($fe,$ee),($ff,$ef),($a8,$a1),($b8,$f1)
                                             );
 win2koitable: array[1..66] of TLittleTable =(($c0,$e1),($c1,$e2),($c2,$f7),($c3,$e7),($c4,$e4),($c5,$e5),
                                             ($c6,$f6),($c7,$fa),($c8,$e9),($c9,$ea),($ca,$eb),($cb,$ec),
                                             ($cc,$ed),($cd,$ee),($ce,$ef),($cf,$f0),($d0,$f2),($d1,$f3),
                                             ($d2,$f4),($d3,$f5),($d4,$e6),($d5,$e8),($d6,$e3),($d7,$fe),
                                             ($d8,$fb),($d9,$fd),($da,$ff),($db,$f9),($dc,$f8),($dd,$fc),
                                             ($de,$e0),($df,$f1),($e0,$c1),($e1,$c2),($e2,$d7),($e3,$c7),
                                             ($e4,$c4),($e5,$c5),($e6,$d6),($e7,$da),($e8,$c9),($e9,$ca),
                                             ($ea,$cb),($eb,$cc),($ec,$cd),($ed,$ce),($ee,$cf),($ef,$d0),
                                             ($f0,$d2),($f1,$d3),($f2,$d4),($f3,$d5),($f4,$c6),($f5,$c8),
                                             ($f6,$c3),($f7,$de),($f8,$db),($f9,$dd),($fa,$df),($fb,$d9),
                                             ($fc,$d8),($fd,$dc),($fe,$c0),($ff,$d1),($a8,$b3),($b8,$a3)
                                             );
 win2mactable: array[1..46] of TLittleTable =(($b9,$dc),($ab,$c7),($bb,$c8),($c0,$80),($c1,$81),($c2,$82),
                                             ($c3,$83),($c4,$84),($c5,$85),($c6,$86),($c7,$87),($c8,$88),
                                             ($c9,$89),($ca,$8a),($cb,$8b),($cc,$8c),($cd,$8d),($ce,$8e),
                                             ($cf,$8f),($d0,$90),($d1,$91),($d2,$92),($d3,$93),($d4,$94),
                                             ($d5,$95),($d6,$96),($d7,$97),($d8,$98),($d9,$99),($da,$9a),
                                             ($db,$9b),($dc,$9c),($dd,$9d),($de,$9e),($df,$9f),($ff,$df),
                                             ($a8,$dd),($b8,$de),($a5,$ae),($b4,$af),($aa,$b8),($ba,$b9),
                                             ($b2,$a7),($b3,$b4),($af,$ba),($bf,$bb));

procedure TCodePageConvertor.FSetInputString(InputString: string);
begin
    FInputString := InputString;
    FOutputString := ConvertString(FInputString, FInputCodePage, FOutputCodePage);
end;

procedure TCodePageConvertor.FSetOutputString(OutputString: string);
begin
  FOutputString := OutputString;
end;

Procedure TCodePageConvertor.FSetInputCodePage(InputCodePage: TCodePage);
begin
   FInputCodepage := InputCodePage;
   FOutputString := ConvertString(FInputString, FInputCodePage, FOutputCodePage);
end;

Procedure TCodePageConvertor.FSetOutputCodePage(OutputCodePage: TCodePage);
begin
   FOutputCodepage := OutputCodePage;
   FOutputString := ConvertString(FInputString, FInputCodePage, FOutputCodePage);
end;

procedure Register;
begin
  RegisterComponents('ConstantSoft', [TCodePageConvertor]);
end;

Function TCodePageConvertor.ConvertString(InputString: string; InputCodePage, OutputCodepage: TCodePage): string;
Var
 oString: string;
     i,j: byte;
begin
   oString := '';
   Case InputCodepage of
    cpALT: begin
            Case OutputCodepage of
             cpALT: oString := InputString;
             cpISO: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If alt2isotable[j][1] = ord(InputString[i]) then oString := oString + chr(alt2isotable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpKOI: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If alt2koitable[j][1] = ord(InputString[i]) then oString := oString + chr(alt2koitable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpMAC: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 34 do begin
                       If alt2mactable[j][1] = ord(InputString[i]) then oString := oString + chr(alt2mactable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpWIN: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If alt2wintable[j][1] = ord(InputString[i]) then oString := oString + chr(alt2wintable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
            end;
           end;
    cpISO: begin
            Case OutputCodepage of
             cpISO: oString := InputString;
             cpALT: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 50 do begin
                       If iso2alttable[j][1] = ord(InputString[i]) then oString := oString + chr(iso2alttable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpKOI: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If iso2koitable[j][1] = ord(InputString[i]) then oString := oString + chr(iso2koitable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpMAC: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If iso2mactable[j][1] = ord(InputString[i]) then oString := oString + chr(iso2mactable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpWIN: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If iso2wintable[j][1] = ord(InputString[i]) then oString := oString + chr(iso2wintable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
            end;
           end;
    cpKOI: begin
            Case OutputCodepage of
             cpKOI: oString := InputString;
             cpALT: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If koi2alttable[j][1] = ord(InputString[i]) then oString := oString + chr(koi2alttable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpISO: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If koi2isotable[j][1] = ord(InputString[i]) then oString := oString + chr(koi2isotable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpMAC: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If koi2mactable[j][1] = ord(InputString[i]) then oString := oString + chr(koi2mactable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpWIN: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If koi2wintable[j][1] = ord(InputString[i]) then oString := oString + chr(koi2wintable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
            end;
           end;
    cpMAC: begin
            Case OutputCodepage of
             cpMAC: oString := InputString;
             cpALT: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 34 do begin
                       If mac2alttable[j][1] = ord(InputString[i]) then oString := oString + chr(mac2alttable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpISO: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If mac2isotable[j][1] = ord(InputString[i]) then oString := oString + chr(mac2isotable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpKOI: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If mac2koitable[j][1] = ord(InputString[i]) then oString := oString + chr(mac2koitable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpWIN: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 43 do begin
                       If mac2wintable[j][1] = ord(InputString[i]) then oString := oString + chr(mac2wintable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
            end;
           end;
    cpWIN: begin
            Case OutputCodepage of
             cpWIN: oString := InputString;
             cpALT: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If win2alttable[j][1] = ord(InputString[i]) then oString := oString + chr(win2alttable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpISO: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If win2isotable[j][1] = ord(InputString[i]) then oString := oString + chr(win2isotable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpKOI: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 66 do begin
                       If win2koitable[j][1] = ord(InputString[i]) then oString := oString + chr(win2koitable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
             cpMAC: begin
                     for i := 1 to length(InputString) do begin
                      for j := 1 to 46 do begin
                       If win2mactable[j][1] = ord(InputString[i]) then oString := oString + chr(win2mactable[j][2]);
                      end;
                      if length(oString) <> i then oString := oString + InputString[i];
                     end;
                    end;
            end;
           end;
   end;
   ConvertString := oString;
end;

end.
