unit libutil;

//{$mode delphi}

interface

uses
  Classes, SysUtils;

function UriToFileName(uri: string): string;
function GetPageResult(code: integer; var Output: TMemoryStream): integer;
function IsImage(AFileName: string): boolean;
function IsFileDownload(AFileName: string): boolean;
function GetContentType(AFileName: string): string;

implementation


function UriToFileName(uri: string): string;
var
  FileName: string;
  I: integer;
begin
  // convert all '/' to '\'
  FileName := trim(Copy(uri,2,length(uri) -1));

  I := Pos('/', FileName);
  while I > 0 do
  begin
    FileName[I] := '\';
    I := Pos('/', FileName);
  end;

  // locate requested file
  FileName := extractfilepath(ParamStr(0)) + FileName;

  if AnsiLastChar(FileName)^ = '\' then
    // folder - reroute to default document
    result := FileName + 'index.html'
  else
    // file - use it
    result := FileName;

end;

function GetPageResult(code: integer; var Output: TMemoryStream): integer;
var
  l: TStringList;
begin
  l := TStringList.Create;
  try
    case code of
      404: l.LoadFromFile(ExtractFilePath(paramstr(0)) + '404.html');
    end;

    l.SaveToStream(Output);
  finally
    l.Free;
  end;

  Result := code;
end;

function IsImage(AFileName: string): boolean;
var
  FileExt: string;
begin
  FileExt := ExtractFileExt(AFileName);

  result := ( (FileExt = '.jpg')
           or (FileExt = '.gif')
           or (FileExt = '.png')
           or (FileExt = '.bmp')
           or (FileExt = '.tif')
           or (FileExt = '.ico') ) and FileExists(AFileName);
end;

function IsFileDownload(AFileName: string): boolean;
var
  FileExt: string;
begin
  FileExt := ExtractFileExt(AFileName);

  result := not ( (FileExt = '.html')
           or (FileExt = '.htm')
           or (FileExt = '.txt')
           or (FileExt = '.css')
           or (FileExt = '.js')
           or (FileExt = '.xhtml')
           or (FileExt = '.dhtml') ) and FileExists(AFileName);
end;

function GetContentType(AFileName: string): string;
var
  FileExt: string;
begin
  FileExt := ExtractFileExt(AFileName);

  if FileExt = '.evy' then Result := 'application/envoy'
  else if FileExt = '.fif' then Result := 'application/fractals'
  else if FileExt = '.spl' then Result := 'application/futuresplash'
  else if FileExt = '.hta' then Result := 'application/hta'
  else if FileExt = '.acx' then Result := 'application/internet-property-stream'
  else if FileExt = '.hqx' then Result := 'application/mac-binhex40'
  else if FileExt = '.doc' then Result := 'application/msword'
  else if FileExt = '.dot' then Result := 'application/msword'
  else if FileExt = '.*' then Result := 'application/octet-stream'
  else if FileExt = '.bin' then Result := 'application/octet-stream'
  else if FileExt = '.class' then Result := 'application/octet-stream'
  else if FileExt = '.dms' then Result := 'application/octet-stream'
  else if FileExt = '.exe' then Result := 'application/octet-stream'
  else if FileExt = '.lha' then Result := 'application/octet-stream'
  else if FileExt = '.lzh' then Result := 'application/octet-stream'
  else if FileExt = '.oda' then Result := 'application/oda'
  else if FileExt = '.axs' then Result := 'application/olescript'
  else if FileExt = '.pdf' then Result := 'application/pdf'
  else if FileExt = '.prf' then Result := 'application/pics-rules'
  else if FileExt = '.p10' then Result := 'application/pkcs10'
  else if FileExt = '.crl' then Result := 'application/pkix-crl'
  else if FileExt = '.ai' then Result := 'application/postscript'
  else if FileExt = '.eps' then Result := 'application/postscript'
  else if FileExt = '.ps' then Result := 'application/postscript'
  else if FileExt = '.rtf' then Result := 'application/rtf'
  else if FileExt = '.setpay' then Result := 'application/set-payment-initiation'
  else if FileExt = '.setreg' then Result := 'application/set-registration-initiation'
  else if FileExt = '.xla' then Result := 'application/vnd.ms-excel'
  else if FileExt = '.xlc' then Result := 'application/vnd.ms-excel'
  else if FileExt = '.xlm' then Result := 'application/vnd.ms-excel'
  else if FileExt = '.xls' then Result := 'application/vnd.ms-excel'
  else if FileExt = '.xlt' then Result := 'application/vnd.ms-excel'
  else if FileExt = '.xlw' then Result := 'application/vnd.ms-excel'
  else if FileExt = '.msg' then Result := 'application/vnd.ms-outlook'
  else if FileExt = '.sst' then Result := 'application/vnd.ms-pkicertstore'
  else if FileExt = '.cat' then Result := 'application/vnd.ms-pkiseccat'
  else if FileExt = '.stl' then Result := 'application/vnd.ms-pkistl'
  else if FileExt = '.pot' then Result := 'application/vnd.ms-powerpoint'
  else if FileExt = '.pps' then Result := 'application/vnd.ms-powerpoint'
  else if FileExt = '.ppt' then Result := 'application/vnd.ms-powerpoint'
  else if FileExt = '.mpp' then Result := 'application/vnd.ms-project'
  else if FileExt = '.wcm' then Result := 'application/vnd.ms-works'
  else if FileExt = '.wdb' then Result := 'application/vnd.ms-works'
  else if FileExt = '.wks' then Result := 'application/vnd.ms-works'
  else if FileExt = '.wps' then Result := 'application/vnd.ms-works'
  else if FileExt = '.hlp' then Result := 'application/winhlp'
  else if FileExt = '.bcpio' then Result := 'application/x-bcpio'
  else if FileExt = '.cdf' then Result := 'application/x-cdf'
  else if FileExt = '.z' then Result := 'application/x-compress'
  else if FileExt = '.tgz' then Result := 'application/x-compressed'
  else if FileExt = '.cpio' then Result := 'application/x-cpio'
  else if FileExt = '.csh' then Result := 'application/x-csh'
  else if FileExt = '.dcr' then Result := 'application/x-director'
  else if FileExt = '.dir' then Result := 'application/x-director'
  else if FileExt = '.dxr' then Result := 'application/x-director'
  else if FileExt = '.dvi' then Result := 'application/x-dvi'
  else if FileExt = '.gtar' then Result := 'application/x-gtar'
  else if FileExt = '.gz' then Result := 'application/x-gzip'
  else if FileExt = '.hdf' then Result := 'application/x-hdf'
  else if FileExt = '.ins' then Result := 'application/x-internet-signup'
  else if FileExt = '.isp' then Result := 'application/x-internet-signup'
  else if FileExt = '.iii' then Result := 'application/x-iphone'
  else if FileExt = '.js' then Result := 'application/x-javascript'
  else if FileExt = '.latex' then Result := 'application/x-latex'
  else if FileExt = '.mdb' then Result := 'application/x-msaccess'
  else if FileExt = '.crd' then Result := 'application/x-mscardfile'
  else if FileExt = '.clp' then Result := 'application/x-msclip'
  else if FileExt = '.dll' then Result := 'application/x-msdownload'
  else if FileExt = '.m13' then Result := 'application/x-msmediaview'
  else if FileExt = '.m14' then Result := 'application/x-msmediaview'
  else if FileExt = '.mvb' then Result := 'application/x-msmediaview'
  else if FileExt = '.wmf' then Result := 'application/x-msmetafile'
  else if FileExt = '.mny' then Result := 'application/x-msmoney'
  else if FileExt = '.pub' then Result := 'application/x-mspublisher'
  else if FileExt = '.scd' then Result := 'application/x-msschedule'
  else if FileExt = '.trm' then Result := 'application/x-msterminal'
  else if FileExt = '.wri' then Result := 'application/x-mswrite'
  else if FileExt = '.cdf' then Result := 'application/x-netcdf'
  else if FileExt = '.nc' then Result := 'application/x-netcdf'
  else if FileExt = '.pma' then Result := 'application/x-perfmon'
  else if FileExt = '.pmc' then Result := 'application/x-perfmon'
  else if FileExt = '.pml' then Result := 'application/x-perfmon'
  else if FileExt = '.pmr' then Result := 'application/x-perfmon'
  else if FileExt = '.pmw' then Result := 'application/x-perfmon'
  else if FileExt = '.p12' then Result := 'application/x-pkcs12'
  else if FileExt = '.pfx' then Result := 'application/x-pkcs12'
  else if FileExt = '.p7b' then Result := 'application/x-pkcs7-certificates'
  else if FileExt = '.spc' then Result := 'application/x-pkcs7-certificates'
  else if FileExt = '.p7r' then Result := 'application/x-pkcs7-certreqresp'
  else if FileExt = '.p7c' then Result := 'application/x-pkcs7-mime'
  else if FileExt = '.p7m' then Result := 'application/x-pkcs7-mime'
  else if FileExt = '.p7s' then Result := 'application/x-pkcs7-signature'
  else if FileExt = '.sh' then Result := 'application/x-sh'
  else if FileExt = '.shar' then Result := 'application/x-shar'
  else if FileExt = '.swf' then Result := 'application/x-shockwave-flash'
  else if FileExt = '.sit' then Result := 'application/x-stuffit'
  else if FileExt = '.sv4cpio' then Result := 'application/x-sv4cpio'
  else if FileExt = '.sv4crc' then Result := 'application/x-sv4crc'
  else if FileExt = '.tar' then Result := 'application/x-tar'
  else if FileExt = '.tcl' then Result := 'application/x-tcl'
  else if FileExt = '.tex' then Result := 'application/x-tex'
  else if FileExt = '.texi' then Result := 'application/x-texinfo'
  else if FileExt = '.texinfo' then Result := 'application/x-texinfo'
  else if FileExt = '.roff' then Result := 'application/x-troff'
  else if FileExt = '.t' then Result := 'application/x-troff'
  else if FileExt = '.tr' then Result := 'application/x-troff'
  else if FileExt = '.man' then Result := 'application/x-troff-man'
  else if FileExt = '.me' then Result := 'application/x-troff-me'
  else if FileExt = '.ms' then Result := 'application/x-troff-ms'
  else if FileExt = '.ustar' then Result := 'application/x-ustar'
  else if FileExt = '.src' then Result := 'application/x-wais-source'
  else if FileExt = '.cer' then Result := 'application/x-x509-ca-cert'
  else if FileExt = '.crt' then Result := 'application/x-x509-ca-cert'
  else if FileExt = '.der' then Result := 'application/x-x509-ca-cert'
  else if FileExt = '.pko' then Result := 'application/ynd.ms-pkipko'
  else if FileExt = '.zip' then Result := 'application/zip'
  else if FileExt = '.rar' then Result := 'application/rar'
  else if FileExt = '.au' then Result := 'audio/basic'
  else if FileExt = '.snd' then Result := 'audio/basic'
  else if FileExt = '.mid' then Result := 'audio/mid'
  else if FileExt = '.rmi' then Result := 'audio/mid'
  else if FileExt = '.mp3' then Result := 'audio/mpeg'
  else if FileExt = '.aif' then Result := 'audio/x-aiff'
  else if FileExt = '.aifc' then Result := 'audio/x-aiff'
  else if FileExt = '.aiff' then Result := 'audio/x-aiff'
  else if FileExt = '.m3u' then Result := 'audio/x-mpegurl'
  else if FileExt = '.ra' then Result := 'audio/x-pn-realaudio'
  else if FileExt = '.ram' then Result := 'audio/x-pn-realaudio'
  else if FileExt = '.wav' then Result := 'audio/x-wav'
  else if FileExt = '.bmp' then Result := 'image/bmp'
  else if FileExt = '.cod' then Result := 'image/cis-cod'
  else if FileExt = '.gif' then Result := 'image/gif'
  else if FileExt = '.png' then Result := 'image/png'
  else if FileExt = '.ief' then Result := 'image/ief'
  else if FileExt = '.jpe' then Result := 'image/jpeg'
  else if FileExt = '.jpeg' then Result := 'image/jpeg'
  else if FileExt = '.jpg' then Result := 'image/jpeg'
  else if FileExt = '.jfif' then Result := 'image/pipeg'
  else if FileExt = '.svg' then Result := 'image/svg+xml'
  else if FileExt = '.tif' then Result := 'image/tiff'
  else if FileExt = '.tiff' then Result := 'image/tiff'
  else if FileExt = '.ras' then Result := 'image/x-cmu-raster'
  else if FileExt = '.cmx' then Result := 'image/x-cmx'
  else if FileExt = '.ico' then Result := 'image/x-icon'
  else if FileExt = '.pnm' then Result := 'image/x-portable-anymap'
  else if FileExt = '.pbm' then Result := 'image/x-portable-bitmap'
  else if FileExt = '.pgm' then Result := 'image/x-portable-graymap'
  else if FileExt = '.ppm' then Result := 'image/x-portable-pixmap'
  else if FileExt = '.rgb' then Result := 'image/x-rgb'
  else if FileExt = '.xbm' then Result := 'image/x-xbitmap'
  else if FileExt = '.xpm' then Result := 'image/x-xpixmap'
  else if FileExt = '.xwd' then Result := 'image/x-xwindowdump'
  else if FileExt = '.mht' then Result := 'message/rfc822'
  else if FileExt = '.mhtml' then Result := 'message/rfc822'
  else if FileExt = '.nws' then Result := 'message/rfc822'
  else if FileExt = '.css' then Result := 'text/css'
  else if FileExt = '.323' then Result := 'text/h323'
  else if FileExt = '.htm' then Result := 'text/html'
  else if FileExt = '.html' then Result := 'text/html'
  else if FileExt = '.stm' then Result := 'text/html'
  else if FileExt = '.uls' then Result := 'text/iuls'
  else if FileExt = '.bas' then Result := 'text/plain'
  else if FileExt = '.c' then Result := 'text/plain'
  else if FileExt = '.h' then Result := 'text/plain'
  else if FileExt = '.txt' then Result := 'text/plain'
  else if FileExt = '.rtx' then Result := 'text/richtext'
  else if FileExt = '.sct' then Result := 'text/scriptlet'
  else if FileExt = '.tsv' then Result := 'text/tab-separated-values'
  else if FileExt = '.htt' then Result := 'text/webviewhtml'
  else if FileExt = '.htc' then Result := 'text/x-component'
  else if FileExt = '.etx' then Result := 'text/x-setext'
  else if FileExt = '.vcf' then Result := 'text/x-vcard'
  else if FileExt = '.mp2' then Result := 'video/mpeg'
  else if FileExt = '.mpa' then Result := 'video/mpeg'
  else if FileExt = '.mpe' then Result := 'video/mpeg'
  else if FileExt = '.mpeg' then Result := 'video/mpeg'
  else if FileExt = '.mpg' then Result := 'video/mpeg'
  else if FileExt = '.mpv2' then Result := 'video/mpeg'
  else if FileExt = '.mov' then Result := 'video/quicktime'
  else if FileExt = '.qt' then Result := 'video/quicktime'
  else if FileExt = '.lsf' then Result := 'video/x-la-asf'
  else if FileExt = '.lsx' then Result := 'video/x-la-asf'
  else if FileExt = '.asf' then Result := 'video/x-ms-asf'
  else if FileExt = '.asr' then Result := 'video/x-ms-asf'
  else if FileExt = '.asx' then Result := 'video/x-ms-asf'
  else if FileExt = '.avi' then Result := 'video/x-msvideo'
  else if FileExt = '.movie' then Result := 'video/x-sgi-movie'
  else if FileExt = '.flr' then Result := 'x-world/x-vrml'
  else if FileExt = '.vrml' then Result := 'x-world/x-vrml'
  else if FileExt = '.wrl' then Result := 'x-world/x-vrml'
  else if FileExt = '.wrz' then Result := 'x-world/x-vrml'
  else if FileExt = '.xaf' then Result := 'x-world/x-vrml'
  else if FileExt = '.xof' then Result := 'x-world/x-vrml'
  else Result := 'text/html';

end;

end.

