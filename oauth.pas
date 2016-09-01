// Copyright 2014, 2015, 2016 Dr C (drcpsn@hotmail.com | http://twitter2imgur.github.io/twitter2imgur/)
//
// This file is part of Twitter2Imgur.
//
// Twitter2Imgur is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Twitter2Imgur is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Twitter2Imgur.  If not, see <http://www.gnu.org/licenses/>.

{$H+}
unit oauth;
// see Twitter oauth pages, eg: http://dev.twitter.com/pages/auth and http://oauth.net/core/1.0/
// and twitter pages on the various resources and params/authentication they require
// (eg: http://dev.twitter.com/doc/get/statuses/home_timeline)

interface

const
   oauth_url_enc_param_chars:set of char=['A'..'Z','a'..'z','0'..'9','-','.','_','~'];

function urlencode_oauth_safe(s:string):string;
function hmac_sha1_data(key,data:string):string;
function generate_oauth_auth_header(httpmethod,url,postdata,consumer_key,consumer_secret_key,token,secret_token:string):string;

implementation

uses sha1, misc;

type
  urltype=record
   protocol,host,doc,params,anchor:string;
   port:word;
  end;

  setofchar=set of char;

const
  oauth_signature_method='HMAC-SHA1';
  oauth_version='1.0';
  nonce_chars:array[0..35] of char=(
   'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'
   ,'0','1','2','3','4','5','6','7','8','9');

  hexchars:array[0..15] of char=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

  base64_chars:array[0..63] of char=('A','B','C','D','E','F','G','H','I','J'
   ,'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b'
   ,'c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t'
   ,'u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/');
  base64_pad='=';

function urlencode(s:string;encchars:setofchar):string;
var l:longint;

 function hexbyte(b:byte):string;
 begin
  hexbyte:=hexchars[(b shr 4) and $F]+hexchars[b and $F];
 end;

begin
 l:=1;
 while l<=length(s) do begin
  if not (s[l] in encchars) then begin
   insert(hexbyte(ord(s[l])),s,l+1);
   s[l]:='%';
   inc(l,2);
  end;
  inc(l);
 end;
 result:=s;
end;

function urlencode_oauth_safe(s:string):string;
begin
 result:=urlencode(s,oauth_url_enc_param_chars);
end;

procedure clear_url(var url:urltype);
begin
 with url do begin
  protocol:='';
  host:='';
  port:=0;
  doc:='';
  params:='';
  anchor:='';
 end;
 end;

function base64_encode(s:string):string; // this could be improved a lot, but don't care enough
var l:longint;
    lw:longword;
    a0,a1,a2,a3:byte;
    pad:string;
begin
 pad:='';
 base64_encode:='';
 while length(s) mod 3>0 do begin s:=s+#0; pad:=pad+base64_pad; end;

 for l:=1 to length(s) div 3 do begin
  lw:=(ord(s[(l-1)*3+1]) shl 16)+(ord(s[(l-1)*3+2]) shl 8)+ord(s[(l-1)*3+3]);
  a0:=(lw shr 18) and $3F;
  a1:=(lw shr 12) and $3F;
  a2:=(lw shr 6) and $3F;
  a3:=lw and $3F;
  base64_encode:=base64_encode+base64_chars[a0]+base64_chars[a1]+base64_chars[a2]+base64_chars[a3];
 end;
 if length(pad)>0 then begin
  setlength(base64_encode,length(base64_encode)-length(pad));
  base64_encode:=base64_encode+pad;
 end;
end;

procedure parse_url(s:string;var url:urltype;defaultprotocol:string);
var l:longint;
    bl:boolean;
begin
 clear_url(url);
 with url do begin
  // protocol
  for l:=1 to length(s) do if s[l]=':' then begin
   protocol:=lowercase(copy(s,1,l-1));
   delete(s,1,l);
   if copy(s,1,1)='/' then delete(s,1,1);
   if copy(s,1,1)='/' then delete(s,1,1);
   break;
  end else if s[l]='/' then break; {no protocol}
  if protocol='' then protocol:=lowercase(defaultprotocol);

  // host
  bl:=false;
  for l:=1 to length(s) do if s[l]='/' then begin
   if bl then begin
    host:=lowercase(copy(s,1,l-1));
    delete(s,1,l-1);
    break;
   end;
  end else bl:=true;
  if host='' then begin host:=s; s:=''; end;

  // port
  bl:=false;
  for l:=1 to length(host) do if host[l]=':' then begin
   bl:=s2w(copy(host,l+1,length(host)),port);
   delete(host,l,length(host));
   break;
  end;

  if not bl then begin {port not set}
   if protocol='http' then port:=80
   else if protocol='https' then port:=443
   else if protocol='ftp' then port:=21;
  end;

  // doc, params
  bl:=false;
  doc:=s;
  for l:=1 to length(doc) do if doc[l]='#' then break
  else if doc[l]='?' then begin
   params:=copy(doc,l+1,length(doc));
   delete(doc,l,length(doc));
   bl:=true;
   break;
  end;

  // anchor
  if bl then begin {params found}
   for l:=1 to length(params) do if params[l]='#' then begin
    anchor:=copy(params,l+1,length(params));
    delete(params,l,length(params));
    break;
   end;
  end else begin
   for l:=1 to length(doc) do if doc[l]='#' then begin
    anchor:=copy(doc,l+1,length(doc));
    delete(doc,l,length(doc));
    break;
   end;
  end;

  if copy(doc,1,1)<>'/' then doc:='/'+doc;
 end;
end;

function hmac_sha1_data(key,data:string):string;
const blocksize=64;
var okeypad,ikeypad:string;
    i:integer;

 function hashs(s:string):string; // sha1 hash of a string
 var td:TSHA1Digest;
 begin
  td:=SHA1String(s);
  setlength(result,sizeof(TSHA1Digest));
  move(td,result[1],sizeof(TSHA1Digest));
 end;

 function sxors(s,s2:string):string; // xor 2 strings together, must be equal length
 var l:longint;
 begin
  for l:=1 to length(s) do s[l]:=chr(ord(s[l]) xor ord(s2[l]));
  result:=s;
 end;

begin
 // see http://en.wikipedia.org/wiki/HMAC-SHA1#Definition_.28from_RFC_2104.29

 if length(key)>blocksize then key:=hashs(key); // key too long, hash it
 if length(key)<blocksize then begin // key too short, pad with zeros
  i:=length(key);
  setlength(key,blocksize);
  fillchar(key[i+1],blocksize-i,#0);
 end;

 setlength(okeypad,blocksize);
 fillchar(okeypad[1],blocksize,$5c); // okeypad = string of $5c

 setlength(ikeypad,blocksize);
 fillchar(ikeypad[1],blocksize,$36); // ikeypad = string of $36

 result:=hashs(sxors(key,okeypad)+hashs(sxors(key,ikeypad)+data));
end;

function generate_oauth_auth_header(httpmethod,url,postdata,consumer_key,consumer_secret_key,token,secret_token:string):string;
type strpair=array[0..1] of string;
var nonce,timestamp,signature,baseurl,s,s2:string;
    l:longint;
    urlrec:urltype;
    params:^strpair;
    param_count,param_alloc:longint;

 procedure add_param(name,value:string);
 begin
  if param_count>=param_alloc then begin
   inc(param_alloc,16);
   reallocmem(params,param_alloc*sizeof(strpair));
   fillchar(params[param_count],(param_alloc-param_count)*sizeof(strpair),0);
  end;
  params[param_count][0]:=name;
  params[param_count][1]:=value;
  inc(param_count);
 end;

 function get_sig_base_str:string;
 var l,l2:longint;
     s:string;
 begin
  // sort params
  for l:=0 to param_count-1 do
   for l2:=l+1 to param_count-1 do
    if (params[l2][0]<params[l][0]) // param name is less
     or ((params[l2][0]=params[l][0]) and (params[l2][1]<params[l][1])) then begin // param names are equal, param value is less
      s:=params[l2][0]; params[l2][0]:=params[l][0]; params[l][0]:=s;
      s:=params[l2][1]; params[l2][1]:=params[l][1]; params[l][1]:=s;
     end;

  get_sig_base_str:=upcase(httpmethod)+'&'
   +urlencode_oauth_safe(baseurl)+'&'
   +urlencode_oauth_safe(params[0][0])+'%3D'+urlencode_oauth_safe(params[0][1]);
   for l:=1 to param_count-1 do
    get_sig_base_str:=get_sig_base_str+'%26'+urlencode_oauth_safe(params[l][0])+'%3D'+urlencode_oauth_safe(params[l][1]);
 end;

begin
 params:=nil;
 param_count:=0;
 param_alloc:=0;

 // gotta break down the url and treat params separately and stuff
 parse_url(url,urlrec,'http');
 baseurl:=lowercase(urlrec.protocol)+'://'+lowercase(urlrec.host);
 if ((lowercase(urlrec.protocol)='http') and (urlrec.port<>80)) or ((lowercase(urlrec.protocol)='https') and (urlrec.port<>443)) then baseurl:=baseurl+':'+l2s(urlrec.port);
 baseurl:=baseurl+urlrec.doc;

 // generate a nonce
 setlength(nonce,20);
 for l:=1 to 20 do nonce[l]:=nonce_chars[random(36)];

 // unix timestamp
 timestamp:=l2s(unixtime);

 add_param('oauth_consumer_key',consumer_key);
 add_param('oauth_nonce',nonce);
 add_param('oauth_signature_method',oauth_signature_method);
 add_param('oauth_timestamp',timestamp);
 add_param('oauth_token',token);
 add_param('oauth_version',oauth_version);

 // add url params
 while urlrec.params<>'' do begin
  s:=str_firstparam(urlrec.params,true,'&');
  s2:=str_lastparam(s,true,'=');
  if s<>'' then add_param(s,s2);
 end;

 // add POST data params
 while postdata<>'' do begin
  s2:=str_firstparam(postdata,true,'&');
  s:=str_firstparam(s2,true,'=');
  add_param(s,s2);
 end;

 signature:=base64_encode(hmac_sha1_data(consumer_secret_key+'&'+secret_token,get_sig_base_str));

 // parameter order doesn't matter in the header, only in the signature base string
 generate_oauth_auth_header:='Authorization: OAuth '
  +'oauth_consumer_key="'+urlencode_oauth_safe(consumer_key)
  +'", oauth_nonce="'+urlencode_oauth_safe(nonce)
  +'", oauth_signature_method="'+urlencode_oauth_safe(oauth_signature_method)
  +'", oauth_signature="'+urlencode_oauth_safe(signature)
  +'", oauth_timestamp="'+urlencode_oauth_safe(timestamp)
  +'", oauth_token="'+urlencode_oauth_safe(token)
  +'", oauth_version="'+urlencode_oauth_safe(oauth_version)+'"';

 // free memory
 for l:=0 to param_count-1 do begin params[l][0]:=''; params[l][1]:=''; end;
 freemem(params,param_alloc*sizeof(strpair));
end;

begin

randomize;

end.
