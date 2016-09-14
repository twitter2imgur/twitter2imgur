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

unit misc;

{$mode objfpc}{$H+}{$I-}

interface

uses
  Classes, SysUtils, LMessages, Graphics;

type
  thread_info=record
  started,running,suspended,abort:boolean;
  id:TThreadID;
 end;

function trim_whitespace(s:string):string;
function str_firstparam(var s:string;remove:boolean;delimiter:char):string;
function str_firstparam(var s:string;remove:boolean):string; inline;
function str_lastparam(var s:string;remove:boolean;delimiter:char):string;
function str_lastparam(var s:string;remove:boolean):string; inline;
function l2s(l:longint):string;
function s2l(s:string;var l:longint):boolean;
function s2w(s:string;var w:word):boolean;
function add_new_record(var p:pointer;var count,alloc:longint;recsize,minrecords:integer):longint; // returns record number
function max(i:array of integer):integer;
procedure read_config_file;
procedure write_config_file;
procedure read_images_file;
procedure write_images_file;
function unixtime:longint;
function datetime_to_unixtime(t:tdatetime):longint;
function show_duration(secs:longint):string;
function trim_quotes(s:string):string;
function json_extra_escapes(s:string):string;
function o_(s,k:string):string;
function do_(s,k:string):string;
function parse_timestamp(s:string;var t:longint):boolean;
function shellexec(executable:string;params:array of string;currentdir:string):boolean;
function load_image_thumbnails(p:pointer):ptrint; // secondary thread
procedure init_thumbnail_imagelist;
procedure thread_activate(var thread:thread_info;proc:TThreadfunc);
function plural(l:longint;singleunit,pluralunits:string):string;
function parse_ini_line(line:string;var val1,val2:string):integer; // returns line type
function s2r(s:string;var r:real):boolean;
function get_img_by_twitter_id(id:string; var num:longint):boolean;
//function fontheight2size(height:integer):integer;
function fontstr(var name:string;size:integer;style:TFontStylesBase;pt:boolean):string;
procedure get_listview_font(var name:string;var size:integer;var style:TFontStylesBase);

const
  hexch:array[0..15] of char=('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');

  windowmsg_thread_ended=lm_user+1;
  windowmsg_thumbnail_loaded=lm_user+2;
  windowmsg_update_listview=lm_user+3;

  thumbnail_width_default=128;
  thumbnail_height_default=72;

//  app_url='https://code.google.com/p/twitter2imgur/';
  app_url='http://twitter2imgur.github.io/twitter2imgur/';
  app_download_url=app_url;
  app_version=1.04;
  app_version_str='1.04';

  ini_line_junk=0;
  ini_line_section=1;
  ini_line_var=2;

  job_flag_error=$1;
  job_flag_twitter_tried=$2;
  job_flag_imgur_tried=$4;
  job_flag_thumbnail_tried=$8;
  job_flag_new=$10;

type
  img_list_type=record
   timestamp,fetched_timestamp,imglist_num:longint;
   twitter_media_id,twitter_url,title_,twitter_hashtags_,local_file,imgur_id,imgur_url,errorinfo:string;
   flags:word;
  end;

  imgur_album_type=record
   title,id:string;
  end;

var
  img_list_CS:system.TRTLCriticalSection;
  configdir,imagesdir,thumbsdir:string;
  epoch:tdatetime;

  img_list:^img_list_type=nil;
  img_list_count:longint=0;
  img_list_alloc:longint=0;

  imgur_albums:^imgur_album_type=nil;
  imgur_album_count:longint=0;
  imgur_album_alloc:longint=0;
  imgur_albums_fetched:boolean=false;

  window_coords_loaded:boolean=false;

  jobs_in_progress:boolean=false;

  img_default:integer=-1;
{  img_loading:integer=-1;
  img_error:integer=-1;}

  twitter_thread,imgur_thread,thumbnail_thread:thread_info;

  tweet_fetch_success:boolean;
  tweet_fetch_info:string;

  // for progress bar and label
  progress_numfiles,progress_jobs_total,progress_jobs_done,
  progress_imgur_currentfile_total,progress_imgur_currentfile_current:longint;

  program_update_msg,program_update_url:string;
  program_update_latestver:real;
  program_update_last_check:longint;
  show_update_notification:boolean=false;

  default_font_name:string;
  default_font_size:integer;
  default_font_style:TFontStylesBase;

  // program options
  twitter_token,twitter_token_secret,twitter_screenname,
  imgur_access_token,imgur_refresh_token,imgur_screenname,
  upload_album,limit_hashtags:string;
  imgur_token_expiry:longint=0;
  last_update:longint=0;
  default_action:longint=0;
  auto_upload_album:boolean=false;
  always_on_top:boolean=false;
  use_systray:boolean=false;
  url_mode:longint=0;
  thumbnail_width:longint=thumbnail_width_default;
  thumbnail_height:longint=thumbnail_height_default;
  auto_fetch:boolean=false;
  auto_fetch_mins:longint=30;
  window_coords:array[0..3] of longint;
  window_maximised:boolean=false;
  font_override:boolean=false;
  no_img_desc:boolean=false;
  cache_thumbnails:boolean=true;

  ssl_version:string;

  last_app_update_check:longint=0;
  last_successful_app_update_check:longint=0;
  app_update_attempt_count:longint=0;
  app_update_check:boolean=true;
  current_fetch_is_auto:boolean;

implementation

uses
  dateutils, process, BGRABitmap, BGRABitmapTypes, unitmain, lclintf, extctrls, controls, forms
  {$ifdef MSWindows},Windows{$endif}
  {$ifdef Unix},baseunix{$endif}
  ;

function trim_whitespace(s:string):string;
begin
 while copy(s,length(s),1)=' ' do delete(s,length(s),1);
 while copy(s,1,1)=' ' do delete(s,1,1);
 result:=s;
end;

function str_firstparam(var s:string;remove:boolean;delimiter:char):string;
var
  l:longint;
  s2,orig_s:string;
  ss:string;
  bl:boolean;
begin
 orig_s:=s;
 while copy(orig_s,1,1)=delimiter do delete(orig_s,1,1);
 s2:='';
 bl:=false;
 for l:=1 to length(orig_s) do if orig_s[l]=delimiter then begin bl:=true; break; end;
 if bl then s2:=copy(orig_s,1,l-1) else s2:=orig_s;
 ss:=orig_s;
 if remove then begin
  if bl then begin
   delete(ss,1,l);
   delete(orig_s,1,l);
   while copy(orig_s,1,1)=delimiter do delete(orig_s,1,1);
  end else orig_s:='';
  s:=orig_s;
 end;
 result:=s2;
end;

function str_firstparam(var s:string;remove:boolean):string; inline;
begin
 result:=str_firstparam(s,remove,' ');
end;

function str_lastparam(var s:string;remove:boolean;delimiter:char):string;
var l:longint;
    s2:string;
    bl:boolean;
begin
 s:=trim_whitespace(s);
 bl:=false;
 for l:=length(s) downto 1 do if s[l]=delimiter then begin
  bl:=true;
  break;
 end;

 if bl then s2:=copy(s,l+1,length(s)) else s2:=s;

 if remove then begin
  if bl then begin
   delete(s,l,length(s));
   s:=trim_whitespace(s);
  end else s:='';
 end;
 result:=s2;
end;

function str_lastparam(var s:string;remove:boolean):string; inline;
begin
 result:=str_lastparam(s,remove,' ');
end;

function l2s(l:longint):string;
begin
 str(l,result);
end;

function s2l(s:string;var l:longint):boolean;
var e:integer;
begin
 val(s,l,e);
 result:=e=0;
end;

function s2w(s:string;var w:word):boolean;
var err:longint;
    num:longint;
begin
 val(s,num,err);
 if (err=0) and (num>=0) and (num<=65535) then begin
  w:=num;
  result:=true;
 end else result:=false;
end;

function add_new_record(var p:pointer;var count,alloc:longint;recsize,minrecords:integer):longint; // returns record number
var
  l:longint;
  pb:^byte;
begin
 if count>=alloc then begin // grow
  l:=alloc*2;
  if l<minrecords then l:=minrecords;
  reallocmem(p,l*recsize);
  pb:=p;
  fillchar(pb[alloc*recsize],(l-alloc)*recsize,0);
  alloc:=l;
 end;
 result:=count;
 inc(count);
end;

function max(i:array of integer):integer;
var count:integer;
begin
 result:=0;
 for count:=0 to high(i) do if i[count]>result then result:=i[count];
end;

procedure read_config_file;
const
  section_unknown=0;
  section_options=1;
var
  filename,line,param,value,s,s2:string;
  t:text;
  i,section:integer;
  l,l2:longint;
  formrect:TRect;
  monitor:TMonitor;
  bl:boolean;
  fontstyle:TFontStyles=[];
begin
 filename:=configdir+'config.ini';
 filemode:=0;
 assign(t,filename);
 reset(t);
 section:=section_options;

 if ioresult=0 then begin
  while not eof(t) do begin
   readln(t,line);
   if ioresult<>0 then break;
   case parse_ini_line(line,param,value) of
    ini_line_section:begin
     if param='options' then section:=section_options else section:=section_unknown;
    end;
    ini_line_var:begin
     if section=section_options then begin
      if param='twitter name' then twitter_screenname:=value
      else if param='twitter token' then twitter_token:=do_(value,'*/*zP|Z@}Ec6dt|oz4n7''e>j,tTa+^8H')
      else if param='twitter secret token' then twitter_token_secret:=do_(value,'ZIR)-5o$Zfiw!"ZU~vKTa}L`"+XDyol)')
      else if param='imgur name' then imgur_screenname:=value
      else if param='imgur token' then imgur_access_token:=do_(value,'c-GA|,MQ(P+9;G\Zx}dN*c0O/zBm+J]8')
      else if param='imgur refresh token' then imgur_refresh_token:=do_(value,'6m-wh0^my$O*I6bG9}qJ:g[$Z<4wW1wa')
      else if param='imgur token expiry' then s2l(value,imgur_token_expiry)
      else if param='auto fetch' then auto_fetch:=value='1'
      else if param='auto fetch mins' then s2l(value,auto_fetch_mins)
      else if param='album' then upload_album:=value
      else if param='auto album' then auto_upload_album:=value='1'
      else if param='last update' then s2l(value,last_update)
      else if param='always on top' then always_on_top:=value='1'
      else if param='use systray' then use_systray:=value='1'
      else if param='default action' then begin
       if value='1' then default_action:=1 else if value='2' then default_action:=2 else default_action:=0;
      end else if param='limit hashtags' then limit_hashtags:=value
      else if param='url mode' then s2l(value,url_mode)
      else if param='no image description' then no_img_desc:=value='1'
      else if param='cache thumbnails' then cache_thumbnails:=value='1'
      else if param='thumbnail size' then begin
       if s2l(str_firstparam(value,true,','),l) then if (l>=0) and (l<=1000) then thumbnail_width:=l;
       if s2l(str_firstparam(value,true,','),l) then if (l>=0) and (l<=1000) then thumbnail_height:=l;
      end else if param='window coords' then begin
       window_coords_loaded:=true;
       for i:=0 to 3 do if not s2l(str_firstparam(value,true,','),window_coords[i]) then window_coords_loaded:=false;
       window_maximised:=str_firstparam(value,true,',')='1';
       if window_coords_loaded then begin // window coords sanity check
        formrect.Left:=window_coords[0];
        formrect.Top:=window_coords[1];
        formrect.Right:=window_coords[0]+window_coords[2];
        formrect.Bottom:=window_coords[1]+window_coords[3];
        monitor:=screen.MonitorFromRect(formrect); // which monitor is the form on?
        if monitor<>nil then begin
         if (window_coords[0]<-10) or (window_coords[0]>monitor.WorkareaRect.Right-10) then window_coords[0]:=0;
         if (window_coords[1]<-10) or (window_coords[1]>monitor.WorkareaRect.Bottom-10) then window_coords[1]:=0;
         if window_coords[2]<FormMain.Constraints.MinWidth then window_coords[2]:=FormMain.Constraints.MinWidth;
         if window_coords[3]<FormMain.Constraints.MinHeight then window_coords[3]:=FormMain.Constraints.MinHeight;
        end;
       end;
      end else if param='list font' then begin
       if value<>'' then begin
        font_override:=true;

        s:=str_lastparam(value,false); // style
        bl:=false;
        while s<>'' do begin
         s2:=lowercase(str_firstparam(s,true,','));
         if s2='bold' then begin bl:=true; include(fontstyle,fsBold); end
         else if s2='italic' then begin bl:=true; include(fontstyle,fsItalic); end;
        end;
        if bl then str_lastparam(value,true);
        FormMain.ListViewFiles.Font.Style:=fontstyle;

        if s2l(str_lastparam(value,false),l) then begin // size
         str_lastparam(value,true);
         if abs(l)<1000 then FormMain.ListViewFiles.Font.Size:=l;{XXXXXXX}
        end;

        FormMain.ListViewFiles.Font.Name:=value; // name
       end;
      end else if param='app update' then begin
       s2:=str_firstparam(value,true,',');
       if s2l(s2,l) and s2l(value,l2) then begin
        last_successful_app_update_check:=l;
        app_update_attempt_count:=l2;
       end;
      end;
     end;
    end;
   end;
  end;
  close(t);
  ioresult;
 end;
end;

procedure write_config_file;
var
  tempname,filename,s:string;
  fontsize:integer;
  fontstyle:TFontStylesBase;
  t:text;
begin
 tempname:=configdir+'config.'+l2s(unixtime)+'.tmp';
 filename:=configdir+'config.ini';
 if not DirectoryExists(configdir) then ForceDirectories(configdir);
 assign(t,tempname);
 rewrite(t);
 writeln(t,'[options]');
 writeln(t,'version='+app_version_str);
 writeln(t,'twitter name='+twitter_screenname);
 writeln(t,'twitter token='+o_(twitter_token,'*/*zP|Z@}Ec6dt|oz4n7''e>j,tTa+^8H'));
 writeln(t,'twitter secret token='+o_(twitter_token_secret,'ZIR)-5o$Zfiw!"ZU~vKTa}L`"+XDyol)'));
 writeln(t,'imgur name='+imgur_screenname);
 writeln(t,'imgur token='+o_(imgur_access_token,'c-GA|,MQ(P+9;G\Zx}dN*c0O/zBm+J]8'));
 writeln(t,'imgur refresh token='+o_(imgur_refresh_token,'6m-wh0^my$O*I6bG9}qJ:g[$Z<4wW1wa'));
 writeln(t,'imgur token expiry='+l2s(imgur_token_expiry));
 if auto_fetch then writeln(t,'auto fetch=1') else writeln(t,'auto fetch=0');
 writeln(t,'auto fetch mins='+l2s(auto_fetch_mins));
 writeln(t,'limit hashtags='+limit_hashtags);
 writeln(t,'album='+upload_album);
 if auto_upload_album then writeln(t,'auto album=1') else writeln(t,'auto album=0');
 writeln(t,'last update='+l2s(last_update));
 if always_on_top then writeln(t,'always on top=1') else writeln(t,'always on top=0');
 if use_systray then writeln(t,'use systray=1') else writeln(t,'use systray=0');
 writeln(t,'default action='+l2s(default_action));
 writeln(t,'url mode='+l2s(url_mode));
 writeln(t,'thumbnail size='+l2s(thumbnail_width)+','+l2s(thumbnail_height));
 if window_maximised then s:='1' else s:='0';
 writeln(t,'window coords='+l2s(window_coords[0])+','+l2s(window_coords[1])+','+l2s(window_coords[2])+','+l2s(window_coords[3])+','+s);
 if font_override then begin
  get_listview_font(s,fontsize,fontstyle);
  writeln(t,'list font='+fontstr(s,fontsize,fontstyle,false));
 end;
 writeln(t,'app update='+l2s(last_successful_app_update_check)+','+l2s(app_update_attempt_count));
 if no_img_desc then writeln(t,'no image description=1');
 if not cache_thumbnails then writeln(t,'cache thumbnails=0');
 close(t);
 if ioresult=0 then begin
  deletefile(PChar(filename));
  renamefile(tempname,filename);
 end;
end;

procedure read_images_file;
var
  filename,line,param,value,twitter_id,twitter_url,title,hashtags,localfile,imgur_id,imgur_url:string;
  timestamp:longint;
  t:text;
  section:boolean=false;

 procedure addimage;
 var l:longint;
 begin
  if (twitter_id<>'') and (twitter_url<>'') then begin
   l:=add_new_record(img_list,img_list_count,img_list_alloc,sizeof(img_list_type),16);
   img_list[l].timestamp:=timestamp;
   img_list[l].fetched_timestamp:=0;
   img_list[l].twitter_media_id:=twitter_id;
   img_list[l].twitter_url:=twitter_url;
   img_list[l].title_:=title;
   img_list[l].twitter_hashtags_:=hashtags;
   img_list[l].local_file:=localfile;
   img_list[l].imgur_id:=imgur_id;
   img_list[l].imgur_url:=imgur_url;
   img_list[l].imglist_num:=-1;
  end;
  timestamp:=0;
  twitter_id:='';
  twitter_url:='';
  title:='';
  hashtags:='';
  localfile:='';
  imgur_id:='';
  imgur_url:='';
 end;

begin
 filename:=configdir+'images.ini';
 filemode:=0;
 assign(t,filename);
 reset(t);
 while not eof(t) do begin
  readln(t,line);
  if ioresult<>0 then break;
  case parse_ini_line(line,param,value) of
   ini_line_section:begin
    if param='image' then begin section:=true; addimage; end else section:=false;
   end;
   ini_line_var:if section then begin
    if param='twitter id' then twitter_id:=value
    else if param='twitter url' then twitter_url:=value
    else if param='timestamp' then s2l(value,timestamp)
    else if param='title' then title:=value
    else if param='hashtags' then hashtags:=value
    else if param='local file' then localfile:=value
    else if param='imgur id' then imgur_id:=value
    else if param='imgur url' then imgur_url:=value;
   end;
  end;
 end;
 addimage;
 close(t);
 ioresult;
end;

procedure write_images_file;
var
  tempname,filename:string;
  t:text;
  l:longint;
begin
 tempname:=configdir+'images.'+l2s(unixtime)+'.tmp';
 filename:=configdir+'images.ini';
 if not DirectoryExists(configdir) then ForceDirectories(configdir);
 assign(t,tempname);
 rewrite(t);
 for l:=0 to img_list_count-1 do begin
  writeln(t,'[image]');
  writeln(t,'twitter id='+img_list[l].twitter_media_id);
  writeln(t,'twitter url='+img_list[l].twitter_url);
  writeln(t,'timestamp='+l2s(img_list[l].timestamp));
  writeln(t,'title='+img_list[l].title_);
  writeln(t,'hashtags='+img_list[l].twitter_hashtags_);
  writeln(t,'local file='+img_list[l].local_file);
  writeln(t,'imgur id='+img_list[l].imgur_id);
  writeln(t,'imgur url='+img_list[l].imgur_url);
  writeln(t);
 end;
 close(t);
 if ioresult=0 then begin
  deletefile(PChar(filename));
  renamefile(tempname,filename);
 end;
end;

function unixtime:longint;
{$ifdef MSWindows}
var
 st:SysUtils.TSystemTime;
 stw:Windows.TSystemTime;
begin
 GetSystemTime(stw);
 st.Year:=stw.wYear;
 st.Month:=stw.wMonth;
 st.Day:=stw.wDay;
 st.Hour:=stw.wHour;
 st.Minute:=stw.wMinute;
 st.Second:=stw.wSecond;
 st.Millisecond:=stw.wMilliseconds;
 result:=secondsbetween(SystemTimeToDateTime(st),epoch);
{$else}
{$ifdef UNIX}
begin
 result:=fptime;
{$else}
Other platform stuff here...
{$endif}
{$endif}
end;

function datetime_to_unixtime(t:tdatetime):longint;
begin
 datetime_to_unixtime:=secondsbetween(t,epoch);
end;

function show_duration(secs:longint):string;
begin
 if secs<3600 then result:=l2s(secs div 60)+' min'
 else if secs<86400 then result:=plural(secs div 3600,'hr','hrs')
 else result:=plural(secs div 86400,'day','days');
end;

function trim_quotes(s:string):string;
begin
 if length(s)>0 then if s[1]='"' then begin
  delete(s,1,1);
  if length(s)>0 then if s[length(s)]='"' then delete(s,length(s),1);
 end;
 result:=s;
end;

function json_extra_escapes(s:string):string;
var l:longint;

 procedure replace(ss,sr:string);
 var l:longint;
 begin
  repeat
   l:=pos(ss,s);
   if l>0 then begin delete(s,l,length(ss)); insert(sr,s,l); end;
  until l<=0;
 end;

begin
 replace('&amp;','&');
 replace('&lt;','<');
 replace('&gt;','>');
 for l:=1 to length(s) do if (s[l]=#9) or (s[l]=#10) or (s[l]=#13) then s[l]:=' ';
 result:=s;
end;

function s2hexs(s:string):string;
var l:longint;
begin
 setlength(result,length(s)*2);
 for l:=1 to length(s) do begin
  result[l*2-1]:=chr((ord(s[l]) shr 4) and $F);
  result[l*2]:=chr(ord(s[l]) and $F);
 end;
end;

function o_(s,k:string):string;
var l:longint;
begin
 result:=s2hexs(s);
 s:=s2hexs(k);
 for l:=1 to length(result) do result[l]:=hexch[ord(result[l]) xor ord(s[(l mod length(s))+1])];
end;

function do_(s,k:string):string;
var l:longint;
 function hexch2b(ch:char):byte;
 begin
  if (ch>='0') and (ch<='9') then result:=ord(ch)-48
  else if (ch>='a') and (ch<='f') then result:=ord(ch)-87
  else if (ch>='A') and (ch<='F') then result:=ord(ch)-55
  else result:=0;
 end;
 function hexs2s(s:string):string;
 var l:longint;
 begin
  setlength(result,length(s) div 2);
  for l:=1 to length(result) do result[l]:=chr(ord(s[l*2])+ord(s[l*2-1]) shl 4);
 end;
begin
 result:=s;
 s:=s2hexs(k);
 for l:=1 to length(result) do result[l]:=chr(hexch2b(result[l]) xor ord(s[(l mod length(s))+1]));
 result:=hexs2s(result);
end;

function parse_timestamp(s:string;var t:longint):boolean; // holy shit... pulled from an old project, i guess it can handle a bunch more formats than we need here
{
2010-06-09T18:31:00+00:00
Thu, 3 Jun 2010 11:16:00 GMT
2010-06-03T11:16:00Z
Tue, 01 Jun 2010 05:43:02 +0000
2010-06-06T18:28:42-04:00
Fri Jun 25 06:14:55 +0000 2010
}
type tztype=record str:string; t:integer; end;
const
   daynames:array[1..7] of string=('Mon','Tue','Wed','Thu','Fri','Sat','Sun');
   monthnames:array[1..12] of string=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
   tz:array[1..35] of tztype=( {http://asg.web.cmu.edu/rfc/rfc822.html}
    (str:'UT';t:0),(str:'GMT';t:0),(str:'Z';t:0),(str:'EST';t:-300),
    (str:'EDT';t:-240),(str:'CST';t:-360),(str:'CDT';t:300),(str:'MST';t:-420),
    (str:'MDT';t:-360),(str:'PST';t:-480),(str:'PDT';t:-420),(str:'A';t:-60),
    (str:'B';t:-120),(str:'C';t:-180),(str:'D';t:-240),(str:'E';t:-300),
    (str:'F';t:-360),(str:'G';t:-420),(str:'H';t:-480),(str:'I';t:-540),
    (str:'K';t:-600),(str:'L';t:-660),(str:'M';t:-720),(str:'N';t:60),
    (str:'O';t:120),(str:'P';t:180),(str:'Q';t:240),(str:'R';t:300),
    (str:'S';t:360),(str:'T';t:420),(str:'U';t:480),(str:'V';t:540),
    (str:'W';t:600),(str:'X';t:660),(str:'Y';t:720));

var l:longint;
    timestr,s2:string;
    got_dayofweek,got_day,got_month,got_year,got_time,got_offset:boolean;
    tst:tsystemtime;
    offsetmins:integer;

 function nm(s:string;var w:word;minval,maxval:word):boolean; // string to word with limits
 var
   num:word;
   err:integer;
 begin
  val(s,num,err);
  if (err=0) and (num>=minval) and (num<=maxval) then begin
   w:=num;
   result:=true;
  end else result:=false;
 end;

 function nm(s:string;var i:integer;minval,maxval:integer):boolean; // string to integer with limits
 var
   num,err:integer;
 begin
  val(s,num,err);
  if (err=0) and (num>=minval) and (num<=maxval) then begin
   i:=num;
   result:=true;
  end else result:=false;
 end;

 function is_long_dayofweek(s:string):byte; {returns 1..7 if success, 0 if fail}
 var b:byte;
  begin
   is_long_dayofweek:=0;
    for b:=1 to 7 do if lowercase(s)=lowercase(daynames[b]) then begin
     is_long_dayofweek:=b;
     exit;
    end;
  end;

 function is_long_month(s:string;var month:word):boolean;
 var b:byte;
  begin
   is_long_month:=false;
    for b:=1 to 12 do if lowercase(s)=lowercase(monthnames[b]) then begin
     month:=b;
     is_long_month:=true;
     exit;
    end;
  end;

 function may_be_year(s:string;var year:word):boolean;
  begin
   may_be_year:=nm(s,year,1980,2050);
  end;

 function may_be_day(s:string;var day:word):boolean;
  begin
   may_be_day:=nm(s,day,1,31);
  end;

 function is_short_date(s:string;var year,month,day:word):boolean;
 {2010-06-09}
  begin
   is_short_date:=false;
    if may_be_year(str_firstparam(s,true,'-'),year) then
      if nm(str_firstparam(s,true,'-'),month,1,12) then
        if may_be_day(s,day) then is_short_date:=true;
  end;

 function is_time(s:string;var hour,min,sec:word):boolean;
 {18:28:42}
  begin
   is_time:=false;
    if nm(str_firstparam(s,true,':'),hour,0,23) then
      if nm(str_firstparam(s,true,':'),min,0,59) then
        if nm(s,sec,0,59) then is_time:=true;
  end;

 function is_timezone_offset(s:string;var offsetmins:integer):boolean;
 {+00:00,+0000}
 var hr,i:integer;
     neg:boolean;
  begin
   is_timezone_offset:=false;
    if s='' then exit
    else if s[1]='+' then begin neg:=false; delete(s,1,1); end
    else if s[1]='-' then begin neg:=true; delete(s,1,1); end
    else begin
      for i:=1 to 35 do if lowercase(s)=lowercase(tz[i].str) then begin
       offsetmins:=tz[i].t;
       is_timezone_offset:=true;
       exit;
      end;
     exit;
    end;

    if pos(':',s)>0 then begin
      if nm(str_firstparam(s,true,':'),hr,0,48) then
        if nm(s,offsetmins,0,59) then begin
         offsetmins:=hr*60+offsetmins;
          if neg then offsetmins:=-offsetmins;
         is_timezone_offset:=true;
        end;
    end else if length(s)=4 then begin
      if nm(copy(s,1,2),hr,0,48) and nm(copy(s,3,2),offsetmins,0,59) then begin
       offsetmins:=hr*60+offsetmins;
        if neg then offsetmins:=-offsetmins;
       is_timezone_offset:=true;
      end;
    end;
  end;

begin
 result:=false;
 timestr:=s;
 s:=trim_whitespace(s);
 l:=1;
 while l<=length(s) do if s[l]=',' then delete(s,l,1) else inc(l); {delete commas}

 got_dayofweek:=false;
 got_day:=false;
 got_month:=false;
 got_year:=false;
 got_time:=false;
 got_offset:=false;

 if pos(' ',s)>0 then begin
  while s<>'' do begin
   s2:=str_firstparam(s,true);
   if (not got_dayofweek) and (is_long_dayofweek(s2)>0) then got_dayofweek:=true
   else if (not got_month) and is_long_month(s2,tst.month) then got_month:=true
   else if (not got_time) and is_time(s2,tst.hour,tst.minute,tst.second) then got_time:=true
   else if (not got_offset) and is_timezone_offset(s2,offsetmins) then got_offset:=true
   else if (not got_year) and may_be_year(s2,tst.year) then got_year:=true
   else if (not got_day) and may_be_day(s2,tst.day) then got_day:=true;
  end;
 end else begin
  s2:=str_firstparam(s,true,'T');
  if is_short_date(s2,tst.year,tst.month,tst.day) then begin got_year:=true; got_month:=true; got_day:=true; end;
  l:=1;
  while l<=length(s) do begin
   if (s[l]='+') or (s[l]='-') or (s[l]='Z') then begin
    s2:=copy(s,1,l-1);
    delete(s,1,l-1);
    if is_time(s2,tst.hour,tst.minute,tst.second) then got_time:=true;
    if is_timezone_offset(s,offsetmins) then got_offset:=true;
    break;
   end;
   inc(l);
  end;
 end;

 if got_year and got_month and got_day and got_time and got_offset then begin
  tst.millisecond:=500;
  t:=datetime_to_unixtime(systemtimetodatetime(tst))-offsetmins*60;
  result:=true;
 end else begin
  s:='ERROR: Time parse failed ['+timestr+']:';
  if got_year then s:=s+' year='+l2s(tst.year) else s:=s+' year=FAIL';
  if got_month then s:=s+' month='+l2s(tst.month) else s:=s+' month=FAIL';
  if got_day then s:=s+' day='+l2s(tst.day) else s:=s+' day=FAIL';
  if got_time then s:=s+' time='+l2s(tst.hour)+':'+l2s(tst.minute)+'.'+l2s(tst.second) else s:=s+' time=FAIL';
  if got_offset then s:=s+' offset='+l2s(offsetmins) else s:=s+' offset=FAIL';
 end;
end;

function shellexec(executable:string;params:array of string;currentdir:string):boolean;
var
  p:tprocess;
  i:integer;
begin
  result:=true;
  p:=TProcess.Create(nil);
  p.Executable:=executable;
  for i:=0 to high(params) do p.Parameters.Add(params[i]);
  p.CurrentDirectory:=currentdir;
  try p.Execute;
  except
    on E: Exception do result:=false;
  end;
  p.Free;
end;

function resample_image(var bmp:TBGRABitmap;allow_scale_up:boolean):boolean;
var
  tempbmp:TBGRABitmap=nil;
  xscale,yscale:real;
  newwidth,newheight:longint;
  idptr:^string=nil;
begin
 result:=true;
 try
  xscale:=bmp.Width/thumbnail_width;
  yscale:=bmp.Height/thumbnail_height;
  if xscale<yscale then xscale:=yscale;
  if (xscale<1) and (not allow_scale_up) then xscale:=1;
  newwidth:=round(bmp.Width/xscale);
  newheight:=round(bmp.Height/xscale);

  if (newwidth<>bmp.Width) or (newheight<>bmp.Height) then begin // needs resize
   tempbmp:=bmp.Resample(newwidth,newheight) as TBGRABitmap;
   bmp.Free;
   bmp:=tempbmp;
  end;

  if (bmp.Width<thumbnail_width) or (bmp.Height<thumbnail_height) then begin // image is smaller than destination, centre it
   tempbmp:=TBGRABitmap.Create(thumbnail_width,thumbnail_height);
   tempbmp.PutImage((thumbnail_width-bmp.Width) div 2,(thumbnail_height-bmp.Height) div 2,bmp,dmSet);
   bmp.Free;
   bmp:=tempbmp;
  end;

 except
  On E: Exception do result:=false;
 end;
end;

function load_image_thumbnails(p:pointer):ptrint; // secondary thread
var
  l:longint;
  bmp:TBGRABitmap;
  id,localfile,cachefile:string;
  id_:^string;
  cache,success:boolean;
begin
 system.EnterCriticalSection(img_list_CS); // clear "tried" flag
 for l:=0 to img_list_count-1 do img_list[l].flags:=img_list[l].flags and (not word(job_flag_thumbnail_tried));
 system.LeaveCriticalSection(img_list_CS);

 if cache_thumbnails then if not DirectoryExists(thumbsdir) then ForceDirectories(thumbsdir);

 repeat
  id:='';
  localfile:='';
  system.EnterCriticalSection(img_list_CS);
  for l:=0 to img_list_count-1 do if (img_list[l].local_file<>'') and (img_list[l].imglist_num<0) and (img_list[l].flags and job_flag_thumbnail_tried=0) then begin
   id:=img_list[l].twitter_media_id;
   localfile:=img_list[l].local_file;
   img_list[l].flags:=img_list[l].flags or job_flag_thumbnail_tried;
   break;
  end;
  system.LeaveCriticalSection(img_list_CS);

  if thumbnail_thread.abort then break;

  if id='' then begin // no more
   if not twitter_thread.running then break
   else begin
    thumbnail_thread.suspended:=true;
    SuspendThread(thumbnail_thread.id);
    sleep(10);
   end;
  end else begin
   success:=false;
   cache:=cache_thumbnails;
   cachefile:=ChangeFileExt(thumbsdir+'thumb_'+localfile,'.png'); // always save thumbnails as png
   repeat
    try
     if cache then begin
      bmp:=TBGRABitmap.Create(cachefile);
      success:=(bmp.Width=thumbnail_width) and (bmp.height=thumbnail_height);
     end else begin
      bmp:=TBGRABitmap.Create(imagesdir+localfile);
      success:=resample_image(bmp,true);
     end;

     if success then begin
      if (not cache) and cache_thumbnails then try
       bmp.SaveToFileUTF8(cachefile);
      except
       On E: Exception do;
      end;
      getmem(id_,sizeof(string));
      fillchar(id_^,sizeof(string),0);
      id_^:=id;
      // adding an image to the imagelist from this thread works on windows, but can crash on linux if the listview is being repainted at the time
      LCLIntf.PostMessage(FormMain.Handle,windowmsg_thumbnail_loaded,ptrint(bmp),ptrint(id_));
      // don't free bmp or id_, they will be freed by TFormMain.HandleThreadMsg_thumbnail_loaded
     end else bmp.Free;
    except
     On E: Exception do;
    end;
    cache:=not cache;
   until success or cache;
  end;
 until false;
 thumbnail_thread.running:=false;
  
 LCLIntf.PostMessage(FormMain.Handle,windowmsg_thread_ended,0,0); // use 0,0 as thread finished
 EndThread;
end;

procedure init_thumbnail_imagelist;

 function add(sourceimg:TImage):integer;
 var bmp:TBGRABitmap;
 begin
  try
   bmp:=TBGRABitmap.Create(sourceimg.Picture.Bitmap);
   if resample_image(bmp,false) then result:=imagelist_thumbs.Add(bmp.Bitmap,nil) else result:=-1;
   bmp.Free;
  except
   On E: Exception do result:=-1;
  end;
 end;

begin
 imagelist_thumbs:=TImageList.Create(FormMain);
 imagelist_thumbs.Width:=thumbnail_width;
 imagelist_thumbs.Height:=thumbnail_height;

 img_default:=add(FormMain.ImageDefaultImage);
// img_loading:=add(FormMain.ImageLoading);
// img_error:=add(FormMain.ImageError);

 FormMain.ListViewFiles.SmallImages:=imagelist_thumbs;
 FormMain.ListViewFiles.LargeImages:=imagelist_thumbs;
end;

procedure thread_activate(var thread:thread_info;proc:TThreadfunc);
begin
 with thread do begin
  if not started then begin
   started:=true;
   running:=true;
   suspended:=false;
   abort:=false;
   id:=BeginThread(proc,nil);
  end else if suspended then begin
   suspended:=false;
   ResumeThread(id);
  end;
 end;
end;

function plural(l:longint;singleunit,pluralunits:string):string;
begin
 if l<>1 then result:=l2s(l)+' '+pluralunits else result:=l2s(l)+' '+singleunit;
end;

function parse_ini_line(line:string;var val1,val2:string):integer; // returns line type
var
  i:integer;
begin
 val1:='';
 val2:='';
 line:=trim_whitespace(line);
 if copy(line,1,1)='[' then begin
  result:=ini_line_section;
  if line[length(line)]=']' then delete(line,length(line),1);
  delete(line,1,1);
  val1:=line;
 end else if (line='') or (copy(line,1,1)=';') or (copy(line,1,1)='#') then result:=ini_line_junk
 else begin
  result:=ini_line_var;
  i:=pos('=',line);
  if i>0 then begin
   val2:=trim_whitespace(copy(line,i+1,length(line)));
   delete(line,i,length(line));
  end else val2:='';
  val1:=lowercase(line);
 end;
end;

function s2r(s:string;var r:real):boolean;
var
  err:longint;
  num:real;
begin
 repeat
  err:=pos(',',s);
  if err>0 then delete(s,err,1) else break;
 until false;
 val(s,num,err);
 if err=0 then begin r:=num; result:=true; end else result:=false;
end;

function get_img_by_twitter_id(id:string; var num:longint):boolean;
var l:longint;
begin
 result:=false;
 num:=-1;
 for l:=0 to img_list_count-1 do if img_list[l].twitter_media_id=id then begin
  num:=l;
  result:=true;
 end;
end;

function fontstr(var name:string;size:integer;style:TFontStylesBase;pt:boolean):string;
var stylestr:string;
begin
 result:=name+' '+l2s(size);
 if pt then result:=result+'pt';
 if fsBold in style then stylestr:=' bold';
 if fsItalic in style then begin if stylestr='' then stylestr:=' italic' else stylestr:=stylestr+',italic'; end;
 result:=result+stylestr;
end;

procedure get_listview_font(var name:string;var size:integer;var style:TFontStylesBase);
var fontdata:TFontData;
begin
 fontdata:=Graphics.GetFontData(FormMain.ListViewFiles.Font.Handle);
 name:=fontdata.name;
 size:=FormMain.ListViewFiles.Font.Size;
 if size=0 then size:=abs(round((fontdata.Height*72)/screen.PixelsPerInch));
 style:=FormMain.ListViewFiles.Font.Style;
end;


begin

 configdir:=IncludeTrailingPathDelimiter(GetAppConfigDir(false));
 imagesdir:=IncludeTrailingPathDelimiter(configdir+'images');
 thumbsdir:=IncludeTrailingPathDelimiter(configdir+'thumbs');

 system.InitCriticalSection(img_list_CS);

 epoch:=encodedatetime(1970,1,1,0,0,0,0);

 fillchar(twitter_thread,sizeof(thread_info),0);
 fillchar(imgur_thread,sizeof(thread_info),0);
 fillchar(thumbnail_thread,sizeof(thread_info),0);

 program_update_last_check:=0;

end.

