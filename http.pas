// Copyright 2014-2017 Dr C (drcpsn@hotmail.com | https://twitter2imgur.github.io/twitter2imgur/)
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

unit http;

{$mode objfpc}{$H+}{$I-}
{$define deflate}
//{$define directupload} // do we pass twitter image url directly to imgur, or upload our own local file

interface

uses
  Classes, SysUtils, ssl_openssl, synautil, synacode;

function twitter_authorise:boolean;
function imgur_authorise:boolean;
function check_for_app_updates(p:pointer):ptrint;
function fetch_tweets(var info:string;var newcount:integer):boolean;
function do_imgur_jobs(p:pointer):ptrint;
procedure clear_imgur_albums;
function delete_imgur_image(id,deletehash:string):boolean;
function refresh_imgur_token:boolean;
function fetch_page(url,httpmethod:string;var pagedata,info,http_if_modified_since,http_if_none_match:string;var page_not_modified:boolean;ttl:integer;extra_headers:string;var compressedsize:longint;postdata,postencoding:string;onstatus_type:integer):boolean;
function fetch_page(url,httpmethod:string;var pagedata,info:string;extra_headers,postdata,postencoding:string;onstatus_type:integer):boolean;
procedure parse_json_node(data:string;keypath:string;var sl:TStringList);
procedure do_twitter_jobs;
function do_job_thread(p:pointer):ptrint;

implementation

uses
  unitmain, httpsend, misc, oauth, dialogs, controls, lclintf, jsondecode, lazutf8
  {$ifdef deflate},zlibinflate{$endif}
  ;

const
{$I 'twitter2imgur.keys'}
// define your own twitter and imgur api keys and secret keys in external file twitter2imgur.keys. eg:
// twitter_api_key = 'foo';
// twitter_api_secret = 'bar';
// imgur_api_key = 'baz';
// imgur_api_secret = 'blah';

function get_json_param(var sl:TStringList;param:string;var value:string;rangestart,rangeend:longint; var pos:longint):boolean;
var
  l:longint;
  p,v:string;
begin
 result:=false;
 pos:=rangestart;
 value:='';
 for l:=rangestart to rangeend do begin
  v:=sl[l];
  p:=str_firstparam(v,true,'=');
  if lowercase(p)=lowercase(param) then begin
   value:=v;
   pos:=l;
   result:=true;
   break;
  end;
 end;
end;

function get_json_param(var sl:TStringList;param:string;var value:string;rangestart,rangeend:longint):boolean; inline;
var pos:longint;
begin
 result:=get_json_param(sl,param,value,rangestart,rangeend,pos);
end;

function get_json_param(var sl:TStringList;param:string;var value:string):boolean;
begin
 result:=get_json_param(sl,param,value,0,sl.Count-1);
end;

function twitter_authorise:boolean;
var
  access_token,access_token_secret,oauth_headers,pagedata,param,value,pin,url,info:string;
  new_token,new_token_secret,new_screenname,new_userid,api_secret:string;
begin
 result:=false;
 {$ifndef nonplainkeys}api_secret:=twitter_api_secret;{$else}api_secret:=do_(twitter_api_secret_o,twitter_api_secret_k);{$endif}
 url:='https://api.twitter.com/oauth/request_token';
 oauth_headers:=generate_oauth_auth_header('GET',url,'',twitter_api_key,api_secret,'','');
 if fetch_page(url,'GET',pagedata,info,oauth_headers,'','',0) then begin
  while pagedata<>'' do begin
   value:=str_firstparam(pagedata,true,'&');
   param:=lowercase(str_firstparam(value,true,'='));
   if param='oauth_token' then access_token:=value
   else if param='oauth_token_secret' then access_token_secret:=value;
  end;

  if MessageDlg('Information','In order to fetch your images, you''ll need to grant this app permission to use your Twitter account (read-only).'#13#13'You''ll now be taken to the Twitter authorization page in your browser so you can authorize it.',mtInformation,[mbOK],0)<>mrOK then exit;

  url:='https://api.twitter.com/oauth/authorize?oauth_token='+access_token;
  OpenURL(url);
  if not InputQuery('Twitter Authorization','After authorizing this app to use your Twitter account, enter the PIN here.'
  +#13#13'Enter PIN:',pin) then exit;

  url:='https://api.twitter.com/oauth/access_token';
  oauth_headers:=generate_oauth_auth_header('GET',url,'',twitter_api_key,api_secret,access_token,access_token_secret)+', oauth_verifier='+pin;
  if fetch_page(url,'GET',pagedata,info,oauth_headers,'','',0) then begin // success
   while pagedata<>'' do begin
    value:=str_firstparam(pagedata,true,'&');
    param:=lowercase(str_firstparam(value,true,'='));
    if param='oauth_token' then new_token:=value
    else if param='oauth_token_secret' then new_token_secret:=value
    else if param='screen_name' then new_screenname:=value
    else if param='user_id' then new_userid:=value;
   end;
   if (new_token<>'') and (new_token_secret<>'') and (new_screenname<>'') and (new_userid<>'') then begin
    result:=true;
    twitter_token:=new_token;
    twitter_token_secret:=new_token_secret;
    twitter_screenname:=new_screenname;
   end else info:='Couldn''t get a proper authorization response.';
  end;
 end;

 if not result then MessageDlg('Account authorization failed',info,mtError,[mbOK],0)
 else MessageDlg('Information','Authorization successful! Now using Twitter account "'+twitter_screenname+'".',mtInformation,[mbOK],0);
end;

function imgur_authorise:boolean; // not threadsafe
var
  url,pin,pagedata,postparams,keypath,info:string;
  sl:TStringList;
  new_token,new_refresh_token,new_screenname,new_expires_in,api_secret:string;
begin
 result:=false;
 if MessageDlg('Information','In order to upload non-anonymous images, you''ll need to grant this app permission to use your Imgur account.'#13#13'You''ll now be taken to the Imgur authorization page in your browser so you can authorize it.',mtInformation,[mbOK],0)<>mrOK then exit;

 {$ifndef nonplainkeys}api_secret:=imgur_api_secret;{$else}api_secret:=do_(imgur_api_secret_o,imgur_api_secret_k);{$endif}
 url:='https://api.imgur.com/oauth2/authorize?response_type=pin&client_id='+imgur_api_key;
 OpenURL(url);
 if not InputQuery('Imgur Authorization','After authorizing this app to use your Imgur account, enter the PIN here.'
 +#13#13'Enter PIN:',pin) then exit;

 postparams:='client_id='+imgur_api_key+'&client_secret='+api_secret+'&grant_type=pin&pin='+pin;
 if fetch_page('https://api.imgur.com/oauth2/token','POST',pagedata,info,'',postparams,'application/x-www-form-urlencoded',0) then begin // success
  sl:=TStringList.Create;
  parse_json_node(pagedata,keypath,sl);
  info:='Couldn''t get a proper authorization response.';
  if get_json_param(sl,'/access_token',new_token) then
   if get_json_param(sl,'/refresh_token',new_refresh_token) then
    if get_json_param(sl,'/account_username',new_screenname) then
     if get_json_param(sl,'/expires_in',new_expires_in) then
      if s2l(new_expires_in,imgur_token_expiry) then begin
       result:=true;
       inc(imgur_token_expiry,unixtime-60);
       imgur_access_token:=new_token;
       imgur_refresh_token:=new_refresh_token;
       imgur_screenname:=new_screenname;
      end;
  sl.Free;
 end;

 if not result then MessageDlg('Account authorization failed',info,mtError,[mbOK],0)
 else MessageDlg('Information','Authorization successful! Now using Imgur account "'+imgur_screenname+'".',mtInformation,[mbOK],0);
end;

function check_for_app_updates(p:pointer):ptrint;
const
  update_url_count=4;
  update_urls:array[0..update_url_count-1] of string=('http://tiny.cc/twitter2imgurupdate','http://tiny.cc/twitter2imgurupdate2','http://bit.do/twitter2imgurupdate','https://bit.do/twitter2imgurupdate2');
var
  tried:array[0..update_url_count-1] of boolean;
  i,i2:integer;
  pagedata,info,line,val1,val2:string;
  datapos:longint;
  section:boolean;
  success:boolean=false;

 function readln_s(var s:string):boolean;
 var
   l:longint;
 begin
  result:=false;
  for l:=datapos to length(pagedata) do begin
   if (pagedata[l]=#13) or (pagedata[l]=#10) then begin
    s:=copy(pagedata,datapos,l-datapos);
    datapos:=l+1;
    if pagedata[l]=#13 then if datapos<=length(pagedata) then if pagedata[datapos]=#10 then inc(datapos);
    result:=true;
    break;
   end else if l=length(pagedata) then begin
    s:=copy(pagedata,datapos,l+1-datapos);
    datapos:=length(pagedata)+1;
    result:=true;
    break;
   end;
  end;
 end;

begin
 result:=0;
 inc(app_update_attempt_count);
 program_update_last_check:=unixtime;
 fillchar(tried,sizeof(tried),0);
 for i:=0 to update_url_count-1 do begin
  if update_thread.abort then break;
  i2:=random(update_url_count-i);
  while tried[i2] do begin inc(i2); if i2>=update_url_count then i2:=0; end;
  tried[i2]:=true;
  if fetch_page(update_urls[i2],'GET',pagedata,info,'','','',0) then begin
   datapos:=1;
   section:=false;
   program_update_latestver:=-1;
   program_update_msg:='An application update is available. Would you like to download it now?';
   program_update_url:=app_download_url;
   while readln_s(line) do begin
    case parse_ini_line(line,val1,val2) of
     ini_line_section:section:=val1='twitter2imgur update';
     ini_line_var:begin
      if section then begin
       if val1='latest version' then success:=s2r(val2,program_update_latestver)
       else if val1='msg' then program_update_msg:=val2
       else if val1='url' then program_update_url:=val2;
      end;
     end;
    end;
   end;
   if success then begin
    last_app_update_check:=unixtime;
    if round(program_update_latestver*1000)>round(app_version*1000) then show_update_notification:=true;
   end;
  end;
  if success then begin
   last_successful_app_update_check:=unixtime;
   app_update_attempt_count:=0;
   break;
  end;
 end;
 update_thread.running:=false;
 LCLIntf.PostMessage(FormMain.Handle,windowmsg_thread_ended,0,0);
 EndThread;
end;

function fetch_tweets(var info:string;var newcount:integer):boolean;
var
  url,pagedata,oauth_headers,keypath,twitter_media_id,twitter_url,hashtags,api_secret,title,s:string;
  sl:TStringList;
  startpoint,endpoint,timestamp,l,mediapoint:longint;
  isnew,match:boolean;

  new_img_list:^img_list_type=nil;
  new_img_list_count:longint=0;
  new_img_list_alloc:longint=0;

 function get_next_tweet:boolean;
 var param,value:string;
 begin
  result:=false;
  startpoint:=endpoint;
  while startpoint<sl.Count-1 do begin
   inc(startpoint);
   value:=sl[startpoint];
   param:=lowercase(str_firstparam(value,true,'='));
   if param='/created_at' then begin
    result:=true;
    endpoint:=startpoint;
    while endpoint<sl.Count-1 do begin
     inc(endpoint);
     value:=sl[endpoint];
     param:=lowercase(str_firstparam(value,true,'='));
     if param='/created_at' then begin
      dec(endpoint);
      exit;
     end;
    end;
   end;
   exit;
  end;
 end;

 function get_next_media(var id,url:string):boolean;
 var param,value:string;
 begin
  result:=false;
  id:='';
  url:='';
  while mediapoint<endpoint do begin
   inc(mediapoint);
   value:=sl[mediapoint];
   param:=lowercase(str_firstparam(value,true,'='));
   if param='/extended_entities/media/id' then begin
    if id='' then id:=value else break; // 2 IDs without a URL
   end else if param='/extended_entities/media/media_url' then begin
    if id<>'' then begin // id and url, success
     url:=value;
     result:=true;
     break;
    end else break; // URL without an ID
   end;
  end;
 end;

 function get_title:string;
 var titlestart,titleend,l,l2:longint;
     s:string;
 begin
  titlestart:=startpoint;
  titleend:=endpoint;
  if get_json_param(sl,'/full_text',result,titlestart,titleend) then begin // got it
   if get_json_param(sl,'/entities/media/indices',s,titlestart,titleend,titlestart) then if s2l(s,l) then begin
    inc(titlestart);
    if get_json_param(sl,'/entities/media/indices',s,titlestart,titleend) then if s2l(s,l2) then
     if (l>=0) and (l2>=0) and (l<UTF8Length(result)) and (l2<=UTF8Length(result)) and (l<l2) then UTF8Delete(result,l+1,l2-l); // remove "media indices" (pic url in the tweet text)
   end;

   result:=trim_whitespace(result);
  end;
 end;

 function hashtag_match(tag:string):boolean;
 var list,tag2:string;
 begin
  if limit_hashtags='' then result:=true
  else if tag='' then result:=false
  else begin
   result:=false;
   tag:=lowercase(tag);
   list:=limit_hashtags;
   while list<>'' do begin
    tag2:=lowercase(str_firstparam(list,true));
    if copy(tag2,1,1)='#' then delete(tag2,1,1);
    if tag=tag2 then begin result:=true; exit; end;
   end;
  end;
 end;

 procedure copy_img_data(var source,dest:img_list_type;clear:boolean);
 begin
  dest.timestamp:=source.timestamp;
  dest.fetched_timestamp:=source.fetched_timestamp;
  dest.imglist_num:=source.imglist_num;
  dest.listview_index:=source.listview_index;
  dest.twitter_media_id:=source.twitter_media_id;
  dest.twitter_url:=source.twitter_url;
  dest.title_:=source.title_;
  dest.twitter_hashtags_:=source.twitter_hashtags_;
  dest.local_file:=source.local_file;
  dest.imgur_id:=source.imgur_id;
  dest.imgur_url:=source.imgur_url;
  dest.imgur_deletehash:=source.imgur_deletehash;
  dest.errorinfo:=source.errorinfo;
  dest.flags:=source.flags;
  dest.deleted:=source.deleted;
  if clear then begin
   source.timestamp:=0;
   source.fetched_timestamp:=0;
   source.imglist_num:=-1;
   source.listview_index:=-1;
   source.twitter_media_id:='';
   source.twitter_url:='';
   source.title_:='';
   source.twitter_hashtags_:='';
   source.local_file:='';
   source.imgur_id:='';
   source.imgur_url:='';
   source.imgur_deletehash:='';
   source.errorinfo:='';
   source.flags:=0;
   source.deleted:=false;
  end;
 end;

begin
 newcount:=0;
 url:='https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name='+urlencode_oauth_safe(twitter_screenname)+'&trim_user=1&tweet_mode=extended';
 {$ifndef nonplainkeys}api_secret:=twitter_api_secret;{$else}api_secret:=do_(twitter_api_secret_o,twitter_api_secret_k);{$endif}
 oauth_headers:=generate_oauth_auth_header('GET',url,'',twitter_api_key,api_secret,twitter_token,twitter_token_secret);
 result:=fetch_page(url,'GET',pagedata,info,oauth_headers,'','',0);
 if result then begin
  sl:=TStringList.Create;
  parse_json_node(pagedata,keypath,sl);

  startpoint:=-1;
  endpoint:=-1;
  repeat
   if not get_next_tweet then break;

   hashtags:=''; // check for hashtag match
   match:=limit_hashtags='';
   l:=startpoint;
   repeat
    if get_json_param(sl,'/entities/hashtags/text',s,l,endpoint,l) then begin
     inc(l);
     if not match then if hashtag_match(s) then match:=true;
     if hashtags='' then hashtags:=s else hashtags:=hashtags+' '+s;
    end else break;
   until false;

   if match then begin
    title:=get_title;

    if get_json_param(sl,'/created_at',s,startpoint,endpoint) then begin
     if not parse_timestamp(s,timestamp) then timestamp:=unixtime;
    end else timestamp:=unixtime;

    mediapoint:=startpoint;
    repeat // tweets can have more than one pic
     if get_next_media(twitter_media_id,twitter_url) then begin // found a pic
      isnew:=true;
      for l:=0 to img_list_count-1 do if twitter_media_id=img_list[l].twitter_media_id then begin isnew:=false; break; end; // already in
      if isnew then begin
       l:=add_new_record(new_img_list,new_img_list_count,new_img_list_alloc,sizeof(img_list_type),16);
       new_img_list[l].timestamp:=timestamp;
       new_img_list[l].fetched_timestamp:=unixtime;
       new_img_list[l].twitter_media_id:=twitter_media_id;
       new_img_list[l].twitter_url:=twitter_url;
       new_img_list[l].title_:=title;
       new_img_list[l].twitter_hashtags_:=hashtags;
       new_img_list[l].imglist_num:=-1;
       new_img_list[l].listview_index:=-1;
       new_img_list[l].flags:=job_flag_new;
      end;
     end else break;
    until false;
   end;
  until false;
  sl.Free;

  system.EnterCriticalSection(img_list_CS);
  if new_img_list_count>0 then begin // new images
   newcount:=new_img_list_count;
   for l:=1 to new_img_list_count do add_new_record(img_list,img_list_count,img_list_alloc,sizeof(img_list_type),16); // grow list
   for l:=img_list_count-1-new_img_list_count downto 0 do copy_img_data(img_list[l],img_list[l+new_img_list_count],false); // make space for new entries
   for l:=0 to new_img_list_count-1 do copy_img_data(new_img_list[l],img_list[l],true); // insert new entries
  end;
  system.LeaveCriticalSection(img_list_CS);
  if new_img_list_alloc>0 then freemem(new_img_list,new_img_list_alloc*sizeof(img_list_type));
 end;
end;

function generate_local_filename(twitter_id,twitter_url:string):string;
var l:longint;
begin
 for l:=length(twitter_url) downto 1 do begin // get file extension from url
  if twitter_url[l]='/' then break else if twitter_url[l]='.' then begin result:=copy(twitter_url,l,length(twitter_url)); break; end;
 end;
 result:=twitter_id+result;
end;

procedure do_twitter_jobs;
var
  l:longint;
  pagedata,info,id,url,localfile:string;
  f:file;
begin
 if not DirectoryExists(imagesdir) then forcedirectories(imagesdir);
 repeat
  id:='';
  url:='';
  localfile:='';
  system.EnterCriticalSection(img_list_CS);
  for l:=img_list_count-1 downto 0 do if (img_list[l].twitter_media_id<>'') and (img_list[l].twitter_url<>'') and (img_list[l].local_file='') and (img_list[l].flags and job_flag_twitter_tried=0) then begin
   id:=img_list[l].twitter_media_id;
   url:=img_list[l].twitter_url;
   img_list[l].flags:=img_list[l].flags or job_flag_twitter_tried;
   break;
  end;
  system.LeaveCriticalSection(img_list_CS);
  if id='' then break // no more
  else begin
   if twitter_thread.abort then break;
   if fetch_page(url+':orig','GET',pagedata,info,'','','',0) then begin
    localfile:=generate_local_filename(id,url);
    assign(f,imagesdir+localfile);
    rewrite(f,1);
    if length(pagedata)>0 then blockwrite(f,pagedata[1],length(pagedata));
    close(f);
    system.EnterCriticalSection(img_list_CS);
    if get_img_by_twitter_id(id,l) then begin
     if ioresult=0 then begin
      img_list[l].local_file:=localfile;
      {$ifndef directupload}thread_activate(imgur_thread,@do_imgur_jobs);{$endif}
      thread_activate(thumbnail_thread,@load_image_thumbnails);
     end else begin
      img_list[l].flags:=img_list[l].flags or job_flag_error;
      img_list[l].errorinfo:='Unable to create file '+imagesdir+localfile;
     end;
    end;
    system.LeaveCriticalSection(img_list_CS);
   end;
   inc(progress_jobs_done);
  end;
 until false;
end;

procedure clear_imgur_albums;
var l:longint;
begin
 for l:=0 to imgur_album_count-1 do begin
  imgur_albums[l].title:='';
  imgur_albums[l].id:='';
 end;
 imgur_album_count:=0;
 imgur_albums_fetched:=false;
end;

function get_imgur_albums:boolean;
var
  pagedata,info,keypath,id,title:string;
  sl:TStringList;
  startpoint,endpoint,l:longint;

 function get_next_album:boolean;
 var param,value:string;
 begin
  result:=false;
  startpoint:=endpoint;
  while startpoint<sl.Count-1 do begin
   inc(startpoint);
   value:=sl[startpoint];
   param:=lowercase(str_firstparam(value,true,'='));
   if param='/data/id' then begin
    result:=true;
    endpoint:=startpoint;
    while endpoint<sl.Count-1 do begin
     inc(endpoint);
     value:=sl[endpoint];
     param:=lowercase(str_firstparam(value,true,'='));
     if param='/data/id' then begin
      dec(endpoint);
      exit;
     end;
    end;
   end;
   exit;
  end;
 end;

begin
 result:=false;
 clear_imgur_albums;
 if fetch_page('https://api.imgur.com/3/account/'+imgur_screenname+'/albums','GET',pagedata,info,'Authorization: Bearer '+imgur_access_token,'','',0) then begin
  result:=true;
  imgur_albums_fetched:=true;
  sl:=TStringList.Create;
  parse_json_node(pagedata,keypath,sl);
  startpoint:=-1;
  endpoint:=-1;
  repeat
   if not get_next_album then break;
   if get_json_param(sl,'/data/id',id,startpoint,endpoint) then
    if get_json_param(sl,'/data/title',title,startpoint,endpoint) then begin
     l:=add_new_record(imgur_albums,imgur_album_count,imgur_album_alloc,sizeof(imgur_album_type),8);
     imgur_albums[l].title:=title;
     imgur_albums[l].id:=id;
    end;
  until false;
  sl.Free;
 end;
end;

function create_imgur_album(name:string;var id:string):boolean;
var
  pagedata,info,keypath,s:string;
  sl:TStringList;
  l:longint;
begin
 result:=false;
 if fetch_page('https://api.imgur.com/3/album/','POST',pagedata,info,'Authorization: Bearer '+imgur_access_token,'title='+EncodeUrl(name)+'&privacy=hidden','application/x-www-form-urlencoded',0) then begin
  sl:=TStringList.Create;
  parse_json_node(pagedata,keypath,sl);
  if not get_json_param(sl,'/success',s) then
  else if lowercase(s)<>'true' then
  else if get_json_param(sl,'/data/id',s) then if s<>'' then begin
   id:=s;
   result:=true;
   l:=add_new_record(imgur_albums,imgur_album_count,imgur_album_alloc,sizeof(imgur_album_type),8);
   imgur_albums[l].title:=name;
   imgur_albums[l].id:=id;
  end;
  sl.Free;
 end;
end;

function delete_imgur_image(id,deletehash:string):boolean;
var
  pagedata,info,keypath,s:string;
  sl:TStringList;
begin
 result:=false;
 if deletehash<>'' then result:=fetch_page('https://api.imgur.com/3/image/'+EncodeURLElement(deletehash),'DELETE',pagedata,info,'Authorization: Client-ID '+imgur_api_key,'','',0)
 else if imgur_screenname<>'' then begin
  if unixtime>imgur_token_expiry then if not refresh_imgur_token then exit;
  result:=fetch_page('https://api.imgur.com/3/image/'+EncodeURLElement(id),'DELETE',pagedata,info,'Authorization: Bearer '+imgur_access_token,'','',0);
 end;

 if result then begin
  sl:=TStringList.Create;
  parse_json_node(pagedata,keypath,sl);
  if get_json_param(sl,'/success',s) then result:=lowercase(s)='true' else result:=false;
  sl.Free;
 end;
end;

function do_imgur_jobs(p:pointer):ptrint;
label retry;
var
  l:longint;
  id,twitterurl,localfile,album,albumid,imgurid,imgururl,imgurdeletehash,errormsg,postdata,pagedata,encoding,keypath,s,title,desc,authheader:string;
  sl:TStringList;
  auth_retried,fetchresult,readfileerror,album_fetch_tried:boolean;

{$ifndef directupload}
const
  boundarycharcount=62;
  boundarychars:array[0..boundarycharcount-1] of char=(
  '0','1','2','3','4','5','6','7','8','9',
  'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z');

 function read_file(name:string;var contents,info:string):boolean;
 var
   f:file;
   size:longint;
 begin
  result:=false;
  contents:='';
  info:='';
  assign(f,name);
  filemode:=0;
  reset(f,1);
  if ioresult<>0 then info:='Unable to open '+name
  else begin
   size:=filesize(f);
   if ioresult<>0 then begin close(f); ioresult; info:='Error reading '+name; end
   else begin
    setlength(contents,size);
    blockread(f,postdata[1],size);
    close(f);
    if ioresult<>0 then begin info:='Error reading '+name; contents:=''; end
    else result:=true;
   end;
  end;
 end;
{$endif}

 function get_album_id(name:string;var id:string):boolean;
 var l:longint;
 begin
  id:='';
  result:=false;
  for l:=0 to imgur_album_count-1 do if name=imgur_albums[l].title then begin result:=true; id:=imgur_albums[l].id; exit; end; // try exact match first
  for l:=0 to imgur_album_count-1 do if lowercase(name)=lowercase(imgur_albums[l].title) then begin result:=true; id:=imgur_albums[l].id; exit; end; // case insensitive match second
 end;

begin
 if (imgur_screenname<>'') and (unixtime>imgur_token_expiry) then if not refresh_imgur_token then exit;
 album_fetch_tried:=false;

 repeat
  id:='';
  twitterurl:='';
  localfile:='';
  album:='';
  imgurid:='';
  imgururl:='';
  imgurdeletehash:='';
  system.EnterCriticalSection(img_list_CS);
  for l:=img_list_count-1 downto 0 do if (img_list[l].twitter_media_id<>''){$ifndef directupload} and (img_list[l].local_file<>''){$endif} and (img_list[l].imgur_url='') and (img_list[l].flags and job_flag_imgur_tried=0) then begin
   id:=img_list[l].twitter_media_id;
   twitterurl:=img_list[l].twitter_url;
   localfile:=img_list[l].local_file;
   title:=img_list[l].title_;
   if imgur_screenname='' then album:='' // no album support for anonymous uploads
   else if auto_upload_album then album:=str_firstparam(img_list[l].twitter_hashtags_,false)
   else album:=upload_album;
   img_list[l].flags:=img_list[l].flags or job_flag_imgur_tried;
   break;
  end;
  system.LeaveCriticalSection(img_list_CS);

  if imgur_thread.abort then break;

  if id='' then begin // no more
   if {$ifdef directupload}true{$else}not twitter_thread.running{$endif} then break else begin
    imgur_thread.suspended:=true;
    SuspendThread(imgur_thread.id);
    sleep(10);
   end;
  end else begin
   // album
   albumid:='';
   if album<>'' then begin // try to get an album id
    if not get_album_id(album,albumid) then begin // don't know this album
     if not album_fetch_tried then begin // if we haven't already made an album fetch attempt, try to fetch them
      album_fetch_tried:=true;
      if get_imgur_albums then get_album_id(album,albumid);
     end;
     if (albumid='') and imgur_albums_fetched then create_imgur_album(album,albumid); // still don't know the album, try creating it
    end;
   end;

   errormsg:='';
   readfileerror:=false;
   {$ifdef directupload} // direct upload, use the twitter pic url
   if not no_img_desc then desc:='&description='+EncodeUrl('Uploaded with Twitter2Imgur');
   fetchresult:=true;
   postdata:='image='+EncodeUrl(twitterurl+':orig')+'&type=URL&name='+EncodeUrl(generate_local_filename(id,twitterurl))+'&title='+EncodeUrl(title)+desc;
   if albumid<>'' then postdata:=postdata+'&album='+EncodeUrl(albumid);

   encoding:='application/x-www-form-urlencoded';
   {$else} // use local file
   postdata:='';
   fetchresult:=read_file(imagesdir+localfile,postdata,s);
   if fetchresult then begin

    setlength(encoding,32); // generate a random boundary for post data
    for l:=1 to 16 do encoding[l]:='-';
    for l:=17 to 32 do encoding[l]:=boundarychars[random(boundarycharcount)];

    postdata:=
      '--'+encoding+#13#10'Content-Disposition: form-data; name="image"'#13#10'Content-Type: application/octet-stream'#13#10#13#10+postdata+#13#10
     +'--'+encoding+#13#10'Content-Disposition: form-data; name="type"'#13#10#13#10'file'#13#10
     +'--'+encoding+#13#10'Content-Disposition: form-data; name="name"'#13#10#13#10+localfile+#13#10
     +'--'+encoding+#13#10'Content-Disposition: form-data; name="title"'#13#10#13#10+title+#13#10;
    if not no_img_desc then postdata:=postdata+'--'+encoding+#13#10'Content-Disposition: form-data; name="description"'#13#10#13#10'Uploaded with Twitter2Imgur'#13#10;
    if albumid<>'' then postdata:=postdata+'--'+encoding+#13#10'Content-Disposition: form-data; name="album"'#13#10#13#10+albumid+#13#10;
    postdata:=postdata+'--'+encoding+'--';

    encoding:='multipart/form-data; boundary='+encoding;
   end else begin readfileerror:=true; errormsg:='Unable to read file '+imagesdir+localfile; end;
   {$endif}
   if fetchresult then begin
    auth_retried:=imgur_screenname=''; // only allow auth retry for non-anonymous
    retry:;
    if imgur_thread.abort then break;
    progress_imgur_currentfile_total:=length(postdata)+250; // headers are counted in onstatus, estimate the length here
    if imgur_screenname='' then authheader:='Authorization: Client-ID '+imgur_api_key else authheader:='Authorization: Bearer '+imgur_access_token;
    fetchresult:=fetch_page('https://api.imgur.com/3/image','POST',pagedata,errormsg,authheader,postdata,encoding,2);
    if pagedata<>'' then begin
     sl:=TStringList.Create;
     keypath:='';
     parse_json_node(pagedata,keypath,sl);
     if not fetchresult then begin // failed
      if get_json_param(sl,'/data/error',s) then begin
       if (not auth_retried) and (pos('token',s)>0) then begin // token expired? refresh and retry
        auth_retried:=true;
        if refresh_imgur_token then begin sl.Free; goto retry; end;
       end else errormsg:='Imgur: '+s+' ('+errormsg+')';
      end;
     end else begin
      if (not get_json_param(sl,'/success',s)) or (lowercase(s)<>'true') then begin fetchresult:=false; errormsg:='Invalid response from server'; end
      else begin // success
       get_json_param(sl,'/data/id',imgurid);
       get_json_param(sl,'/data/link',imgururl);
       get_json_param(sl,'/data/deletehash',imgurdeletehash);
      end;
     end;
     sl.Free;
    end else begin fetchresult:=false; errormsg:='Invalid response from server'; end;
   end;

   system.EnterCriticalSection(img_list_CS);
   if get_img_by_twitter_id(id,l) then begin
    if fetchresult then begin
     img_list[l].imgur_id:=imgurid;
     img_list[l].imgur_url:=imgururl;
     img_list[l].imgur_deletehash:=imgurdeletehash;
     img_list[l].fetched_timestamp:=unixtime;
     LCLIntf.PostMessage(FormMain.Handle,windowmsg_update_listview,0,0);
    end else begin
     img_list[l].flags:=img_list[l].flags or job_flag_error;
     img_list[l].errorinfo:=errormsg;
     if readfileerror then img_list[l].local_file:=''; // clear this so it'll be redownloaded next update
    end;
   end;
   system.LeaveCriticalSection(img_list_CS);
  end;
  inc(progress_jobs_done);
  progress_imgur_currentfile_total:=0;
  progress_imgur_currentfile_current:=0;
 until false;
  
 imgur_thread.running:=false;
 LCLIntf.PostMessage(FormMain.Handle,windowmsg_thread_ended,0,0);
 EndThread;
end;

function refresh_imgur_token:boolean;
var
  pagedata,info,keypath,s,api_secret:string;
  sl:TStringList;
begin
 result:=false;
 {$ifndef nonplainkeys}api_secret:=imgur_api_secret;{$else}api_secret:=do_(imgur_api_secret_o,imgur_api_secret_k);{$endif}
 pagedata:='refresh_token='+EncodeUrl(imgur_refresh_token)+'&client_id='+EncodeUrl(imgur_api_key)+'&client_secret='+EncodeUrl(api_secret)+'&grant_type=refresh_token';
 if fetch_page('https://api.imgur.com/oauth2/token','POST',pagedata,info,'Authorization: Bearer '+imgur_access_token,pagedata,'application/x-www-form-urlencoded',0) then begin
  sl:=TStringList.Create;
  parse_json_node(pagedata,keypath,sl);
  if get_json_param(sl,'/access_token',s) then begin
   result:=true;
   imgur_access_token:=s;
   if get_json_param(sl,'/refresh_token',s) then imgur_refresh_token:=s;
   if get_json_param(sl,'/expires_in',s) then if s2l(s,imgur_token_expiry) then inc(imgur_token_expiry,unixtime-60);
  end;
  sl.Free;
 end;
end;

procedure parse_json_node(data:string;keypath:string;var sl:TStringList);
 var
   key,value:string;
   subsl:TStringList;
   i:integer;
 begin
  if JSONDataIsKeyValuePair(data) then begin
   key:=trim_quotes(json_extra_escapes(JSONEscapeDecode(JSONGetKey(data))));
   value:=JSONGetValue(data); // don't escape here, breaks added items that look like a list, eg: "foo  :)"
   keypath:=keypath+'/'+key;
   parse_json_node(value,keypath,sl);
  end else if JSONDataIsList(data) then begin
   subsl:=TStringList.Create;
   JSON2StringList(data,subsl);
   i:=subsl.Count;
   for i:=0 to subsl.Count-1 do parse_json_node(subsl[i],keypath,sl);
   subsl.Free;
  end else sl.Add(keypath+'='+trim_quotes(json_extra_escapes(JSONEscapeDecode(data))));
 end;

function absolute_url(baseurl,newurl:string):string; // Half-assed, but should be good enough
var
 prot,user,pass,host,port,path,para:string;
 i:integer;
begin
 result:=newurl;
 if (newurl='') or (pos('://',newurl)>0) then exit;

 parseurl(baseurl,prot,user,pass,host,port,path,para);
 if (lowercase(prot)='http') and (port='80') then
 else if (lowercase(prot)='https') and (port='443') then
 else if (lowercase(prot)='ftp') and (port='21') then
 else prot:=prot+':'+port;

 if newurl[1]='/' then newurl:=prot+'://'+host+newurl
 else begin
  if copy(path,length(path),1)<>'/' then
   for i:=length(path) downto 1 do if path[i]='/' then begin
    delete(path,i+1,length(path));
    break;
   end;
  if path='' then path:='/';
  newurl:=prot+'://'+host+path+newurl;
 end;
 result:=newurl;
end;

function fetch_page(url,httpmethod:string;var pagedata,info,http_if_modified_since,http_if_none_match:string;var page_not_modified:boolean;ttl:integer;extra_headers:string;var compressedsize:longint;postdata,postencoding:string;onstatus_type:integer):boolean;
label
  retry;
var
  http:THTTPSend;
  size:int64;
  redirecturl,extra_header,encoding:string;
  do_retry:boolean;

 function get_header(name:string;var value:string):boolean;
 var i:integer;
 begin
  result:=false;
  value:='';
  name:=lowercase(name)+':';
  for i:=0 to http.Headers.Count-1 do if lowercase(copy(http.Headers[i],1,length(name)))=name then begin
   value:=trim_whitespace(copy(http.Headers[i],length(name)+1,length(http.Headers[i])));
   result:=true;
   break;
  end;
 end;

begin
 retry:;
 compressedsize:=0;
 do_retry:=false;
 size:=0;
 result:=false;
 info:='';
 page_not_modified:=false;
 http:=thttpsend.create;
 http.UserAgent:='';

 if http_if_modified_since<>'' then http.Headers.Add('If-Modified-Since: '+http_if_modified_since);
 if http_if_none_match<>'' then http.Headers.Add('If-None-Match: '+http_if_none_match);

 {$ifdef deflate}http.Headers.Add('Accept-Encoding: gzip, deflate');{$endif}

 while extra_headers<>'' do begin
  extra_header:=str_firstparam(extra_headers,true,#13);
  if copy(extra_header,1,1)=#10 then delete(extra_header,1,1);
  if extra_header<>'' then http.Headers.Add(extra_header);
 end;

 if postdata<>'' then begin
  http.MimeType:=postencoding;
  http.Document.Write(postdata[1],length(postdata));
 end;
 if onstatus_type=2 then http.Sock.OnStatus:=@FormMain.imgur_sock_onstatus;

 try
  result:=http.HTTPMethod(httpmethod,url);
 finally
  if result then begin
   size:=http.Document.Size;
   if http.ResultCode<>304 then begin
    setlength(pagedata,size);
    if size>0 then size:=http.Document.read(pagedata[1],size);
   end;
   compressedsize:=size;

   {$ifdef deflate}
   if (http.ResultCode<>304) and get_header('content-encoding',encoding) then begin
    if lowercase(encoding)='gzip' then result:=zlib_inflate_str(true,pagedata)
    else if lowercase(encoding)='deflate' then result:=zlib_inflate_str(false,pagedata);
    size:=length(pagedata);
    if not result then info:='HTTP gzip/deflate decode error';
   end;
   {$endif}

   if (http.ResultCode=200) or (http.ResultCode=206) then begin
    get_header('last-modified',http_if_modified_since);
    get_header('etag',http_if_none_match);
   end;

   if (http.ResultCode=200) or (http.ResultCode=206) then info:='OK ('+l2s(http.Document.Size)+' bytes)'
   else if (http.ResultCode=301) or (http.ResultCode=302) or (http.ResultCode=303) or (http.ResultCode=307) or (http.ResultCode=308) then begin
    result:=false;
    if ttl=0 then info:='Too many HTTP redirects ('+l2s(http.ResultCode)+')'
    else begin
     dec(ttl);
     if get_header('location',redirecturl) then redirecturl:=absolute_url(url,redirecturl);
     if redirecturl='' then info:='HTTP redirect error'
     else begin
      url:=redirecturl;
      do_retry:=true;
     end;
    end;
   end else if http.ResultCode=304 then begin
    page_not_modified:=true;
    info:='Page not modified';
   end else begin
    result:=false;
    info:='HTTP Error '+l2s(http.ResultCode)+' ('+http.ResultString+')';
   end;
  end else begin
   info:=http.Sock.LastErrorDesc;
   if info='' then info:='Error '+l2s(http.Sock.LastError);
  end;
 http.free;
 end;
 if do_retry then goto retry;
end;

function fetch_page(url,httpmethod:string;var pagedata,info:string;extra_headers,postdata,postencoding:string;onstatus_type:integer):boolean;
var ims,inm:string;
  page_not_modified:boolean;
  compressedsize:longint;
begin
 result:=fetch_page(url,httpmethod,pagedata,info,ims,inm,page_not_modified,5,extra_headers,compressedsize,postdata,postencoding,onstatus_type);
end;

function do_job_thread(p:pointer):ptrint;
var
  info:string;
  l:longint;
  tw:boolean=false;
  im:boolean=false;
  new_image_count:integer=0;
begin
 tweet_fetch_success:=fetch_tweets(info,new_image_count);
 if tweet_fetch_success then begin
  if not twitter_thread.abort then begin
   LCLIntf.PostMessage(FormMain.Handle,windowmsg_update_listview,new_image_count,0);
   system.EnterCriticalSection(img_list_CS);
   for l:=0 to img_list_count-1 do begin // what needs to be done?
    if img_list[l].local_file='' then begin tw:=true; inc(progress_jobs_total); end;
    if img_list[l].imgur_url='' then inc(progress_jobs_total);
    if (img_list[l].local_file='') or (img_list[l].imgur_url='') then inc(progress_numfiles);
    if {$ifndef directupload}(img_list[l].local_file<>'') and {$endif}(img_list[l].imgur_url='') then im:=true;
   end;
   system.LeaveCriticalSection(img_list_CS);
   if im then thread_activate(imgur_thread,@do_imgur_jobs); // usually won't happen here for non-direct upload, we'll most likely start it from do_twitter_jobs
   if tw then do_twitter_jobs;
  end;
 end else tweet_fetch_info:=info;
 info:='';

// if app_update_check and (last_app_update_check+24*60*60<unixtime) then check_for_app_updates;

 twitter_thread.running:=false;
 // make sure the other threads don't get stuck in a suspended state
 if (imgur_thread.running) and (imgur_thread.suspended) then thread_activate(imgur_thread,@do_imgur_jobs);
 if (thumbnail_thread.running) and (thumbnail_thread.suspended) then thread_activate(thumbnail_thread,@load_image_thumbnails);
 LCLIntf.PostMessage(FormMain.Handle,windowmsg_thread_ended,0,0);
 EndThread;
end;

function get_ssl_lib_version:string;
var ssl:TSSLOpenSSL;
begin
 ssl:=TSSLOpenSSL.Create(nil);
 result:=ssl.libversion;
 ssl.Free;
end;

begin

 ssl_version:=get_ssl_lib_version;

end.

