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

unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Menus, ExtCtrls, types, LMessages, Buttons, misc, blcksock, LCLType;

type

  { TFormMain }

  TFormMain = class(TForm)
    BitBtnFolder: TBitBtn;
    BitBtnTwitter: TBitBtn;
    BitBtnImgur: TBitBtn;
    ButtonAbout: TButton;
    ButtonUpdate: TButton;
    ButtonSettings: TButton;
    ImageDefaultImage: TImage;
    LabelStatusBar1: TLabel;
    LabelStatus: TLabel;
    LabelFiles: TLabel;
    LabelProgress: TLabel;
    LabelStatusBar2: TLabel;
    ListViewFiles: TListView;
    MenuItemSeparator2: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemFetch: TMenuItem;
    MenuItemShow: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemSep: TMenuItem;
    MenuItemOpenFolder: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemViewURL: TMenuItem;
    MenuItemViewLocal: TMenuItem;
    MenuItemCopyURL: TMenuItem;
    PopupMenuTray: TPopupMenu;
    PopupMenuImages: TPopupMenu;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    procedure BitBtnFolderClick(Sender: TObject);
    procedure BitBtnImgurClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure BitBtnTwitterClick(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ListViewFilesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ListViewFilesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure ListViewFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewFilesKeyPress(Sender: TObject; var Key: char);
    procedure ListViewFilesResize(Sender: TObject);
    procedure ListViewFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure MenuItemCopyURLClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemFetchClick(Sender: TObject);
    procedure MenuItemOpenFolderClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemViewLocalClick(Sender: TObject);
    procedure MenuItemViewURLClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ExitTimerTimer(Sender: TObject);
    procedure HandleThreadMsg_thread_ended(var Msg: TLMessage); message windowmsg_thread_ended;
    procedure HandleThreadMsg_thumbnail_loaded(var Msg: TLMessage); message windowmsg_thumbnail_loaded;
    procedure HandleThreadMsg_update_listview(var Msg: TLMessage); message windowmsg_update_listview;
    procedure imgur_sock_onstatus(Sender:TObject;Reason:THookSocketReason;const Value:String);
    procedure TrayIcon1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure set_main_form_control_states;
procedure populate_mainform_listview(reset_scroll:boolean);
function get_item_url(num:longint;orig,trim_protocol:boolean):string;

var
  FormMain: TFormMain;
  imagelist_thumbs:TImageList=nil;

implementation

uses unitsettings, lclintf, http, clipbrd, BGRABitmap, unitabout, unitdeleteimage;

{$R *.lfm}

{ TFormMain }

var
  formshown:boolean=false;
  need_progressbar_hide:boolean=false;
  window_coords_history:array[0..3] of longint;
  windowstate_history:TWindowState=wsNormal;
  buttonstate:boolean=false;
  abort_clicked:boolean=false;
  job_cleanup_run:boolean=false;
  want_exit:boolean=false;
  ExitTimer:TTimer=nil;
  first_update:boolean=true;

procedure program_shutdown;
var l:longint;
begin
 for l:=0 to img_list_alloc-1 do with img_list[l] do begin
  twitter_media_id:='';
  twitter_url:='';
  title_:='';
  twitter_hashtags_:='';
  local_file:='';
  imgur_id:='';
  imgur_url:='';
  imgur_deletehash:='';
  errorinfo:='';
 end;
 if img_list_alloc>0 then freemem(img_list,img_list_alloc*sizeof(img_list_type));

 for l:=0 to imgur_album_alloc-1 do with imgur_albums[l] do begin
  title:='';
  id:='';
 end;
 if imgur_album_alloc>0 then freemem(imgur_albums,imgur_album_alloc*sizeof(imgur_album_type));

 system.DoneCriticalSection(img_list_CS);

 {$ifdef filelock}
 {$I-}
 close(configfile);
 ioresult;
 {$I+}
 {$endif}
end;

function get_selected_file(var i:integer):boolean;
var li:TListItem;
begin
 result:=false;
 i:=-1;
 li:=FormMain.ListViewFiles.Selected;
 if li<>nil then begin
//  i:=li.Index;
  i:=ptrint(li.Data);
  result:=true;
 end;
end;

function get_item_url(num:longint;orig,trim_protocol:boolean):string;
begin
 if img_list[num].imgur_url<>'' then begin
  if url_mode=1 then result:=img_list[num].imgur_url else result:='http://imgur.com/'+img_list[num].imgur_id;
 end else begin
  result:=img_list[num].twitter_url;
  if orig then result:=result+':orig';
 end;
 if trim_protocol then begin
  if lowercase(copy(result,1,7))='http://' then delete(result,1,7)
  else if lowercase(copy(result,1,8))='https://' then delete(result,1,8);
 end;
end;

procedure set_status_label;
var i:integer;
begin
 if not jobs_in_progress then begin
  if last_update=0 then FormMain.LabelStatus.Caption:='Last fetch: Never'
  else FormMain.LabelStatus.Caption:='Last fetch: '+show_duration(unixtime-last_update)+' ago';
  i:=((FormMain.ButtonUpdate.Left)-FormMain.LabelStatus.Width) div 2;
  if i<8 then i:=8;
  FormMain.LabelStatus.Left:=i;
 end;
end;

procedure run_action(item,runtype:integer);
begin
 if (item>=0) and (item<img_list_count) then begin
  if runtype=0 then OpenURL(get_item_url(item,true,false))
  else if runtype=1 then begin
   if img_list[item].local_file<>'' then OpenDocument(imagesdir+img_list[item].local_file);
  end else if runtype=2 then Clipboard.AsText:=get_item_url(item,true,false);
 end;
end;

procedure set_main_form_control_states;
begin
 FormMain.LabelProgress.Visible:=jobs_in_progress;
 FormMain.ProgressBar1.Visible:=jobs_in_progress;
 FormMain.LabelStatus.Visible:=not jobs_in_progress;
 if not jobs_in_progress then set_status_label;
 FormMain.ButtonUpdate.Enabled:=(not jobs_in_progress) or buttonstate;
end;

procedure populate_mainform_listview(reset_scroll:boolean);
var
  l:longint=0;
  itemcount:longint=0;
  limit:longint;
  li:TListItem;
  thumbnail_update_wanted:boolean;
begin
 thumbnail_update_wanted:=false;
 system.EnterCriticalSection(img_list_CS);

 limit:=img_list_count;
 if trim_image_list and (trim_image_list_count<limit) then limit:=trim_image_list_count;

 if (FormMain.ListViewFiles.Items.Count>0) and (reset_scroll or (limit<FormMain.ListViewFiles.Items.Count)) then FormMain.ListViewFiles.Items[0].MakeVisible(false); // scrolls to top

 for l:=0 to img_list_count-1 do begin
  if not img_list[l].deleted then begin
   if itemcount>=FormMain.ListViewFiles.Items.Count then li:=FormMain.ListViewFiles.Items.Add
   else li:=FormMain.ListViewFiles.Items[itemcount];

   li.Data:=pointer(l);
   img_list[l].listview_index:=itemcount;

   if img_list[l].imglist_num>=0 then li.ImageIndex:=img_list[l].imglist_num
   else begin
    if img_list[l].local_file<>'' then thumbnail_update_wanted:=true;
    li.ImageIndex:=img_default;
   end;
   li.Caption:=''; // force redraw
   li.Caption:='  '+get_item_url(l,false,true);
   inc(itemcount);
   if itemcount>=limit then break;
  end else img_list[l].listview_index:=-1;
 end;
 for l:=FormMain.ListViewFiles.Items.Count-1 downto itemcount do FormMain.ListViewFiles.Items[l].Delete;

 system.LeaveCriticalSection(img_list_CS);

 FormMain.LabelFiles.Caption:='Recent images ('+l2s(FormMain.ListViewFiles.Items.Count)+')';

 if thumbnail_update_wanted then thread_activate(thumbnail_thread,@load_image_thumbnails);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
 if not formshown then begin
  ButtonUpdate.AnchorSide[akBottom].Control:=nil;
  ButtonUpdate.Constraints.MinHeight:=ButtonUpdate.Height;
  ButtonUpdate.Constraints.MaxHeight:=ButtonUpdate.Height;
  windowstate_history:=wsNormal;
  if window_coords_loaded then begin
   Left:=window_coords[0];
   Top:=window_coords[1];
   Width:=window_coords[2];
   Height:=window_coords[3];
   if window_maximised then WindowState:=wsMaximized;
  end else begin
   window_coords[0]:=Left;
   window_coords[1]:=Top;
   window_coords[2]:=Width;
   window_coords[3]:=Height;
   window_maximised:=false;
  end;
  move(window_coords,window_coords_history,sizeof(window_coords));
  if always_on_top then begin
   FormMain.FormStyle:=fsSystemStayOnTop;
   FormSettings.FormStyle:=fsSystemStayOnTop;
   FormAbout.FormStyle:=fsSystemStayOnTop;
  end;
  {$ifndef MSWindows}FormMain.ListViewFiles.AutoWidthLastColumn:=true;{$endif}
  populate_mainform_listview(false);
  formshown:=true;
  if app_update_check and (last_app_update_check+24*60*60<unixtime) then thread_activate(update_thread,@check_for_app_updates);
 end;
 set_main_form_control_states;
end;

procedure TFormMain.FormWindowStateChange(Sender: TObject);
begin
 // Keeping valid (restored) window coordinates is a pain in the ass (Form.Restored* seem broken).
 //
 // order of events:
 //
 // normal->minimised: WindowState changes to wsMinimized -> Left,Top,Width,Height retain correct (non-minimised) values -> OnWindowStateChange fires
 // minimised->normal: WindowState changes to wsNormal -> Left,Top,Width,Height retain correct (non-minimised) values -> OnWindowStateChange fires
 // normal->maximised: Left,Top,Width,Height change to incorrect (maximised) values -> OnChangeBounds fires -> WindowState changes to wsMaximized -> OnWindowStateChange fires
 // maximised->normal: Left,Top,Width,Height change to correct (non-maximised) values -> OnChangeBounds fires -> WindowState changes to wsNormal -> OnWindowStateChange fires
 // maximised->minimised: WindowState changes to wsMinimized -> Left,Top,Width,Height retain incorrect (maximised) values -> OnWindowStateChange fires
 // minimised->maximised: WindowState changes to wsMaximized -> Left,Top,Width,Height retain incorrect (maximised) values -> OnWindowStateChange fires
 //
 // solution:
 // track two levels of window coords history in OnChangeBounds, and in OnWindowStateChange, detect when
 // normal->maximised, maximised->minimised or minimised->maximised and undo one level of window coords history.
 // first level history then always contains the correct coords.

 if ((windowstate_history=wsNormal) and (WindowState=wsMaximized))
 or ((windowstate_history=wsMaximized) and (WindowState=wsMinimized))
 or ((windowstate_history=wsMinimized) and (WindowState=wsMaximized)) then begin
  move(window_coords_history,window_coords,sizeof(window_coords));
 end;
 windowstate_history:=WindowState;
 window_maximised:=WindowState=wsMaximized;

 set_status_label;
end;

procedure TFormMain.ListViewFilesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var i:integer;
begin
 if get_selected_file(i) then begin
  MenuItemViewURL.Default:=default_action=0;
  MenuItemViewLocal.Default:=default_action=1;
  MenuItemCopyURL.Default:=default_action=2;
  MenuItemViewLocal.Enabled:=img_list[i].local_file<>'';
  PopupMenuImages.PopUp;
 end;
end;

procedure TFormMain.ListViewFilesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var l:longint;
begin
 l:=Item.Index;
 if (l>=0) and (l<ListViewFiles.Items.Count) then begin
  if img_list[l].flags and job_flag_error<>0 then TCustomListView(Sender).Canvas.Font.Color:=clRed
  else if (img_list[l].imgur_id='') or (img_list[l].local_file='') then TCustomListView(Sender).Canvas.Font.Color:=clGray
  else if img_list[l].flags and job_flag_new<>0 then TCustomListView(Sender).Canvas.Font.Color:=clBlue
  else TCustomListView(Sender).Canvas.Font.Color:=TCustomListView(Sender).Color;
 end;
end;

procedure TFormMain.ListViewFilesDblClick(Sender: TObject);
var i:integer;
begin
 if get_selected_file(i) then run_action(i,default_action);
end;

procedure TFormMain.ListViewFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key=VK_DELETE then begin
  if allow_image_delete then if get_selected_file(del_index) then FormDeleteImage.ShowModal;
  Key:= 0;
 end;
end;

procedure TFormMain.ListViewFilesKeyPress(Sender: TObject; var Key: char);
var i:integer;
begin
 if key=#13 then begin
  if get_selected_file(i) then run_action(i,default_action);
 end else if key=#27 then ListViewFiles.ClearSelection;
end;

procedure TFormMain.ListViewFilesResize(Sender: TObject);
begin
 {$ifdef MSWindows}
 ListViewFiles.Column[0].Width:=ListViewFiles.ClientWidth;
 {$endif}
end;

procedure update_status_bar(selnum:integer);
begin
 if selnum<0 then begin
  FormMain.LabelStatusBar1.Caption:=' ';
  FormMain.LabelStatusBar2.Caption:=' ';
 end else begin
  FormMain.LabelStatusBar1.Caption:=img_list[selnum].title_;
  FormMain.LabelStatusBar2.Caption:=show_duration(unixtime-img_list[selnum].timestamp)+' ago';
 end;
end;

procedure TFormMain.ListViewFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var i:integer;
begin
 get_selected_file(i);
 update_status_bar(i);
end;

procedure TFormMain.MenuItemCopyURLClick(Sender: TObject);
var i:integer;
begin
 if get_selected_file(i) then run_action(i,2);
end;

procedure TFormMain.MenuItemDeleteClick(Sender: TObject);
begin
 if allow_image_delete then if get_selected_file(del_index) then FormDeleteImage.ShowModal;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFormMain.MenuItemFetchClick(Sender: TObject);
begin
 ButtonUpdateClick(MenuItemFetch);
end;

procedure TFormMain.MenuItemOpenFolderClick(Sender: TObject);
var i:integer;
begin
 if get_selected_file(i) then begin
  if not DirectoryExists(imagesdir) then forcedirectories(imagesdir);
  {$ifdef MSWindows}shellexec('explorer.exe',['/select,"'+imagesdir+img_list[i].local_file+'"'],'');
   {$else}{$ifdef Darwin}shellexec('open',[imagesdir],'');
    {$else}{$ifdef UNIX}shellexec('xdg-open',[imagesdir],'');
    {$else}Other platform stuff here...
   {$endif}
  {$endif}
 {$endif}
 end;
end;

procedure TFormMain.MenuItemShowClick(Sender: TObject);
begin
 Position:=poDesigned;
 ShowOnTop;
 TrayIcon1.Hide;
end;

procedure TFormMain.MenuItemViewLocalClick(Sender: TObject);
var i:integer;
begin
 if get_selected_file(i) then run_action(i,1);
end;

procedure TFormMain.MenuItemViewURLClick(Sender: TObject);
var i:integer;
begin
 if get_selected_file(i) then run_action(i,0);
end;

procedure update_job_progress;
var
  imgur_frac:real;
  progress:integer;
begin
 if progress_jobs_total>0 then begin
  imgur_frac:=0;
  if imgur_thread.started then if progress_imgur_currentfile_total>0 then imgur_frac:=progress_imgur_currentfile_current/progress_imgur_currentfile_total;

  progress:=round(((progress_jobs_done+imgur_frac)/progress_jobs_total)*100);
  if (progress_jobs_done<progress_jobs_total) and (progress>=100) then progress:=99
  else if progress>100 then progress:=100;
  FormMain.LabelProgress.Caption:='Transferring '+plural(progress_numfiles,'image','images')+' ('+l2s(progress)+'%)';

  progress:=round(((progress_jobs_done+imgur_frac)/progress_jobs_total)*FormMain.ProgressBar1.Max);
  if (progress_jobs_done<progress_jobs_total) and (progress>=FormMain.ProgressBar1.Max) then progress:=FormMain.ProgressBar1.Max-1
  else if progress>FormMain.ProgressBar1.Max then progress:=FormMain.ProgressBar1.Max;

  if progress>FormMain.ProgressBar1.Max then progress:=FormMain.ProgressBar1.Max;
  if progress>FormMain.ProgressBar1.Position then FormMain.ProgressBar1.Position:=progress;
 end;
end;

procedure do_fetch(auto:boolean);
var l:longint;
begin
 current_fetch_is_auto:=auto;
 for l:=0 to img_list_count-1 do begin
  img_list[l].flags:=0;
  img_list[l].errorinfo:='';
 end;
 abort_clicked:=false;
 job_cleanup_run:=false;
 tweet_fetch_success:=false;
 tweet_fetch_info:='';
 jobs_in_progress:=true;
 need_progressbar_hide:=false;
 progress_jobs_total:=0;
 progress_jobs_done:=0;
 progress_numfiles:=0;
 progress_imgur_currentfile_total:=0;
 progress_imgur_currentfile_current:=0;
 FormMain.ProgressBar1.Position:=0;
 FormMain.LabelProgress.Caption:='Fetching tweets...';
 set_main_form_control_states;
 FormMain.Timer1.Interval:=50; // fast updates for displaying progress
 thread_activate(twitter_thread,@do_job_thread);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
var l:longint;
  updatelist:boolean=false;
begin
 set_status_label;
 get_selected_file(l);
 update_status_bar(l);
 if first_update then begin
  first_update:=false;
  Timer1.Interval:=60000;
 end;
 if jobs_in_progress then begin
  if not need_progressbar_hide then update_job_progress
  else begin
   jobs_in_progress:=false;
   set_main_form_control_states;
   Timer1.Interval:=60000;
  end;
 end else begin
  for l:=0 to img_list_count-1 do if img_list[l].flags and job_flag_new<>0 then begin
   if unixtime-img_list[l].fetched_timestamp>90 then begin
    img_list[l].flags:=img_list[l].flags and word(not job_flag_new);
    updatelist:=true;
   end;
  end;
  if updatelist then populate_mainform_listview(false);

  if auto_fetch and (not buttonstate) and (not FormSettings.Showing) and (twitter_screenname<>'') and (unixtime>=last_update+auto_fetch_mins*60) then do_fetch(true);
 end;
end;

procedure TFormMain.ExitTimerTimer(Sender: TObject);
begin
 if twitter_thread.started then KillThread(twitter_thread.id);
 if imgur_thread.started then KillThread(imgur_thread.id);
 if thumbnail_thread.started then KillThread(thumbnail_thread.id);
 if update_thread.started then KillThread(update_thread.id);
 OnCloseQuery:=nil;
 Close;
end;

procedure TFormMain.ButtonSettingsClick(Sender: TObject);
begin
 FormSettings.ShowModal;
end;

procedure TFormMain.BitBtnImgurClick(Sender: TObject);
begin
 OpenURL('https://imgur.com/');
end;

procedure TFormMain.BitBtnFolderClick(Sender: TObject);
begin
 if not DirectoryExists(imagesdir) then forcedirectories(imagesdir);
 {$ifdef MSWindows}shellexec('explorer.exe',[imagesdir],'');
  {$else}{$ifdef Darwin}shellexec('open',[imagesdir],'');
   {$else}{$ifdef UNIX}shellexec('xdg-open',[imagesdir],'');
   {$else}Other platform stuff here...
  {$endif}
 {$endif}
{$endif}
end;

procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
 FormAbout.ShowModal;
end;

procedure TFormMain.BitBtnTwitterClick(Sender: TObject);
begin
 OpenURL('https://twitter.com/');
end;

procedure TFormMain.ButtonUpdateClick(Sender: TObject);
begin
 if (not buttonstate) and (not jobs_in_progress) and (twitter_screenname<>'') then do_fetch(false)
 else if buttonstate then begin // Cancel
  if Sender<>MenuItemFetch then begin
   twitter_thread.abort:=true;
   imgur_thread.abort:=true;
   abort_clicked:=true;
  end;
 end else if twitter_screenname='' then MessageDlg('Missing Account','You need to enter a Twitter account in the settings before you can fetch and upload images.',mtInformation,[mbOK],0);
end;

procedure TFormMain.FormChangeBounds(Sender: TObject);
begin
 // see note in OnWindowStateChange
 if formshown then begin
  move(window_coords,window_coords_history,sizeof(window_coords));
  window_coords[0]:=Left;
  window_coords[1]:=Top;
  window_coords[2]:=Width;
  window_coords[3]:=Height;
 end;
 set_status_label;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 write_config_file;
 write_images_file;
 program_shutdown;
end;

procedure TFormMain.HandleThreadMsg_thread_ended(var Msg: TLMessage);
var l,errorcount:longint;
    s:string;
    saveconfig:boolean=false;
begin
 if twitter_thread.started and (not twitter_thread.running) then begin twitter_thread.started:=false; CloseThread(twitter_thread.id); saveconfig:=true; end;
 if imgur_thread.started and (not imgur_thread.running) then begin imgur_thread.started:=false; CloseThread(imgur_thread.id); saveconfig:=true; end;
 if thumbnail_thread.started and (not thumbnail_thread.running) then begin thumbnail_thread.started:=false; CloseThread(thumbnail_thread.id); end;
 if update_thread.started and (not update_thread.running) then begin update_thread.started:=false; CloseThread(update_thread.id); end;

 if saveconfig then begin write_images_file; write_config_file; end;

 if jobs_in_progress and (not twitter_thread.started) and (not imgur_thread.started) and (not job_cleanup_run) then begin
  job_cleanup_run:=true;
  if abort_clicked then begin
   Timer1.Enabled:=false;
   if not want_exit then MessageDlg('Information','Fetch cancelled.',mtInformation,[mbOK],0);
   Timer1.Enabled:=true;
  end;

  if not want_exit then begin
   ProgressBar1.Position:=ProgressBar1.Max;
   ButtonUpdate.Caption:='&Fetch';
   buttonstate:=false;
   set_main_form_control_states;

   last_update:=unixtime;
   Timer1.Interval:=4000; // hide the progress bar after 4 sec
   need_progressbar_hide:=true;
   populate_mainform_listview(false);

   if not tweet_fetch_success then begin
    LabelProgress.Caption:='Failed to fetch tweets!';
    if not current_fetch_is_auto then MessageDlg('Error','Unable to fetch tweets from Twitter account.'#13#13+tweet_fetch_info,mtError,[mbOK],0);
   end else begin
    if abort_clicked then LabelProgress.Caption:='Fetch cancelled!'
    else if progress_numfiles=0 then LabelProgress.Caption:='Done! No new images.'
    else LabelProgress.Caption:='Done! '+plural(progress_numfiles,'new image','new images')+'.';

    errorcount:=0;
    for l:=0 to img_list_count-1 do if img_list[l].flags and job_flag_error<>0 then begin
     inc(errorcount);
     s:=s+#13+img_list[l].twitter_media_id+': '+img_list[l].errorinfo;
    end;
    if (errorcount>0) and (not current_fetch_is_auto) then MessageDlg('Error',plural(errorcount,'image','images')+' could not be transferred:'#13+s+#13#13'Trying again may fix the issue.',mtError,[mbOK],0);

   end;
   abort_clicked:=false;
  end;
 end;

 if not want_exit then begin
  if show_update_notification then begin
   show_update_notification:=false;
   app_update_check:=false;
   if messagedlg(Application.Title+' Update',program_update_msg,mtConfirmation,[mbYes,mbNo],0,mbYes)=mrYes then OpenURL(program_update_url);
   app_update_check:=true;
   last_app_update_check:=unixtime;
  end else if (app_update_attempt_count>=10) and (last_successful_app_update_check+60*60*24*90<unixtime) then begin // >=10 failed attempts and 90 days since last successful check
   if last_successful_app_update_check>0 then s:=' for '+show_duration(unixtime-last_successful_app_update_check) else s:='';
   if messagedlg(Application.Title+' Update',Application.Title+' has been unable to check for updates'+s+'. Would you like to manually check now?',mtConfirmation,[mbYes,mbNo],0,mbYes)=mrYes then OpenURL(app_url);
   app_update_attempt_count:=0;
   last_successful_app_update_check:=unixtime;
  end;
 end;

 if want_exit and (not twitter_thread.started) and (not imgur_thread.started) and (not thumbnail_thread.started) and (not update_thread.started) then begin
  OnCloseQuery:=nil;
  Close;
 end;
end;

procedure TFormMain.HandleThreadMsg_thumbnail_loaded(var Msg: TLMessage);
var
  bmp:TBGRABitmap;
  l,l2:longint;
  id:^string;
begin
 move(msg.wParam,bmp,sizeof(pointer));
 move(msg.lParam,id,sizeof(pointer));

 system.EnterCriticalSection(img_list_CS);
 if get_img_by_twitter_id(id^,l) then begin
  l2:=imagelist_thumbs.Add(bmp.Bitmap,nil);
  img_list[l].imglist_num:=l2;
  if (img_list[l].listview_index>=0) and (img_list[l].listview_index<ListViewFiles.Items.Count) then ListViewFiles.Items[img_list[l].listview_index].ImageIndex:=l2;
 end;
 system.LeaveCriticalSection(img_list_CS);

 bmp.Free;
 id^:='';
 freemem(id,sizeof(string));
 {$ifdef Unix}Application.ProcessMessages;{$endif} // mitigates the UI locking up on Linux if thumbnails come in too fast
end;

procedure TFormMain.HandleThreadMsg_update_listview(var Msg: TLMessage);
begin
 if (not buttonstate) and ((twitter_thread.running) or (imgur_thread.running)) then begin // change Update to Cancel
  buttonstate:=true;
  ButtonUpdate.Caption:='&Cancel';
  set_main_form_control_states;
 end;
 populate_mainform_listview(msg.wParam<>0);
end;

procedure TFormMain.imgur_sock_onstatus(Sender:TObject;Reason:THookSocketReason;const Value:String);
var l:longint;
begin
 if Reason=HR_WriteCount then if s2l(value,l) then begin
  inc(progress_imgur_currentfile_current,l);
  update_job_progress;
 end;
end;

procedure TFormMain.TrayIcon1Click(Sender: TObject);
begin
 Position:=poDesigned;
 ShowOnTop;
 TrayIcon1.Hide;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 if use_systray and Visible then begin
  TrayIcon1.Show;
  Hide;
  CanClose:=False;
 end else begin
  if update_thread.started then begin update_thread.started:=false; KillThread(update_thread.id); end;
  if twitter_thread.started or imgur_thread.started or thumbnail_thread.started then begin
   twitter_thread.abort:=true;
   imgur_thread.abort:=true;
   thumbnail_thread.abort:=true;
   want_exit:=true;
   CanClose:=false;
   ExitTimer:=TTimer.Create(nil);
   ExitTimer.Interval:=10000;
   ExitTimer.Enabled:=true;
   ExitTimer.OnTimer:=@ExitTimerTimer;
  end else CanClose:=true;
 end;
end;

procedure TFormMain.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  if msg.CharCode=VK_F5 then begin
   handled:=true;
   if (not buttonstate) and (not jobs_in_progress) then FormMain.ButtonUpdateClick(nil);
  end;
end;

end.

