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

unit unitsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ButtonFont: TButton;
    ButtonReset: TButton;
    ButtonTwitterClear: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonImgurAccount: TButton;
    ButtonTwitterAccount: TButton;
    ButtonImgurClear: TButton;
    CheckBoxUseTray: TCheckBox;
    CheckBoxAutoFetch: TCheckBox;
    CheckBoxAlwaysOnTop: TCheckBox;
    ComboBoxURLMode: TComboBox;
    ComboBoxDefaultAction: TComboBox;
    EditAutoUpdateMins: TEdit;
    EditImageSizeX: TEdit;
    EditImageSizeY: TEdit;
    EditLimitByHashtag: TEdit;
    EditAlbum: TEdit;
    FontDialog1: TFontDialog;
    GroupBoxTransfers: TGroupBox;
    GroupBoxWindow: TGroupBox;
    GroupBoxTwitterAccount: TGroupBox;
    GroupBoxImgurAccount: TGroupBox;
    ImageTwitter: TImage;
    ImageImgur: TImage;
    LabelMins: TLabel;
    LabelFont: TLabel;
    LabelImageSize: TLabel;
    LabelImageSizeBy: TLabel;
    LabelURLMode: TLabel;
    LabelLimitByhashtag: TLabel;
    LabelAlbum: TLabel;
    LabelDefaultAction: TLabel;
    LabelImgurAccount: TLabel;
    LabelTwitterAccount: TLabel;
    RadioButtonAlbum: TRadioButton;
    RadioButtonAutoAlbum: TRadioButton;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure ButtonImgurAccountClick(Sender: TObject);
    procedure ButtonImgurClearClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonTwitterAccountClick(Sender: TObject);
    procedure ButtonTwitterClearClick(Sender: TObject);
    procedure CheckBoxAutoFetchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButtonAlbumChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

uses misc, http, unitmain, unitabout;

{$R *.lfm}

{ TFormSettings }

var
  font_changed,font_reset:boolean;
  font_name:string;
  font_size:integer;
  font_style:TFontStylesBase;

procedure set_settings_form_control_states;
begin
 if twitter_screenname<>'' then begin
  FormSettings.LabelTwitterAccount.Font.Style:=[fsBold];
  FormSettings.LabelTwitterAccount.Caption:=twitter_screenname;
  FormSettings.ButtonTwitterClear.Enabled:=true;
 end else begin
  FormSettings.LabelTwitterAccount.Font.Style:=[];
  FormSettings.LabelTwitterAccount.Caption:='<None>';
  FormSettings.ButtonTwitterClear.Enabled:=false;
 end;

 if imgur_screenname<>'' then begin
  FormSettings.LabelImgurAccount.Font.Style:=[fsBold];
  FormSettings.LabelImgurAccount.Caption:=imgur_screenname;
  FormSettings.ButtonImgurClear.Enabled:=true;
 end else begin
  FormSettings.LabelImgurAccount.Font.Style:=[];
  FormSettings.LabelImgurAccount.Caption:='<None>';
  FormSettings.ButtonImgurClear.Enabled:=false;
 end;

 FormSettings.EditAutoUpdateMins.Enabled:=FormSettings.CheckBoxAutoFetch.Checked;
 FormSettings.EditAlbum.Enabled:=FormSettings.RadioButtonAlbum.Checked;
 FormSettings.ButtonFont.Caption:=fontstr(font_name,font_size,font_style,true);
end;

procedure TFormSettings.FormShow(Sender: TObject);
begin
 font_changed:=false;
 font_reset:=false;
 get_listview_font(font_name,font_size,font_style);
 CheckBoxAutoFetch.Checked:=auto_fetch;
 EditAutoUpdateMins.Text:=l2s(auto_fetch_mins);
 EditLimitByHashtag.Text:=limit_hashtags;
 EditAlbum.Text:=upload_album;
 RadioButtonAlbum.Checked:=not auto_upload_album;
 RadioButtonAutoAlbum.Checked:=auto_upload_album;
 CheckBoxAlwaysOnTop.Checked:=always_on_top;
 CheckBoxUseTray.Checked:=use_systray;
 if (default_action>=0) and (default_action<=2) then ComboBoxDefaultAction.ItemIndex:=default_action;
 if (url_mode>=0) and (url_mode<=1) then ComboBoxURLMode.ItemIndex:=url_mode;
 EditImageSizeX.Text:=l2s(thumbnail_width);
 EditImageSizeY.Text:=l2s(thumbnail_height);
 set_settings_form_control_states;
 ButtonCancel.SetFocus;
end;

procedure TFormSettings.RadioButtonAlbumChange(Sender: TObject);
begin
 set_settings_form_control_states;
end;

procedure TFormSettings.ButtonTwitterAccountClick(Sender: TObject);
begin
 if twitter_authorise then begin
  set_settings_form_control_states;
  set_main_form_control_states;
  write_config_file;
 end;
end;

procedure TFormSettings.ButtonTwitterClearClick(Sender: TObject);
begin
 if MessageDlg('Confirmation','Forget Twitter account "'+twitter_screenname+'"?',mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
  twitter_token:='';
  twitter_token_secret:='';
  twitter_screenname:='';
  set_settings_form_control_states;
  set_main_form_control_states;
  write_config_file;
 end;
end;

procedure TFormSettings.CheckBoxAutoFetchChange(Sender: TObject);
begin
 set_settings_form_control_states;
end;

procedure TFormSettings.ButtonCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFormSettings.ButtonFontClick(Sender: TObject);
begin
 FontDialog1.Font.Name:=font_name;
 FontDialog1.Font.Size:=font_size;
 FontDialog1.Font.Style:=font_style;
 if FontDialog1.Execute then begin
  font_changed:=true;
  font_reset:=false;
  font_name:=FontDialog1.Font.Name;
  font_size:=FontDialog1.Font.Size;
  font_style:=FontDialog1.Font.Style;
  set_settings_form_control_states;
 end;
end;

procedure TFormSettings.ButtonImgurAccountClick(Sender: TObject);
begin
 if imgur_authorise then begin
  clear_imgur_albums;
  set_settings_form_control_states;
  set_main_form_control_states;
  write_config_file;
 end;
end;

procedure TFormSettings.ButtonImgurClearClick(Sender: TObject);
begin
 if MessageDlg('Confirmation','Forget Imgur account "'+imgur_screenname+'"?',mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
  imgur_access_token:='';
  imgur_refresh_token:='';
  imgur_screenname:='';
  imgur_token_expiry:=0;
  clear_imgur_albums;
  set_settings_form_control_states;
  set_main_form_control_states;
  write_config_file;
 end;
end;

procedure TFormSettings.ButtonOKClick(Sender: TObject);
var x,y:longint;
begin
 auto_fetch:=CheckBoxAutoFetch.Checked;
 if s2l(EditAutoUpdateMins.Text,auto_fetch_mins) then begin
  if auto_fetch_mins<1 then auto_fetch_mins:=1 else if auto_fetch_mins>10080 then auto_fetch_mins:=10080;
 end else auto_fetch_mins:=30;
 limit_hashtags:=EditLimitByHashtag.Text;
 upload_album:=FormSettings.EditAlbum.Text;
 auto_upload_album:=not FormSettings.RadioButtonAlbum.Checked;
 always_on_top:=CheckBoxAlwaysOnTop.Checked;
 use_systray:=CheckBoxUseTray.Checked;
 if always_on_top then begin
  FormMain.FormStyle:=fsSystemStayOnTop;
  FormSettings.FormStyle:=fsSystemStayOnTop;
  FormAbout.FormStyle:=fsSystemStayOnTop;
 end else begin
  FormMain.FormStyle:=fsNormal;
  FormSettings.FormStyle:=fsNormal;
  FormAbout.FormStyle:=fsNormal;
 end;
 default_action:=ComboBoxDefaultAction.ItemIndex;
 url_mode:=ComboBoxURLMode.ItemIndex;

 if s2l(EditImageSizeX.Text,x) then begin
  if x<1 then x:=1 else if x>1000 then x:=1000;
 end else x:=thumbnail_width;
 if s2l(EditImageSizeY.Text,y) then begin
  if y<1 then y:=1 else if y>1000 then y:=1000;
 end else y:=thumbnail_height;

 if (x<>thumbnail_width) or (y<>thumbnail_height) then begin
  if thumbnail_thread.started then begin // don't mess with the thumbnail imagelist while thumbnails are being generated
   thumbnail_thread.abort:=true;
   while thumbnail_thread.running do sleep(20);
   Application.ProcessMessages; // ensures thumbnails thread is closed before proceeding so it can be restarted immediately
  end;
  thumbnail_width:=x;
  thumbnail_height:=y;
  FormMain.ListViewFiles.SmallImages:=nil;
  FormMain.ListViewFiles.LargeImages:=nil;
  for x:=0 to img_list_count-1 do img_list[x].imglist_num:=-1;
  imagelist_thumbs.Clear;
  imagelist_thumbs.Free;
  init_thumbnail_imagelist;
 end;

 if font_reset then begin
  font_override:=false;
  FormMain.ListViewFiles.Font.Name:='default';
  FormMain.ListViewFiles.Font.Size:=default_font_size{-12};
  FormMain.ListViewFiles.Font.Style:=default_font_style{[fsBold]};
 end else if font_changed then begin
  font_override:=true;
  FormMain.ListViewFiles.Font.Name:=font_name;
  FormMain.ListViewFiles.Font.Size:=font_size;
  FormMain.ListViewFiles.Font.Bold:=fsBold in font_style;
  FormMain.ListViewFiles.Font.Italic:=fsItalic in font_style;
 end;

 write_config_file;
 populate_mainform_listview;
 Close;
end;

procedure TFormSettings.ButtonResetClick(Sender: TObject);
begin
 CheckBoxAutoFetch.Checked:=false;
 EditAutoUpdateMins.Text:='30';

 EditLimitByhashTag.Text:='';
 RadioButtonAlbum.Checked:=true;
 EditAlbum.Text:='';

 EditImageSizeX.Text:=l2s(thumbnail_width_default);
 EditImageSizeY.Text:=l2s(thumbnail_height_default);

 font_name:=default_font_name;
 font_size:=default_font_size;
 font_style:=default_font_style;
 font_reset:=true;

 ComboBoxURLMode.ItemIndex:=0;
 ComboBoxDefaultAction.ItemIndex:=0;
 CheckBoxAlwaysOnTop.Checked:=false;
 CheckBoxUseTray.Checked:=false;

 set_settings_form_control_states;
end;

end.

