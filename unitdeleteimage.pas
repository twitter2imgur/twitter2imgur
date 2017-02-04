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

unit unitdeleteimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormDeleteImage }

  TFormDeleteImage = class(TForm)
    ButtonDelete: TButton;
    ButtonCancel: TButton;
    CheckBoxDelT2I: TCheckBox;
    CheckBoxDelLocal: TCheckBox;
    CheckBoxDelImgur: TCheckBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelWarning: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormDeleteImage: TFormDeleteImage;

implementation

uses misc, unitmain, http;

var
 del_image_id:string;
 can_delete_imgur:boolean;

{$R *.lfm}

{ TFormDeleteImage }

procedure set_delete_form_control_states;
begin
 FormDeleteImage.CheckBoxDelImgur.Enabled:=can_delete_imgur and FormDeleteImage.CheckBoxDelT2I.Checked;
 if not FormDeleteImage.CheckBoxDelImgur.Enabled then FormDeleteImage.CheckBoxDelImgur.Checked:=false;
 FormDeleteImage.CheckBoxDelLocal.Enabled:=FormDeleteImage.CheckBoxDelT2I.Checked;
 if not FormDeleteImage.CheckBoxDelLocal.Enabled then FormDeleteImage.CheckBoxDelLocal.Checked:=false;
 FormDeleteImage.ButtonDelete.Enabled:=FormDeleteImage.CheckBoxDelT2I.Checked or FormDeleteImage.CheckBoxDelImgur.Checked or FormDeleteImage.CheckBoxDelLocal.Checked;
end;

procedure TFormDeleteImage.ButtonCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFormDeleteImage.ButtonDeleteClick(Sender: TObject);
var l:longint;
begin
 system.EnterCriticalSection(img_list_CS);
 if get_img_by_twitter_id(del_image_id,l) then begin
  // delete from imgur
  if CheckBoxDelImgur.Checked then
   if not delete_imgur_image(img_list[l].imgur_id,img_list[l].imgur_deletehash) then
    if MessageDlg('Error','Unable to delete image from Imgur. Continue anyway?',mtError,[mbYes,mbNo],0)=mrNo then begin
     system.LeaveCriticalSection(img_list_CS);
     Close;
     exit;
    end;

  // delete local file
  if CheckBoxDelLocal.Checked and (img_list[l].local_file<>'') then DeleteFile(imagesdir+img_list[l].local_file);
  // delete thumbnail if it exists
  sysutils.DeleteFile(thumbcache(img_list[l].local_file));
  // remove from list
  img_list[l].deleted:=true;
 end;

 system.LeaveCriticalSection(img_list_CS);
 Close;
 write_images_file;
 populate_mainform_listview(false);
end;

procedure TFormDeleteImage.CheckBoxChange(Sender: TObject);
begin
 set_delete_form_control_states;
end;

procedure TFormDeleteImage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 Image1.Picture.Bitmap.Clear;
 del_image_id:='';
end;

procedure TFormDeleteImage.FormShow(Sender: TObject);
var l:longint;
begin
 l:=del_index;
 system.EnterCriticalSection(img_list_CS);
 if (l>=0) and (l<img_list_count) then begin
  del_image_id:=img_list[l].twitter_media_id;
  can_delete_imgur:=(img_list[l].imgur_deletehash<>'') or (imgur_screenname<>'');
  Label1.Caption:=get_item_url(l,false,true);
  Label2.Caption:=img_list[l].title_;
  Label3.Caption:=show_duration(unixtime-img_list[l].timestamp)+' ago';
  CheckBoxDelT2I.Checked:=true;
  CheckBoxDelImgur.Checked:=false;
  CheckBoxDelLocal.Checked:=false;
  set_delete_form_control_states;
  if img_list[l].imglist_num>=0 then imagelist_thumbs.GetBitmap(img_list[l].imglist_num,Image1.Picture.Bitmap)
  else imagelist_thumbs.GetBitmap(0,Image1.Picture.Bitmap);
  Image1.Width:=imagelist_thumbs.Width;
  Image1.Height:=imagelist_thumbs.Height;
 end else Close;
 system.LeaveCriticalSection(img_list_CS);
end;


end.

