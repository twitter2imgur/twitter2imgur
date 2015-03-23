// Copyright 2014, 2015 Dr C (drcpsn@hotmail.com | http://twitter2imgur.github.io/twitter2imgur/)
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

program twitter2imgur;

{$mode objfpc}{$H+}

uses
  cmem,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, unitmain, unitsettings, unitabout, misc, Graphics, Dialogs;

{$R *.res}

begin
  Application.Title:='Twitter2Imgur';
  Application.Initialize;
  Application.ShowButtonGlyphs:=sbgSystem;

  Application.CreateForm(TFormMain, FormMain);
  get_listview_font(default_font_name,default_font_size,default_font_style);

  Application.CreateForm(TFormSettings, FormSettings);

  Application.CreateForm(TFormAbout, FormAbout);
  FormAbout.Caption:='About '+Application.Title;
  FormAbout.LabelTitle.Caption:=FormAbout.LabelTitle.Caption+' v'+app_version_str;
  FormAbout.LabelURL.Caption:=app_url;
  FormAbout.LabelURL.Hint:=app_url;

  read_config_file;
  read_images_file;
  init_thumbnail_imagelist;

  if ssl_version='' then begin
   {$ifdef MSWindows}MessageDlg('Missing OpenSSL Libraries','OpenSSL libraries libeay32.dll and ssleay32.dll'{$ifdef CPU32}+' (32 bit)'{$endif}{$ifdef CPU64}+' (64 bit)'{$endif}+' were not found. Twitter and Imgur operations will not work without these files.',mtError,[mbOK],0);
   {$else}
     {$ifdef Unix}
       {$ifdef Darwin}MessageDlg('Missing OpenSSL Libraries','Twitter and Imgur operations require the OpenSSL libraries installed on your system.',mtError,[mbOK],0);
       {$else}MessageDlg('Missing OpenSSL Libraries','Twitter and Imgur operations require OpenSSL. Please install libssl-dev on your system and restart '+Application.Title+'.',mtError,[mbOK],0);{$endif}
     {$else}MessageDlg('Missing OpenSSL Libraries','Twitter and Imgur operations require the OpenSSL libraries installed on your system.',mtError,[mbOK],0);{$endif}
   {$endif}
  end;

  Application.Run;
end.

