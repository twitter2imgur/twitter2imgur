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

program twitter2imgur;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  cmem, Interfaces, Forms, unitmain, unitsettings, unitabout, misc, Graphics,
  Dialogs, LCLVersion, ssl_openssl_lib, sysutils, unitdeleteimage;

function buildinfo:string;
begin
 result:={$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+' '+
  {$ifdef LCLGtk}'GTK'
   {$else}{$ifdef LCLGtk2}'GTK2'
    {$else}{$ifdef LCLWin32}'Win32'
     {$else}{$ifdef LCLWinCE}'WinCE'
      {$else}{$ifdef LCLCarbon}'Carbon'
       {$else}{$ifdef LCLQt}'QT'
        {$else}{$ifdef LCLFpGui}'fpGUI'
         {$else}{$ifdef LCLNoGui}'NoGUI'
          {$else}{$ifdef LCLCocoa}'Cocoa'
           {$else}{$ifdef LCLCustomDrawn}'CustomDrawn'
           {$else}'?'
           {$endif}
          {$endif}
         {$endif}
        {$endif}
       {$endif}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
  {$endif}
 +' LCL'+lcl_version+' FPC'+{$I %FPCVERSION%}+' '+{$I %DATE%};
 if ssl_version<>'' then result:=result+' '+ssl_version else result:=result+' NO OPENSSL';
end;

{$R *.res}

begin
  Application.Title:='Twitter2Imgur';
  Application.Initialize;
  Application.ShowButtonGlyphs:=sbgSystem;

  Application.CreateForm(TFormMain, FormMain);
  FormMain.Caption:=Application.Title{$ifdef debug}+' [Debug]'{$endif};
  FormMain.TrayIcon1.Hint:=Application.Title;
  get_listview_font(default_font_name,default_font_size,default_font_style);
  {$ifdef Darwin}
    FormMain.ButtonSettings.BorderSpacing.Bottom:=20;
    FormMain.BitBtnTwitter.Constraints.MinWidth:=40;
    FormMain.BitBtnTwitter.BorderSpacing.InnerBorder:=2;
    FormMain.BitBtnImgur.Constraints.MinWidth:=40;
    FormMain.BitBtnImgur.BorderSpacing.InnerBorder:=2;
    FormMain.BitBtnFolder.Constraints.MinWidth:=40;
    FormMain.BitBtnFolder.BorderSpacing.InnerBorder:=2;
  {$endif}

  Application.CreateForm(TFormSettings, FormSettings);

  Application.CreateForm(TFormAbout, FormAbout);
  FormAbout.Caption:='About '+Application.Title;
  FormAbout.LabelTitle.Caption:=FormAbout.LabelTitle.Caption+' v'+app_version_str;
  FormAbout.LabelURL.Caption:=app_url;
  FormAbout.LabelURL.Hint:=app_url;
  FormAbout.LabelBuildInfo.Caption:='Build info: '+buildinfo;

  read_config_file;
  read_images_file;
  init_thumbnail_imagelist;

  FormMain.MenuItemSeparator2.Visible:=allow_image_delete;
  FormMain.MenuItemDelete.Visible:=allow_image_delete;

  if ssl_version='' then begin
   {$ifdef MSWindows}MessageDlg('Missing OpenSSL Libraries','OpenSSL libraries '+ssl_openssl_lib.DLLSSLName+' and '+ssl_openssl_lib.DLLUtilName{$ifdef CPU32}+' (32 bit)'{$endif}{$ifdef CPU64}+' (64 bit)'{$endif}+' could not be loaded. Twitter and Imgur operations will not work without these files.'#13#13'Installing OpenSSL yourself may fix the problem.',mtError,[mbOK],0);
   {$else}
     {$ifdef Unix}
       {$ifdef Darwin}MessageDlg('Missing OpenSSL Libraries','Twitter and Imgur operations require the OpenSSL libraries installed on your system.',mtError,[mbOK],0);
       {$else}MessageDlg('Missing OpenSSL Libraries','Twitter and Imgur operations require OpenSSL. Please install libssl-dev on your system and restart '+Application.Title+'.',mtError,[mbOK],0);{$endif}
     {$else}MessageDlg('Missing OpenSSL Libraries','Twitter and Imgur operations require the OpenSSL libraries installed on your system.',mtError,[mbOK],0);{$endif}
   {$endif}
  end;
  Application.CreateForm(TFormDeleteImage, FormDeleteImage);
  Application.Run;
end.

