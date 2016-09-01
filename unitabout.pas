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

unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, lclintf;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonOK: TButton;
    ImageLogo: TImage;
    LabelBuildInfo: TLabel;
    LabelCopyright: TLabel;
    LabelURL: TLabel;
    LabelDesc: TLabel;
    LabelTitle: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure LabelURLClick(Sender: TObject);
    procedure LabelURLMouseEnter(Sender: TObject);
    procedure LabelURLMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses misc;

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.ButtonOKClick(Sender: TObject);
begin
 Close;
end;

procedure TFormAbout.LabelURLClick(Sender: TObject);
begin
 OpenURL(app_url);
end;

procedure TFormAbout.LabelURLMouseEnter(Sender: TObject);
begin
 LabelURL.Font.Style:=[];
end;

procedure TFormAbout.LabelURLMouseLeave(Sender: TObject);
begin
 LabelURL.Font.Style:=[fsUnderline];
end;

end.

