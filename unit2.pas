// ***********************************************************************
// ***********************************************************************
// mxHour 1.x
// Author and copyright: Massimo Nardello, Modena (Italy) 2021.
// Free software released under GPL licence version 3 or later.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version. You can read the version 3
// of the Licence in http://www.gnu.org/licenses/gpl-3.0.txt
// or in the file Licence.txt included in the files of the
// source code of this software.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// ***********************************************************************
// ***********************************************************************

unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CocoaThemes, LCLIntf;

type

  { Tfmcopyright }

  Tfmcopyright = class(TForm)
    bnOK: TButton;
    imImage: TImage;
    rmCopyrightText: TMemo;
    stText1: TLabel;
    stText2: TLabel;
    stText3: TLabel;
    stText4: TLabel;
    procedure bnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure stText4Click(Sender: TObject);
  private

  public

  end;

var
  fmcopyright: Tfmcopyright;

implementation

{$R *.lfm}

procedure TfmCopyright.FormCreate(Sender: TObject);
begin
  rmCopyrightText.Lines.Add(
     'This program is free software: you can redistribute it and/or modify it ' +
     'under the terms of the GNU General Public License as published by the Free ' +
     'Software Foundation, either version 3 of the License, or (at your option) ' +
     'any later version. You can read the version 3 of the Licence in ' +
     'http://www.gnu.org/licenses gpl-3.0.txt. For other information you can also ' +
     'see http://www.gnu.org/licenses.');
  rmCopyrightText.Lines.Add('');
   rmCopyrightText.Lines.Add(
     'This program is distributed in the hope ' +
     'that it will be useful, but WITHOUT ANY WARRANTY; without even the implied ' +
     'warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU ' +
     'General Public License for more details.');
   rmCopyrightText.Lines.Add('');
   rmCopyrightText.Lines.Add('This software has been written with Lazarus. ' +
   'The icon has been taken from https://icon-icons.com/icon/gnome-schedule/94438.');
end;

procedure Tfmcopyright.bnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmCopyright.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure Tfmcopyright.FormShow(Sender: TObject);
begin
  if IsPaintDark = True then
  begin
    rmCopyrightText.Font.Color := clWhite;
  end
  else
  begin
    rmCopyrightText.Font.Color := clBlack;
  end;
end;

procedure Tfmcopyright.stText4Click(Sender: TObject);
begin
  OpenURL('https://github.com/maxnd/mxHour');
end;

end.

