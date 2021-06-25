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

unit Unit1;

{$mode objfpc}{$H+}
{$ifdef Darwin} {$modeswitch objectivec1} {$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, Types, IniFiles, LazUTF8, LazFileUtils, DefaultTranslator
  {$ifdef Windows} , windows {$endif}
  {$ifdef Darwin} , CocoaAll, CocoaUtils {$endif};

  {$ifdef Darwin}
    const
      NSWindowTitleVisible = 0;
      NSWindowTitleHidden = 1;
  {$endif}

  type
    {$ifdef Darwin}
      NSWindowTitleVisibility = NSInteger;
      NSWindowGlam = objccategory external (NSWindow)
        function titleVisibility: NSWindowTitleVisibility; message 'titleVisibility';
        procedure setTitleVisibility(AVisibility: NSWindowTitleVisibility); message 'setTitleVisibility:';
        function titlebarAppearsTransparent: Boolean; message 'titlebarAppearsTransparent';
        procedure setTitlebarAppearsTransparent(AFlag: Boolean); message 'setTitlebarAppearsTransparent:';
    end;
    {$endif}

  { TfmMain }

  TfmMain = class(TForm)
    clColour: TColorDialog;
    lbBottom: TLabel;
    lbHour: TLabel;
    miStart: TMenuItem;
    miReset: TMenuItem;
    N3: TMenuItem;
    miColBack: TMenuItem;
    N2: TMenuItem;
    miSmall: TMenuItem;
    miNormal: TMenuItem;
    miBig: TMenuItem;
    miQuit: TMenuItem;
    miColor: TMenuItem;
    miCopyright: TMenuItem;
    N1: TMenuItem;
    miMenu: TPopupMenu;
    pmMenu: TPopupMenu;
    tmTimer: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbHourMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbHourMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure lbHourMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbHourMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure miColBackClick(Sender: TObject);
    procedure miBigClick(Sender: TObject);
    procedure miColorClick(Sender: TObject);
    procedure miCopyrightClick(Sender: TObject);
    procedure miNormalClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure miResetClick(Sender: TObject);
    procedure miSmallClick(Sender: TObject);
    procedure miStartClick(Sender: TObject);
    procedure pmMenuPopup(Sender: TObject);
    procedure tmTimerTimer(Sender: TObject);
  private
    procedure SetHour;

  public

  end;

var
  fmMain: TfmMain;
  myHomeDir, myConfigFile: string;
  MouseIsDown: boolean;
  PX, PY: integer;
  clBack: TColor = clDefault;
  ttTime: TTime;

resourcestring

  dateformat = 'en';

implementation

uses Unit2;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
var
  MyIni: TIniFile;
  Style: Longint;
begin
  fmMain.Width := 375;
  fmMain.Height := 150;
  if dateformat = 'en' then
  begin
    fmMain.Width := 575;
  end;
  {$ifdef Darwin}
    myHomeDir := GetUserDir + 'Library/Preferences/';
    myConfigFile := 'mxhour.plist';
  {$endif}
  {$ifdef Windows}
    myHomeDir := GetUserDir + 'AppData\Local\mxHour\';
    myConfigFile := 'mxhour.ini';
  {$endif}
  if DirectoryExists(myHomeDir) = False then
  begin
    CreateDirUTF8(myHomeDir);
  end;
  if FileExistsUTF8(myHomeDir + myConfigFile) then
  begin
    try
      MyIni := TIniFile.Create(myHomeDir + myConfigFile);
      fmMain.Top := MyIni.ReadInteger('mxhour', 'top', 0);
      fmMain.Left := MyIni.ReadInteger('mxhour', 'left', 0);
      fmMain.Width := MyIni.ReadInteger('mxhour', 'width', 375);
      fmMain.Height := MyIni.ReadInteger('mxhour', 'heigth', 150);
      fmMain.AlphaBlendValue := MyIni.ReadInteger('mxhour', 'alpha', 170);;
      lbHour.Font.Color := StringToColor(MyIni.ReadString('mxhour',
        'fontcolor', 'clLime'));
      lbBottom.Font.Color := lbHour.Font.Color;
      clBack := StringToColor(MyIni.ReadString('mxhour',
        'color', 'clDefault'));
    finally
      MyIni.Free;
    end;
  end;
  {$ifdef Windows}
  miQuit.ShortCut := 16465;
  {$endif}
  {$ifdef Darwin}
  miQuit.ShortCut := 4123;
  {$endif}
  SetHour;
end;

procedure TfmMain.FormShow(Sender: TObject);
  {$ifdef Darwin}
  var
    myWin :NSWindow;
  {$endif}
begin
  {$ifdef Darwin}
  myWin := NSView(Self.Handle).window;
  myWin.setTitlebarAppearsTransparent(True);
  myWin.setTitleVisibility(NSWindowTitleHidden);
  myWin.setBackgroundColor(ColorToNSColor(clBack));
  {$endif}
  {$ifdef Windows}
  fmMain.Color := clWhite;
  miColBack.Visible := False;
  {$endif}
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MyIni: TIniFile;
begin
  try
    MyIni := TIniFile.Create(myHomeDir + myConfigFile);
    MyIni.WriteInteger('mxhour', 'top', fmMain.Top);
    MyIni.WriteInteger('mxhour', 'left', fmMain.Left);
    MyIni.WriteInteger('mxhour', 'width', fmMain.Width);
    MyIni.WriteInteger('mxhour', 'heigth', fmMain.Height);
    MyIni.WriteInteger('mxhour', 'alpha', fmMain.AlphaBlendValue);
    MyIni.WriteString('mxhour', 'fontcolor', ColorToString(lbHour.Font.Color));
    MyIni.WriteString('mxhour', 'color', ColorToString(clBack));
  finally
    MyIni.Free;
  end;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  {$ifdef Darwin}
  if ((key = 27) and (ssMeta in Shift)) then
  begin
    Close;
  end
  else
  begin
    key := 0;
  end;
  {$endif}
  {$ifdef Windows}
  if ((key = Ord('Q')) and (ssCtrl in Shift)) then
  begin
    Close;
  end
  else
  begin
    key := 0;
  end;
  {$endif}
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  lbHour.OptimalFill := False;
  lbHour.OptimalFill := True;
end;

procedure TfmMain.lbHourMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    MouseIsDown := True;
    PX := X;
    PY := Y;
  end;
end;

procedure TfmMain.lbHourMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MouseIsDown then
  begin
    fmMain.Left := fmMain.Left + (X - PX);
    fmMain.Top := fmMain.Top + (Y - PY);
    {$ifdef Windows}
    Refresh;
    {$endif}
  end;
end;

procedure TfmMain.lbHourMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown := False;
end;

procedure TfmMain.lbHourMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if WheelDelta > 0 then
  begin
    if fmMain.AlphaBlendValue < 250 then
      fmMain.AlphaBlendValue := fmMain.AlphaBlendValue + 1;
  end
  else
  if WheelDelta < 0 then
  begin
    if fmMain.AlphaBlendValue > 40 then
      fmMain.AlphaBlendValue := fmMain.AlphaBlendValue - 1;
  end;
end;

procedure TfmMain.miStartClick(Sender: TObject);
begin
  miStart.Checked := not miStart.Checked;
  if miStart.Checked = True then
  begin
    miReset.Enabled := True;
    ttTime := Now();
  end
  else
  begin
    miReset.Enabled := False;
  end;
  SetHour;
end;

procedure TfmMain.miResetClick(Sender: TObject);
begin
  ttTime := Now();
  SetHour;
end;

procedure TfmMain.miBigClick(Sender: TObject);
begin
  fmMain.Width := 500;
  fmMain.Height := 200;
  if dateformat = 'en' then
  begin
    fmMain.Width := 800;
  end;
end;

procedure TfmMain.miNormalClick(Sender: TObject);
begin
  fmMain.Width := 375;
  fmMain.Height := 150;
  if dateformat = 'en' then
  begin
    fmMain.Width := 575;
  end;
end;

procedure TfmMain.miSmallClick(Sender: TObject);
begin
  fmMain.Width := 250;
  fmMain.Height := 100;
  if dateformat = 'en' then
  begin
    fmMain.Width := 400;
  end;
end;

procedure TfmMain.pmMenuPopup(Sender: TObject);
begin
  if fmMain.Active = False then
  begin
    fmMain.BringToFront;
  end;
end;

procedure TfmMain.miColorClick(Sender: TObject);
begin
  clColour.Color := lbHour.Font.Color;
  if clColour.Execute then
  begin
    lbHour.Font.Color := clColour.Color;
    lbBottom.Font.Color := clColour.Color;
  end;
end;

procedure TfmMain.miColBackClick(Sender: TObject);
begin
  clColour.Color := clBack;
  if clColour.Execute then
  begin
    clBack := clColour.Color;
    fmMain.FormShow(nil);
  end;
end;

procedure TfmMain.miCopyrightClick(Sender: TObject);
begin
  fmCopyright.ShowModal;
end;

procedure TfmMain.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.tmTimerTimer(Sender: TObject);
  {$ifdef Darwin}
  var
    myWin :NSWindow;
  {$endif}
begin
  SetHour;
  {$ifdef Darwin}
  myWin := NSView(Self.Handle).window;
  if myWin.isOnActiveSpace = False then
  begin
    myWin.orderFront(nil);
  end;
  {$endif}
end;

procedure TfmMain.SetHour;
begin
  if miStart.Checked = False then
  begin
    lbBottom.caption := '';
    if dateformat = 'en' then
    begin
      lbHour.Caption := FormatDateTime('hh.nn am/pm', Now());
    end
    else
    begin
      lbHour.Caption := FormatDateTime('hh.nn', Now());
    end;
  end
  else
  begin
    lbHour.Caption := FormatDateTime('hh.nn', Now() - ttTime);
    if dateformat = 'en' then
    begin
      lbBottom.Caption := FormatDateTime('hh.nn am/pm', Now());
    end
    else
    begin
      lbBottom.Caption := FormatDateTime('hh.nn', Now());
    end;
  end;
end;

end.


