[Setup]
AppName=mxHour
AppVersion=1.0.0
AppPublisher=Massimo Nardello
AppPublisherURL=https://github.com/maxnd/mxHour
DefaultDirName={pf}\mxHour
DefaultGroupName=mxHour
UninstallDisplayIcon={app}\mxhour.exe
LicenseFile=license.rtf
OutputDir=OutputDir=\

[Files]
Source: "mxhour.exe"; DestDir: "{app}"
Source: "mxhour.po"; DestDir: "{app}"
Source: "mxhour.it.po"; DestDir: "{app}"


[Icons]
Name: "{commonprograms}\mxHour"; Filename: "{app}\mxhour.exe"
Name: "{commondesktop}\mxHour"; Filename: "{app}\mxhour.exe"
Name: "{group}\mxHour"; Filename: "{app}\mxhour.exe"
