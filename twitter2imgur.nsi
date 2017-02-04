; Uses KillProc plugin: http://nsis.sourceforge.net/KillProc_plug-in

!define bits "32"
; "64" for 64 bit, any non-empty string for 32 bit
!define Version "1.05"
!define AppName "Twitter2Imgur"
!define ExeName "twitter2imgur.exe"
!define AppDesc "Uploads Twitter Images to Imgur"

!if ${bits} == "64"
  OutFile "twitter2imgur_x64-windows-${Version}-setup.exe"
  InstallDir "$PROGRAMFILES64\${AppName}"
  
  !include "x64.nsh"
  
  Function .onInit
    ${If} ${RunningX64}
    ${Else}
      MessageBox MB_OK|MB_ICONSTOP "This installer is for 64 bit Windows versions. You require the 32 bit installer."
      Abort
    ${EndIf}
    
    SetRegView 64
  FunctionEnd
  
  Function un.onInit
    SetRegView 64
  FunctionEnd
!else
  OutFile "twitter2imgur-windows-${Version}-setup.exe"
  InstallDir "$PROGRAMFILES\${AppName}"
!endif

;  Files: make sure all are included in install and uninstall
;  File twitter2imgur.exe
;  File license.txt
;  File README.txt
;  File libeay32.dll
;  File ssleay32.dll

Name "${AppName}"
InstallDirRegKey HKLM "Software\${AppName}" "Install_Dir"
AllowSkipFiles off
LicenseData "license.txt"
RequestExecutionLevel none
XPStyle on
VIAddVersionKey "ProductName" "${AppName}"
VIAddVersionKey "Comments" "https://twitter2imgur.github.io/twitter2imgur/"
VIAddVersionKey "FileDescription" "${AppName}"
VIAddVersionKey "FileVersion" "${Version}"
VIAddVersionKey "LegalCopyright" "(c) 2014-2017 Dr C (drcpsn@hotmail.com)"
VIProductVersion "${Version}.0.0"

Page license
Page components
Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles


Section "${AppName} (required)"
  SectionIn RO
  SetOutPath $INSTDIR
  SetAutoClose true

  instretry:
  StrCpy $0 ${ExeName}
  KillProc::FindProcesses
  StrCmp $0 "0" instcontinue
  StrCmp $0 "-1" instcontinue
  MessageBox MB_RETRYCANCEL|MB_ICONEXCLAMATION| "Please close ${AppName} before continuing." IDRETRY instretry IDCANCEL instcontinue
  instcontinue:
  
  File twitter2imgur.exe
  File license.txt
  File README.txt
  !if ${bits} == "64"
    File openssl-win64\libeay32.dll
    File openssl-win64\ssleay32.dll
  !else
    File openssl-win32\libeay32.dll
    File openssl-win32\ssleay32.dll
  !endif
  
  WriteRegStr HKLM "SOFTWARE\${AppName}" "Install_Dir" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "DisplayName" "${AppName}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "DisplayIcon" "$INSTDIR\${ExeName}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "DisplayVersion" "${Version}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "HelpLink" "https://twitter2imgur.github.io/twitter2imgur/"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "Publisher" "Dr C"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "URLInfoAbout" "https://twitter2imgur.github.io/twitter2imgur/"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
SectionEnd


Section "Start Menu Shortcut"
  CreateShortCut "$SMPROGRAMS\${AppName}.lnk" "$INSTDIR\${ExeName}" "" "" "" "" "" "${AppDesc}"
SectionEnd


Section /o "Desktop Shortcut"
  CreateShortCut "$DESKTOP\${AppName}.lnk" "$INSTDIR\${ExeName}" "" "" "" "" "" "${AppDesc}"
SectionEnd


Function .onInstSuccess
  MessageBox MB_OK|MB_ICONINFORMATION| "Installation complete!"
  Exec '"$INSTDIR\${ExeName}"'
FunctionEnd


Section "Uninstall"

  uninstretry:
  StrCpy $0 ${ExeName}
  KillProc::FindProcesses
  StrCmp $0 "0" uninstcontinue
  StrCmp $0 "-1" uninstcontinue
  MessageBox MB_RETRYCANCEL|MB_ICONEXCLAMATION| "Please close Twitter2Imgur before continuing." IDRETRY uninstretry IDCANCEL uninstcontinue
  uninstcontinue:

; Remove files and uninstaller
  Delete "$INSTDIR\twitter2imgur.exe"
  Delete "$INSTDIR\license.txt"
  Delete "$INSTDIR\README.txt"
  Delete "$INSTDIR\libeay32.dll"
  Delete "$INSTDIR\ssleay32.dll"
  Delete "$INSTDIR\uninstall.exe"
  RMDir "$INSTDIR"
  
; Remove shortcuts, if any
  Delete "$DESKTOP\${AppName}.lnk"
  Delete "$SMPROGRAMS\${AppName}.lnk"
  SetShellVarContext all
  Delete "$DESKTOP\${AppName}.lnk"
  Delete "$SMPROGRAMS\${AppName}.lnk"
  Delete "$SMSTARTUP\${AppName}.lnk"
  
; Remove registry keys
  DeleteRegKey HKLM "SOFTWARE\${AppName}"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${AppName}"

SectionEnd
