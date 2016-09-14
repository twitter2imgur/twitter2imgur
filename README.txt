Twitter2Imgur by Dr C (drcpsn@hotmail.com)
Release 1.04 (14 Sep 2016)

A small app designed to fetch images posted from a Twitter account and upload
them to an Imgur account. It is written in Free Pascal/Lazarus.


1) Project Homepage:

http://twitter2imgur.github.io/twitter2imgur/ (formerly https://code.google.com/p/twitter2imgur/)


2) How to Use:

Windows: Run twitter2imgur-windows-xxx.exe installer, or extract
twitter2imgur-windows-xxx.zip to a folder and run twitter2imgur.exe.

Linux: Extract twitter2imgur-linux-xxx.tar.bz2 to a folder and run
twitter2imgur.


3) Building From Source (for advanced users):

Twitter2Imgur can be built from source, and should build for Windows and most
*nix platforms. However, the app's official private Twitter and Imgur API keys
are not provided. You will need to generate your own and place them in a file
in the source directory named "twitter2imgur.keys". The file format is:

	twitter_api_key = 'TWITTER_API_KEY_HERE';
	twitter_api_secret = 'TWITTER_API_SECRET_HERE';
	imgur_api_key = 'IMGUR_API_KEY_HERE';
	imgur_api_secret = 'IMGUR_API_SECRET_HERE';

To build:
   1) Install Lazarus (http://www.lazarus-ide.org/)
   2) Extract Twitter2Imgur source archive
   3) Open project file (twitter2imgur.lpi) in Lazarus
   4) Hit Run -> Build


4) Changelog:

Version 1.04 (14 Sep 2016)
- Switched to bundling Indy's OpenSSL DLLs on Windows (OpenSSL was failing to load on systems without Visual Studio runtime installed)

Version 1.03 (1 Sep 2016)
- Image thumbnails are now cached for faster loading
- Added option to automatically fetch every x minutes
- Added option to send to notification area (system tray) when closed
- Updated OpenSSL DLLs on Windows

Version 1.02 (23 Mar 2015)
- Updated links with new GitHub homepage
- Updated bundled OpenSSL DLLs on Windows

Version 1.01 (16 Feb 2015)
- Reversed the order images are uploaded in
- Added F5 keyboard shortcut for Update

Version 1.0 (6 Nov 2014)
- Initial release
