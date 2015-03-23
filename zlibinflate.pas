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

unit zlibinflate;

{$mode objfpc}{$H+}

interface

function zlib_inflate(is_gzip:boolean;source:pointer;sourcesize:cardinal;var dest:pointer;var destsize,destalloc:cardinal):boolean;
function zlib_inflate_str(is_gzip:boolean;var s:string):boolean;

implementation

uses
  zbase,zinflate,crc;

const
  gz_magic:array[0..1] of byte=($1F,$8B); // gzip magic header

  gzflag_ASCII_FLAG  = $01; // bit 0 set: file probably ascii text
  gzflag_HEAD_CRC    = $02; // bit 1 set: header CRC present
  gzflag_EXTRA_FIELD = $04; // bit 2 set: extra field present
  gzflag_ORIG_NAME   = $08; // bit 3 set: original file name present
  gzflag_COMMENT     = $10; // bit 4 set: file comment present
  gzflag_RESERVED    = $E0; // bits 5..7: reserved

type
  gzm_stream = record      // from gzio.gz_stream
   stream      : z_stream;
   outbuf      : Pbyte;    // output buffer
   crc         : cardinal; // crc32 of uncompressed data
   mode        : char;     // 'w' or 'r'
  end;

function read_gz_header(data:pointer;var datapos:cardinal;datasize:cardinal):boolean;
var
  len:cardinal;
  data_b:^byte;
  flags,b:byte;
begin
 datapos:=0;
 result:=false;
 if datasize<10 then exit;
 data_b:=data;
 if (data_b[0]<>gz_magic[0]) or (data_b[1]<>gz_magic[1]) then exit; {no gz header}

 if data_b[2]<>Z_DEFLATED then exit; // method
 flags:=data_b[3]; // flags
 if flags and gzflag_RESERVED<>0 then exit;
 // don't care about time(4), xflags(1) and OS code(1)
 datapos:=10;

 if flags and gzflag_EXTRA_FIELD<>0 then begin // extra field
  if datasize<datapos+2 then exit;
  len:=data_b[datapos]+(cardinal(data_b[datapos+1]) shl 8);
  inc(datapos,2);
  if datasize<datapos+len then exit;
  inc(datapos,len);
 end;

 if flags and gzflag_ORIG_NAME<>0 then begin // original file name
  repeat
   if datasize<datapos+1 then exit;
   b:=data_b[datapos];
   inc(datapos);
   if b=0 then break;
  until false;
 end;

 if flags and gzflag_COMMENT<>0 then begin // .gz file comment
  repeat
   if datasize<datapos+1 then exit;
   b:=data_b[datapos];
   inc(datapos);
   if b=0 then break;
  until false;
 end;

 if flags and gzflag_HEAD_CRC<>0 then begin // header crc
  if datasize<datapos+2 then exit;
  inc(datapos,2);
 end;

 result:=true;
end;

function zlib_inflate(is_gzip:boolean;source:pointer;sourcesize:cardinal;var dest:pointer;var destsize,destalloc:cardinal):boolean;
const
  minbuffer=1024;
  maxbuffer=16384;
var
  sourceindex,sourcelen,destlen,checkcrc,checklen:cardinal;
  source_b,dest_b:^byte;
  inflateresult:longint;
  g:gzm_stream;
begin
 result:=false;
 dest:=nil;
 destsize:=0;
 destalloc:=0;
 sourceindex:=0;

 if is_gzip then if not read_gz_header(source,sourceindex,sourcesize) then exit;

 fillchar(g,sizeof(g),0);
 if is_gzip then g.crc:=crc32(0,nil,0);
 g.mode:='r';
 if inflateInit2_(g.stream,-MAX_WBITS,ZLIB_VERSION,sizeof(z_stream))<>Z_OK then exit; // windowBits is passed < 0 to tell that there is no zlib header

 source_b:=source;

 destalloc:=1024;
 for sourcelen:=1 to 10 do if destalloc<(sourcesize-sourceindex)*2 then destalloc:=destalloc*2 else break; // alloc a sensible default amount of memory, 1k to 1M
 destalloc:=16384;
 getmem(dest,destalloc);
 dest_b:=dest;

 repeat
  if destalloc-destsize<minbuffer then begin // grow
   destalloc:=destalloc*2;
   reallocmem(dest,destalloc);
   dest_b:=dest;
  end;

  g.stream.next_out:=@dest_b[destsize];
  if destalloc-destsize<maxbuffer then g.stream.avail_out:=destalloc-destsize else g.stream.avail_out:=maxbuffer;

  if g.stream.avail_in<=0 then begin
   if sourceindex>=sourcesize then begin inflateresult:=Z_DATA_ERROR; break; end; // error, we've run out of source data but haven't hit the end of the stream
  end;

  g.stream.next_in:=@source_b[sourceindex];
  if sourcesize-sourceindex<maxbuffer then g.stream.avail_in:=sourcesize-sourceindex else g.stream.avail_in:=maxbuffer;
  // 1..16k avail_in, 1k..16k avail_out

  sourcelen:=g.stream.avail_in;
  destlen:=g.stream.avail_out;
  inflateresult:=inflate(g.stream,Z_NO_FLUSH);
  sourcelen:=sourcelen-g.stream.avail_in; // compressed bytes we used
  destlen:=destlen-g.stream.avail_out; // uncompressed bytes we outputted
  if is_gzip and (destlen>0) then g.crc:=crc32(g.crc,@dest_b[destsize],destlen); // update crc
  inc(sourceindex,sourcelen);
  inc(destsize,destlen);
  if inflateresult<>Z_OK then break; // could be Z_STREAM_END
 until false;

 if inflateresult=Z_STREAM_END then begin // Check CRC and original size
  inflateresult:=Z_OK;
  if not is_gzip then begin
   if (g.stream.avail_in<>0) or (sourceindex<>sourcesize) then inflateresult:=Z_DATA_ERROR;
  end else begin
   if (g.stream.avail_in<>8) or (sourceindex<>sourcesize-8) then inflateresult:=Z_DATA_ERROR
   else begin
    source_b:=g.stream.next_in;
    checkcrc:=source_b[0]+(source_b[1] shl 8)+(source_b[2] shl 16)+(source_b[3] shl 24);
    checklen:=source_b[4]+(source_b[5] shl 8)+(source_b[6] shl 16)+(source_b[7] shl 24);
    g.stream.avail_in:=0;
    if (g.crc<>checkcrc) or (g.stream.total_out<>checklen) then inflateresult:=Z_DATA_ERROR;
   end;
  end;
 end;

 if (g.stream.state<>nil) then inflateEnd(g.stream);
 result:=inflateresult=Z_OK;
 dest:=dest_b;
end;

function zlib_inflate_str(is_gzip:boolean;var s:string):boolean;
var
  output:pointer;
  outsize,outalloc:cardinal;
begin
 outsize:=0;
 outalloc:=0;
 output:=nil;
 if zlib_inflate(is_gzip,@s[1],length(s),output,outsize,outalloc) then begin
  setlength(s,outsize);
  if outsize>0 then move(output^,s[1],outsize);
  result:=true;
 end else result:=false;
 if outalloc>0 then freemem(output,outalloc);
end;


end.

