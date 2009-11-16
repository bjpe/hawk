-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Mime
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3
   
   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $
  
   Static map for MIME-Types
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Mime where

import qualified Data.Map as M
  ( Map
  , lookup
  , fromList
  )

import Data.Maybe (fromMaybe)

getMimeType :: String -> String
getMimeType = fromMaybe "text/plain" . lookupMimeType

lookupMimeType :: String -> Maybe String
lookupMimeType = flip M.lookup mimeTypes

mimeTypes :: M.Map String String
mimeTypes = M.fromList
   [  x     ".3gp"          "video/3gpp"
   ,  x     ".a"            "application/octet-stream"
   ,  x     ".ai"           "application/postscript"
   ,  x     ".aif"          "audio/x-aiff"
   ,  x     ".aiff"         "audio/x-aiff"
   ,  x     ".asc"          "application/pgp-signature"
   ,  x     ".asf"          "video/x-ms-asf"
   ,  x     ".asm"          "text/x-asm"
   ,  x     ".asx"          "video/x-ms-asf"
   ,  x     ".atom"         "application/atom+xml"
   ,  x     ".au"           "audio/basic"
   ,  x     ".avi"          "video/x-msvideo"
   ,  x     ".bat"          "application/x-msdownload"
   ,  x     ".bin"          "application/octet-stream"
   ,  x     ".bmp"          "image/bmp"
   ,  x     ".bz2"          "application/x-bzip2"
   ,  x     ".c"            "text/x-c"
   ,  x     ".cab"          "application/vnd.ms-cab-compressed"
   ,  x     ".cc"           "text/x-c"
   ,  x     ".chm"          "application/vnd.ms-htmlhelp"
   ,  x     ".class"        "application/octet-stream"
   ,  x     ".com"          "application/x-msdownload"
   ,  x     ".conf"         "text/plain"
   ,  x     ".cpp"          "text/x-c"
   ,  x     ".crt"          "application/x-x509-ca-cert"
   ,  x     ".css"          "text/css"
   ,  x     ".csv"          "text/csv"
   ,  x     ".cxx"          "text/x-c"
   ,  x     ".deb"          "application/x-debian-B.package"
   ,  x     ".der"          "application/x-x509-ca-cert"
   ,  x     ".diff"         "text/x-diff"
   ,  x     ".djv"          "image/vnd.djvu"
   ,  x     ".djvu"         "image/vnd.djvu"
   ,  x     ".dll"          "application/x-msdownload"
   ,  x     ".dmg"          "application/octet-stream"
   ,  x     ".doc"          "application/msword"
   ,  x     ".dot"          "application/msword"
   ,  x     ".dtd"          "application/xml-dtd"
   ,  x     ".dvi"          "application/x-dvi"
   ,  x     ".ear"          "application/java-archive"
   ,  x     ".eml"          "message/rfc822"
   ,  x     ".eps"          "application/postscript"
   ,  x     ".exe"          "application/x-msdownload"
   ,  x     ".f"            "text/x-fortran"
   ,  x     ".f77"          "text/x-fortran"
   ,  x     ".f90"          "text/x-fortran"
   ,  x     ".flv"          "video/x-flv"
   ,  x     ".for"          "text/x-fortran"
   ,  x     ".gem"          "application/octet-stream"
   ,  x     ".gemspec"      "text/x-script.ruby"
   ,  x     ".gif"          "image/gif"
   ,  x     ".gz"           "application/x-gzip"
   ,  x     ".h"            "text/x-c"
   ,  x     ".hh"           "text/x-c"
   ,  x     ".htm"          "text/html"
   ,  x     ".html"         "text/html"
   ,  x     ".ico"          "image/vnd.microsoft.icon"
   ,  x     ".ics"          "text/calendar"
   ,  x     ".ifb"          "text/calendar"
   ,  x     ".iso"          "application/octet-stream"
   ,  x     ".jar"          "application/java-archive"
   ,  x     ".java"         "text/x-java-source"
   ,  x     ".jnlp"         "application/x-java-jnlp-file"
   ,  x     ".jpeg"         "image/jpeg"
   ,  x     ".jpg"          "image/jpeg"
   ,  x     ".js"           "application/javascript"
   ,  x     ".json"         "application/json"
   ,  x     ".log"          "text/plain"
   ,  x     ".m3u"          "audio/x-mpegurl"
   ,  x     ".m4v"          "video/mp4"
   ,  x     ".man"          "text/troff"
   ,  x     ".mathml"       "application/mathml+xml"
   ,  x     ".mbox"         "application/mbox"
   ,  x     ".mdoc"         "text/troff"
   ,  x     ".me"           "text/troff"
   ,  x     ".mid"          "audio/midi"
   ,  x     ".midi"         "audio/midi"
   ,  x     ".mime"         "message/rfc822"
   ,  x     ".mml"          "application/mathml+xml"
   ,  x     ".mng"          "video/x-mng"
   ,  x     ".mov"          "video/quicktime"
   ,  x     ".mp3"          "audio/mpeg"
   ,  x     ".mp4"          "video/mp4"
   ,  x     ".mp4v"         "video/mp4"
   ,  x     ".mpeg"         "video/mpeg"
   ,  x     ".mpg"          "video/mpeg"
   ,  x     ".ms"           "text/troff"
   ,  x     ".msi"          "application/x-msdownload"
   ,  x     ".odp"          "application/vnd.oasis.opendocument.presentation"
   ,  x     ".ods"          "application/vnd.oasis.opendocument.spreadsheet"
   ,  x     ".odt"          "application/vnd.oasis.opendocument.text"
   ,  x     ".ogg"          "application/ogg"
   ,  x     ".p"            "text/x-pascal"
   ,  x     ".pas"          "text/x-pascal"
   ,  x     ".pbm"          "image/x-portable-bitmap"
   ,  x     ".pdf"          "application/pdf"
   ,  x     ".pem"          "application/x-x509-ca-cert"
   ,  x     ".pgm"          "image/x-portable-graymap"
   ,  x     ".pgp"          "application/pgp-encrypted"
   ,  x     ".pkg"          "application/octet-stream"
   ,  x     ".pl"           "text/x-script.perl"
   ,  x     ".pm"           "text/x-script.perl-module"
   ,  x     ".png"          "image/png"
   ,  x     ".pnm"          "image/x-portable-anymap"
   ,  x     ".ppm"          "image/x-portable-pixmap"
   ,  x     ".pps"          "application/vnd.ms-powerpoint"
   ,  x     ".ppt"          "application/vnd.ms-powerpoint"
   ,  x     ".ps"           "application/postscript"
   ,  x     ".psd"          "image/vnd.adobe.photoshop"
   ,  x     ".py"           "text/x-script.python"
   ,  x     ".qt"           "video/quicktime"
   ,  x     ".ra"           "audio/x-pn-realaudio"
   ,  x     ".rake"         "text/x-script.ruby"
   ,  x     ".ram"          "audio/x-pn-realaudio"
   ,  x     ".rar"          "application/x-rar-compressed"
   ,  x     ".rb"           "text/x-script.ruby"
   ,  x     ".rdf"          "application/rdf+xml"
   ,  x     ".roff"         "text/troff"
   ,  x     ".rpm"          "application/x-redhat-B.package-manager"
   ,  x     ".rss"          "application/rss+xml"
   ,  x     ".rtf"          "application/rtf"
   ,  x     ".ru"           "text/x-script.ruby"
   ,  x     ".s"            "text/x-asm"
   ,  x     ".sgm"          "text/sgml"
   ,  x     ".sgml"         "text/sgml"
   ,  x     ".sh"           "application/x-sh"
   ,  x     ".sig"          "application/pgp-signature"
   ,  x     ".snd"          "audio/basic"
   ,  x     ".so"           "application/octet-stream"
   ,  x     ".svg"          "image/svg+xml"
   ,  x     ".svgz"         "image/svg+xml"
   ,  x     ".swf"          "application/x-shockwave-flash"
   ,  x     ".t"            "text/troff"
   ,  x     ".tar"          "application/x-tar"
   ,  x     ".tbz"          "application/x-bzip-compressed-tar"
   ,  x     ".tcl"          "application/x-tcl"
   ,  x     ".tex"          "application/x-tex"
   ,  x     ".texi"         "application/x-texinfo"
   ,  x     ".texinfo"      "application/x-texinfo"
   ,  x     ".text"         "text/plain"
   ,  x     ".tif"          "image/tiff"
   ,  x     ".tiff"         "image/tiff"
   ,  x     ".torrent"      "application/x-bittorrent"
   ,  x     ".tr"           "text/troff"
   ,  x     ".txt"          "text/plain"
   ,  x     ".vcf"          "text/x-vcard"
   ,  x     ".vcs"          "text/x-vcalendar"
   ,  x     ".vrml"         "model/vrml"
   ,  x     ".war"          "application/java-archive"
   ,  x     ".wav"          "audio/x-wav"
   ,  x     ".wma"          "audio/x-ms-wma"
   ,  x     ".wmv"          "video/x-ms-wmv"
   ,  x     ".wmx"          "video/x-ms-wmx"
   ,  x     ".wrl"          "model/vrml"
   ,  x     ".wsdl"         "application/wsdl+xml"
   ,  x     ".xbm"          "image/x-xbitmap"
   ,  x     ".xhtml"        "application/xhtml+xml"
   ,  x     ".xls"          "application/vnd.ms-excel"
   ,  x     ".xml"          "application/xml"
   ,  x     ".xpm"          "image/x-xpixmap"
   ,  x     ".xsl"          "application/xml"
   ,  x     ".xslt"         "application/xslt+xml"
   ,  x     ".yaml"         "text/yaml"
   ,  x     ".yml"          "text/yaml"
   ,  x     ".zip"          "application/zip"
   ]
   where x a b = (a, b)
