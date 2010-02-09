#!/bin/sh
# param1: projectname with no starting capitol letter
# param2: projectname with starting capitol letter
tdir=""
case $# in
 1) name=$1 ;;
 2) name=$2 ;;
 3) name=$2
    tdir=$3 ;;
 *) echo "Usage: $0 <projectname> <Projectname> [<target_dir>]\n"
    exit 1 ;;
esac

target=$1
files=$PWD/files

mkdir $tdir
cd $tdir
mkdir $target
cd $target

echo "creating folder structure ..."
#create folder structure
mkdir App
mkdir App/Controller
mkdir App/Model
mkdir App/template
mkdir App/template/Index
mkdir App/template/Layouts
mkdir App/View
mkdir Config
mkdir db
mkdir public
mkdir public/images
mkdir public/stylesheets
mkdir Test

echo "copying project files ..."
# create cabal file
echo "Name: $name\nVersion: 0.0.1\nDescription: My Hawk Project\nLicense: \nAuthor: \nCabal-Version: >=1.6\n\nExecutable: $target\n  ghc-options: -Wall\n  Build-Depends: \n  main-is: Main.hs" > $name.cabal

echo "{-# LANGUAGE TemplateHaskell #-}\n
module App.View.IndexView where\n
\n
import Hawk.Controller\n
import Hawk.View.Template.DataType\n
import qualified Hawk.View.Template.HtmlHelper as H\n
\n
\$(viewDataType \"Index\" \"index\")\n
\n
indexXhtml :: a -> StateController IndexIndex\n
indexXhtml _ =\n
  return IndexIndex\n
    { title = [H.text \"My Hawk Project: $name\"]
    }" > App/View/IndexView.hs

# .
cp $files/Main.hs .
cp $files/Setup.hs .
# App
cp $files/IndexController.hs App/Controller
cp $files/index.xhtml App/template/Index
cp $files/header.xhtml App/template/Layouts
cp $files/pageheader.xhtml App/template/Layouts
cp $files/pagefooter.xhtml App/template/Layouts
# Config
cp $files/Config.hs Config
cp $files/Routes.hs Config
# db
cp $files/database.db db
# public
cp $files/401.html public
cp $files/404.html public
cp $files/500.html public
# public/images
cp $files/favicon.ico public/images
cp $files/info.png public/images
cp $files/success.png public/images
cp $files/warning.png public/images
# public/stylesheets
cp $files/style.css public/stylesheets

echo "finished"

exit 0
