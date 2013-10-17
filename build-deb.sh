#!/bin/bash
#
# Authors: Jorge Espada <jespada@yuilop.com> & Manuel Rubio <manuel@yuilop.com>
#
# You need to have installed rubygems, fpm[0] gem (gem install fpm) and build-essential
# [0] https://github.com/jordansissel/fpm/wiki

USER=ebot
GROUP=ebot

INSTDIR=$(pwd)/installdir
FPM=$(gem which fpm | sed 's/\/lib\/fpm.rb/\/bin\/fpm/g')
TAG=$(git describe --always --tag)

if [ ! -z "$1" ]; then
    TAG="$1"
fi

#check if gem and fpm are installed
echo "You must have rubygems, fpm, and build-essential installed..."

gem list --local | grep fpm

if [[ $? -ne 0 ]]; then
    echo "Please verify the output of: gem list --local | grep fpm , remember you need tubygems and fpm installed"
    exit 1
fi

#clean compile and make the package
rm -rf deps/*
rm -rf apps/*/logs
rm -rf apps/test/*.beam
rebar clean get-deps compile generate

if [[ $? -ne 0 ]]; then
    echo "Please check dependencies, compelation went wrong"
    exit 1
fi

rm -rf $INSTDIR
mkdir -p $INSTDIR/ebot
cp -a rel/ebot/* $INSTDIR/ebot

#build the package
pushd $INSTDIR
$FPM -s dir -t deb -n ebot -v $TAG -C $INSTDIR --description "Regular xmpp bot" -p ebot-VERSION_ARCH.deb --config-files ebot/etc/app.config --prefix /opt --deb-user $USER --deb-group $GROUP --maintainer '"Pepe" <jlnavarro111@gmail.com>' ebot

