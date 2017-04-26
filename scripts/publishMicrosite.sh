#!/usr/bin/env bash
set -e

git config --global user.email "jonnylaw"
git config --global user.name "law.jonny@googlemail.com"
git config --global push.default simple

sbt docs/publishMicrosite