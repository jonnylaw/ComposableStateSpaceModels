#!/bin/sh

openssl aes-256-cbc -K $encrypted_31c5b797caf8_ke -iv $encrypted_31c5b797caf8_iv -in travis-deploy-key.enc -out travis-deploy-key -d;
chmod 600 travis-deploy-key;
cp travis-deploy-key ~/.ssh/id_rsa;