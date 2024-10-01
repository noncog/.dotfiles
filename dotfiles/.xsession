#!/bin/sh
exec systemctl --user start --wait xsession.target
