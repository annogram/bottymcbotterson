#!/bin/bash
HOME=/home/ec2-user
APP=$HOME/botty_mc_botterson
LOG=$HOME/deploy.log
/bin/echo "$(date '+%Y-%m-%d %X'): ** Before Install Hook Started **" >> $LOG

# Do some actions before the installation

/bin/echo "$(date '+%Y-%m-%d %X'): ** Before Install Hook Completed **" >> $LOG