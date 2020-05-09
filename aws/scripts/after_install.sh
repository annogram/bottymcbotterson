#!/bin/bash
HOME=/home/ubuntu
LOG=$HOME/deploy.log

/bin/echo "$(date '+%Y-%m-%d'): ** After Install Hook Started **" >> $LOG
/bin/echo "$(date '+%Y-%m-%d'): Changing owner and group of application... " >> $LOG

# verify that the application directory has the correct owner/group
/usr/bin/sudo /bin/chown -R ubuntu:ubuntu /home/ubuntu/botty_mc_botterson
echo -e "Done" >> $LOG

/bin/echo "$(date '+%Y-%m-%d %X'): ** After Install Hook Completed **" >> $LOG