#!/bin/bash
HOME=/home/ubuntu
APP=$HOME/botty_mc_botterson
CMD=$APP/botty_mc_start.sh
LOG=$HOME/deploy.log

# This example assumes the sample 'botty_mc_start.sh' script has been added to the the directory 'botty_mc_botterson' and serves as the application launcher

/bin/echo "$(date '+%Y-%m-%d %X'): ** Application Start Hook Started **" >> $LOG
/bin/echo "$(date '+%Y-%m-%d %X'): Event: $LIFECYCLE_EVENT" >> $LOG

pwd >> $LOG
cd $APP

if [ -f $CMD ]
then
    echo $APP >> $LOG
    pwd >> $LOG
    $CMD start
    /bin/echo "$(date '+%Y-%m-%d %X'): Starting $APPLICATION_NAME" >> $LOG
else
    /bin/echo "$(date '+%Y-%m-%d %X'): $CMD not found. Proceeding with deployment" >> $LOG
fi
/bin/echo "$(date '+%Y-%m-%d %X'): ** Application Start Hook Completed **" >> $LOG
