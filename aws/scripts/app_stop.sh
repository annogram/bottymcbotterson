#!/bin/bash

# This example assumes the sample 'my_app.sh' script has been added to the the directory 'my_app' and serves as the application launcher

HOME=/home/ubuntu
APP=$HOME/botty_mc_botterson
PIDF=$APP/botty_mc_start.pid
CMD=$APP/botty_mc_start.sh
LOG=$HOME/deploy.log

/bin/echo "$(date '+%Y-%m-%d %X'): ****************************************************************" >> $LOG
/bin/echo "$(date '+%Y-%m-%d %X'): Initializing Deployment for $APPLICATION_NAME - $DEPLOYMENT_ID " >> $LOG
/bin/echo "$(date '+%Y-%m-%d %X'): ****************************************************************" >> $LOG
/bin/echo "$(date '+%Y-%m-%d %X'): ** Application Stop Hook Started **" >> $LOG
/bin/echo "$(date '+%Y-%m-%d %X'): Event: $LIFECYCLE_EVENT" >> $LOG

cd $APP

if [ -f $CMD ]
then
    $CMD stop
    /bin/echo "$(date '+%Y-%m-%d %X'): Stopping $APPLICATION_NAME" >> $LOG
elif [ -f $PIDF ]
then
    PID=`cat $PIDF`
    kill -9 $PID
    /bin/echo "$(date '+%Y-%m-%d %X'): Killing $APPLICATION_NAME [$PID]" >> $LOG
    rm $PIDF
else
    /bin/echo "$(date '+%Y-%m-%d %X'): $CMD not found. Proceeding with deployment" >> $LOG
fi
/bin/echo "$(date '+%Y-%m-%d %X'): ** Application Stop Hook Completed **" >> $LOG