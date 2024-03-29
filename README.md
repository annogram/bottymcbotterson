# BottyMcBotterson

A discord bot to collect useful information on my discord servers, also serves as learnings and showings for my haskell language training.

To add this bot to your own server use the following [link](https://discordapp.com/oauth2/authorize?client_id=348437835382259723&scope=bot&permissions=350272) (minimum permissions are given for functionality)

## Features

1. Integrates with COVID api to retrieve up to date data

![covid](res/covid.png)

2. Multithreaded and quick, Botty compiles to an executable file that runs withought needing a `node`, `python`, `java`, `or`, `.net` runtime
3. Dynamically populated `/help` command that is populated based on the features added to the bot

![help](res/help.png)

4. Quirky but usable interactions, the bot will notify you immediately when it has understood your command and will let you know
5. Flashpolls so users can vote on whatever they want

![poll](res/poll.png)

# Prerequisites

To compile and build this project you will need

- Haskell stack
- A discord application setup at - https://discordapp.com/developers/
- An environment variable for the bot stored in the variable `DISCORD_CLIENT_SECRET`

# To build

Open the root directory in terminal or VSCode and run `stack build` this will pull down the GHC version this project is using and the dependencies

# To publish

This project is published using gitlab ci/ci. The `.gitlab-ci.yml` has the build and deployment steps.

## To host

Ideally you'd want this hosted on raspberry pi, but it can be hosted on any compute service (AWS, Azure, GCP even heroku)

### On a pi

Compile the linux binary and upload it to your pi, setup the process within `systemd` google it. do it this way it will always boot on the correct stage of the boot process, you can also debug boot errors

1. Create a new service in the directory `/lib/systemd/system/discord.service`
2. make sure the linux executable is set to execute mode in the pi
3. use the following template to setup the service code. 
```ini
 [Unit]
 Description=My Sample Service
 After=multi-user.target

 [Service]
 Type=idle
 ExecStart=/usr/bin/python /home/pi/sample.py

 [Install]
 WantedBy=multi-user.target
```
- Add logging to a text file if you don't like the boot logging that comes with `systemd`
- `/home/pi/runtime > /home/pi/runtime.log 2>&1` this will log error and logs
4. Add the new service to the boot with the following commands
```bash
sudo systemctl daemon-reload
sudo systemctl enable sample.service
```
5. Reboot the pi. the service should be running when you start
    - `systemctl` is the tool you want to use to see running services
    - `journalctl` will give you the logs of a running process

## More detailed information

Please see the wiki for more detailed information on this project
