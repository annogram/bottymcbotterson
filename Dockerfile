FROM alpine:3.11.5
COPY ./build ./build
WORKDIR ./build
ENTRYPOINT ["discord-bot-exe"]