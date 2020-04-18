FROM alpine:3.7
COPY ./build ./build
WORKDIR ./build
ENTRYPOINT ["discord-bot-exe"]