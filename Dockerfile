FROM snoyberg/snoyman.com-base:e28104f640df94f6a2fb178716ced0ab52a8fede as build-app

RUN mkdir -p /artifacts/bin
COPY . /app
RUN cd /app && stack install --local-bin-path /artifacts/bin

FROM fpco/pid1:18.04

COPY --from=build-app /artifacts/bin/snoymancom /usr/local/bin
