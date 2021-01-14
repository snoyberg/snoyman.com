FROM rust:1.49.0 as build

COPY . /src
RUN mkdir /out
RUN cargo install --path /src --root /out

FROM fpco/pid1:18.04

COPY --from=build /out/bin/snoycron /usr/local/bin/snoycron
