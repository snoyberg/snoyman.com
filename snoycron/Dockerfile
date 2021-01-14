FROM rust:1.49.0 as build

COPY . /src
RUN mkdir /out
RUN cargo install --path /src --root /out

# Should use something lighter-weight, but this solves the system lib issue nicely
# Maybe I should just use rustls instead...
FROM rust:1.49.0

COPY --from=build /out/bin/snoycron /usr/local/bin/snoycron
