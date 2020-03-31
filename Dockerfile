FROM fpco/pid1:18.04 as base-run

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  netbase

FROM base-build as build-app

RUN mkdir -p /artifacts/bin
COPY site /src/site
COPY stack.yaml /src/stack.yaml
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM base-run

ENV PORT 3000
WORKDIR /app
CMD ["/usr/local/bin/snoymancom"]

COPY --from=build-app /artifacts/bin/snoymancom /usr/local/bin

COPY content /app/content
