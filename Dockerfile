FROM docker.pkg.github.com/snoyberg/snoyman.com/base-build:7adf81919bfd60d553b252004df4f8f391458b28 as build-app

RUN mkdir -p /artifacts/bin
COPY site /src/site
COPY stack.yaml /src/stack.yaml
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM docker.pkg.github.com/snoyberg/snoyman.com/base-run:2baa393d8e35596f9c2eb84bd4e833d745079eed

WORKDIR /app
CMD ["/usr/local/bin/snoymancom", "prod", "3000"]

COPY --from=build-app /artifacts/bin/snoymancom /usr/local/bin
