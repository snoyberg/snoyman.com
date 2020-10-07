FROM docker.pkg.github.com/snoyberg/snoyman.com/base-build:7adf81919bfd60d553b252004df4f8f391458b28 as build-app

RUN mkdir -p /artifacts/bin
COPY site /src/site
COPY stack.yaml /src/stack.yaml
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM docker.pkg.github.com/snoyberg/snoyman.com/base-run:b41a44cd43b49ca308affb3fd327ce285ab70d49

WORKDIR /app
CMD ["/usr/local/bin/snoymancom", "prod", "3000"]

COPY --from=build-app /artifacts/bin/snoymancom /usr/local/bin
