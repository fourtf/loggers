FROM haskell:8.6.5 as mybuild
WORKDIR /opt/loggers
RUN cabal update
COPY ./loggers.cabal /opt/loggers/loggers.cabal
RUN cabal install --only-dependencies -j4
COPY ./src /opt/loggers/src
COPY ./LICENSE /opt/loggers/LICENSE
RUN cabal install
RUN strip /root/.cabal/bin/loggers


FROM debian:stretch
RUN apt update
RUN apt install -y --no-install-recommends libgmp10 ca-certificates
COPY --from=mybuild /root/.cabal/bin/loggers /app/loggers
ENTRYPOINT ["/app/loggers"]
