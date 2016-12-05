FROM haskell:8.0

RUN useradd -m app && useradd -m build

WORKDIR /build
COPY *.cabal stack.yaml ./
RUN chown -R build:build /build
USER build
RUN stack update
RUN stack install --dependencies-only

USER root
COPY . .
RUN chown -R build:build /build
USER build
RUN stack install

USER root
WORKDIR /app
RUN cp /home/build/.local/bin/drstun /app/

USER app
EXPOSE 3478
ENTRYPOINT ["/app/drstun"]
