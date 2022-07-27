# stack-ready ghc 9.0.2 compiled against musl
FROM nycticoracs/ghc-musl-with-stack as builder
WORKDIR /opt/app/
# build dependencies
COPY ./test-mongo.cabal ./stack.yaml ./
RUN stack build --fast --system-ghc --only-dependencies --no-library-profiling
# build package
COPY . .
RUN stack install --system-ghc --local-bin-path .
# runner
FROM alpine:latest as runner
WORKDIR /opt/app/
COPY --from=builder /opt/app/test-mongo-exe .
# run tests from the executable
CMD ./test-mongo-exe