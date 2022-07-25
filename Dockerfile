FROM haskell:9.0.2 as builder
WORKDIR /opt/app/
# build dependencies
COPY ./test-mongo.cabal ./stack.yaml ./
RUN stack upgrade && stack build --fast --only-dependencies --no-library-profiling
# build package
COPY . .
RUN stack build
# run tests
CMD stack test