FROM haskell:9.0.2 as builder
WORKDIR /opt/app/
COPY ./test-mongo.cabal ./stack.yaml ./
# dependencies layer
RUN stack upgrade && stack build --fast --only-dependencies --no-library-profiling
# main layer
COPY . .
RUN stack build --fast

ENV hname=cluster0.xpugk.mongodb.net
ENV dname=test-db
ENV uname=tester
ENV pword=95BqF22T78gUUanH
ENV coll=test-collection

CMD stack test --fast