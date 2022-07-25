FROM haskell:9.0.2 as builder
WORKDIR /opt/app/
# build dependencies
COPY ./test-mongo.cabal ./stack.yaml ./
RUN stack upgrade && stack build --fast --only-dependencies --no-library-profiling
# build package
COPY . .
RUN stack build
# set up ENVvars
# - hostname
# - database name
# - user name
# - user password
# - test collection
ENV hname=cluster0.xpugk.mongodb.net
ENV dname=test-db
ENV uname=tester
ENV pword=95BqF22T78gUUanH
ENV coll=test-collection
# run tests
CMD stack test