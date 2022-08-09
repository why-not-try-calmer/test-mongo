# test-mongo

## Setup
There is a Docker image available for [testing](https://github.com/why-not-try-calmer/ringo/pkgs/container/test-mongo).

```
docker pull ghcr.io/why-not-try-calmer/test-mongo:master
docker run -it -t test-mongo --rm --env-file ./ENVARS.txt
```
NB:`--env-file` must reference a file containing the credentials to be passed as environment variables to the container -- see `ENVARS.txt` in the present repository.

For example, this tries to connect to a __ReplicaSet__:

```
is_replica_set=true
hname=cluster0.xpugk.mongodb.net
dname=test-db
uname=tester
pword=95BqF22T78gUUanH
coll=test-collection
```

while this tries to connect to a __standard Mongo database server__:

```
is_replica_set=false
hname=127.0.0.1
dname=test-db
uname=tester
pword=95BqF22T78gUUanH
coll=test-collection
```

## Testing
The test will ensure two preliminaies:
1. It will parse the strings passed in ENVARS.txt, instantiate a Config, try to connect and authenticate.
2. It will delete all items on the target collection.

At this point the test will exist if (1) failed, but nothing will be reported regarding (2).

3. It will test writes, reads and deletions. At this stage each failure or success is reported independently.

## Building
After cloning and cd-ing to the direct, build with:

```
docker build -t test-mongo
```

See "Testing" above to run the tests.
