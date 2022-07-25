# test-mongo
After cloning and cd-ing to the direct, build and run the tests with:

```
docker build . -t tester
docker run -it --rm localhost/tester
```

If needed you can adjust the connnection details passed as environment variables in the Dockerfile.