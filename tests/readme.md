## Test the `alspac` R package

Test the `extractVars` and `extractDataset` functions
in the current environment by running
```
make all
```

To run the tests in a docker container with a specific version of R,
first edit the ALSPACDIR variable in `run-tests.sh`
to the ALSPAC data directory. 

Then run `run-tests.sh`.
```
bash run-tests.sh
```
Output files will appear in the `outputs` directory.

This script will first build a container:
```
docker build --build-arg WHEN=2020-06-22 -t alspac-r-test .
```

It will then run the tests in the container:
```
docker run -v $ALSPACDIR:/home/alspac -v $CURRENTDIR:/home/example alspac-r-test "cd /home/example; make all"
```

To debug an example interactively:
```
docker run -it -v $ALSPACDIR:/home/alspac -v $CURRENTDIR:/home/example alspac-r-test /bin/bash
```



