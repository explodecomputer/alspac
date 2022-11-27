## Test the `alspac` R package

To run the tests in a docker container with a specific version of R,
first edit the ALSPACDIR variable in `run-tests.sh`
to the ALSPAC data directory. 

Then run `run-tests.sh`.
```
bash run-tests.sh
```
Output files will appear in the `outputs` directory.
This script will first build a container,
and then run the tests in the container.

To debug an example interactively:
```
docker run -it -v $ALSPACDIR:/home/alspac -v $CURRENTDIR:/home/example alspac-r-test /bin/bash
```



