# Update ALSPAC dictionary

This can be done by running the following (from `/path/to/alspac/inst`:

Build the docker image that installs the `alspac` R package

```
docker build -t ralspac:latest .
```

Mount the R drive:

```
mkdir -p mnt
mount -t cifs -o user=gh13047 //central-gpfs.isys.bris.ac.uk/ALSPAC-Data mnt
```

Run the container

```
docker run -d --rm -e PASSWORD=123qwe -p 8787:8787 \
-v "$(pwd):/home/rstudio/alspac" \
-v "$(pwd)/mnt:/home/rstudio/mnt \
--name ralspac ralspac:latest 
```

Update the dictionary

```
docker exec -it \
-w /home/rstudio/alspac/inst \
ralspac \
Rscript create_dictionary_from_stata.R /home/rstudio/mnt 4
```
