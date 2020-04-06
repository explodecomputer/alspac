# Update ALSPAC dictionary

This can be done by running the following (from `/path/to/alspac/inst`:

Build the docker image that installs the `alspac` R package

```
docker build -t ralspac:latest .
```

Mount the R drive:

```
mkdir -p mnt
sudo mount -t cifs -o user=gh13047 //central-gpfs.isys.bris.ac.uk/ALSPAC-Data mnt
```

Update the dictionary within the container

```
cd ../
docker run --rm -e PASSWORD=123qwe -p 8787:8787 \
-v "$(pwd):/home/rstudio/alspac" \
-v "$(pwd)/inst/mnt:/home/rstudio/mnt" \
-w /home/rstudio/alspac/inst \
--name ralspac ralspac:latest \
Rscript create_dictionary_from_stata.R /home/rstudio/mnt 4
```

Unmount the R drive

```
sudo umount inst/mnt
```

Commit and update package version

