# Update ALSPAC dictionary

Each time the R drive files are updated (e.g. in the `Current` or `Useful_data` folders) we need to collate all the meta data from each of the stata files. The `create_dictionary_from_stata.R` automates this process. But the script needs access to the R drive, and ideally runs in parallel and not through the VPN (which is very slow).

Solution is to run the script on the `crashdown.epi.bris.ac.uk` docker cluster. Steps are:

1. Create the Docker environment to run the script (just need to do this once)
2. Mount the R drive, run the script within Docker, unmount the R drive (do this for every update)
3. Update the GitHub repository to a new version and push
4. Update the Shiny app


## 1. Create docker environment

This can be done by running the following (from `/path/to/alspac/inst`). Build the docker image that installs the `alspac` R package

```
sudo docker build -t ralspac:latest .
```

## 2. Update the package

Mount the R drive using a samba share:

```
mkdir -p mnt
sudo mount -t cifs -o user=$USER,vers=3.0 //central-gpfs.isys.bris.ac.uk/ALSPAC-Data mnt
```

(Note that on the crashdown server `crashdown.epi.bris.ac.uk` needed to install the `cifs-utils` package in order to mount (e.g. `sudo apt-get install cifs-utils`).)

Update the dictionary within the container

```
cd ../
sudo docker run --rm -e PASSWORD=123qwe -p 8787:8787 \
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

This should have edited one or both of the following files:

```
data/current.rdata
data/useful.rdata
```

## 3. Commit and update package version

1. See the history of versions using `git tag`
2. Edit the `../DESCRIPTION` file to bump to the next version
3. Commit the changes to the `rdata` files and the `DESCRIPTION` file
4. Tag the commit `git tag <new tag>`
5. Push `git push origin <new tag>`

## 4. Update Shiny app

See info here https://github.com/explodecomputer/alspac-shiny

