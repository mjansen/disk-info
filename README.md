# disk-info
maintain checksum information about files on a disk, or just a directory.

This is just a simple utility to help keep an inventory of the contents of a disk.
This is useful if you need to have multiple copies of some data.
It is expected that rsync is used to copy disks, but we also need to have checksums (here: md5sum) of all the files in a manifest.

## todo

- [ ] verify checksums
- [ ] sync disks
- [ ] compare disks
- [ ] try to make the use of external commands optional (find, md5sum)
