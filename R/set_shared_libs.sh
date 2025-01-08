#! /bin/bash
cd ~/..
sudo mkdir shared
sudo groupadd -gid 1007 dst_devs
sudo usermod -a -G dst_devs pmelloy
sudo usermod -a -G dst_devs shiny
sudo chown pmelloy:dst_devs
mkdir shared/rlibs
sudo chmod -R 765 /home/shared
echo R_LIBS_USER="/homevol/shared/rlibs" >> /etc/environment
echo R_LIBS_USER="/homevol/shared/rlibs" >> ~/.Renviron
