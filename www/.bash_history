su - root
sudo apt-get update
sudo apt-get install vsftpd
sudo cp /etc/vsftpd.conf /etc/vsftpd.conf.orig
sudo ufw status
sudo adduser ardmin
sudo mkdir /home/ardmin/ftp
sudo chown nobody:nogroup /home/ardmin/ftp
sudo chmod a-w /home/ardmin/ftp
sudo ls -la /home/ardmin/ftp
sudo mkdir /home/ardmin/ftp/files
sudo chown ardmin:ardmin /home/ardmin/ftp/files
sudo ls -la /home/ardmin/ftp
echo "vsftpd test file" | sudo tee /home/ardmin/ftp/files/test.txt
sudo nano /etc/vsftpd.conf
echo "ardalan" | sudo tee -a /etc/vsftpd.userlist
echo "ardmin" | sudo tee -a /etc/vsftpd.userlist
cat /etc/vsftpd.userlist
sudo systemctl restart vsftpd
ftp -p 203.0.113.0
ftp -p 167.99.130.74
echo "userlist_deny=NO" >> /etc/vsftpd/vsftpd.conf 
sudo echo "userlist_deny=NO" >> /etc/vsftpd/vsftpd.conf 
sudo vi /etc/vsftpd.conf
sudo nano /etc/vsftpd.conf
sudo systemctl restart vsftpd
sudo systemctl enable vsftpd
sudo ufw allow ftp
sudo ufw status
sudo ufw allow 20/tcp
sudo ufw reload
service vsftpd status
sudo nano /etc/vsftpd/user_list
cat  /etc/vsftpd/user_list
vi /etc/vsftpd/vsftpd.conf
sudo vi /etc/vsftpd.conf
su - ardmin
cd /srv/ftp
ls
sudo touch default{1..10}
ls
root
su - root
