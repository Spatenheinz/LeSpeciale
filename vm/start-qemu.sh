#!/usr/bin/env sh

# If you are running linux with kvm enabled consider adding -enable-kvm

qemu-system-x86_64 \
	-m 2G \
	-smp 2 \
	-kernel ./bzImage \
	-append "console=ttyS0 root=/dev/sda earlyprintk=serial net.ifnames=0" \
	-drive file=./bullseye.img,format=raw \
	-net user,host=10.0.2.10,hostfwd=tcp:127.0.0.1:10021-:22 \
        -net nic,model=virtio \
	-nographic \
	-pidfile vm.pid \
	-enable-kvm \
	-virtfs local,path=$1,security_model=passthrough,mount_tag=hostshare \
	2>&1 | tee vm.log

# qemu-system-x86_64 \
# 	-m 2G \
# 	-smp 2 \
# 	-kernel ./bzImage \
# 	-append "console=ttyS0 root=/dev/sda earlyprintk=serial net.ifnames=0" \
# 	-drive file=./buster.img,format=raw \
# 	-net user,host=10.0.2.10,hostfwd=tcp:127.0.0.1:10021-:22 \
#         -net nic,model=virtio \
# 	-nographic \
# 	-pidfile vm.pid \
# 	2>&1 | tee vm.log
