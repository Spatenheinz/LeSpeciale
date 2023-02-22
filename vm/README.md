Using qemu to run Debian with a specific kernel
===============================================

1. Make a Debian image (a one time thing)

   Run `create-image.sh -d bullseye` to make a `bullseye` Debian
   image. The `create-image.sh` snagged from the syzkaller
   project. With the small addition that a `sudo` user `normie` is
   added.

   The will create the files `bullseye.img`, `bullseye.id_rsa` and
   `bullseye.id_rsa.pub`.

2. Make a kernel

    * `git clone --branch v6.2-rc8 git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git $KERNEL`
    * Generate default configs

      ```
      cd $KERNEL
      make defconfig
      make kvm_guest.config
      ```

    * Enable required config options. Edit `.config` file manually and
      enable them (or do that through make menuconfig if you prefer).

      ```
      CONFIG_CONFIGFS_FS=y
      CONFIG_SECURITYFS=y
      CONFIG_E1000=y
      CONFIG_BINFMT_MISC=y
      CONFIG_BPF=y
      CONFIG_HAVE_EBPF_JIT=y
      CONFIG_ARCH_WANT_DEFAULT_BPF_JIT=y
      CONFIG_BPF_SYSCALL=y
      CONFIG_BPF_JIT=y
      CONFIG_BPF_JIT_ALWAYS_ON=y
      CONFIG_BPF_JIT_DEFAULT_ON=y
      CONFIG_BPFILTER=y
      CONFIG_BPF_EVENTS=y
      CONFIG_CMDLINE_BOOL=y
      ```

    * Regenerate config

      ```
      make olddefconfig
      ```

    * Build the kernel

      ```
      make -j`nproc`
      ```

    * Copy the kernel to the same directory, `$DIR`, as the Debian
      image and the `start-qemu.sh` script.

      ```
      cp $KERNEL/arch/x86/boot/bzImage $DIR
      ```

3. Run `start-qemu.sh` in the same directory, `$DIR`, as the Debian
   image and the `bzImage` kernel.

4. In an other terminal you can now ssh into the VM:

   ```
   ssh -i $DIR/bullseye.id_rsa -p 10021 -o "StrictHostKeyChecking no" normie@localhost
   ```

5. You can mount the home folder of the `normie` user with sshfs:
  ```
  sshfs normie@localhost:/home/normie /mnt/speciale -o IdentityFile=bullseye.id_rsa -p 10021
  ```


Notes about what Mads has done and stuff that doesn't work
========
- Obtaining the kernel:
        `git checkout v5.11`

- Patching the kernel to reintroduce the bug:
        There is a patch-file, `vm/stuff/verifier.patch`. 
        Apply it using `git apply verifier.patch`.
        
        
- Building the kernel:
        I installed and symlinked gcc-8 to `/usr/bin/gcc` (in the vagrant-box).
        `sudo apt install gcc-8`
        `sudo ln -sf gcc-8 /usr/bin/gcc`

- Installing qemu:
        `sudo apt install qemu-system-x86`

- bpftool: 
        libbfd in bullseye is version libbfd-2.31.1-system.so
        We build with libbfd-2.37-system.so in the ubuntu-vagrant box. 
        `bpftool` is available from buster-backports
        `sudo echo "deb https://deb.debian.org/debian buster-backports main" >> /etc/apt/sources.list`
        sadly, the distributed version does not have libbfd-support, so we can only dump xlated
        programs, not JITed programs. 
        I think we need to build bpftool in the buster-image.

- Building and running the `run-bpf` go-program from `https://github.com/cloudflare/cloudflare-blog/tree/master/2019-04-ebpf-alu32`:
        This was a messy experience. 
        Process:
            - Clone the entire repo, copy the specific folder `2019-04-ebpf-alu32` into the buster-image.
            - Install `clang`, `llvm`, `golang`, `ninja-build` and `git`. (I might have forgotten a dependency)
            - Building requires kernel headers. 
              The kernel headers are not available in the vm, 
              since we built the kernel ourselves outside the vm. 
              I copied a bunch of headers from the vagrant-box by hand, including 
              - `/usr/include/asm/types.h` 
              - `/usr/include/asm/bitsperlong.h`
              - `/usr/include/asm/posix_types.h`
              - `/usr/include/asm/posix_types_64.h`
            
            I am surprised it worked. 
